library(tidyverse)
library(lubridate)
library(readxl)
library(httr)
library(gridExtra)
library(ggiraphExtra)
library(leaps)
library(caret) #Package to perform leave-one-out cross validation
library(ggrepel)
library(scatterplot3d)
library(glmnet)
library(lars)
library(splines)

### This r script sets up and performs the regression analysis
#### Author: S. Paltra @ TU Berlin

source("PrepIncidenceData.R")
source("PrepMobilityData.R")
source("PrepWeatherData.R")

mobility_data <- mobility_data %>% filter(Bundesland == "Gesamt")
joinedDataFrame <- inner_join(incidence_data, mobility_data, by = "Date")
                                                             
#Joining the data frames
weather_data_all <- weather_data_all %>% filter(Bundesland == "Gesamt")
joinedDataFrame <- left_join(joinedDataFrame, weather_data_all, by = "Date")

#Only for 2020 (and a bit of 2021, as not too many people were vaccinated and alpha wasn't dominant yet)
joinedDataFrame <- filter(joinedDataFrame, Date < "2021-01-01") %>%
                      distinct()

joinedDataFrame <- joinedDataFrame %>% mutate(Welle = case_when(Date < "2020-05-17" ~ "First Wave",
                                                                Date >= "2020-05-17" & Date <= "2020-09-27" ~ "Summer period",
                                                                Date > "2020-09-27" ~ "Second Wave")) %>% 
                                                                mutate(Welle_1week = case_when(Date < as.Date("2020-05-17") - 7 ~ "First Wave",
                                                                Date >= as.Date("2020-05-17") - 7 & Date <= as.Date("2020-09-27") - 7 ~ "Summer period",
                                                                Date > as.Date("2020-09-27") - 7 ~ "Second Wave")) %>% 
                                                                mutate(Welle_2weeks = case_when(Date < as.Date("2020-05-17") - 14 ~ "First Wave",
                                                                Date >= as.Date("2020-05-17") - 14 & Date <= as.Date("2020-09-27") - 14 ~ "Summer period",
                                                                Date > as.Date("2020-09-27") - 14 ~ "Second Wave")) %>%
                                                                mutate(Welle_3weeks = case_when(Date < as.Date("2020-05-17") - 21 ~ "First Wave",
                                                                Date >= as.Date("2020-05-17") - 21 & Date <= as.Date("2020-09-27") - 21 ~ "Summer period",
                                                                Date > as.Date("2020-09-27") - 21 ~ "Second Wave")) %>%
                                                                mutate(Welle_4weeks = case_when(Date < as.Date("2020-05-17") - 28 ~ "First Wave",
                                                                Date >= as.Date("2020-05-17") - 28 & Date <= as.Date("2020-09-27") - 28 ~ "Summer period",
                                                                Date > as.Date("2020-09-27") - 28 ~ "Second Wave")) %>%
                                                                mutate(Welle_5weeks = case_when(Date < as.Date("2020-05-17") - 35 ~ "First Wave",
                                                                Date >= as.Date("2020-05-17") - 35 & Date <= as.Date("2020-09-27") - 35 ~ "Summer period",
                                                                Date > as.Date("2020-09-27") - 35 ~ "Second Wave"))

                                                                
# For one of the weeks the precipitation is missing
#We assume that during this week the precipitation is equal to the average of the week before and the week after
joinedDataFrame[4, 20] <- (joinedDataFrame[3,20]+joinedDataFrame[5,20])/2

####### Different types of models #######

# D = outOfHomeDuration
# I = changeOfIncidence
# tmax = tmax
# tavg = tavg
# out = outdoorFraction
# out2 = outdoorFraction2

#Preparing structure to save results of different regression models
resultsList <- list()

ids <- c("oOH", "oOH2", "oOH3", "oOH+oOH2", "oOH+tmax", "oOH+tavg",
          "oOH*tmax", "oOH*tavg", "oOH+oOH2+out", "oOH+oOH2+out2", "oOH+out", "oOH+out2", "oOH2+out", "oOH2+out2", "oOH*out", "oOH*out2",
          "oOH+prcp", "oOH+tmax+prcp", "oOH+tavg+prcp", "oOH+out+prcp", "oOH+out2+prcp",
          "oOH2:tmax:prcp_noInt", "oOH2:tavg:prcp_noInt", "oOH2:out2:prcp_noInt", "oOH:out2:prcp",
          "oOH2*out", "oOH2*out2", "oOH*tmax2", "oOH*tavg2",
          "oOH*tmax*prcp", "oOH*tavg*prcp", "oOH*out*prcp", "oOH*out2*prcp", "logoOH+logtmax",
          "c_oOH", "c_oOH+c_tmax", "c_oOH+c_tavg", "c_oOH+c_out", "c_oOH+c_out2",
          "c_oOH*c_tmax", "c_oOH*c_tavg", "c_oOH*c_out", "c_oOH*c_out2", "oOH+oOH2+out2", "oOH+oOH2+out2_0Intercept", "oOH+oOH2+out2+gradtmax",
          "polyoOH+polyout2", "oOH*out2+oOH2", "oOH+oOH2*out2", "polyoOH3+polyout23", "polyoOH2+polyout23", "polyoOH2+out2+out23", "oOH2+out2_noInt", "oOH2*out2_noInt",
          "oOH2+tmax_noInt", "oOH2+tavg_noInt", "oOH2+out2+prcp_noInt", "logoOH+oOH2+out2_0Intercept", "oOH2+indoor", "polyoOH2+polyindoor_step1", "polyoOH2+polyindoor_step2", "polyoOH2+polyindoor_step3", "polyoOH2+polyindoor_step4", "polyoOH2+polyindoor_step5",
          "oOH+indoor", "oOH+oOH2+indoor", "oOH2+indoor", "oOH2*indoor", "oOH3+indoor", "oOH4+indoor", "oOH3+out2",
          "oOH2+oOH2:indoor", "oOH2+oOH2:indoor_noInt", "indoor+oOH2:indoor", "oOH2:indoor", "oOH2+oOH2:tmax", "oOH2+oOH2:tavg",
          "oOH+oOH:indoor", "indoor+oOH:indoor", "oOH:indoor", "out2_noInt",
          "oOH3+oOH3:indoor", "indoor+oOH3:indoor", "oOH3:indoor", "oOH4+oOH4:indoor", "oOH5+oOH5:indoor", "oOH6+oOH6:indoor",
          "oOH212+oOH212:indoor", "oOH213+oOH213:indoor", "oOH223+oOH223:indoor",
          "log_oOH", "log_oOH_noInt", "polyoOH3", "oOH2+oOH2:out2", "oOH2+oOH2:out2_noInt",
          "oOH2+oOH2:tmax", "oOH2+oOH2*tmax_noInt", "oOH2+oOH2:tavg", "oOH2+oOH2*tavg_noInt", "oOH2_noInt",  "oOH3+oOHout+oOH2out", "oOH2*out2_noInt", "oOH2+oOH2:google_noInt", "oOH2+oOH2:adjout_noInt")

for(id in ids){
  resultsList[[id]] <- list()
}

lags <- c("cOI", "cOI_1weekbefore", "cOI_2weeksbefore", "cOI_3weeksbefore", "cOI_4weeksbefore", "cOI_5weeksbefore")
lags <- c("cOI_2weeksbefore")

joinedDataFrame <- joinedDataFrame %>%
        mutate(outOfHomeDurationSquared = outOfHomeDuration*outOfHomeDuration) %>% 
        mutate(outOfHomeDurationSquared12 = outOfHomeDuration^(5/2)) %>%
        mutate(outOfHomeDurationSquared13 = outOfHomeDuration^(7/3)) %>%
        mutate(outOfHomeDurationSquared23 = outOfHomeDuration^(8/3)) %>%
         mutate(outOfHomeDurationCubed = outOfHomeDuration*outOfHomeDuration*outOfHomeDuration) %>%
         mutate(outOfHomeDuration4 = outOfHomeDuration*outOfHomeDuration*outOfHomeDuration*outOfHomeDuration) %>%
        mutate(outOfHomeDuration5 = outOfHomeDurationCubed*outOfHomeDurationSquared) %>%
        mutate(outOfHomeDuration6 = outOfHomeDuration5*outOfHomeDuration) %>%
        mutate(outdoorFraction2Squared = outdoorFraction2*outdoorFraction2) %>%
        mutate(indoorFractionSquared = indoorFraction * indoorFraction) %>%
        mutate(indoorFractionCubed = indoorFraction * indoorFraction * indoorFraction) %>%
        mutate(tmaxSquared = tmax * tmax) %>%
        mutate(tavgSquared = tavg * tavg) %>%
        mutate(logOutOfHomeDuration = log(outOfHomeDuration)) %>%
        mutate(logtmax = log(tmax)) %>%
        mutate(logtavg = log(tavg)) %>%
        mutate(onePlusPercChange = 1 + percentageChangeComparedToBeforeCorona/100) %>%
        mutate(centDiffquot_tmax = (lead(tmax)-lag(tmax))/2) %>%
        mutate(outdoorFraction2Cubed = outdoorFraction2 *outdoorFraction2 * outdoorFraction2) %>%
        mutate(logcOI_2weeksbefore = log(cOI_2weeksbefore)) %>%
        mutate(outOfHomeDuration_c = outOfHomeDuration - mean(outOfHomeDuration)) %>% 
        mutate(outOfHomeDurationSquared_c = outOfHomeDuration_c * outOfHomeDuration_c) %>%
        mutate(outOfHomeDurationCubed_c = outOfHomeDuration_c * outOfHomeDuration_c * outOfHomeDuration_c) %>%
        mutate(indoorFraction_c = indoorFraction - mean(indoorFraction)) %>%
        mutate(indoorFractionSquared_c = indoorFraction_c *indoorFraction_c) %>%
        mutate(indoorFractionCubed_c =  indoorFraction_c * indoorFraction_c * indoorFraction_c) %>%
          mutate(indoorFraction_s = scale(indoorFraction)) %>%
        mutate(indoorFractionSquared_s = indoorFraction_s *indoorFraction_s) %>%
        mutate(indoorFractionCubed_s =  indoorFraction_s * indoorFraction_s * indoorFraction_s) %>%
        mutate(outOfHomeDuration_orth = poly(outOfHomeDuration,3)[,1]) %>%
        mutate(outOfHomeDurationSquared_orth = poly(outOfHomeDuration,3)[,2]) %>%
        mutate(outOfHomeDurationCubed_orth = poly(outOfHomeDuration,3)[,3]) %>%
        mutate(indoorFraction_orth = poly(indoorFraction,3)[,1]) %>%
        mutate(indoorFractionSquared_orth = poly(indoorFraction,3)[,2]) %>%
        mutate(indoorFractionCubed_orth = poly(indoorFraction,3)[,3]) %>%
        mutate(log_cOI_2weeksbefore = log(cOI_2weeksbefore))

#Add google mobility data on parks to full data frame
joinedDataFrame <- cbind(joinedDataFrame, google_mobility_data_weekly$parks)
colnames(joinedDataFrame)[69] <- "GoogleParks"

# Adjusted Outdoor fraction: Once the temperature has dropped and once people have moved indoors,
# they do not move outdoors anymore
joinedDataFrame <- joinedDataFrame %>% mutate(adjustedOutdoor = outdoorFraction2)
joinedDataFrame[25, 70] <- 1
joinedDataFrame[26, 70] <- 1
joinedDataFrame[27, 70] <- 1
joinedDataFrame[28, 70] <- 1
joinedDataFrame[29, 70] <- 1

set.seed(15)

#Running the models
for (id in ids) {
  for (lag in lags) {
  if (lag == "cOI") {
    joinedDataFrame_reduced <- joinedDataFrame %>%  filter(Date > "2020-03-08")  %>% filter(Date < "2020-12-20")
    joinedDataFrame_reduced$labeled <- ifelse(joinedDataFrame_reduced[[lag]] < 1.5, "", as.character(joinedDataFrame_reduced$Date, format = "%d/%b/%y"))
    chosen_welle <- "Welle"
  } else if (lag == "cOI_1weekbefore") {
    joinedDataFrame_reduced <- joinedDataFrame  %>%  filter(Date > "2020-03-08") %>% filter(Date < "2020-12-20") #%>% mutate(Date = Date + 7)
    joinedDataFrame_reduced$labeled <- ifelse(joinedDataFrame_reduced[[lag]] < 1.5, "", as.character(joinedDataFrame_reduced$Date+7, format = "%d/%b/%y"))
    chosen_welle <- "Welle_1week"
  } else if (lag == "cOI_2weeksbefore") {
    joinedDataFrame_reduced <- joinedDataFrame  %>%  filter(Date > "2020-03-08") %>% filter(Date < "2020-12-20")  #%>% mutate(Date = Date + 14)
    joinedDataFrame_reduced$labeled <- ifelse(joinedDataFrame_reduced[[lag]] < 1.5, "", as.character(joinedDataFrame_reduced$Date+14, format = "%d/%b/%y"))
    chosen_welle <- "Welle_2weeks"
  } else if (lag == "cOI_3weeksbefore") {
    joinedDataFrame_reduced <- joinedDataFrame  %>%  filter(Date > "2020-03-08") %>% filter(Date < "2020-12-20")  #%>% mutate(Date = Date + 21)
    joinedDataFrame_reduced$labeled <- ifelse(joinedDataFrame_reduced[[lag]] < 1.5, "", as.character(joinedDataFrame_reduced$Date+21, format = "%d/%b/%y"))
    chosen_welle <- "Welle_3weeks"
  } else if (lag == "cOI_4weeksbefore") {
    joinedDataFrame_reduced <- joinedDataFrame  %>%  filter(Date > "2020-03-08") %>% filter(Date < "2020-12-20") #%>% mutate(Date = Date + 28)
    joinedDataFrame_reduced$labeled <- ifelse(joinedDataFrame_reduced[[lag]] < 1.5, "", as.character(joinedDataFrame_reduced$Date+28, format = "%d/%b/%y"))
    chosen_welle <- "Welle_4weeks"
  } else if (lag == "cOI_5weeksbefore") {
    joinedDataFrame_reduced <- joinedDataFrame  %>%  filter(Date > "2020-03-08") %>% filter(Date < "2020-12-20")  #%>% mutate(Date = Date + 35)
    joinedDataFrame_reduced$labeled <- ifelse(joinedDataFrame_reduced[[lag]] < 1.5, "", as.character(joinedDataFrame_reduced$Date+35, format = "%d/%b/%y"))
    chosen_welle <- "Welle_5weeks"
  }
  resultsList[[id]][[lag]] <- list()
  if (id == "oOH") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration")
  } else if (id == "oOH2") {
    formula.lm <- paste0(lag, " ~ outOfHomeDurationSquared")
  } else if (id == "oOH2_noInt") {
    formula.lm <- paste0(lag, " ~ 0 + outOfHomeDurationSquared")
  } else if (id == "oOH3") {
    formula.lm <- paste0(lag, " ~ outOfHomeDurationCubed")
  } else if (id == "oOH+oOH2") {
    formula.lm <- paste0(lag, " ~ outOfHomeDurationSquared + outOfHomeDuration")
  } else if (id == "oOH+tmax") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + tmax")
  } else if (id == "oOH+tavg") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + tavg")
  } else if (id == "oOH*tmax") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration * tmax")
  } else if (id == "oOH*tavg") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration * tavg")
  } else if (id == "oOH+out") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + outdoorFraction")
  } else if (id == "oOH+out2") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + outdoorFraction2")
  } else if (id == "oOH2+indoor") {
     formula.lm <- paste0(lag, " ~ outOfHomeDurationSquared + indoorFraction")
  } else if (id == "oOH4+indoor") {
     formula.lm <- paste0(lag, " ~ outOfHomeDuration4 + indoorFraction")
  } else if (id == "oOH2+out") {
    formula.lm <- paste0(lag, " ~ outOfHomeDurationSquared + outdoorFraction")
  } else if (id == "oOH2+out2") {
    formula.lm <- paste0(lag, " ~ outOfHomeDurationSquared + outdoorFraction2")
  } else if (id == "oOH2+out2_noInt") {
    formula.lm <- paste0(lag, " ~ 0+ outOfHomeDurationSquared + outdoorFraction2")
  } else if (id == "oOH3+out2") {
    formula.lm <- paste0(lag, " ~ outOfHomeDurationCubed + outdoorFraction2")
  } else if (id == "oOH2+out2+prcp_noInt") {
    formula.lm <- paste0(lag, " ~ 0+ outOfHomeDurationSquared + outdoorFraction2 + prcp")
  } else if (id == "oOH2+tmax_noInt") {
    formula.lm <- paste0(lag, " ~ 0+ outOfHomeDurationSquared + tmax")
  } else if (id == "oOH2+tavg_noInt") {
    formula.lm <- paste0(lag, " ~ 0+ outOfHomeDurationSquared + tavg")
  } else if (id == "oOH2*out2_noInt") {
    formula.lm <- paste0(lag, " ~ 0+ outOfHomeDurationSquared * outdoorFraction2")
  } else if (id == "oOH+oOH2+out") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + outOfHomeDurationSquared + outdoorFraction")
  } else if (id == "oOH+oOH2+out2") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + outOfHomeDurationSquared + outdoorFraction2")
  } else if (id == "oOH*out") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration * outdoorFraction")
  } else if (id == "oOH*out2") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration * outdoorFraction2")
  } else if (id == "oOH+prcp") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + prcp")
  } else if (id == "oOH+tmax+prcp") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + tmax + prcp")
  }  else if (id == "oOH+tavg+prcp") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + tavg + prcp")
  } else if (id == "oOH+out+prcp") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + outdoorFraction + prcp")
  } else if (id == "oOH+out2+prcp") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + outdoorFraction2 + prcp")
  }  else if (id == "oOH2:tmax:prcp_noInt") {
    formula.lm <- paste0(lag, " ~ 0 + outOfHomeDurationSquared + tmax + prcp + outOfHomeDurationSquared:tmax + outOfHomeDurationSquared:prcp")
  }  else if (id == "oOH2:tavg:prcp_noInt") {
    formula.lm <- paste0(lag, " ~ 0 + outOfHomeDurationSquared + tavg + prcp + outOfHomeDurationSquared:tavg + outOfHomeDurationSquared:prcp")
  }  else if (id == "oOH2:out2:prcp_noInt") {
    formula.lm <- paste0(lag, " ~ 0 + outOfHomeDurationSquared + outdoorFraction2 + prcp + outOfHomeDurationSquared:outdoorFraction2 + outOfHomeDurationSquared:prcp")
  } else if (id == "oOH:out2:prcp") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + outOfHomeDuration:outdoorFraction2 + outOfHomeDuration:prcp")
  } else if (id == "oOH2*out") {
    formula.lm <- paste0(lag, "~ outOfHomeDurationSquared*outdoorFraction")
  } else if (id == "oOH2*out2") {
    formula.lm <- paste0(lag, "~ outOfHomeDurationSquared*outdoorFraction2")
  } else if (id == "oOH2*out2_noInt") {
    formula.lm <- paste0(lag, "~ 0 + outOfHomeDurationSquared*outdoorFraction2")
  }  else if (id == "oOH2+oOH2:out2") {
    formula.lm <- paste0(lag, "~ outOfHomeDurationSquared + outOfHomeDurationSquared:outdoorFraction2")
  } else if (id == "oOH2+oOH2:out2_noInt") {
    formula.lm <- paste0(lag, "~ 0 + outOfHomeDurationSquared + outOfHomeDurationSquared:outdoorFraction2")
  } else if (id == "oOH2+oOH2:google_noInt") {
    formula.lm <- paste0(lag, "~ 0 + outOfHomeDurationSquared + outOfHomeDurationSquared:GoogleParks")
  } else if (id == "oOH2+oOH2:adjout_noInt") {
    formula.lm <- paste0(lag, "~ 0 + outOfHomeDurationSquared + outOfHomeDurationSquared:adjustedOutdoor")
  } else if (id == "oOH2+oOH2:tmax") {
    formula.lm <- paste0(lag, "~ outOfHomeDurationSquared + outOfHomeDurationSquared:tmax")
  } else if (id == "oOH2+oOH2*tmax_noInt") {
    formula.lm <- paste0(lag, "~ 0 + outOfHomeDurationSquared + outOfHomeDurationSquared*tmax")
  } else if (id == "oOH2+oOH2:tavg") {
    formula.lm <- paste0(lag, "~ outOfHomeDurationSquared + outOfHomeDurationSquared:tavg")
  } else if (id == "oOH2+oOH2*tavg_noInt") {
    formula.lm <- paste0(lag, "~ 0 + outOfHomeDurationSquared + outOfHomeDurationSquared*tavg")
  }  else if (id == "oOH*tmax2") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration * tmaxSquared")
  } else if (id == "oOH*tavg2") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration * tavgSquared")
  } else if (id == "oOH*tmax*prcp") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration * tmax * prcp")
  } else if (id == "oOH*tavg*prcp") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration * tavg * prcp")
  } else if (id == "oOH*out*prcp") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration * outdoorFraction * prcp")
  } else if (id == "oOH*out2*prcp") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration * outdoorFraction2 * prcp")
  } else if (id == "logoOH+logtmax") {
    formula.lm <- paste0(lag, " ~ logOutOfHomeDuration + logtmax")
  } else if (id == "oOH+oOH2+out2") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + outOfHomeDurationSquared + outdoorFraction2")
  }  else if (id == "oOH+oOH2+out2_0Intercept") {
    formula.lm <- paste0(lag, " ~ 0 + outOfHomeDurationSquared + outdoorFraction2")
  } else if (id == "logoOH+oOH2+out2_0Intercept") {
    formula.lm <- "logcOI_2weeksbefore ~ 0 + outOfHomeDurationSquared + outdoorFraction2"
  } else if (id == "oOH+oOH2+out2+gradtmax") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + outOfHomeDurationSquared + outdoorFraction2 + centDiffquot_tmax")
    joinedDataFrame_reduced <- joinedDataFrame_reduced[-1,]
    joinedDataFrame_reduced <- joinedDataFrame_reduced[-nrow(joinedDataFrame_reduced),]
  } else if (id == "polyoOH+polyout2") {
    formula.lm <- paste0(lag, " ~ poly(outOfHomeDuration,2, raw=TRUE)+poly(outdoorFraction2,2, raw=TRUE)")
  } else if (id == "oOH*out2+oOH2") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration*outdoorFraction2 + outOfHomeDurationSquared")
  } else if (id == "oOH+oOH2*out2") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + outOfHomeDurationSquared*outdoorFraction2")
  } else if (id == "polyoOH3+polyout23") {
    formula.lm <- paste0(lag, " ~ poly(outOfHomeDuration,3, raw= TRUE) + poly(outdoorFraction2,3, raw=TRUE)")
  } else if (id == "polyoOH2+polyout23") {
    formula.lm <- paste0(lag, " ~ poly(outOfHomeDuration,2, raw=TRUE) + poly(outdoorFraction2,3, raw=TRUE)")
  } else if (id == "polyoOH2+out2+out23") {
    formula.lm <- paste0(lag, " ~ poly(outOfHomeDuration,2, raw=TRUE) + outdoorFraction2 + outdoorFraction2Cubed")
  } else if (id == "polyoOH2+polyindoor_step1"){
    formula.lm <- paste0(lag, " ~ poly(outOfHomeDuration,3, raw=TRUE) + poly(indoorFraction,3, raw=TRUE)")
  } else if (id == "polyoOH2+polyindoor_step2"){
    formula.lm <- paste0(lag, " ~ poly(outOfHomeDuration,2, raw=TRUE) + poly(indoorFraction,3, raw=TRUE)")
  } else if (id == "polyoOH2+polyindoor_step3"){
    formula.lm <- paste0(lag, " ~ poly(outOfHomeDuration,2, raw=TRUE) + indoorFraction + indoorFractionCubed")
  } else if (id == "polyoOH2+polyindoor_step4"){
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + outOfHomeDurationSquared + indoorFraction")
  } else if (id == "polyoOH2+polyindoor_step5"){
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + indoorFraction")
  } else if (id == "oOH+indoor") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + indoorFraction")
  } else if (id == "oOH+oOH2+indoor") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + outOfHomeDurationSquared + indoorFraction")
  } else if (id == "oOH2+indoor") {
    formula.lm <- paste0(lag, " ~ outOfHomeDurationSquared + indoorFraction")
  } else if (id == "oOH3+indoor") {
    formula.lm <- paste0(lag, " ~ outOfHomeDurationCubed + indoorFraction")
  } else if (id == "oOH2*indoor") {
    formula.lm <- paste0(lag, " ~ outOfHomeDurationSquared * indoorFraction")
  } else if (id == "oOH2+oOH2:indoor") {
    formula.lm <- paste0(lag, " ~ outOfHomeDurationSquared + outOfHomeDurationSquared : indoorFraction")
  } else if (id == "oOH2+oOH2:indoor_noInt") {
    formula.lm <- paste0(lag, " ~ 0 + outOfHomeDurationSquared + outOfHomeDurationSquared : indoorFraction")
  } else if (id == "oOH2+oOH2:tmax") {
    formula.lm <- paste0(lag, " ~ outOfHomeDurationSquared + outOfHomeDurationSquared : tmax")
  } else if (id == "oOH2+oOH2:tavg") {
    formula.lm <- paste0(lag, " ~ outOfHomeDurationSquared + outOfHomeDurationSquared : tavg")
  } else if (id == "oOH212+oOH212:indoor") {
    formula.lm <- paste0(lag, " ~ outOfHomeDurationSquared12 + outOfHomeDurationSquared12 : indoorFraction")
  }  else if (id == "oOH213+oOH213:indoor") {
    formula.lm <- paste0(lag, " ~ outOfHomeDurationSquared13 + outOfHomeDurationSquared13 : indoorFraction")
  }  else if (id == "oOH223+oOH223:indoor") {
    formula.lm <- paste0(lag, " ~ outOfHomeDurationSquared23 + outOfHomeDurationSquared23 : indoorFraction")
  } else if (id == "oOH4+oOH4:indoor") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration4 + outOfHomeDuration4 : indoorFraction")
  }  else if (id == "oOH5+oOH5:indoor") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration5 + outOfHomeDuration5 : indoorFraction")
  }  else if (id == "oOH6+oOH6:indoor") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration6 + outOfHomeDuration6 : indoorFraction")
  } else if (id == "indoor+oOH2:indoor") {
    formula.lm <- paste0(lag, " ~ indoorFraction + outOfHomeDurationSquared : indoorFraction")
  }  else if (id == "oOH2:indoor") {
    formula.lm <- paste0(lag, " ~ outOfHomeDurationSquared : indoorFraction")
  } else if (id == "oOH+oOH:indoor") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + outOfHomeDuration : indoorFraction")
  }  else if (id == "indoor+oOH:indoor") {
    formula.lm <- paste0(lag, " ~ indoorFraction + outOfHomeDuration : indoorFraction")
  }  else if (id == "oOH:indoor") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration : indoorFraction")
  } else if (id == "oOH3+oOH3:indoor") {
    formula.lm <- paste0(lag, " ~ outOfHomeDurationCubed + outOfHomeDurationCubed : indoorFraction")
  }  else if (id == "indoor+oOH3:indoor") {
    formula.lm <- paste0(lag, " ~ indoorFraction + outOfHomeDurationCubed : indoorFraction")
  }  else if (id == "oOH3:indoor") {
    formula.lm <- paste0(lag, " ~ outOfHomeDurationCubed : indoorFraction")
  } else if (id == "log_oOH") {
    formula.lm <- paste0("log_cOI_2weeksbefore ~ logOutOfHomeDuration")
  } else if (id == "log_oOH_noInt") {
    formula.lm <- paste0("log_cOI_2weeksbefore ~ 0 + logOutOfHomeDuration")
  } else if (id == "polyoOH3") {
    formula.lm <- paste0("log_cOI_2weeksbefore ~ poly(outOfHomeDuration,3)")
  } else if (id == "oOH3+oOHout+oOH2out") {
    formula.lm <- paste0(lag, "~ 0+ outOfHomeDurationCubed + outOfHomeDuration:outdoorFraction2 + outOfHomeDurationSquared:outdoorFraction2")
  } else if(id == "out2_noInt"){
    formula.lm <- paste0(lag, "~ 0 + outdoorFraction2")
  }

    model.lm <- lm(formula = formula.lm, data = joinedDataFrame_reduced) # Regression
    resultsList[[id]][[lag]][["Model"]] <- model.lm
    resultsList[[id]][[lag]][["AIC"]] <- AIC(model.lm)
    resultsList[[id]][[lag]][["BIC"]] <- BIC(model.lm)

    #Leave one out cross validation
    train.control <- trainControl(method = "LOOCV")
    model.loocv <- train(eval(parse(text=formula.lm)), data = joinedDataFrame_reduced, method = "lm", trControl = train.control)
    resultsList[[id]][[lag]][["Model_LOOCV"]] <- model.loocv

    #10-fold cross validation
    train.control <- trainControl(method = "cv", number = 10)
    model.10foldcv <- train(eval(parse(text=formula.lm)), data = joinedDataFrame_reduced, method = "lm", trControl = train.control, tuneGrid  = expand.grid(intercept = FALSE))
    resultsList[[id]][[lag]][["Model_10foldCV"]] <- model.10foldcv

    joinedDataFrame_reduced[[chosen_welle]] <- factor(joinedDataFrame_reduced[[chosen_welle]], levels = c("First Wave", "Summer period", "Second Wave"))
    joinedDataFrame_reduced <- joinedDataFrame_reduced %>% mutate(predicted = predict(model.lm))
    resultsList[[id]][[lag]][["PlotActualFitted"]]  <- local({
    lag <- lag

    #Plot observed vs. predicted values
    g <- ggplot() +
    #geom_smooth(aes(x=predict(model.lm), y = .data[[lag]], color="#4b4b4b"), method="lm", size = 2) +
    geom_text_repel(data = joinedDataFrame_reduced, aes(x = predicted, y = .data[[lag]], label = labeled)) +
    xlab("Predicted Values") +
    ylab("Observed Values") +
    #ggtitle(paste0("lag: ", lag, ", Model: ", id)) +
    geom_abline(aes(intercept = 0, slope = 1, color = "#666666"), size = 1.5) +
    scale_color_identity(labels = c("x=y"), guide = "legend") +
    theme_minimal() +
    theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
    scale_x_continuous(breaks=c(0.6,0.8,1.0,1.2,1.4)) +
    scale_y_continuous(breaks=c(0.6,0.8,1.0,1.2,1.4, 1.6)) +
    theme(legend.position = "bottom", legend.title = element_blank(), text = element_text(size = 25)) +
    geom_point(data = joinedDataFrame_reduced, aes(x = predicted, y = .data[[lag]], fill = .data[[chosen_welle]]), size = 2.5,  shape = 21, colour = "transparent") +
    scale_fill_manual(values = c("#1B9E77",
                               "#7570B3", "#D95F02"))
    })

    date_breaks <- data.frame(start = c(as.Date("2020-03-22"), as.Date("2020-05-17"), as.Date("2020-09-28")),
                              end = c(as.Date("2020-05-17"), as.Date("2020-09-28"), as.Date("2021-01-01")),
                              colors = c("First Wave", "Summer Period", "Second Wave"))
    date_breaks$colors <- factor(date_breaks$colors, levels = c("First Wave", "Summer Period", "Second Wave"))
    joinedDataFrame_reduced <- joinedDataFrame_reduced %>% mutate(predicted = predict(model.lm))

  #Plot observed and predicted values over time
  g2 <- ggplot(joinedDataFrame_reduced) +
  theme_minimal() +
  geom_rect(data = date_breaks,
            aes(xmin = start,
                xmax = end,
                ymin = - Inf,
                ymax = Inf,
                fill = colors),
            alpha = 0.3) +
  scale_fill_manual(values = c("#1B9E77",
                               "#7570B3", "#D95F02")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%d/%b/%y") +
  ylab("Growth Multiplier") +
  xlab("") +
  geom_point(aes(x = Date, y = .data[[lag]]), color = "#666666", size = 2.5) +
  geom_line(aes(x = Date, y = predicted), size = 2.5, show.legend=FALSE)  +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(text = element_text(size = 25), legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))
  resultsList[[id]][[lag]][["PlotActualFitted_overTime"]] <- g2
  }
}

#Print plot of actual vs predicted values
#To create the plot for a different model, one needs to adapt the model name in the first []
resultsList[["oOH2+oOH2:out2_noInt"]][["cOI_2weeksbefore"]][["PlotActualFitted"]]

ggsave("ActualvsPredicted.pdf", dpi = 500, w = 9, h = 4.5)
ggsave("ActualvsPredicted.png", dpi = 500, w = 9, h = 4.5)

#Print plot of actual and predicted values over time
#To create the plot for a different model, one needs to adapt the model name in the first []
resultsList[["oOH2+oOH2:out2_noInt"]][["cOI_2weeksbefore"]][["PlotActualFitted_overTime"]]

ggsave("ActualvsPredicted_overTime.pdf", dpi = 500, w = 9, h = 5.5)
ggsave("ActualvsPredicted_overTime.png", dpi = 500, w = 9, h = 5.5)
