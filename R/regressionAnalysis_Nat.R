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

ggplot(weather_data_all) +
geom_point(aes(x = Date, y = TStar), color = "#666666") +
theme_minimal() +
xlab("Date") +
ylab("T*") +
 scale_x_date(date_breaks = "1 month", date_labels = "%d/%b/%y")

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
                                                                Date >= "2020-05-17" & Date <= "2020-09-27" ~ "Summer break",
                                                                Date > "2020-09-27" ~ "Second Wave")) %>% 
                                                                mutate(Welle_1week = case_when(Date < as.Date("2020-05-17") - 7 ~ "First Wave",
                                                                Date >= as.Date("2020-05-17") - 7 & Date <= as.Date("2020-09-27") - 7 ~ "Summer break",
                                                                Date > as.Date("2020-09-27") - 7 ~ "Second Wave")) %>% 
                                                                mutate(Welle_2weeks = case_when(Date < as.Date("2020-05-17") - 14 ~ "First Wave",
                                                                Date >= as.Date("2020-05-17") - 14 & Date <= as.Date("2020-09-27") - 14 ~ "Summer break",
                                                                Date > as.Date("2020-09-27") - 14 ~ "Second Wave")) %>%
                                                                mutate(Welle_3weeks = case_when(Date < as.Date("2020-05-17") - 21 ~ "First Wave",
                                                                Date >= as.Date("2020-05-17") - 21 & Date <= as.Date("2020-09-27") - 21 ~ "Summer break",
                                                                Date > as.Date("2020-09-27") - 21 ~ "Second Wave")) %>%
                                                                mutate(Welle_4weeks = case_when(Date < as.Date("2020-05-17") - 28 ~ "First Wave",
                                                                Date >= as.Date("2020-05-17") - 28 & Date <= as.Date("2020-09-27") - 28 ~ "Summer break",
                                                                Date > as.Date("2020-09-27") - 28 ~ "Second Wave")) %>%
                                                                mutate(Welle_5weeks = case_when(Date < as.Date("2020-05-17") - 35 ~ "First Wave",
                                                                Date >= as.Date("2020-05-17") - 35 & Date <= as.Date("2020-09-27") - 35 ~ "Summer break",
                                                                Date > as.Date("2020-09-27") - 35 ~ "Second Wave"))

                                                                


joinedDataFrame[4, 20] <- (joinedDataFrame[3,20]+joinedDataFrame[5,20])/2

####### Different types of models #######

# D = outOfHomeDuration
# I = changeOfIncidence
# tmax = tmax
# tavg = tavg
# out = outdoorFraction
# out2 = outdoorFraction2

#Performing linear regression
resultsList <- list()
resultsList[["oOH"]] <- list()
resultsList[["oOH2"]] <- list()
resultsList[["oOH3"]] <- list()
resultsList[["oOH+oOH2"]] <- list()
resultsList[["oOH+tmax"]] <- list()
resultsList[["oOH+tavg"]] <- list()
resultsList[["oOH*tmax"]] <- list()
resultsList[["oOH*tavg"]] <- list()
resultsList[["oOH+out"]] <- list()
resultsList[["oOH+out2"]] <- list()
resultsList[["oOH2+out"]] <- list()
resultsList[["oOH2+out2"]] <- list()
resultsList[["oOH2+indoor"]] <- list()
resultsList[["oOH+oOH2+out"]] <- list()
resultsList[["oOH+oOH2+out2"]] <- list()
resultsList[["oOH+oOH2:out2_noInt"]] <- list()
resultsList[["oOH2+oOH2_tmax"]] <- list()
resultsList[["oOH2+oOH2:tmax_noInt"]] <- list()
resultsList[["oOH2+oOH2:tavg"]] <- list()
resultsList[["oOH2+oOH2:tavg_noInt"]] <- list()
resultsList[["oOH*out"]] <- list()
resultsList[["oOH*out2"]] <- list()
resultsList[["oOH+prcp"]] <- list()
resultsList[["oOH+tmax+prcp"]] <- list()
resultsList[["oOH+tavg+prcp"]] <- list()
resultsList[["oOH+out+prcp"]] <- list()
resultsList[["oOH+out2+prcp"]] <- list()
resultsList[["oOH:tmax:prcp"]] <- list()
resultsList[["oOH:tavg:prcp"]] <- list()
resultsList[["oOH:out:prcp"]] <- list()
resultsList[["oOH2*out"]] <- list()
resultsList[["oOH2*out2"]] <- list()
resultsList[["oOH2+oOH2:out2"]] <- list()
resultsList[["oOH2*out"]] <- list()
resultsList[["oOH*tmax2"]] <- list()
resultsList[["oOH*tavg2"]] <- list()
resultsList[["oOH*tmax*prcp"]] <- list()
resultsList[["oOH*tavg*prcp"]] <- list()
resultsList[["oOH*out*prcp"]] <- list()
resultsList[["oOH*out2*prcp"]] <- list()
resultsList[["logoOH+logtmax"]] <- list()
resultsList[["polyoOH+polyout2"]] <- list()
resultsList[["oOH*out2+oOH2"]] <- list()
resultsList[["oOH+oOH2*out2"]] <- list()
resultsList[["polyoOH3+polyout23"]] <- list()
resultsList[["polyoOH2+polyout23"]] <- list()
resultsList[["polyoOH2+out2+out23"]] <- list()
resultsList[["oOH2+out2_noInt"]] <- list()
resultsList[["oOH2*out2_noInt"]] <- list()
resultsList[["oOH2+tmax_noInt"]] <- list()
resultsList[["oOH2+tavg_noInt"]] <- list()
resultsList[["oOH2+out2+prcp_noInt"]] <- list()
resultsList[["logoOH+oOH2+out2_0Intercept"]] <- list()
resultsList[["polyoOH2+polyindoor_step1"]] <- list()
resultsList[["polyoOH2+polyindoor_step2"]] <- list()
resultsList[["polyoOH2+polyindoor_step3"]] <- list()
resultsList[["polyoOH2+polyindoor_step4"]] <- list()
resultsList[["polyoOH2+polyindoor_step5"]] <- list()
resultsList[["oOH+indoor"]] <- list()
resultsList[["oOH+oOH2+indoor"]] <- list()
resultsList[["oOH2+indoor"]] <- list()
resultsList[["oOH2*indoor"]] <- list()
resultsList[["oOH2+oOH2:indoor"]] <- list()
resultsList[["oOH212+oOH212:indoor"]] <- list()
resultsList[["oOH213+oOH213:indoor"]] <- list()
resultsList[["oOH223+oOH223:indoor"]] <- list()
resultsList[["oOH4+oOH4:indoor"]] <- list()
resultsList[["oOH5+oOH5:indoor"]] <- list()
resultsList[["oOH6+oOH6:indoor"]] <- list()
resultsList[["indoor+oOH2:indoor"]] <- list()
resultsList[["oOH2:indoor"]] <- list()
resultsList[["oOH3+indoor"]] <- list()
resultsList[["oOH4+indoor"]] <- list()
resultsList[["oOH3+out2"]] <- list()
resultsList[["oOH+oOH:indoor"]] <- list()
resultsList[["indoor+oOH:indoor"]] <- list()
resultsList[["oOH:indoor"]] <- list()
resultsList[["oOH3+oOH3:indoor"]] <- list()
resultsList[["indoor+oOH3:indoor"]] <- list()
resultsList[["oOH3:indoor"]] <- list()
resultsList[["stepwise_step1"]] <- list()
resultsList[["stepwise_step2"]] <- list()
resultsList[["stepwise_step1b"]] <- list()
 
ids <- c("oOH", "oOH2", "oOH3", "oOH+oOH2", "oOH+tmax", "oOH+tavg",
          "oOH*tmax", "oOH*tavg", "oOH+oOH2+out", "oOH+oOH2+out2", "oOH+out", "oOH+out2", "oOH2+out", "oOH2+out2", "oOH*out", "oOH*out2",
          "oOH+prcp", "oOH+tmax+prcp", "oOH+tavg+prcp", "oOH+out+prcp", "oOH+out2+prcp",
          "oOH:tmax:prcp", "oOH:tavg:prcp", "oOH:out:prcp", "oOH:out2:prcp",
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
          "log_oOH", "log_oOH_noInt", "polyoOH3", "test", "test2", "test3", "oOH2+oOH2:out2", "oOH2+oOH2:out2_noInt",
          "oOH2+oOH2:tmax", "oOH2+oOH2:tmax_noInt", "oOH2+oOH2:tavg", "oOH2+oOH2:tavg_noInt", "oOH2_noInt",  "oOH3+oOHout+oOH2out")

lags <- c("cOI", "cOI_1weekbefore", "cOI_2weeksbefore", "cOI_3weeksbefore", "cOI_4weeksbefore", "cOI_5weeksbefore")

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
        #mutate(indoorFraction = indoorFraction * 100) %>%
        #mutate(outdoorFraction2 = outdoorFraction2 * 100) %>%
        #mutate(cOI_2weeksbefore = (cOI_2weeksbefore-1)*100) %>%
        mutate(log_cOI_2weeksbefore = log(cOI_2weeksbefore))
# joinedDataFrame <- joinedDataFrame[complete.cases(joinedDataFrame), ]
set.seed(15)

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
  }  else if (id == "oOH:tmax:prcp") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + outOfHomeDuration:tmax + outOfHomeDuration:prcp")
  }  else if (id == "oOH:tavg:prcp") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + outOfHomeDuration:tavg + outOfHomeDuration:prcp")
  }  else if (id == "oOH:out:prcp") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + outOfHomeDuration:outdoorFraction + outOfHomeDuration:prcp")
  } else if (id == "oOH:out2:prcp") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + outOfHomeDuration:outdoorFraction2 + outOfHomeDuration:prcp")
  } else if (id == "oOH2*out") {
    formula.lm <- paste0(lag, "~ outOfHomeDurationSquared*outdoorFraction")
  } else if (id == "oOH2*out2") {
    formula.lm <- paste0(lag, "~ outOfHomeDurationSquared*outdoorFraction2")
  } else if (id == "oOH2+oOH2:out2") {
    formula.lm <- paste0(lag, "~ outOfHomeDurationSquared + outOfHomeDurationSquared:outdoorFraction2")
  } else if (id == "oOH2+oOH2:out2_noInt") {
    formula.lm <- paste0(lag, "~ 0 + outOfHomeDurationSquared + outOfHomeDurationSquared:outdoorFraction2")
  }  else if (id == "oOH2+oOH2:tmax") {
    formula.lm <- paste0(lag, "~ outOfHomeDurationSquared + outOfHomeDurationSquared:tmax")
  } else if (id == "oOH2+oOH2:tmax_noInt") {
    formula.lm <- paste0(lag, "~ 0 + outOfHomeDurationSquared + outOfHomeDurationSquared:tmax")
  } else if (id == "oOH2+oOH2:tavg") {
    formula.lm <- paste0(lag, "~ outOfHomeDurationSquared + outOfHomeDurationSquared:tavg")
  } else if (id == "oOH2+oOH2:tavg_noInt") {
    formula.lm <- paste0(lag, "~ 0 + outOfHomeDurationSquared + outOfHomeDurationSquared:tavg")
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
  } else if (id == "test") {
    formula.lm <- paste0(lag, "~ outOfHomeDurationSquared:indoorFraction + outOfHomeDurationCubed:indoorFraction")
  } else if (id == "test2") {
    formula.lm <- paste0(lag, "~ outOfHomeDurationSquared + outOfHomeDurationCubed + outOfHomeDurationSquared:indoorFraction + outOfHomeDurationCubed:indoorFraction")
  } else if (id == "test3") {
    formula.lm <- paste0(lag, "~ outOfHomeDurationSquared_c + outOfHomeDurationCubed_c + outOfHomeDurationSquared_c:indoorFraction + outOfHomeDurationCubed_c:indoorFraction")
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

    joinedDataFrame_reduced[[chosen_welle]] <- factor(joinedDataFrame_reduced[[chosen_welle]], levels = c("First Wave", "Summer break", "Second Wave"))
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
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", legend.title = element_blank(), text = element_text(size = 13)) +
    geom_point(data = joinedDataFrame_reduced, aes(x = predicted, y = .data[[lag]], fill = .data[[chosen_welle]]), size = 4,  shape = 21, colour = "transparent") +
    scale_fill_manual(values = c("#1B9E77",
                               "#7570B3", "#D95F02"))
    })

    date_breaks <- data.frame(start = c(as.Date("2020-03-22"), as.Date("2020-05-17"), as.Date("2020-09-28")),
                              end = c(as.Date("2020-05-17"), as.Date("2020-09-28"), as.Date("2021-01-01")),
                              colors = c("First Wave", "Summer Break", "Second Wave"))
    date_breaks$colors <- factor(date_breaks$colors, levels = c("First Wave", "Summer Break", "Second Wave"))
    joinedDataFrame_reduced <- joinedDataFrame_reduced %>% mutate(predicted = predict(model.lm))

  #Plot observed and predicted values over time
  g2 <- ggplot(joinedDataFrame_reduced) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank(), text = element_text(size = 13)) +
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
  geom_point(aes(x = Date, y = .data[[lag]]), color = "#666666", size = 2) +
  geom_line(aes(x = Date, y = predicted), size = 1.2, show.legend=FALSE)  +
  theme(text = element_text(size = 13), legend.position = "bottom", legend.title = element_blank()) +
  theme(axis.ticks.x = element_line(),
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))
  resultsList[[id]][[lag]][["PlotActualFitted_overTime"]] <- g2
  }
}

### MODEL SELECTION ###

#Model selection -> using INDOOR fraction
joinedDataFrame_indoor <- joinedDataFrame %>% filter(cOI_2weeksbefore < 2) %>%
                            filter(Date + 14 < "2021-01-01") %>% select(cOI_2weeksbefore, outOfHomeDuration, outOfHomeDurationSquared, outOfHomeDurationCubed, indoorFraction, indoorFractionSquared, indoorFractionCubed) %>% 
                            mutate(oOHindoorFraction = outOfHomeDuration * indoorFraction) %>%
                            mutate(oOH2indoorFraction = outOfHomeDurationSquared * indoorFraction) %>%
                            mutate(oOH3indoorFraction = outOfHomeDurationCubed * indoorFraction) %>% 
                            mutate(oOHindoorFraction2 = outOfHomeDuration * indoorFractionSquared) %>%
                            mutate(oOH2indoorFraction2 = outOfHomeDurationSquared * indoorFractionSquared) %>%
                            mutate(oOH3indoorFraction2 = outOfHomeDurationCubed * indoorFractionSquared) %>% 
                            mutate(oOHindoorFraction3 = outOfHomeDuration * indoorFractionCubed) %>%
                            mutate(oOH2indoorFraction3 = outOfHomeDurationSquared * indoorFractionCubed) %>%
                            mutate(oOH3indoorFraction3 = outOfHomeDurationCubed * indoorFractionCubed) 
y_train <- joinedDataFrame_indoor$cOI_2weeksbefore
joinedDataFrame_indoor <- joinedDataFrame_indoor %>% select(-cOI_2weeksbefore)
joinedDataFrame_indoor <- as.matrix(joinedDataFrame_indoor)

joinedDataFrame <- joinedDataFrame %>% filter(cOI_2weeksbefore<2) %>% filter(Date + 14 < "2021-01-01") %>%
mutate(Prediction = predict(resultsList[["oOH2+oOH2:out2"]][["cOI_2weeksbefore"]][["Model"]]), cOI_2weeksbefore)

#using LASSO to perform model selection
# 10-fold CV to find the optimal lambda
iteratons_lasso <- c()
for(i in 1 :100) {
enet.cv=cv.glmnet(x=joinedDataFrame_indoor, y=y_train,alpha=1, type="deviance", family="gaussian", standardize=TRUE, nfolds=10)
## Fit lasso model with 100 values for lambda
enet_mdl = glmnet(x=joinedDataFrame_indoor, y=y_train,alpha=1,standardize=TRUE,nlambda=100)
## Extract coefficients at optimal lambda
coef(enet_mdl,s = enet.cv$lambda.min)
summary(enet_mdl,s = enet.cv$lambda.min)
iteratons_lasso <- append(iteratons_lasso, enet.cv$lambda.min)
}
hist(iterations_lasso)
coef(enet_mdl,s = mean(iterations_lasso))
coef(enet_mdl,s = median(iterations_lasso))


#using ELASTIC NET to perform model selection
# 10-fold CV to find the optimal lambda
iterations_elasticnet <- c()
for(i in 1 : 100) {
enet.cv=cv.glmnet(x=joinedDataFrame_indoor, y=y_train,alpha=0.5, type="deviance", family="gaussian", standardize=TRUE, nfolds=10)
## Fit lasso model with 100 values for lambda
enet_mdl = glmnet(x=joinedDataFrame_indoor, y=y_train,alpha=0.5,standardize=TRUE,nlambda=100)
## Extract coefficients at optimal lambda
coef(enet_mdl,s=enet.cv$lambda.min)
summary(enet_mdl,s=enet.cv$lambda.min)
iterations_elasticnet <- append(iterations_elasticnet, enet.cv$lambda.min)
}
hist(iterations_elasticnet)
coef(enet_mdl, s = mean(iterations_elasticnet))
coef(enet_mdl, s = median(iterations_elasticnet))

#using REG SUBSET selection to perform model selection
mod.full <-  regsubsets(cOI_2weeksbefore ~ ., method = "exhaustive", data = as.data.frame(joinedDataFrame_indoor), nvmax = 10)
mod.summary <-  summary(mod.full)
which.min(mod.summary$bic)
which.max(mod.summary$adjr2)
coefficients(mod.full, id = 2)

plot(mod.summary$cp, xlab = "Subset Size", ylab="Cp", pch=20, type="l")
points(which.min(mod.summary$cp), min(mod.summary$cp), pch=4, col="red", lwd=7)

plot(mod.summary$bic, xlab="Subset Size", ylab="BIC", pch=20, type="l")
points(which.min(mod.summary$bic), min(mod.summary$bic), pch=4, col="red", lwd=7)

plot(mod.summary$adjr2, xlab="Subset Size", ylab="AdjrR^2", pch=20, type="l")
points(which.max(mod.summary$adjr2), max(mod.summary$adjr2), pch=4, col="red", lwd=7)

#Model selection --> using INDOOR fraction and enforcing that the MAXIMAL DEGREE is 3
joinedDataFrame_indoor <- joinedDataFrame %>% filter(cOI_2weeksbefore < 2) %>%
                            filter(Date + 14 < "2021-01-01") %>% select(cOI_2weeksbefore, outOfHomeDuration, outOfHomeDurationSquared, outOfHomeDurationCubed, indoorFraction, indoorFractionSquared, indoorFractionCubed) %>% 
                            mutate(oOHindoorFraction = outOfHomeDuration * indoorFraction) %>%
                            mutate(oOH2indoorFraction = outOfHomeDurationSquared * indoorFraction) %>%
                            #mutate(oOH3indoorFraction = outOfHomeDurationCubed * indoorFraction) %>% 
                            mutate(oOHindoorFraction2 = outOfHomeDuration * indoorFractionSquared) 
                           # mutate(oOH2indoorFraction2 = outOfHomeDurationSquared * indoorFractionSquared) %>%
                            #mutate(oOH3indoorFraction2 = outOfHomeDurationCubed * indoorFractionSquared) %>% 
                            #mutate(oOHindoorFraction3 = outOfHomeDuration * indoorFractionCubed) %>%
                            #mutate(oOH2indoorFraction3 = outOfHomeDurationSquared * indoorFractionCubed) %>%
                            #mutate(oOH3indoorFraction3 = outOfHomeDurationCubed * indoorFractionCubed) 
y_train <- joinedDataFrame_indoor$cOI_2weeksbefore
joinedDataFrame_indoor <- joinedDataFrame_indoor %>% select(-cOI_2weeksbefore)
joinedDataFrame_indoor <- as.matrix(joinedDataFrame_indoor)

# Using LASSO to perform model selection
# 10-fold CV to find the optimal lambda
iteratons_lasso <- c()
for(i in 1 :100) {
enet.cv=cv.glmnet(x=joinedDataFrame_indoor, y=y_train,alpha=1, type="deviance", family="gaussian", standardize=TRUE, nfolds=10, , intercept = FALSE)
## Fit lasso model with 100 values for lambda
enet_mdl = glmnet(x=joinedDataFrame_indoor, y=y_train,alpha=1,standardize=TRUE,nlambda=100, intercept = FALSE)
## Extract coefficients at optimal lambda
coef(enet_mdl,s = enet.cv$lambda.min)
summary(enet_mdl,s = enet.cv$lambda.min)
iteratons_lasso <- append(iteratons_lasso, enet.cv$lambda.min)
}
hist(iterations_lasso)
coef(enet_mdl,s = mean(iterations_lasso))
coef(enet_mdl,s = median(iterations_lasso))


# Using ELASTIC NET to perform model selection
# 10-fold CV to find the optimal lambda
iterations_elasticnet <- c()
for(i in 1 : 100) {
enet.cv=cv.glmnet(x=joinedDataFrame_indoor, y=y_train,alpha=0.5, type="deviance", family="gaussian", standardize=TRUE, nfolds=10, intercept = FALSE)
## Fit lasso model with 100 values for lambda
enet_mdl = glmnet(x=joinedDataFrame_indoor, y=y_train,alpha=0.5,standardize=TRUE,nlambda=100, intercept = FALSE)
## Extract coefficients at optimal lambda
coef(enet_mdl,s=enet.cv$lambda.min)
summary(enet_mdl,s=enet.cv$lambda.min)
iterations_elasticnet <- append(iterations_elasticnet, enet.cv$lambda.min)
}
hist(iterations_elasticnet)
coef(enet_mdl, s = mean(iterations_elasticnet))
coef(enet_mdl, s = median(iterations_elasticnet))

# Using REG SUBSET to perform model selection
mod.full <-  regsubsets(cOI_2weeksbefore ~ ., intercept = FALSE, method = "exhaustive", data = as.data.frame(joinedDataFrame_indoor), nvmax = 3)
mod.summary <-  summary(mod.full)
which.min(mod.summary$cp)
which.max(mod.summary$adjr2)
coefficients(mod.full, id = 2)

plot(mod.summary$cp, xlab = "Subset Size", ylab="Cp", pch=20, type="l")
points(which.min(mod.summary$cp), min(mod.summary$cp), pch=4, col="red", lwd=7)

plot(mod.summary$bic, xlab="Subset Size", ylab="BIC", pch=20, type="l")
points(which.min(mod.summary$bic), min(mod.summary$bic), pch=4, col="red", lwd=7)

plot(mod.summary$adjr2, xlab="Subset Size", ylab="AdjrR^2", pch=20, type="l")
points(which.max(mod.summary$adjr2), max(mod.summary$adjr2), pch=4, col="red", lwd=7)

#Model selection -> using OUTDOOR fraction
joinedDataFrame_outdoor <- joinedDataFrame %>% filter(cOI_2weeksbefore < 2) %>%
                            filter(Date + 14 < "2021-01-01") %>% select(cOI_2weeksbefore, outOfHomeDuration, outOfHomeDurationSquared, outOfHomeDurationCubed, outdoorFraction2, outdoorFraction2Squared, outdoorFraction2Cubed) %>% 
                            mutate(oOHoutdoorFraction = outOfHomeDuration * outdoorFraction2) %>%
                            mutate(oOH2outdoorFraction = outOfHomeDurationSquared * outdoorFraction2) %>%
                            mutate(oOH3outdoorFraction = outOfHomeDurationCubed * outdoorFraction2) %>% 
                            mutate(oOHoutdoorFraction2 = outOfHomeDuration * outdoorFraction2Squared) %>%
                            mutate(oOH2outdoorFraction2 = outOfHomeDurationSquared * outdoorFraction2Squared) %>%
                            mutate(oOH3outdoorFraction2 = outOfHomeDurationCubed * outdoorFraction2Squared) %>% 
                            mutate(oOHoutdoorFraction3 = outOfHomeDuration * outdoorFraction2Cubed) %>%
                            mutate(oOH2outdoorFraction3 = outOfHomeDurationSquared * outdoorFraction2Cubed) %>%
                            mutate(oOH3outdoorFraction3 = outOfHomeDurationCubed * outdoorFraction2Cubed) 
y_train <- joinedDataFrame_outdoor$cOI_2weeksbefore
joinedDataFrame_outdoor <- joinedDataFrame_outdoor %>% select(-cOI_2weeksbefore)
joinedDataFrame_outdoor <- as.matrix(joinedDataFrame_outdoor)

#Using LASSO to perform model selection
# 10-fold CV to find the optimal lambda
iteratopms_lasso <- c()
for(i in 1 :100) {
enet.cv=cv.glmnet(x=joinedDataFrame_outdoor, y=y_train,alpha=1, type="deviance", family="gaussian", standardize=TRUE, nfolds=10, nzero=3)
## Fit lasso model with 100 values for lambda
enet_mdl = glmnet(x=joinedDataFrame_outdoor, y=y_train,alpha=1,standardize=TRUE,nlambda=100, nzero=3)
## Extract coefficients at optimal lambda
coef(enet_mdl,s=enet.cv$lambda.min)
iterations_lasso <- append(iterations_lasso, enet.cv$lambda.min)
}
coef(enet_mdl,s=mean(iterations_lasso))
coef(enet_mdl,s=median(iterations_lasso))

#Using ELASTIC NET to perform model selection
# 10-fold CV to find the optimal lambda
iteration_elasticnet <- c()
for(i in 1:100){
enet.cv=cv.glmnet(x=joinedDataFrame_outdoor, y=y_train,alpha=0.5, type="deviance", family="gaussian", standardize=TRUE, nfolds=10, nzero=3)
## Fit lasso model with 100 values for lambda
enet_mdl = glmnet(x=joinedDataFrame_outdoor, y=y_train,alpha=0.5,standardize=TRUE,nlambda=100, nzero=3)
## Extract coefficients at optimal lambda
coef(enet_mdl,s=enet.cv$lambda.min)
iteration_elasticnet <- append(iteration_elasticnet, enet.cv$lambda.min)
}
coef(enet_mdl,s=mean(iteration_elasticnet))
coef(enet_mdl,s=median(iteration_elasticnet))

#Using REG SUBSET to perform model selection
mod.full <-  regsubsets(cOI_2weeksbefore ~ ., data = as.data.frame(joinedDataFrame_outdoor), nvmax = 10)
mod.summary <-  summary(mod.full)
which.min(mod.summary$bic)
which.max(mod.summary$adjr2)
coefficients(mod.full, id=8)

plot(mod.summary$cp, xlab="Subset Size", ylab="Cp", pch=20, type="l")
points(which.min(mod.summary$cp), min(mod.summary$cp), pch=4, col="red", lwd=7)

plot(mod.summary$bic, xlab="Subset Size", ylab="BIC", pch=20, type="l")
points(which.min(mod.summary$bic), min(mod.summary$bic), pch=4, col="red", lwd=7)

plot(mod.summary$adjr2, xlab="Subset Size", ylab="AdjrR^2", pch=20, type="l")
points(which.max(mod.summary$adjr2), max(mod.summary$adjr2), pch=4, col="red", lwd=7)


#Model selection -> using OUTDOOR fraction enforcing that the MAXIMAL DEGREE is equal to 3 and a NO INTERCEPT model
joinedDataFrame_outdoor <- joinedDataFrame %>% filter(cOI_2weeksbefore < 2) %>%
                            filter(Date + 14 < "2021-01-01") %>% select(cOI_2weeksbefore, outOfHomeDuration, outOfHomeDurationSquared, outOfHomeDurationCubed, outdoorFraction2, outdoorFraction2Squared, outdoorFraction2Cubed) %>% 
                            mutate(oOHoutdoorFraction = outOfHomeDuration * outdoorFraction2) %>%
                            mutate(oOH2outdoorFraction = outOfHomeDurationSquared * outdoorFraction2) %>%
                            #mutate(oOH3outdoorFraction = outOfHomeDurationCubed * outdoorFraction2) %>% 
                            mutate(oOHoutdoorFraction2 = outOfHomeDuration * outdoorFraction2Squared) 
                            #mutate(oOH2outdoorFraction2 = outOfHomeDurationSquared * outdoorFraction2Squared) %>%
                            #mutate(oOH3outdoorFraction2 = outOfHomeDurationCubed * outdoorFraction2Squared) %>% 
                            #mutate(oOHoutdoorFraction3 = outOfHomeDuration * outdoorFraction2Cubed) %>%
                            #mutate(oOH2outdoorFraction3 = outOfHomeDurationSquared * outdoorFraction2Cubed) %>%
                            #mutate(oOH3outdoorFraction3 = outOfHomeDurationCubed * outdoorFraction2Cubed) 
y_train <- joinedDataFrame_outdoor$cOI_2weeksbefore
joinedDataFrame_outdoor <- joinedDataFrame_outdoor %>% select(-cOI_2weeksbefore)
joinedDataFrame_outdoor <- as.matrix(joinedDataFrame_outdoor)

# Using LASSO to perform model selection
# 10-fold CV to find the optimal lambda
iteratopms_lasso <- c()
for(i in 1 :100) {
enet.cv=cv.glmnet(x=joinedDataFrame_outdoor, y=y_train,alpha=1, type="deviance", family="gaussian", standardize=TRUE, nfolds=10, intercept = FALSE)
## Fit lasso model with 100 values for lambda
enet_mdl = glmnet(x=joinedDataFrame_outdoor, y=y_train,alpha=1,standardize=TRUE,nlambda=100, intercept = FALSE)
## Extract coefficients at optimal lambda
coef(enet_mdl,s=enet.cv$lambda.min)
iterations_lasso <- append(iterations_lasso, enet.cv$lambda.min)
}
coef(enet_mdl,s=mean(iterations_lasso))
coef(enet_mdl,s=median(iterations_lasso))

# Using ELASTIC NET to perform model selection
# 10-fold CV to find the optimal lambda
iteration_elasticnet <- c()
for(i in 1:100){
enet.cv=cv.glmnet(x=joinedDataFrame_outdoor, y=y_train,alpha=0.5, type="deviance", family="gaussian", standardize=TRUE, nfolds=10, intercept = FALSE)
## Fit lasso model with 100 values for lambda
enet_mdl = glmnet(x=joinedDataFrame_outdoor, y=y_train,alpha=0.5,standardize=TRUE,nlambda=100, intercept = FALSE)
## Extract coefficients at optimal lambda
coef(enet_mdl,s=enet.cv$lambda.min)
iteration_elasticnet <- append(iteration_elasticnet, enet.cv$lambda.min)
}
coef(enet_mdl,s=mean(iteration_elasticnet))
coef(enet_mdl,s=median(iteration_elasticnet))

# Using REG SUBSET to perform model selection
mod.full <-  regsubsets(cOI_2weeksbefore ~ ., intercept = FALSE, data = as.data.frame(joinedDataFrame_outdoor), nvmax = 3)
mod.summary <-  summary(mod.full)
which.min(mod.summary$cp)
which.max(mod.summary$adjr2)
coefficients(mod.full, id=2)

plot(mod.summary$cp, xlab="Subset Size", ylab="Cp", pch=20, type="l")
points(which.min(mod.summary$cp), min(mod.summary$cp), pch=4, col="red", lwd=7)

plot(mod.summary$bic, xlab="Subset Size", ylab="BIC", pch=20, type="l")
points(which.min(mod.summary$bic), min(mod.summary$bic), pch=4, col="red", lwd=7)

plot(mod.summary$adjr2, xlab="Subset Size", ylab="AdjrR^2", pch=20, type="l")
points(which.max(mod.summary$adjr2), max(mod.summary$adjr2), pch=4, col="red", lwd=7)

#Further model comparison, now considering tmax and tavg
summary(resultsList[["oOH2+oOH2:tmax"]][["cOI_2weeksbefore"]][["Model"]])
AIC(resultsList[["oOH2+oOH2:tmax"]][["cOI_2weeksbefore"]][["Model"]])
BIC(resultsList[["oOH2+oOH2:tmax"]][["cOI_2weeksbefore"]][["Model"]])
summary(resultsList[["oOH2+oOH2:tavg"]][["cOI_2weeksbefore"]][["Model"]])
AIC(resultsList[["oOH2+oOH2:tavg"]][["cOI_2weeksbefore"]][["Model"]])
BIC(resultsList[["oOH2+oOH2:tavg"]][["cOI_2weeksbefore"]][["Model"]])

# Creation of scatter plots of dependent vs. independent variable
# Further: Regression line for linear/quadratic/cubic model is created and added to the scatter plot
oOH_x <- seq(min(joinedDataFrame$outOfHomeDuration), max(joinedDataFrame$outOfHomeDuration), length.out = 40)
oOH_y <- resultsList[["oOH"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[1]] + resultsList[["oOH"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[2]]*oOH_x

oOH2_x <- seq(min(joinedDataFrame$outOfHomeDuration), max(joinedDataFrame$outOfHomeDuration), length.out = 40)
oOH2_y <- resultsList[["oOH2"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[1]] + resultsList[["oOH2"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[2]]*oOH_x^2

oOH3_x <- seq(min(joinedDataFrame$outOfHomeDuration), max(joinedDataFrame$outOfHomeDuration), length.out = 40)
oOH3_y <- resultsList[["oOH3"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[1]] + resultsList[["oOH3"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[2]]*oOH_x^3


p1 <- joinedDataFrame %>% filter(cOI_2weeksbefore < 2) %>% filter(Date + 14 < "2021-01-01") %>%
ggplot(aes(x = outOfHomeDuration, y = cOI_2weeksbefore)) + 
geom_point() +
#geom_line(aes(oOH_x, oOH_y, color = "linear")) + #Adds regression line to scatter plot
#geom_line(aes(oOH2_x, oOH2_y, color = "quadratic")) +
#geom_line(aes(oOH3_x, oOH3_y, , color = "cubic")) +
theme_minimal() +
theme(legend.position = "bottom", text = element_text(size = 12), legend.title=element_blank()) +
xlab("Daily Out Of Home Duration \n per Person") +
ylab("Growth Multiplier")

p2 <- joinedDataFrame %>% filter(cOI_2weeksbefore < 2) %>% filter(Date + 14 < "2021-01-01") %>%
ggplot(aes(x = outdoorFraction2, y = cOI_2weeksbefore)) + 
geom_point() +
theme_minimal() +
theme(legend.position = "bottom", text = element_text(size = 12), legend.title=element_blank()) +
xlab("Outdoor \n Fraction") +
ylab("")

p3 <- joinedDataFrame %>% filter(cOI_2weeksbefore < 2) %>% filter(Date + 14 < "2021-01-01") %>%
ggplot(aes(x = outdoorFraction2*outOfHomeDuration, y = cOI_2weeksbefore)) + 
geom_point() +
theme_minimal() +
theme(legend.position = "bottom", text = element_text(size = 12), legend.title=element_blank()) +
xlab("Daily Out Of Home Duration/Person \n times Outdoor Fraction") +
ylab("")

p <- arrangeGrob(p1,p2, nrow=1)

ggsave("Independentvsdependent.pdf", p, dpi = 500, w = 9, h = 3)

# Extracting cross validation results, we are only looking at the cross validation results for the chosen model. 
# Model name in following line needs to be changed if one's interested in another model
cross_validation_results <- data.frame(resultsList[["oOH2+oOH2:out2_noInt"]][["cOI_2weeksbefore"]][["Model_10foldCV"]]$resample$MAE, resultsList[["oOH2+indoor"]][["cOI_2weeksbefore"]][["Model_10foldCV"]]$resample$RMSE, resultsList[["oOH2+indoor"]][["cOI_2weeksbefore"]][["Model_10foldCV"]]$resample$Resample)
colnames(cross_validation_results) <- c("MAE", "RMSE", "Fold")

cross_validation_results[1,3] <- "01"
cross_validation_results[2,3] <- "02"
cross_validation_results[3,3] <- "03"
cross_validation_results[4,3] <- "04"
cross_validation_results[5,3] <- "05"
cross_validation_results[6,3] <- "06"
cross_validation_results[7,3] <- "07"
cross_validation_results[8,3] <- "08"
cross_validation_results[9,3] <- "09"
cross_validation_results[10,3] <- "10"

p1 <- ggplot(data = cross_validation_results, aes(x = Fold, y = RMSE, group = 1)) +
geom_boxplot(color= "#7570B3", size = 1.2) +
geom_point(color = "#666666", size = 1.5) +
theme_minimal() +
theme(text = element_text(size = 13)) +
theme(axis.ticks.x = element_line(), 
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))

p2 <- ggplot(data = cross_validation_results, aes(x = Fold, y = MAE, group = 1)) +
geom_boxplot(color= "#7570B3", size = 1.2) +
geom_point(color = "#666666", size = 1.5) +
theme_minimal() +
theme(text = element_text(size = 13)) +
theme(axis.ticks.x = element_line(), 
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))

p <- arrangeGrob(p2,p1, nrow=1)

#PLOTTING REGRESSION RESULTS IN 3D
# Model output using 2(!) variables 
summary(resultsList[["oOH2+oOH2:out2_noInt"]][["cOI_2weeksbefore"]][["Model"]])
coefficients(mod.full, id=2)

reduced_joinedDataFrame <- joinedDataFrame %>% filter(cOI_2weeksbefore < 2) %>% filter(Date +14 < "2021-01-01") %>% 
  select("outOfHomeDurationCubed", "outOfHomeDurationSquared","outOfHomeDuration", "outdoorFraction2", "cOI_2weeksbefore", "Welle") %>%
  mutate(Welle_color = case_when(Welle == "First Wave" ~ "#1B9E77",
                      Welle == "Summer break" ~ "#7570B3", Welle == "Second Wave" ~ "#D95F02"))

my_surface <- function(f, n=10, ...) {
  x <- seq(min(reduced_joinedDataFrame$outOfHomeDuration), max(reduced_joinedDataFrame$outOfHomeDuration), length = 100)
  y <- seq(min(reduced_joinedDataFrame$outdoorFraction2), max(reduced_joinedDataFrame$outdoorFraction2), length = 100)
  z <- outer(x,y,f)
  surface3d(x, y, z, ...)
}

# Coefficients have been manually extracted
f <- function(outOfHomeDuration, outdoorFraction2){
  0.024224 * outOfHomeDuration * outOfHomeDuration  -0.005036  * outOfHomeDuration * outOfHomeDuration * outdoorFraction2
}

x1 <- reduced_joinedDataFrame$outOfHomeDurationSquared
x2 <- reduced_joinedDataFrame$outdoorFraction2
y <- f(x1, x2)

plot3d(reduced_joinedDataFrame$outOfHomeDuration,reduced_joinedDataFrame$outdoorFraction2,reduced_joinedDataFrame$cOI_2weeksbefore, type="p", col = reduced_joinedDataFrame$Welle_color, xlab="out Of Home Duration", ylab="outdoor Fraction", zlab="change of Incidence", size = 7)
my_surface(f, alpha = 0.2)


#Model output using 3(!) variables 
summary(resultsList[["oOH3+oOHout+oOH2out"]][["cOI_2weeksbefore"]][["Model"]])

coefficients(mod.full, id=3)

my_surface <- function(f, n=10, ...) {
  x <- seq(min(reduced_joinedDataFrame$outOfHomeDuration), max(reduced_joinedDataFrame$outOfHomeDuration), length = 100)
  y <- seq(min(reduced_joinedDataFrame$outdoorFraction2), max(reduced_joinedDataFrame$outdoorFraction2), length = 100)
  z <- outer(x,y,f)
  surface3d(x, y, z, ...)
}


f <- function(outOfHomeDuration = reduced_joinedDataFrame$outOfHomeDuration, outdoorFraction2 =  reduced_joinedDataFrame$outdoorFraction2){
  0.003426 * outOfHomeDuration * outOfHomeDuration * outOfHomeDuration + 0.309101 * outOfHomeDuration * outdoorFraction2  -0.048321 * outOfHomeDuration * outOfHomeDuration * outdoorFraction2
}
x1 <- reduced_joinedDataFrame$outOfHomeDuration
x2 <- reduced_joinedDataFrame$outdoorFraction2
y <- f(x1, x2, x3, x4)

plot3d(x1,x2,reduced_joinedDataFrame$cOI_2weeksbefore, type="p", col = reduced_joinedDataFrame$Welle_color, xlab="out Of Home Duration", ylab="outdoor Fraction", zlab="change of Incidence", size = 7)
my_surface(f, alpha = 0.2)
