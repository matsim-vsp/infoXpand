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

source("PrepIncidenceData_Fed.R")
source("PrepMobilityData_Fed.R")
source("PrepWeatherData_Fed.R")

joinedDataFrame <- inner_join(incidence_data, mobility_data, by = c("Bundesland_id", "Date"))
                                                             
#Joining the data frames
joinedDataFrame <- left_join(joinedDataFrame, weather_data_all, by = c("Bundesland_id", "Date", "Bundesland"))

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

                                                                

####### Different types of models #######

# D = outOfHomeDuration
# I = changeOfIncidence
# tmax = tmax
# tavg = tavg
# out = outdoorFraction
# out2 = outdoorFraction2

#Performing linear regression
resultsList <- list()
 
id <- "oOH2+oOH2:out2_noInt"

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
        #mutate(indoorFraction = indoorFraction * 100) %>%
        #mutate(outdoorFraction2 = outdoorFraction2 * 100) %>%
        #mutate(cOI_2weeksbefore = (cOI_2weeksbefore-1)*100) %>%
        mutate(log_cOI_2weeksbefore = log(cOI_2weeksbefore))
# joinedDataFrame <- joinedDataFrame[complete.cases(joinedDataFrame), ]
set.seed(15)

federal_states <- c("SH", "HH", "NI", "HB", "NW", "HE", "RP", "BW", "BY", "SL", "BE", "BB", "MV", "SN", "ST", "TH")

for (fed_state in federal_states) {
    joinedDataFrame_reduced <- joinedDataFrame  %>%  filter(Date > "2020-03-08") %>% 
                                                     filter(Date < "2020-12-20") %>%
                                                     filter(Kurzform == fed_state) 
    joinedDataFrame_reduced$labeled <- ifelse(joinedDataFrame_reduced[[lag]] < 1.5, "", as.character(joinedDataFrame_reduced$Date+14, format = "%d/%b/%y"))
    chosen_welle <- "Welle_2weeks"
  resultsList[[fed_state]][[lag]] <- list()
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
    resultsList[[fed_state]][[lag]][["Model"]] <- model.lm
    resultsList[[fed_state]][[lag]][["AIC"]] <- AIC(model.lm)
    resultsList[[fed_state]][[lag]][["BIC"]] <- BIC(model.lm)

    #Leave one out cross validation
    train.control <- trainControl(method = "LOOCV")
    model.loocv <- train(eval(parse(text=formula.lm)), data = joinedDataFrame_reduced, method = "lm", trControl = train.control)
    resultsList[[fed_state]][[lag]][["Model_LOOCV"]] <- model.loocv

    #10-fold cross validation
    train.control <- trainControl(method = "cv", number = 10)
    model.10foldcv <- train(eval(parse(text=formula.lm)), data = joinedDataFrame_reduced, method = "lm", trControl = train.control, tuneGrid  = expand.grid(intercept = FALSE))
    resultsList[[fed_state]][[lag]][["Model_10foldCV"]] <- model.10foldcv

    joinedDataFrame_reduced[[chosen_welle]] <- factor(joinedDataFrame_reduced[[chosen_welle]], levels = c("First Wave", "Summer break", "Second Wave"))
    joinedDataFrame_reduced <- joinedDataFrame_reduced %>% mutate(predicted = predict(model.lm))
    resultsList[[fed_state]][[lag]][["PlotActualFitted"]]  <- local({
    lag <- lag
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
    print(g)
    })

  date_breaks <- data.frame(start = c(as.Date("2020-03-22"), as.Date("2020-05-17"), as.Date("2020-09-28")),
                          end = c(as.Date("2020-05-17"), as.Date("2020-09-28"), as.Date("2021-01-01")),
                          colors = c("First Wave", "Summer Break", "Second Wave"))
date_breaks$colors <- factor(date_breaks$colors, levels = c("First Wave", "Summer Break", "Second Wave"))
joinedDataFrame_reduced <- joinedDataFrame_reduced %>% mutate(predicted = predict(model.lm))

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
  resultsList[[fed_state]][[lag]][["PlotActualFitted_overTime"]] <- g2

    resultsList[[fed_state]][[lag]][["ResvsFitted"]] <- function() {
    plot(model.lm, which = 1)
    }
    resultsList[[fed_state]][[lag]][["Qqplot"]] <- function() {
    plot(model.lm, which = 2)
    }
    resultsList[[fed_state]][[lag]][["ScaleLoc"]] <- function() {
    plot(model.lm, which = 3)
    }
    resultsList[[fed_state]][[lag]][["Cooksdist"]] <- function() {
    plot(model.lm, which = 4)
    }

  }
}


summary(resultsList[["SH"]][["cOI_2weeksbefore"]][["Model"]])
summary(resultsList[["HH"]][["cOI_2weeksbefore"]][["Model"]])
summary(resultsList[["HB"]][["cOI_2weeksbefore"]][["Model"]])
summary(resultsList[["BE"]][["cOI_2weeksbefore"]][["Model"]])
summary(resultsList[["BY"]][["cOI_2weeksbefore"]][["Model"]])
