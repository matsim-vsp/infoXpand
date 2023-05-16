library(tidyverse)
library(lubridate)
library(readxl)
library(httr)
library(gridExtra)
library(ggiraphExtra)
library(leaps)
library(caret) #Package to perform leave-one-out cross validation

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

joinedDataFrame <- joinedDataFrame %>% mutate(Welle = case_when(Date < "2020-05-17" ~ "1st Wave",
                                                                Date >= "2020-05-17" & Date <= "2020-09-27" ~ "Summer break",
                                                                Date > "2020-09-27" ~ "2nd Wave"))


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
resultsList[["oOH+oOH2"]] <- list()
resultsList[["oOH+tmax"]] <- list()
resultsList[["oOH+tavg"]] <- list()
resultsList[["oOH*tmax"]] <- list()
resultsList[["oOH*tavg"]] <- list()
resultsList[["oOH+out"]] <- list()
resultsList[["oOH+out2"]] <- list()
resultsList[["oOH2+out"]] <- list()
resultsList[["oOH2+out2"]] <- list()
resultsList[["oOH+oOH2+out"]] <- list()
resultsList[["oOH+oOH2+out2"]] <- list()
resultsList[["oOH+oOH2+out2_0Intercept"]] <- list()
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
resultslist[["polyoOH3+polyout23"]] <- list()
resultslist[["polyoOH2+polyout23"]] <- list()
resultslist[["polyoOH2+out2+out23"]] <- list()
resultslist[["oOH2+out2_noInt"]] <- list()

ids <- c("oOH", "oOH2", "oOH+oOH2", "oOH+tmax", "oOH+tavg",
          "oOH*tmax", "oOH*tavg", "oOH+oOH2+out", "oOH+oOH2+out2", "oOH+out", "oOH+out2", "oOH2+out", "oOH2+out2", "oOH*out", "oOH*out2",
          "oOH+prcp", "oOH+tmax+prcp", "oOH+tavg+prcp", "oOH+out+prcp", "oOH+out2+prcp",
          "oOH:tmax:prcp", "oOH:tavg:prcp", "oOH:out:prcp", "oOH:out2:prcp",
          "oOH2*out", "oOH2*out2", "oOH*tmax2", "oOH*tavg2",
          "oOH*tmax*prcp", "oOH*tavg*prcp", "oOH*out*prcp", "oOH*out2*prcp", "logoOH+logtmax",
          "c_oOH", "c_oOH+c_tmax", "c_oOH+c_tavg", "c_oOH+c_out", "c_oOH+c_out2",
          "c_oOH*c_tmax", "c_oOH*c_tavg", "c_oOH*c_out", "c_oOH*c_out2", "oOH+oOH2+out2", "oOH+oOH2+out2_0Intercept", "oOH+oOH2+out2+gradtmax",
          "polyoOH+polyout2", "oOH*out2+oOH2", "oOH+oOH2*out2", "polyoOH3+polyout23", "polyoOH2+polyout23", "polyoOH2+out2+out23", "oOH2+out2_noInt")

lags <- c("cOI", "cOI_1weekbefore", "cOI_2weeksbefore", "cOI_3weeksbefore", "cOI_4weeksbefore", "cOI_5weeksbefore")

joinedDataFrame <- joinedDataFrame %>%
        mutate(outOfHomeDurationSquared = outOfHomeDuration*outOfHomeDuration) %>%
        mutate(outdoorFraction2Squared = outdoorFraction2*outdoorFraction2) %>%
        mutate(tmaxSquared = tmax * tmax) %>%
        mutate(tavgSquared = tavg * tavg) %>%
        mutate(logOutOfHomeDuration = log(outOfHomeDuration)) %>%
        mutate(logtmax = log(tmax)) %>%
        mutate(logtavg = log(tavg)) %>%
        mutate(onePlusPercChange = 1 + percentageChangeComparedToBeforeCorona/100) %>%
        mutate(c_outOfHomeDuration = outOfHomeDuration - mean(outOfHomeDuration)) %>%
        mutate(c_tmax = tmax - mean(tmax)) %>%
        mutate(c_tavg = tavg - mean(tavg)) %>%
        mutate(c_outdoorFraction = outdoorFraction - mean(outdoorFraction)) %>%
        mutate(c_outdoorFraction2 = outdoorFraction2 - mean(outdoorFraction2)) %>%
        mutate(stand_outOfHome = scale(outOfHomeDuration),
              stand_outOfHomeSquared = scale(outOfHomeDurationSquared),
              stand_tmax = scale(tmax)) %>%
        mutate(centDiffquot_tmax = (lead(tmax)-lag(tmax))/2) %>%
        mutate(outdoorFraction2Cubed = outdoorFraction2 *outdoorFraction2 * outdoorFraction2)


# joinedDataFrame <- joinedDataFrame[complete.cases(joinedDataFrame), ]

for (id in ids) {
  for (lag in lags) {
  if (lag == "cOI") {
    joinedDataFrame_reduced <- joinedDataFrame %>% filter(cOI < 2)
  } else if (lag == "cOI_1weekbefore") {
    joinedDataFrame_reduced <- joinedDataFrame %>% filter(cOI_1weekbefore < 2)
  } else if (lag == "cOI_2weeksbefore") {
    joinedDataFrame_reduced <- joinedDataFrame %>% filter(cOI_2weeksbefore < 2)
  } else if (lag == "cOI_3weeksbefore") {
    joinedDataFrame_reduced <- joinedDataFrame %>% filter(cOI_3weeksbefore < 2)
  } else if (lag == "cOI_4weeksbefore") {
    joinedDataFrame_reduced <- joinedDataFrame %>% filter(cOI_4weeksbefore < 2)
  } else if (lag == "cOI_5weeksbefore") {
    joinedDataFrame_reduced <- joinedDataFrame %>% filter(cOI_5weeksbefore < 2)
  }
  resultsList[[id]][[lag]] <- list()
  if (id == "oOH") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration")
  } else if (id == "oOH2") {
    formula.lm <- paste0(lag, " ~ outOfHomeDurationSquared")
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
  } else if (id == "oOH2+out") {
    formula.lm <- paste0(lag, " ~ outOfHomeDurationSquared + outdoorFraction")
  } else if (id == "oOH2+out2") {
    formula.lm <- paste0(lag, " ~ outOfHomeDurationSquared + outdoorFraction2")
  } else if (id == "oOH2+out2_noInt") {
    formula.lm <- paste0(lag, " ~ 0+ outOfHomeDurationSquared + outdoorFraction2")
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
  } else if (id == "oOH*tmax2") {
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
  } else if (id == "c_oOH") {
    formula.lm <- paste0(lag, " ~ c_outOfHomeDuration")
  } else if (id == "c_oOH+c_tmax") {
    formula.lm <- paste0(lag, " ~ c_outOfHomeDuration + c_tmax")
  } else if (id == "c_oOH+c_tavg") {
    formula.lm <- paste0(lag, " ~ c_outOfHomeDuration + c_tavg")
  } else if (id == "c_oOH+c_out") {
    formula.lm <- paste0(lag, " ~ c_outOfHomeDuration + c_outdoorFraction")
  } else if (id == "c_oOH+c_out2") {
    formula.lm <- paste0(lag, " ~ c_outOfHomeDuration + c_outdoorFraction2")
  } else if (id == "c_oOH*c_tmax") {
    formula.lm <- paste0(lag, " ~ c_outOfHomeDuration * c_tmax")
  } else if (id == "c_oOH*c_tavg") {
    formula.lm <- paste0(lag, " ~ c_outOfHomeDuration * c_tavg")
  } else if (id == "c_oOH*c_out") {
    formula.lm <- paste0(lag, " ~ c_outOfHomeDuration * c_outdoorFraction")
  } else if (id == "c_oOH*c_out2") {
    formula.lm <- paste0(lag, " ~ c_outOfHomeDuration * c_outdoorFraction2")
  }  else if (id == "oOH+oOH2+out2") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + outOfHomeDurationSquared + outdoorFraction2")
  }  else if (id == "oOH+oOH2+out2_0Intercept") {
    formula.lm <- paste0(lag, " ~ 0 + outOfHomeDurationSquared + outdoorFraction2")
  } else if (id == "oOH+oOH2+out2+gradtmax") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + outOfHomeDurationSquared + outdoorFraction2 + centDiffquot_tmax")
    joinedDataFrame_reduced <- joinedDataFrame_reduced[-1,]
    joinedDataFrame_reduced <- joinedDataFrame_reduced[-nrow(joinedDataFrame_reduced),]
  } else if (id == "polyoOH+polyout2") {
    formula.lm <- paste0(lag, " ~ poly(outOfHomeDuration,2)+poly(outdoorFraction2,2)")
  } else if (id == "oOH*out2+oOH2") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration*outdoorFraction2 + outOfHomeDurationSquared")
  } else if (id == "oOH+oOH2*out2") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + outOfHomeDurationSquared*outdoorFraction2")
  } else if (id == "polyoOH3+polyout23") {
    formula.lm <- paste0(lag, " ~ poly(outOfHomeDuration,3) + poly(outdoorFraction2,3)")
  } else if (id == "polyoOH2+polyout23") {
    formula.lm <- paste0(lag, " ~ poly(outOfHomeDuration,2) + poly(outdoorFraction2,3)")
  } else if (id == "polyoOH2+out2+out23") {
    formula.lm <- paste0(lag, " ~ poly(outOfHomeDuration,2) + outdoorFraction2 + outdoorFraction2Cubed")
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
    model.10foldcv <- train(eval(parse(text=formula.lm)), data = joinedDataFrame_reduced, method = "lm", trControl = train.control)
    resultsList[[id]][[lag]][["Model_10foldCV"]] <- model.10foldcv

    joinedDataFrame_reduced$labeled <- ifelse(joinedDataFrame_reduced[[lag]] < 1.5, "", as.character(joinedDataFrame_reduced$Date))
    resultsList[[id]][[lag]][["PlotActualFitted"]] <- ggplot(data = joinedDataFrame_reduced) + 
    geom_smooth(aes(x=predict(model.lm), y = .data[[lag]], color="#4b4b4b"), method="lm", size = 2) +
    geom_text(aes(x = predict(model.lm), y = .data[[lag]], label=labeled),hjust=1.2, vjust=0.5) +
    xlab("Predicted Values") +
    ylab("Observed Values") +
    ggtitle(paste0("lag: ", lag, ", Model: ", id)) +
    #geom_abline(aes(intercept = 0, slope = 1, color = "blue")) +
    scale_color_identity(labels = c("Regression line"), guide = "legend") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", legend.title = element_blank(), legend.text=element_text(size=12)) +
   geom_point(data = joinedDataFrame_reduced, aes(x = predict(model.lm), y = .data[[lag]], fill=Welle), size=3, shape=21)


    resultsList[[id]][[lag]][["ResvsFitted"]] <- function() {
    plot(model.lm, which = 1)
    }
    resultsList[[id]][[lag]][["Qqplot"]] <- function() {
    plot(model.lm, which = 2)
    }
    resultsList[[id]][[lag]][["ScaleLoc"]] <- function() {
    plot(model.lm, which = 3)
    }
    resultsList[[id]][[lag]][["Cooksdist"]] <- function() {
    plot(model.lm, which = 4)
    }

  }
}

#Stepwise Model selection
#Step 1
model_step1 <- resultsList[["polyoOH3+polyout23"]][["cOI_2weeksbefore"]][["Model"]]
AIC(model_step1)
BIC(model_step1)
summary(resultsList[["polyoOH3+polyout23"]][["cOI_2weeksbefore"]][["Model"]])
confint(resultsList[["polyoOH3+polyout23"]][["cOI_2weeksbefore"]][["Model"]])
#Step2
model_step2 <- resultsList[["polyoOH2+polyout23"]][["cOI_2weeksbefore"]][["Model"]]
AIC(model_step2)
BIC(model_step2)
summary(resultsList[["polyoOH2+polyout23"]][["cOI_2weeksbefore"]][["Model"]])
confint(resultsList[["polyoOH2+polyout23"]][["cOI_2weeksbefore"]][["Model"]])

#Step3
model_step3 <- resultsList[["polyoOH2+out2+out23"]][["cOI_2weeksbefore"]][["Model"]]
AIC(model_step3)
BIC(model_step3)
summary(resultsList[["polyoOH2+out2+out23"]][["cOI_2weeksbefore"]][["Model"]])
confint(resultsList[["polyoOH2+out2+out23"]][["cOI_2weeksbefore"]][["Model"]])

#Step4 
model_step4 <- resultsList[["oOH+oOH2+out2"]][["cOI_2weeksbefore"]][["Model"]]
AIC(model_step4)
BIC(model_step4)
summary(resultsList[["oOH+oOH2+out2"]][["cOI_2weeksbefore"]][["Model"]])
confint(resultsList[["oOH+oOH2+out2"]][["cOI_2weeksbefore"]][["Model"]])

#Step5
model_step5 <- resultsList[["oOH2+out2"]][["cOI_2weeksbefore"]][["Model"]]
AIC(model_step5)
BIC(model_step5)
summary(resultsList[["oOH2+out2"]][["cOI_2weeksbefore"]][["Model"]])
confint(resultsList[["oOH2+out2"]][["cOI_2weeksbefore"]][["Model"]])

#Step6
model_step6 <- resultsList[["oOH2+out2_noInt"]][["cOI_2weeksbefore"]][["Model"]]
AIC(model_step6)
BIC(model_step6)
summary(resultsList[["oOH2+out2_noInt"]][["cOI_2weeksbefore"]][["Model"]])
confint(resultsList[["oOH2+out2_noInt"]][["cOI_2weeksbefore"]][["Model"]])






#We have decided on the model oOH+oOH2+out2
#We now check if the relationship changes over time
#We therefore consider the following 3 models
# (1) Using only data from the 1st wave
# (2) Using only data from the 2nd wave
# (3) Using data from all of 2020


joinedDataFrame_1st <- joinedDataFrame %>% filter(Welle == "1. Welle")
joinedDataFrame_2nd <- joinedDataFrame %>% filter(Welle == "2. Welle")

resultsList[["oOH"]][["cOI_2weeksbefore"]][["Model"]] <- lm(cOI_2weeksbefore ~ outOfHomeDuration, data = joinedDataFrame_1st)
resultsList[["oOH"]][["cOI_2weeksbefore"]][["Model"]] <- lm(cOI_2weeksbefore ~ outOfHomeDuration, data = joinedDataFrame_2nd)
 
resultsList[["oOH+out2_1st"]][["cOI_2weeksbefore"]][["Model"]] <- lm(cOI_2weeksbefore ~ outOfHomeDuration + outdoorFraction2, data = joinedDataFrame_1st)
resultsList[["oOH+out2_2nd"]][["cOI_2weeksbefore"]][["Model"]] <- lm(cOI_2weeksbefore ~ outOfHomeDuration + outdoorFraction2, data = joinedDataFrame_2nd)
