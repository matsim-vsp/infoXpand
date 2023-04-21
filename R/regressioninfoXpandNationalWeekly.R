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

joinedDataFrame <- joinedDataFrame %>% mutate(Welle = case_when(Date < "2020-06-01" ~ "1. Welle",
                                                                Date >= "2020-06-01" & Date <= "2020-10-01" ~ "Sommer",
                                                                Date > "2020-10-01" ~ "2. Welle"))

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


ids <- c("oOH", "oOH2", "oOH+oOH2", "oOH+tmax", "oOH+tavg",
          "oOH*tmax", "oOH*tavg", "oOH+out", "oOH+out2", "oOH*out", "oOH*out2",
          "oOH+prcp", "oOH+tmax+prcp", "oOH+tavg+prcp", "oOH+out+prcp", "oOH+out2+prcp",
          "oOH:tmax:prcp", "oOH:tavg:prcp", "oOH:out:prcp", "oOH:out2:prcp",
          "oOH2*out", "oOH2*out2", "oOH*tmax2", "oOH*tavg2",
          "oOH*tmax*prcp", "oOH*tavg*prcp", "oOH*out*prcp", "oOH*out2*prcp", "logoOH+logtmax",
          "c_oOH", "c_oOH+c_tmax", "c_oOH+c_tavg", "c_oOH+c_out", "c_oOH+c_out2",
          "c_oOH*c_tmax", "c_oOH*c_tavg", "c_oOH*c_out", "c_oOH*c_out2")

lags <- c("cOI", "cOI_1weekbefore", "cOI_2weeksbefore", "cOI_3weeksbefore", "cOI_4weeksbefore", "cOI_5weeksbefore")

joinedDataFrame <- joinedDataFrame %>%
        mutate(outOfHomeDurationSquared = outOfHomeDuration*outOfHomeDuration) %>%
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
        mutate(c_outdoorFraction2 = outdoorFraction2 - mean(outdoorFraction2)) 

joinedDataFrame <- joinedDataFrame[complete.cases(joinedDataFrame), ]

for (id in ids) {
  for (lag in lags) {
  if (lag == "cOI") {
    joinedDataFrame <- joinedDataFrame %>% filter(cOI < 2)
  } else if (lag == "cOI_1weekbefore") {
    joinedDataFrame <- joinedDataFrame %>% filter(cOI_1weekbefore < 2)
  } else if (lag == "cOI_2weeksbefore") {
    joinedDataFrame <- joinedDataFrame %>% filter(cOI_2weeksbefore < 2)
  } else if (lag == "cOI_3weeksbefore") {
    joinedDataFrame <- joinedDataFrame %>% filter(cOI_3weeksbefore < 2)
  } else if (lag == "cOI_4weeksbefore") {
    joinedDataFrame <- joinedDataFrame %>% filter(cOI_4weeksbefore < 2)
  } else if (lag == "cOI_5weeksbefore") {
    joinedDataFrame <- joinedDataFrame %>% filter(cOI_5weeksbefore < 2)
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
  } 

    model.lm <- lm(formula = formula.lm, data = joinedDataFrame) # Regression
    resultsList[[id]][[lag]][["Model"]] <- model.lm
    resultsList[[id]][[lag]][["AIC"]] <- AIC(model.lm)
    resultsList[[id]][[lag]][["BIC"]] <- BIC(model.lm)

    #Leave one out cross validation
    train.control <- trainControl(method = "LOOCV")
    model.loocv <- train(eval(parse(text=formula.lm)), data = joinedDataFrame, method = "lm", trControl = train.control)
    resultsList[[id]][[lag]][["Model_LOOCV"]] <- model.loocv

    resultsList[[id]][[lag]][["PlotActualFitted"]] <- ggplot(data = joinedDataFrame) + 
    geom_smooth(aes(x=predict(model.lm), y = .data[[lag]], color="#4b4b4b"), method="lm", size = 2) +
    xlab("Prediction") +
    ylab("Actual change of Incidence") +
    ggtitle(paste0("lag: ", lag, ", Model: ", id)) +
    geom_abline(aes(intercept = 0, slope = 1, color = "blue")) +
    scale_color_identity(labels = c("x=y", "Regression line"), guide = "legend") +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
   geom_point(data = joinedDataFrame, aes(x = predict(model.lm), y = .data[[lag]], fill=Welle), size=3, shape=21)


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

summary(resultsList[["oOH"]][["cOI"]][["Model"]])
summary(resultsList[["oOH"]][["cOI_1weekbefore"]][["Model"]])
summary(resultsList[["oOH"]][["cOI_2weeksbefore"]][["Model"]])
summary(resultsList[["oOH*out2"]][["cOI_2weeksbefore"]][["Model"]])
summary(resultsList[["oOH"]][["cOI_3weeksbefore"]][["Model"]])
summary(resultsList[["oOH"]][["cOI_4weeksbefore"]][["Model"]])
summary(resultsList[["oOH"]][["cOI_5weeksbefore"]][["Model"]])

summary(resultsList[["oOH+tmax"]][["cOI_2weeksbefore"]][["Model"]])
summary(resultsList[["oOH+tavg"]][["cOI_2weeksbefore"]][["Model"]])
summary(resultsList[["oOH+out"]][["cOI_2weeksbefore"]][["Model"]])
summary(resultsList[["oOH+out2"]][["cOI_2weeksbefore"]][["Model"]])

summary(resultsList[["c_oOH"]][["cOI_2weeksbefore"]][["Model"]])
summary(resultsList[["c_oOH+c_tmax"]][["cOI_2weeksbefore"]][["Model"]])
summary(resultsList[["c_oOH+c_tavg"]][["cOI_2weeksbefore"]][["Model"]])
summary(resultsList[["c_oOH+c_out"]][["cOI_2weeksbefore"]][["Model"]])
summary(resultsList[["c_oOH+c_out2"]][["cOI_2weeksbefore"]][["Model"]])
summary(resultsList[["c_oOH*c_tmax"]][["cOI_2weeksbefore"]][["Model"]])
summary(resultsList[["c_oOH*c_tavg"]][["cOI_2weeksbefore"]][["Model"]])
summary(resultsList[["c_oOH*c_out"]][["cOI_2weeksbefore"]][["Model"]])
summary(resultsList[["c_oOH*c_out2"]][["cOI_2weeksbefore"]][["Model"]])

resultsList[["oOH+out2"]][["cOI_2weeksbefore"]][["ResvsFitted"]]()
resultsList[["oOH+out"]][["cOI_2weeksbefore"]][["Qqplot"]]()
resultsList[["oOH+out"]][["cOI_2weeksbefore"]][["ScaleLoc"]]()
resultsList[["oOH+out"]][["cOI_2weeksbefore"]][["Cooksdist"]]()

grid.arrange(resultsList[["oOH"]][["cOI"]][["PlotActualFitted"]], resultsList[["oOH"]][["cOI_1weekbefore"]][["PlotActualFitted"]],
resultsList[["oOH"]][["cOI_2weeksbefore"]][["PlotActualFitted"]], resultsList[["oOH"]][["cOI_3weeksbefore"]][["PlotActualFitted"]], nrow=2)

grid.arrange(resultsList[["oOH*out"]][["cOI"]][["PlotActualFitted"]], resultsList[["oOH*out"]][["cOI_1weekbefore"]][["PlotActualFitted"]],
resultsList[["oOH*out"]][["cOI_2weeksbefore"]][["PlotActualFitted"]], resultsList[["oOH*out"]][["cOI_3weeksbefore"]][["PlotActualFitted"]], nrow=2)

g <- arrangeGrob(resultsList[["oOH*out"]][["cOI"]][["PlotActualFitted"]], resultsList[["oOH*out"]][["cOI_1weekbefore"]][["PlotActualFitted"]],
resultsList[["oOH*out"]][["cOI_2weeksbefore"]][["PlotActualFitted"]], resultsList[["oOH*out"]][["cOI_3weeksbefore"]][["PlotActualFitted"]], nrow=2)


grid.arrange(resultsList[["oOH*tmax"]][["cOI"]][["PlotActualFitted"]], resultsList[["oOH*tmax"]][["cOI_1weekbefore"]][["PlotActualFitted"]],
resultsList[["oOH*tmax"]][["cOI_2weeksbefore"]][["PlotActualFitted"]], resultsList[["oOH*tmax"]][["cOI_3weeksbefore"]][["PlotActualFitted"]], nrow=2)


g <- arrangeGrob(resultsList[["oOH"]][["cOI"]][["PlotActualFitted"]], resultsList[["oOH"]][["cOI_1weekbefore"]][["PlotActualFitted"]],
resultsList[["oOH"]][["cOI_2weeksbefore"]][["PlotActualFitted"]], resultsList[["oOH"]][["cOI_3weeksbefore"]][["PlotActualFitted"]], nrow=2)