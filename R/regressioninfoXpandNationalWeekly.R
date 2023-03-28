library(tidyverse)
library(lubridate)
library(readxl)
library(httr)
library(gridExtra)
library(ggiraphExtra)
library(leaps)

source("PrepIncidenceData.R")
source("PrepMobilityData.R")
source("PrepWeatherData.R")

mobility_data <- mobility_data %>% filter(Bundesland == "Gesamt")
joinedDataFrame <- inner_join(incidence_data, mobility_data, by = "Date")
                                                             
#Joining the data frames
weather_data_all <- weather_data_all %>% filter(Bundesland == "Gesamt")
joinedDataFrame <- left_join(joinedDataFrame, weather_data_all, by = "Date")

#Only for 2020 (and a bit of 2021, as not too many people were vaccinated and alpha wasn't dominant yet)
joinedDataFrame <- filter(joinedDataFrame, Date < "2021-02-22") %>%
                      distinct()


joinedDataFrame <- joinedDataFrame %>% mutate(Welle = case_when(Date < "2020-06-01" ~ "1. Welle",
                                                                Date >= "2020-06-01" & Date <= "2020-10-01" ~ "Sommer",
                                                                Date > "2020-10-01" ~ "2. Welle"))

#Plotting changeOfIncidence over time and outOfHomeDuration over time and OutdoorFactor over time
nestedplotlist <- list()

incidencePlot <- ggplot(data = joinedDataFrame, aes(x = Date, y = IncidenceWed)) +
  geom_point() +
  theme_minimal()
changeOfIncidencePlot <- ggplot(data = joinedDataFrame, aes(x = Date, y = changeOfIncidencelaggedWed2)) +
  geom_point() +
  theme_minimal()
mobilityPlot <- ggplot(data = joinedDataFrame, aes(x = Date, y = outOfHomeDuration)) +
  geom_point() +
  theme_minimal()
tempPlot <- ggplot(data = joinedDataFrame, aes(x = Date, y = tmax)) +
  geom_point() +
  geom_hline(yintercept = 22.5) +
  geom_hline(yintercept = 12.5) +
  theme_minimal() +
  ylab("Tmax")
outOfHomevsIncidence <- ggplot(data=joinedDataFrame, aes(x=outOfHomeDuration, y=changeOfIncidencelaggedWed2)) +
  geom_point() +
  theme_minimal()
outOfHomeSquaredvsIncidence <- ggplot(data=joinedDataFrame, aes(x=outOfHomeDuration*outOfHomeDuration, y=changeOfIncidencelaggedWed2)) +
  geom_point() +
  theme_minimal()
outOfHomePolymvsIncidence <- ggplot(data=joinedDataFrame, aes(x=outOfHomeDuration*outOfHomeDuration+outOfHomeDuration, y=changeOfIncidencelaggedWed2)) +
  geom_point() +
  theme_minimal()
outOfHomePolymOutdoorvsIncidence <- ggplot(data=joinedDataFrame, aes(x=outOfHomeDuration*outOfHomeDuration+outdoorFraction, y=changeOfIncidencelaggedWed2e)) +
  geom_point() +
  theme_minimal()
outOfHomeOutdoorIncidence <- ggplot(data=joinedDataFrame, aes(x=outOfHomeDuration+outdoorFraction2, y=changeOfIncidencelaggedWed2)) +
  geom_point() +
  theme_minimal()
outdoorIncidence <- ggplot(data=joinedDataFrame, aes(x=outdoorFraction, y=changeOfIncidencelaggedWed2)) +
  geom_point() +
  theme_minimal()
tmaxIncidence <- ggplot(data=joinedDataFrame, aes(x=tmax, y=changeOfIncidencelaggedWed2)) +
  geom_point() +
  theme_minimal()
nestedplotlist[["incidencePlot"]] <- incidencePlot
nestedplotlist[["changeOfIncidencePlot"]] <- changeOfIncidencePlot
nestedplotlist[["mobilityPlot"]] <- mobilityPlot
nestedplotlist[["tempPlot"]] <- tempPlot
nestedplotlist[["outOfHomevsIncidence"]] <- outOfHomevsIncidence
nestedplotlist[["outOfHomeSquaredvsIncidence"]] <- outOfHomeSquaredvsIncidence
nestedplotlist[["outOfHomePolymvsIncidence"]] <- outOfHomePolymvsIncidence
nestedplotlist[["outOfHomePolymOutdoorvsIncidence"]] <- outOfHomePolymOutdoorvsIncidence
nestedplotlist[["outOfHomeOutdoorvsIncidence"]] <- outOfHomeOutdoorIncidence
nestedplotlist[["outdoorIncidence"]] <- outdoorIncidence
nestedplotlist[["tmaxvsIncidence"]] <- tmaxIncidence

#Examplary plot
g <- arrangeGrob(nestedplotlist[["incidencePlot"]], nestedplotlist[["changeOfIncidencePlot"]], nestedplotlist[["mobilityPlot"]], nestedplotlist[["tempPlot"]], nrow=4)

grid.arrange(nestedplotlist[["outOfHomevsIncidence"]],nestedplotlist[["outOfHomeSquaredvsIncidence"]],nestedplotlist[["outOfHomePolymvsIncidence"]],nestedplotlist[["outOfHomePolymOutdoorvsIncidence"]], nestedplotlist[["outOfHomeOutdoorvsIncidence"]], nestedplotlist[["tmaxvsIncidence"]], nrow=6)


####### From here on Trying to explore correlations #######
#Exploratory work, for now everything will remain. Later, whatever we deem unnecessary will be 

joinedDataFrameBerlin <- filter(joinedDataFrame, Bundesland == "Berlin")
joinedDataFrameBerlin <- joinedDataFrameBerlin[-1,] #Removing the 1st line as it we do not have a changeOfIncidence here

#1)
# 1a) Look at correlaction over whole time
cor(joinedDataFrameBerlin$changeOfIncidencelagged, joinedDataFrameBerlin$outOfHomeDuration)
# 1b) Over whole time, but outOfHomeDuration^2
cor(joinedDataFrameBerlin$changeOfIncidence, joinedDataFrameBerlin$outOfHomeDuration*joinedDataFrameBerlin$outOfHomeDuration)

#2)
#Look for correlations during 3 different t_max intervals : [-\infty, 12.5], (12.5,22.5), [22.5, \infty]
#Note: This does not lead to satisfactory correlactions
joinedDataFrameColdWeater <- filter(joinedDataFrameBerlin, tmax <= 12.5)
cor(joinedDataFrameColdWeater$changeOfIncidence, joinedDataFrameColdWeater$outOfHomeDuration)
joinedDataFrameWarmWeater <- filter(joinedDataFrameBerlin, tmax >= 22.5)
cor(joinedDataFrameWarmWeater$changeOfIncidence, joinedDataFrameWarmWeater$outOfHomeDuration) #Smallest correlation, additional idea: maybe the school holidays influence this somehow?
cor(joinedDataFrameWarmWeater$changeOfIncidence, joinedDataFrameWarmWeater$outdoorFraction) 
joinedDataFrameMediocreWeather <- filter(joinedDataFrameBerlin, tmax > 12.5) %>%
                                  filter(tmax < 22.5)
cor(joinedDataFrameMediocreWeather$changeOfIncidence, joinedDataFrameMediocreWeather$outOfHomeDuration)
cor(joinedDataFrameMediocreWeather$changeOfIncidence, joinedDataFrameMediocreWeather$tmax)

#3)
#Instead of splitting the whole thing by temperature, we go wave-wise # 2023-01-01 : The filtered intervals are similar to the intervals in 2)
#1st summer plateau from May - End ofSeptember
joinedDataFrameSummer <- filter(joinedDataFrameBerlin, Date < "2020-09-21")
cor(joinedDataFrameSummer$changeOfIncidence, joinedDataFrameSummer$outOfHomeDuration)
#Performing a linear regression solely on this part, looks like this
ggplot(joinedDataFrameSummer, aes(y = changeOfIncidence, x = outOfHomeDuration)) +
geom_point() +
geom_smooth(method = "lm") +
theme_minimal()
#Computing the cross-corelation of the two time series, to get an idea about lag
ccf(joinedDataFrameSummer$outOfHomeDuration, joinedDataFrameSummer$changeOfIncidence)

#2nd : rest
joinedDataFrameFall <- filter(joinedDataFrameBerlin, Date > "2020-09-21")
cor(joinedDataFrameFall$changeOfIncidence, joinedDataFrameFall$outOfHomeDuration)
#Performing a lin regression solely on this part, looks like this
ggplot(joinedDataFrameFall, aes(x = outOfHomeDuration,y = changeOfIncidence)) +
geom_point() +
geom_smooth(method = "lm") +
theme_minimal()
#Computing the cross-corelation of the two time series, to get an idea about lag
ccf(joinedDataFrameFall$outOfHomeDuration, joinedDataFrameFall$changeOfIncidence)


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
resultsList[["oOH+prcp"]] <- list()
resultsList[["oOH+tmax+prcp"]] <- list()
resultsList[["oOH+out+prcp"]] <- list()
resultsList[["oOH:out2:prcpvsI"]] <- list()
resultsList[["oOH*out"]] <- list()
resultsList[["oOH*tmax2"]] <- list()
resultsList[["oOH*tmax*prcp"]] <- list()
resultsList[["logoOH+logtmax"]] <- list()
resultsList[["1+percChange*tmax"]] <- list()


ids <- c("oOH", "oOH2", "oOH+oOH2", "oOH+tmax", "oOH+tavg",
          "oOH*tmax", "oOH*tavg", "oOH+out", "oOH+out2", 
          "oOH+prcp", "oOH+tmax+prcp", "oOH+out+prcp", "oOH:out2:prcpvsI",
          "oOH*out", "oOH*tmax2", "oOH*tmax*prcp", "logoOH+logtmax", "1+percChange*tmax")

lags <- c("cOI", "cOI_1weekbefore", "cOI_2weeksbefore", "cOI_3weeksbefore", "cOI_4weeksbefore", "cOI_5weeksbefore")

joinedDataFrame <- joinedDataFrame %>%
        mutate(outOfHomeDurationSquared = outOfHomeDuration*outOfHomeDuration) %>%
        mutate(tmaxSquared = tmax * tmax) %>%
        mutate(logOutOfHomeDuration = log(outOfHomeDuration)) %>%
        mutate(logtmax = log(tmax)) %>%
        mutate(onePlusPercChange = 1 + percentageChangeComparedToBeforeCorona/100) %>%
        distinct()

joinedDataFrame <- joinedDataFrame[complete.cases(joinedDataFrame), ]

for (id in ids){
  for (lag in lags){
  if(lag == "cOI") {
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
  } else if (id == "oOH+prcp") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + prcp")
  } else if (id == "oOH+tmax+prcp") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + tmax + prcp")
  } else if (id == "oOH+out+prcp") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration + outdoorFraction + prcp")
  } else if (id == "oOH:out2:prcpvsI") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration+outOfHomeDuration:outdoorFraction2+outOfHomeDuration:prcp")
  } else if (id == "oOH*out") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration*outdoorFraction")
  } else if (id == "oOH*tmax2") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration * tmaxSquared")
  } else if (id == "oOH*tmax*prcp") {
    formula.lm <- paste0(lag, " ~ outOfHomeDuration * tmax * prcp")
  } else if (id == "logoOH+logtmax") {
    formula.lm <- paste0(lag, " ~ logOutOfHomeDuration + logtmax")
  } else if (id == "1+percChange*tmax") {
    formula.lm <- paste0(lag, " ~ onePlusPercChange * tmax")
  }

    model.lm <- lm(formula = formula.lm, data = joinedDataFrame) # Regression

    plot23 <- ggplot(data = joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
    geom_smooth(aes(x=predict(model.lm), y = .data[[lag]], color="lightgrey"), method="lm", se=FALSE, size = 2) +
    xlab("Prediction") +
    ylab("Actual change of Incidence") +
    ggtitle(lag) +
    geom_abline(aes(intercept = 0, slope = 1, color = "blue")) +
    scale_color_identity(labels = c("x=y", "Regression line"), guide = "legend") +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
   geom_point(data = joinedDataFrame, aes(x = predict(model.lm), y = .data[[lag]], fill=Welle), size=3, shape=21)
    resultsList[[id]][[lag]][["Model"]] <- model.lm
    resultsList[[id]][[lag]][["PlotActualFitted"]] <- plot23

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
summary(resultsList[["oOH"]][["cOI_3weeksbefore"]][["Model"]])
summary(resultsList[["oOH"]][["cOI_4weeksbefore"]][["Model"]])
summary(resultsList[["oOH"]][["cOI_5weeksbefore"]][["Model"]])
resultsList[["oOH"]][["cOI_1weekbefore"]][["PlotActualFitted"]]

grid.arrange(resultsList[["oOH"]][["cOI"]][["PlotActualFitted"]], resultsList[["oOH"]][["cOI_1weekbefore"]][["PlotActualFitted"]],
resultsList[["oOH"]][["cOI_2weeksbefore"]][["PlotActualFitted"]], resultsList[["oOH"]][["cOI_3weeksbefore"]][["PlotActualFitted"]], nrow=2)

g <- arrangeGrob(resultsList[["oOH"]][["cOI"]][["PlotActualFitted"]], resultsList[["oOH"]][["cOI_1weekbefore"]][["PlotActualFitted"]],
resultsList[["oOH"]][["cOI_2weeksbefore"]][["PlotActualFitted"]], resultsList[["oOH"]][["cOI_3weeksbefore"]][["PlotActualFitted"]], nrow=2)