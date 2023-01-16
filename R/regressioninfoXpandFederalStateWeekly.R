library(tidyverse)
library(lubridate)
library(readxl)
library(httr)
library(gridExtra)

#Using R 4.1.1

#Looking at this on a federal state level, so here's a little reminder of the state's capitals. Weather IDs are taken from meteostat and were (for this exercise) extracted manually
# Baden-Württemberg (Hauptstadt: Stuttgart) 10737
# Bayern (München) 10865
# Berlin (Berlin) 10382
# Brandenburg (Potsdam) 10379
# Bremen (Bremen) 10224
# Hessen (Wiesbaden) 10633
# Mecklenburg-Vorpommern (Schwerin) 10162
# Niedersachsen (Hannover) 10338
# Nordrhein-Westfalen (Düsseldorf) 10400
# Rheinland-Pfalz (Mainz) D3137
# Saarland (Saarbrücken) D6217
# Sachsen (Dresden) D1051
# Sachsen-Anhalt (Magdeburg) 10361
# Schleswig-Holstein (Kiel) 10044
# Thüringen (Erfurt) 10554

#Setting up da data frame containing the states and the corresponding weather IDs
dictStateId <- data.frame(matrix(nrow = 0, ncol = 2))
colnames(dictStateId) <- c("Bundesland", "ID")
dictStateId[nrow(dictStateId) + 1, ] <- c("Baden-Württemberg", 10738)
dictStateId[nrow(dictStateId) + 1, ] <- c("Bayern", 10865)
dictStateId[nrow(dictStateId) + 1, ] <- c("Berlin", 10382)
dictStateId[nrow(dictStateId) + 1, ] <- c("Brandenburg", 10379)
dictStateId[nrow(dictStateId) + 1, ] <- c("Bremen", 10224)
dictStateId[nrow(dictStateId) + 1, ] <- c("Hamburg", 10147)
dictStateId[nrow(dictStateId) + 1, ] <- c("Hessen", 10633)
dictStateId[nrow(dictStateId) + 1, ] <- c("Mecklenburg-Vorpommern", 10162)
dictStateId[nrow(dictStateId) + 1, ] <- c("Niedersachsen", 10338)
dictStateId[nrow(dictStateId) + 1, ] <- c("Nordrhein-Westfalen", 10400)
dictStateId[nrow(dictStateId) + 1, ] <- c("Rheinland-Pfalz", "D3137")
dictStateId[nrow(dictStateId) + 1, ] <- c("Saarland", "D6217")
dictStateId[nrow(dictStateId) + 1, ] <- c("Sachsen", "D1051")
dictStateId[nrow(dictStateId) + 1, ] <- c("Sachsen-Anhalt", 10361)
dictStateId[nrow(dictStateId) + 1, ] <- c("Schleswig-Holstein", 10044)
dictStateId[nrow(dictStateId) + 1, ] <- c("Thüringen", 10554)

#Reading incidence data in, data hereby comes from RKI
url1 <-'https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Kum_Tab_Archiv.xlsx?__blob=publicationFile'
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
incidenceDataArchiv <- read_excel(tf, 3)
colnames(incidenceDataArchiv) <- incidenceDataArchiv[4, ]
colnames(incidenceDataArchiv)[1] <- "Bundesland"
incidenceDataArchiv <- incidenceDataArchiv[-(1:4), ]
incidenceDataArchiv <- pivot_longer(incidenceDataArchiv, names_to="Date", values_to="Incidence", cols=colnames(incidenceDataArchiv)[2:ncol(incidenceDataArchiv)])
incidenceDataArchiv$Date <- as.integer(incidenceDataArchiv$Date)
incidenceDataArchiv$Date <- as.Date(incidenceDataArchiv$Date,origin="1899-12-30")

url1 <- 'https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Kum_Tab_aktuell.xlsx?__blob=publicationFile'
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
incidenceData <- read_excel(tf, 4)
colnames(incidenceData) <- incidenceData[4, ]
colnames(incidenceData)[1] <- "Bundesland"
incidenceData <- incidenceData[-(1:4),] #Removing the 1st line, as this solely contains the column names
# hospitalizationData <- mutate(hospitalizationData, BundeslandId=c("08", "09", "11", "12", "04", "02", "06", "13", "03", "05", "07", "10", "14", "15", "01", "16", "00"))
incidenceData <- pivot_longer(incidenceData, names_to="Date", values_to="Incidence", cols=colnames(incidenceData)[2:ncol(incidenceData)])
incidenceData$Date <- paste(substr(incidenceData$Date, start = 7, stop = 10), substr(incidenceData$Date, start = 4, stop = 5), as.character(substr(incidenceData$Date, start = 1, stop = 2)), sep ="-" )
incidenceData$Date <- as.Date(incidenceData$Date)
incidenceData$Date <- incidenceData$Date

incidenceData <- rbind(incidenceDataArchiv, incidenceData)
incidenceData$Incidence <- as.double(incidenceData$Incidence)
incidendeData <- filter(incidenceData, Date < "2021-01-01")

incidenceData <- incidenceData %>%
  mutate(week = week(Date)) %>%
  mutate(year = year(Date)) %>%
  group_by(year, week, Bundesland) %>%
  summarise(Date = min(Date)+4, Incidence = mean(Incidence))

#Adding the change of incidence to the data frame
incidenceData <- incidenceData %>% group_by(Bundesland) %>%
summarise(Date = Date, Incidence = Incidence, changeOfIncidence = Incidence/lag(Incidence))

## Estimation for all states simultaneously

#Reading in Mobility data on federal state level, using weekly data
mobilityData <- read_delim("https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/episim/mobilityData/bundeslaender/mobilityData_OverviewBL_weekly.csv")

#Little bit of cleaning, turning date column into right format, renaming columns
mobilityData$date <- paste(substr(mobilityData$date, start = 1, stop = 4), substr(mobilityData$date, start = 5, stop = 6), as.character(substr(mobilityData$date, start = 7, stop = 8)), sep ="-" )
mobilityData$date <- as.Date(mobilityData$date)
colnames(mobilityData)[2] <- "Bundesland"
colnames(mobilityData)[1] <- "Date"

joinedDataFrame <- inner_join(incidenceData, mobilityData, by = c("Bundesland", "Date"))

 joinedDataFrame <- mutate(joinedDataFrame, changeOfIncidencelagged = lead(changeOfIncidence))
 joinedDataFrame <- mutate(joinedDataFrame, changeOfIncidencelagged2 = lead(changeOfIncidencelagged))

#Weather data
weatherDataAll <- data.frame(matrix(nrow = 0, ncol = 3))
for (state in 1:length(dictStateId$Bundesland)){
ID <- dictStateId[as.integer(state), 2]
weatherData <- read_delim(paste0("https://bulk.meteostat.net/daily/", ID, ".csv.gz"))
colnames(weatherData) <- c("Date", "tavg", "tmin", "tmax", "prcp", "snow", "wdir", "wspd", "wpgt", "pres", "tsun")

weatherData$Date <- as.Date(weatherData$Date)
weatherData <- weatherData[, c("Date", "tmax", "tavg", "prcp")]
weatherData$Bundesland <- dictStateId[as.integer(state), 1]

weatherDataAll <- rbind(weatherDataAll, weatherData)
}

weatherDataAll <- filter(weatherDataAll, Date < "2021-01-01") %>%
filter(Date > "2020-01-01") %>%
  mutate(week = week(Date)) %>%
  mutate(year = year(Date)) %>%
  group_by(year, week, Bundesland) %>%
  summarise(Date = min(Date)+4, tmax = mean(tmax), tavg = mean(tavg), prcp = mean(prcp))


#Converting the weather data to an "outdoor fraction"
#Below a certain temperature, everything happens indoords, above a certain temperature everything happens outdoors and in between we linearize
weatherDataAll <- mutate(weatherDataAll, outdoorFraction = case_when(22.5 >= tmax & tmax >= 12.5 ~ 2 - 0.1*(22.5 - tmax),
                                                                   tmax < 12.5 ~ 2,
                                                                  tmax > 22.5 ~ 1))
#Alternative to compute outdoor fraction
weatherDataAll <- mutate(weatherDataAll, outdoorFraction2 = case_when(22.5 >= tmax & tmax >= 12.5 ~ 0.1*(22.5 - tmax),
                                                                   tmax < 12.5 ~ 0,
                                                                  tmax > 22.5 ~ 1))                                                                 
#Joining the data frames
joinedDataFrame <- left_join(joinedDataFrame, weatherDataAll, by = c("Bundesland", "Date"))
joinedDataFrame <- mutate(joinedDataFrame, explanatory = outOfHomeDuration*outOfHomeDuration*(outdoorFraction)) #What am I using this one for?

#Only for 2020 (and a bit of 2021, as not too many people were vaccinated and alpha wasn't dominant yet)
joinedDataFrame <- filter(joinedDataFrame, Date < "2021-02-22") #Not quite sure why we are filtering again

#Plotting changeOfIncidence over time and outOfHomeDuration over time and OutdoorFactor over time
nestedplotlist <- list()

for (federalstate in dictStateId$Bundesland){
incidencePlot <- ggplot(data=filter(joinedDataFrame, Bundesland==federalstate), aes(x=Date, y=Incidence)) +
  geom_point() +
  theme_minimal()
changeOfIncidencePlot <- ggplot(data=filter(joinedDataFrame, Bundesland==federalstate), aes(x=Date, y=changeOfIncidence)) +
  geom_point() +
  theme_minimal()
mobilityPlot <- ggplot(data=filter(joinedDataFrame, Bundesland==federalstate), aes(x=Date, y=outOfHomeDuration)) +
  geom_point() +
  theme_minimal()
tempPlot <- ggplot(data=filter(joinedDataFrame, Bundesland==federalstate), aes(x=Date, y=tmax)) +
  geom_point() +
  geom_hline(yintercept = 22.5) +
  geom_hline(yintercept = 12.5) +
  theme_minimal() +
  ylab("Tmax")
outOfHomevsIncidence <- ggplot(data=filter(joinedDataFrame, Bundesland==federalstate), aes(x=outOfHomeDuration, y=changeOfIncidence)) +
  geom_point() +
  theme_minimal()
outOfHomeSquaredvsIncidence <- ggplot(data=filter(joinedDataFrame, Bundesland==federalstate), aes(x=outOfHomeDuration*outOfHomeDuration, y=changeOfIncidence)) +
  geom_point() +
  theme_minimal()
outOfHomePolymvsIncidence <- ggplot(data=filter(joinedDataFrame, Bundesland==federalstate), aes(x=outOfHomeDuration*outOfHomeDuration+outOfHomeDuration, y=changeOfIncidence)) +
  geom_point() +
  theme_minimal()
outOfHomePolymOutdoorvsIncidence <- ggplot(data=filter(joinedDataFrame, Bundesland==federalstate), aes(x=outOfHomeDuration*outOfHomeDuration+outdoorFraction, y=changeOfIncidence)) +
  geom_point() +
  theme_minimal()
outOfHomeOutdoorIncidence <- ggplot(data=filter(joinedDataFrame, Bundesland==federalstate), aes(x=outOfHomeDuration+outdoorFraction2, y=changeOfIncidence)) +
  geom_point() +
  theme_minimal()
outdoorIncidence <- ggplot(data=filter(joinedDataFrame, Bundesland==federalstate), aes(x=outdoorFraction, y=changeOfIncidence)) +
  geom_point() +
  theme_minimal()
tmaxIncidence <- ggplot(data=filter(joinedDataFrame, Bundesland==federalstate), aes(x=tmax, y=changeOfIncidence)) +
  geom_point() +
  theme_minimal()
nestedplotlist[[federalstate]] <- list()
nestedplotlist[[federalstate]][["incidencePlot"]] <- incidencePlot
nestedplotlist[[federalstate]][["changeOfIncidencePlot"]] <- changeOfIncidencePlot
nestedplotlist[[federalstate]][["mobilityPlot"]] <- mobilityPlot
nestedplotlist[[federalstate]][["tempPlot"]] <- tempPlot
nestedplotlist[[federalstate]][["outOfHomevsIncidence"]] <- outOfHomevsIncidence
nestedplotlist[[federalstate]][["outOfHomeSquaredvsIncidence"]] <- outOfHomeSquaredvsIncidence
nestedplotlist[[federalstate]][["outOfHomePolymvsIncidence"]] <- outOfHomePolymvsIncidence
nestedplotlist[[federalstate]][["outOfHomePolymOutdoorvsIncidence"]] <- outOfHomePolymOutdoorvsIncidence
nestedplotlist[[federalstate]][["outOfHomeOutdoorvsIncidence"]] <- outOfHomeOutdoorIncidence
nestedplotlist[[federalstate]][["outdoorIncidence"]] <- outdoorIncidence
nestedplotlist[[federalstate]][["tmaxvsIncidence"]] <- tmaxIncidence
}

#Examplary plot for Berlin
grid.arrange(nestedplotlist[["Berlin"]][["incidencePlot"]], nestedplotlist[["Berlin"]][["changeOfIncidencePlot"]], nestedplotlist[["Berlin"]][["mobilityPlot"]], nestedplotlist[["Berlin"]][["tempPlot"]], nrow=4)

grid.arrange(nestedplotlist[["Berlin"]][["outOfHomevsIncidence"]],nestedplotlist[["Berlin"]][["outOfHomeSquaredvsIncidence"]],nestedplotlist[["Berlin"]][["outOfHomePolymvsIncidence"]],nestedplotlist[["Berlin"]][["outOfHomePolymOutdoorvsIncidence"]], nestedplotlist[["Berlin"]][["outOfHomeOutdoorvsIncidence"]], nestedplotlist[["Berlin"]][["tmaxvsIncidence"]], nrow=6)



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
 
# 1) D vs I
#Performing linear regression
nestedplotlist <- list()
for (federalstate in dictStateId$Bundesland){
  joinedDataFramefedstate <- filter(joinedDataFrame, Bundesland == federalstate) %>% na.omit(joinedDataFramefedstate) %>%
  filter(changeOfIncidencelagged2 < 2)
DvsI.lm <- lm(changeOfIncidencelagged2 ~ outOfHomeDuration, data=joinedDataFramefedstate) #Examplary regression for Bayern
plot22 <- ggplot(joinedDataFramefedstate) +
geom_point(aes(x=outOfHomeDuration, y = changeOfIncidencelagged2)) +
geom_smooth(aes(x= outOfHomeDuration, y = changeOfIncidencelagged2), method = "lm") +
labs(title = federalstate) +
theme_minimal()
nestedplotlist[[federalstate]] <- list()
nestedplotlist[[federalstate]][["Regression"]] <- DvsI.lm
nestedplotlist[[federalstate]][["Plot"]] <- plot22
}

DvsI.lm <- lm(changeOfIncidencelagged ~ outOfHomeDuration, data=filter(joinedDataFrame, changeOfIncidencelagged < 1.6))
ggplot(data=filter(joinedDataFrame, changeOfIncidencelagged < 1.6)) +
geom_point(aes(x=outOfHomeDuration, y = changeOfIncidencelagged)) +
geom_smooth(aes(x= outOfHomeDuration, y = changeOfIncidencelagged), method = "lm") +
facet_wrap(vars(Bundesland)) +
theme_minimal()

ggplot(data=filter(joinedDataFrame, changeOfIncidencelagged < 1.6)) +
geom_point(aes(x=outOfHomeDuration, y = changeOfIncidencelagged, col = Bundesland)) +
geom_smooth(aes(x= outOfHomeDuration, y = changeOfIncidencelagged), method = "lm") +
theme_minimal()

# 2) D^2 vs I
nestedplotlist <- list()
for(federalstate in dictStateId$Bundesland){
D2vsI.lm <- lm(changeOfIncidencelagged ~ outOfHomeDuration*outOfHomeDuration, data=filter(joinedDataFrame, Bundesland==federalstate)) #Examplary regression for Bayern
plot22 <- ggplot(data=filter(joinedDataFrame, changeOfIncidencelagged < 2 && Bundesland == federalstate)) +
geom_point(aes(x=outOfHomeDuration*outOfHomeDuration, y = changeOfIncidencelagged)) +
geom_smooth(aes(x= outOfHomeDuration*outOfHomeDuration, y = changeOfIncidencelagged), method = "lm") +
labs(title = federalstate) +
theme_minimal()
nestedplotlist[[federalstate]] <- list()
nestedplotlist[[federalstate]][["Regression"]] <- D2vsI.lm
nestedplotlist[[federalstate]][["Plot"]] <- plot22
}

# 3) D + D^2 vs I
nestedplotlist <- list()
for(federalstate in dictStateId$Bundesland){
DplusD2vsI.lm <- lm(changeOfIncidencelagged ~ poly(outOfHomeDuration,2), data=filter(joinedDataFrame, Bundesland==federalstate)) #Examplary regression for Bayern
plot22 <- ggplot(data=filter(joinedDataFrame, Bundesland == federalstate), aes(x=outOfHomeDuration, y = changeOfIncidencelagged)) +
geom_point() +
stat_smooth(formula = y ~ poly(x,2), method = "lm") +
labs(title = federalstate) +
theme_minimal()
nestedplotlist[[federalstate]] <- list()
nestedplotlist[[federalstate]][["Regression"]] <- DplusD2vsI.lm
nestedplotlist[[federalstate]][["Plot"]] <- plot22
}

# 4a) D + tmax vs I
nestedplotlist <- list()
for(federalstate in dictStateId$Bundesland){
DplustmaxvsI.lm <- lm(changeOfIncidencelagged2 ~ outOfHomeDuration+tmax, data=filter(joinedDataFrame, Bundesland==federalstate)) #Examplary regression for Bayern
plot22 <- ggplot(data=filter(joinedDataFrame, changeOfIncidencelagged2 < 2 && Bundesland == federalstate)) +
geom_point(aes(x=outOfHomeDuration+tmax, y = changeOfIncidencelagged2)) +
geom_smooth(aes(x= outOfHomeDuration+tmax, y = changeOfIncidencelagged2), method = "lm") +
labs(title = federalstate) +
theme_minimal()
nestedplotlist[[federalstate]] <- list()
nestedplotlist[[federalstate]][["Regression"]] <- DplustmaxvsI.lm
nestedplotlist[[federalstate]][["Plot"]] <- plot22
}

# 4b) D + tavg vs I
nestedplotlist <- list()
for (federalstate in dictStateId$Bundesland){
DplustavgvsI.lm <- lm(changeOfIncidencelagged2 ~ outOfHomeDuration+tavg, data=filter(joinedDataFrame, changeOfIncidencelagged2 < 1.6 & Bundesland==federalstate)) #Examplary regression for Bayern
plot22 <- ggplot(data=filter(joinedDataFrame, changeOfIncidencelagged2 < 1.6 & Bundesland == federalstate)) +
geom_point(aes(x=outOfHomeDuration+tavg, y = changeOfIncidencelagged2)) +
geom_smooth(aes(x= outOfHomeDuration+tavg, y = changeOfIncidencelagged2), method = "lm") +
labs(title = federalstate) +
theme_minimal()
nestedplotlist[[federalstate]] <- list()
nestedplotlist[[federalstate]][["Regression"]] <- DplustavgvsI.lm
nestedplotlist[[federalstate]][["Plot"]] <- plot22
}

# 5a) D*tmax vs I
nestedplotlist <- list()
for(federalstate in dictStateId$Bundesland){
DtimestmaxvsI.lm <- lm(changeOfIncidencelagged ~ outOfHomeDuration*tmax, data=filter(joinedDataFrame, Bundesland==federalstate))
plot22 <- ggplot(data=filter(joinedDataFrame, changeOfIncidencelagged < 1.5 & Bundesland == federalstate)) +
geom_point(aes(x=outOfHomeDuration*tmax, y = changeOfIncidencelagged)) +
geom_smooth(aes(x= outOfHomeDuration*tmax, y = changeOfIncidencelagged), method = "lm") +
labs(title = federalstate) +
theme_minimal()
nestedplotlist[[federalstate]] <- list()
nestedplotlist[[federalstate]][["Regression"]] <- DtimestmaxvsI.lm
nestedplotlist[[federalstate]][["Plot"]] <- plot22
}

# 5b) D*tavg vs I
nestedplotlist <- list()
for(federalstate in dictStateId$Bundesland){
plot22 <- ggplot(data=filter(joinedDataFrame, changeOfIncidencelagged2 < 1.6 & Bundesland == federalstate )) +
geom_point(aes(x=outOfHomeDuration*tavg, y = changeOfIncidencelagged2)) +
geom_smooth(aes(x= outOfHomeDuration*tavg, y = changeOfIncidencelagged2), method = "lm") +
labs(title = federalstate) +
theme_minimal()
nestedplotlist[[federalstate]] <- list()
nestedplotlist[[federalstate]][["Plot"]] <- plot22
}

# 6a) D + out vs I
nestedplotlist <- list()
for(federalstate in dictStateId$Bundesland){
plot22 <- ggplot(data=filter(joinedDataFrame, changeOfIncidencelagged2 < 1.6 & Bundesland == federalstate)) +
geom_point(aes(x=outOfHomeDuration+outdoorFraction, y = changeOfIncidencelagged2)) +
geom_smooth(aes(x= outOfHomeDuration+outdoorFraction, y = changeOfIncidencelagged2), method = "lm") +
labs(title = federalstate) +
theme_minimal()
nestedplotlist[[federalstate]] <- list()
nestedplotlist[[federalstate]][["Regression"]] <- DplusoutvsI.lm
nestedplotlist[[federalstate]][["Plot"]] <- plot22
}

# 6b) D + out2 vs I
nestedplotlist <- list()
for(federalstate in dictStateId$Bundesland){
plot22 <- ggplot(data=filter(joinedDataFrame, changeOfIncidencelagged2 < 2 && Bundesland == federalstate)) +
geom_point(aes(x=outOfHomeDuration+outdoorFraction2, y = changeOfIncidencelagged2)) +
geom_smooth(aes(x= outOfHomeDuration+outdoorFraction2, y = changeOfIncidencelagged2), method = "lm") +
labs(title = federalstate) +
theme_minimal()
nestedplotlist[[federalstate]] <- list()
nestedplotlist[[federalstate]][["Plot"]] <- plot22
}

# 7a) D * out vs I
nestedplotlist <- list()
for(federalstate in dictStateId$Bundesland){
plot22 <- ggplot(data=filter(joinedDataFrame, changeOfIncidencelagged2 < 2 && Bundesland == federalstate)) +
geom_point(aes(x=outOfHomeDuration*outdoorFraction+outOfHomeDuration+outdoorFraction, y = changeOfIncidencelagged2)) +
geom_smooth(aes(x= outOfHomeDuration*outdoorFraction+outOfHomeDuration+outdoorFraction, y = changeOfIncidencelagged2), method = "lm") +
labs(title = federalstate) +
theme_minimal()
nestedplotlist[[federalstate]] <- list()
nestedplotlist[[federalstate]][["Plot"]] <- plot22
}

# 7b) D * out2 vs I
nestedplotlist <- list()
for(federalstate in dictStateId$Bundesland){
plot22 <- ggplot(data=filter(joinedDataFrame, changeOfIncidencelagged2 < 2 && Bundesland == federalstate )) +
geom_point(aes(x=outOfHomeDuration*outdoorFraction2+outOfHomeDuration+outdoorFraction2, y = changeOfIncidencelagged2)) +
geom_smooth(aes(x= outOfHomeDuration*outdoorFraction2+outOfHomeDuration+outdoorFraction2, y = changeOfIncidencelagged2), method = "lm") +
labs(title = federalstate) +
theme_minimal()
nestedplotlist[[federalstate]] <- list()
nestedplotlist[[federalstate]][["Plot"]] <- plot22
}

# 8) D + prcp
nestedplotlist <- list()
for(federalstate in dictStateId$Bundesland){
DplustmaxvsI.lm <- lm(changeOfIncidencelagged2 ~ outOfHomeDuration+prcp, data=filter(joinedDataFrame, Bundesland==federalstate)) #Examplary regression for Bayern
plot22 <- ggplot(data=filter(joinedDataFrame, changeOfIncidencelagged2 < 2 && Bundesland == federalstate)) +
geom_point(aes(x=outOfHomeDuration+prcp, y = changeOfIncidencelagged2)) +
geom_smooth(aes(x= outOfHomeDuration+prcp, y = changeOfIncidencelagged2), method = "lm") +
labs(title = federalstate) +
theme_minimal()
nestedplotlist[[federalstate]] <- list()
nestedplotlist[[federalstate]][["Regression"]] <- DplustmaxvsI.lm
nestedplotlist[[federalstate]][["Plot"]] <- plot22
}

# 9) D + outdoorFraction + prcp
nestedplotlist <- list()
for(federalstate in dictStateId$Bundesland){
DplustmaxvsI.lm <- lm(changeOfIncidencelagged2 ~ outOfHomeDuration+tmax, data=filter(joinedDataFrame, Bundesland==federalstate)) #Examplary regression for Bayern
plot22 <- ggplot(data=filter(joinedDataFrame, changeOfIncidencelagged2 < 2 && Bundesland == federalstate)) +
geom_point(aes(x=outOfHomeDuration+prcp, y = changeOfIncidencelagged2)) +
geom_smooth(aes(x= outOfHomeDuration+prcp, y = changeOfIncidencelagged2), method = "lm") +
labs(title = federalstate) +
theme_minimal()
nestedplotlist[[federalstate]] <- list()
nestedplotlist[[federalstate]][["Regression"]] <- DplustmaxvsI.lm
nestedplotlist[[federalstate]][["Plot"]] <- plot22
}


#Performing multiple polynomial regression
 
 joinedDataFrame.pm1 <- lm(changeOfIncidence ~ polym(outOfHomeDuration, tmax, degree=2), data=filter(joinedDataFrame, Bundesland=="Bayern")) #ToDo: How to visualize this?

fpoly2<-function(x1,x2){
  return(joinedDataFrame.pm1$coefficients["(Intercept)"]+
  joinedDataFrame.pm1$coefficients["polym(outOfHomeDuration, outdoorFraction2, degree = 2)1.0"]*x1+
  joinedDataFrame.pm1$coefficients["polym(outOfHomeDuration, outdoorFraction2, degree = 2)2.0"]*x1*x1+
  joinedDataFrame.pm1$coefficients["polym(outOfHomeDuration, outdoorFraction2, degree = 2)0.1"]*x2+
  joinedDataFrame.pm1$coefficients["polym(outOfHomeDuration, outdoorFraction2, degree = 2)1.1"]*x1*x2+
  joinedDataFrame.pm1$coefficients["polym(outOfHomeDuration, outdoorFraction2, degree = 2)0.2"]*x2*x2)
}

joinedDataFrameBayern <- filter(joinedDataFrame, Bundesland == "Bayern")
x1x2_poly <- c(joinedDataFrameBayern$outOfHomeDuration, joinedDataFrameBayern$outdoorFraction)
y_poly <- poly(x1x2_poly)
dataframex1x2y <- data.frame(x1x2_poly, y_poly)

ggplot() +
geom_point(data=dataframex1x2y, aes(x= x1x2_poly, y = X1), color= "blue") +
geom_point(data=joinedDataFrameBayern, aes(x=))
theme_minimal()

#Plotting the model(s)

grid.arrange(nestedplotlist[["Baden-Württemberg"]][["Plot"]],
nestedplotlist[["Bayern"]][["Plot"]],
nestedplotlist[["Berlin"]][["Plot"]],
nestedplotlist[["Brandenburg"]][["Plot"]],
nestedplotlist[["Bremen"]][["Plot"]],
nestedplotlist[["Hamburg"]][["Plot"]],
nestedplotlist[["Hessen"]][["Plot"]],
nestedplotlist[["Mecklenburg-Vorpommern"]][["Plot"]],
nestedplotlist[["Niedersachsen"]][["Plot"]],
nestedplotlist[["Nordrhein-Westfalen"]][["Plot"]],
nestedplotlist[["Rheinland-Pfalz"]][["Plot"]],
nestedplotlist[["Saarland"]][["Plot"]],
nestedplotlist[["Sachsen"]][["Plot"]],
nestedplotlist[["Sachsen-Anhalt"]][["Plot"]],
nestedplotlist[["Schleswig-Holstein"]][["Plot"]],
nestedplotlist[["Thüringen"]][["Plot"]], nrow = 4)
