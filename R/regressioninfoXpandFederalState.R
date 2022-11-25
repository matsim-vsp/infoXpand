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
dictStateId <- data.frame(matrix(nrow=0, ncol=2))
colnames(dictStateId) <- c("Bundesland", "ID")
dictStateId[nrow(dictStateId)+1,] <- c("Baden-Württemberg", 10738)
dictStateId[nrow(dictStateId)+1,] <- c("Bayern", 10865)
dictStateId[nrow(dictStateId)+1,] <- c("Berlin", 10382)
dictStateId[nrow(dictStateId)+1,] <- c("Brandenburg", 10379)
dictStateId[nrow(dictStateId)+1,] <- c("Bremen", 10224)
dictStateId[nrow(dictStateId)+1,] <- c("Hamburg", 10147)
dictStateId[nrow(dictStateId)+1,] <- c("Hessen", 10633)
dictStateId[nrow(dictStateId)+1,] <- c("Mecklenburg-Vorpommern", 10162)
dictStateId[nrow(dictStateId)+1,] <- c("Niedersachsen", 10338)
dictStateId[nrow(dictStateId)+1,] <- c("Nordrhein-Westfalen", 10400)
dictStateId[nrow(dictStateId)+1,] <- c("Rheinland-Pfalz", "D3137")
dictStateId[nrow(dictStateId)+1,] <- c("Saarland", "D6217")
dictStateId[nrow(dictStateId)+1,] <- c("Sachsen", "D1051")
dictStateId[nrow(dictStateId)+1,] <- c("Sachsen-Anhalt", 10361)
dictStateId[nrow(dictStateId)+1,] <- c("Schleswig-Holstein", 10044)
dictStateId[nrow(dictStateId)+1,] <- c("Thüringen", 10554)

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

url1<-'https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Kum_Tab_aktuell.xlsx?__blob=publicationFile'
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


weeklyincidendeData <- incidenceData %>%
  mutate(week = week(Date)) %>%
  mutate(year = year(Date)) %>%
  group_by(year, week, Bundesland) %>%
  summarise(Date = min(Date) + 4, Incidence = mean(Incidence))


#Computing the change in incidence every week
incidenceData <- weeklyincidendeData
incidenceData <- mutate(incidenceData, changeOfIncidence = 1)
for(row in 2:nrow(incidenceData)){
  incidenceData[row, "changeOfIncidence"] <- incidenceData[row, "Incidence"]/incidenceData[row-1, "Incidence"]
}

for(maysixth in which(incidenceData$Date == "2020-05-12")){
  incidenceData[maysixth, 4] <- 1
}
incidenceData$Bundesland <- as.character(incidenceData$Bundesland)

## Estimation for all states simultaneously

#Reading in Mobility data on federal state level, using weekly data
mobilityData <- read_delim("https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/episim/mobilityData/bundeslaender/mobilityData_OverviewBL_weekly.csv")

#Little bit of cleaning, turning date column into right format, renaming columns
mobilityData$date <- paste(substr(mobilityData$date, start = 1, stop = 4), substr(mobilityData$date, start = 5, stop = 6), as.character(substr(mobilityData$date, start = 7, stop = 8)), sep ="-" )
mobilityData$date <- as.Date(mobilityData$date)
colnames(mobilityData)[2] <- "Bundesland"

joinedDataFrame <- left_join(incidenceData, mobilityData, by = c("Bundesland", "date"))

#Weather data
weatherDataAll <- data.frame(matrix(nrow = 0, ncol = 3))
for(state in 1:length(dictStateId$Bundesland)){
ID <- dictStateId[as.integer(state), 2]
weatherData <- read_delim(paste0("https://bulk.meteostat.net/daily/", ID, ".csv.gz"))
colnames(weatherData) <- c("Date", "tavg", "tmin", "tmax", "prcp", "snow", "wdir", "wspd", "wpgt", "pres", "tsun")

weatherData$Date <- as.Date(weatherData$Date)
weatherData <-weatherData[,c("Date", "tmax")]
weatherData$Bundesland <- dictStateId[as.integer(state), 1]

weatherDataAll <- rbind(weatherDataAll, weatherData)
}

colnames(weatherDataAll)[1] <- c("date")

#Converting the weather data to an "outdoor fraction"
#Below a certain temperature, everything happens indoords, above a certain temperature everything happens outdoors and in between we linearize
weatherDataAll <- mutate(weatherDataAll, outdoorFraction = case_when(22.5 >= tmax & tmax >= 12.5 ~ 2 - 0.1*(22.5 - tmax),
                                                                   tmax < 12.5 ~ 2,
                                                                  tmax > 22.5 ~ 1))
#Joining the data frames
joinedDataFrame <- inner_join(joinedDataFrame, weatherDataAll, by = c("Bundesland", "date"))
joinedDataFrame <- joinedDataFrame[,c("Date", "Bundesland", "Incidence", "changeOfIncidence", "outOfHomeDuration","tmax", "outdoorFraction")]
joinedDataFrame <- mutate(joinedDataFrame, explanatory = outOfHomeDuration*outOfHomeDuration*(outdoorFraction))
joinedDataFrame$Incidence <- as.double(joinedDataFrame$Incidence)

#Only for 2020 (and a bit of 2021, as not too many people were vaccinated and alpha wasn't dominant yet)
joinedDataFrame <- filter(joinedDataFrame, date < "2021-02-22")

#Plotting changeOfIncidence over time and outOfHomeDuration over time and OutdoorFactor over time
#Note : We're only displaying data for Berlin
incidencePlot <- ggplot(data=filter(joinedDataFrame, Bundesland=="Bremen"), aes(x=date, y=Incidence)) +
  geom_point() +
  theme_minimal()
changeOfIncidencePlot <- ggplot(data=filter(joinedDataFrame, Bundesland=="Berlin"), aes(x=date, y=changeOfIncidence)) +
  geom_point() +
  theme_minimal()
mobilityPlot <- ggplot(data=filter(joinedDataFrame, Bundesland=="Berlin"), aes(x=date, y=outOfHomeDuration)) +
  geom_point() +
  theme_minimal()
tempPlot <- ggplot(data=filter(joinedDataFrame, Bundesland=="Berlin"), aes(x=date, y=tmax)) +
  geom_point() +
  geom_hline(yintercept = 22.5) +
  geom_hline(yintercept = 12.5) +
  theme_minimal() +
  ylab("Tmax")
grid.arrange(incidencePlot, changeOfIncidencePlot, mobilityPlot, tempPlot , nrow=4)

#From here on Trying to explore correlations
#Exploratory work, for now everything will remain. Later, whatever we deem unnecessary will be 

joinedDataFrameBerlin <- filter(joinedDataFrame, Bundesland == "Berlin")
joinedDataFrameBerlin <- joinedDataFrameBerlin[-1,] #Removing the 1st line as it we do not have a changeOfIncidence here

#1)
# 1a) Look at correlaction over whole time
cor(joinedDataFrameBerlin$changeOfIncidence, joinedDataFrameBerlin$outOfHomeDuration)
# 1b) Over whole time, but outOfHomeDuration^2
cor(joinedDataFrameBerlin$changeOfIncidence, joinedDataFrameBerlin$outOfHomeDuration*joinedDataFrameBerlin$outOfHomeDuration)

#2)
#Look for correlations during 3 different t_max intervals : [-\infty, 12.5], (12.5,22.5), [22.5, \infty]
#Note: This does not lead to satisfactory correlactions
joinedDataFrameColdWeater <- filter(joinedDataFrameBerlin, tmax <= 12.5)
cor(joinedDataFrameColdWeater$changeOfIncidence, joinedDataFrameColdWeater$outOfHomeDuration)
joinedDataFrameWarmWeater <- filter(joinedDataFrameBerlin, tmax >= 22.5)
cor(joinedDataFrameWarmWeater$changeOfIncidence, joinedDataFrameWarmWeater$outOfHomeDuration) #Smallest correlation, additional idea: maybe the school holidays influence this somehow?
joinedDataFrameMediocreWeather <- filter(joinedDataFrameBerlin, tmax > 12.5) %>%
                                  filter(tmax < 22.5)
cor(joinedDataFrameMediocreWeather$changeOfIncidence, joinedDataFrameMediocreWeather$outOfHomeDuration)

#3) 
#Instead of splitting the whole thing by temperature, we go wave-wise
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

#Performing linear regression
joinedDataFrame.lm <- lm(changeOfIncidence ~ outOfHomeDuration, data=joinedDataFrame)

#Performing multiple polynomial regression
joinedDataFrame.pm1 <- lm(changeOfIncidence ~ polym(outOfHomeDuration, outdoorFraction, degree=2), data=joinedDataFrame) #ToDo: How to visualize this?