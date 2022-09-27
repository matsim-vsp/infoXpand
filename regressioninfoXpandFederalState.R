library(tidyverse)
library(lubridate)
library(readxl)
library(httr)

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
 colnames(incidenceDataArchiv) <- incidenceDataArchiv[4,]
 colnames(incidenceDataArchiv)[1] <- "Bundesland"
 incidenceDataArchiv <- incidenceDataArchiv[-(1:4),] 
 incidenceDataArchiv <- pivot_longer(incidenceDataArchiv, names_to="Date", values_to="Incidence", cols=colnames(incidenceDataArchiv)[2:ncol(incidenceDataArchiv)])
 incidenceDataArchiv$Date <- as.integer(incidenceDataArchiv$Date)
 incidenceDataArchiv$Date <- as.Date(incidenceDataArchiv$Date,origin="1899-12-30")

url1<-'https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Kum_Tab_aktuell.xlsx?__blob=publicationFile'
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
incidenceData <- read_excel(tf, 4)
colnames(incidenceData) <- incidenceData[4,]
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

#Incidence data is provided daily, need to compute weekly averages
weeklyincidendeData <- data.frame(matrix(nrow=0, ncol=3))
colnames(weeklyincidendeData) <- c("Bundesland", "Date", "Incidence")

for(bundesland in unique(incidenceData$Bundesland)){
  filteredBundesland <- filter(incidenceData, Bundesland == bundesland)
  date = min(incidenceData$Date)
  while(date < max(incidenceData$Date)){
  filteredWeek <- filter(filteredBundesland, Date >= date & Date < date + 7)
  weeklyincidendeData[nrow(weeklyincidendeData)+1, 1] = bundesland
  weeklyincidendeData[nrow(weeklyincidendeData), 2] = date + 4
  weeklyincidendeData[nrow(weeklyincidendeData), 3] = mean(filteredWeek$Incidence)
  date = date + 7
  }
}

weeklyincidendeData$Date <- as.Date(weeklyincidendeData$Date, origin="1970-01-01")

#Computing the change in incidence every week
incidenceData <- weeklyincidendeData
incidenceData <- mutate(incidenceData, changeOfIncidence = 1)
for(row in 2:nrow(incidenceData)){
  incidenceData[row, 4] <- incidenceData[row, 3]/incidenceData[row-1, 3]
}

for(maysixth in which(incidenceData$Date=="2020-05-12")){
  incidenceData[maysixth, 4] <- 1
}

## Estimation for all states simultaneously

#Reading in Mobility data on federal state level, using weekly data
mobilityData <- read_delim("https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/episim/mobilityData/bundeslaender/mobilityData_OverviewBL_weekly.csv")

#Little bit of cleaning, turning date column into right format, renaming columns
mobilityData$date <- paste(substr(mobilityData$date, start = 1, stop = 4), substr(mobilityData$date, start = 5, stop = 6), as.character(substr(mobilityData$date, start = 7, stop = 8)), sep ="-" )
mobilityData$date <- as.Date(mobilityData$date)
colnames(mobilityData)[1] <- "Date"
colnames(mobilityData)[2] <- "Bundesland"

joinedDataFrame <- inner_join(incidenceData, mobilityData)

#Weather data 
weatherDataAll <- data.frame(matrix(nrow=0, ncol=3))
for(state in 1:length(dictStateId$Bundesland)){
ID <- dictStateId[as.integer(state),2]
weatherData <- read_delim(paste0("https://bulk.meteostat.net/daily/", ID, ".csv.gz"))
colnames(weatherData) <- c("Date", "tavg", "tmin", "tmax", "prcp", "snow", "wdir", "wspd", "wpgt", "pres", "tsun")

weatherData$Date <- as.Date(weatherData$Date)
weatherData <-weatherData[,c("Date", "tmax")]
weatherData$Bundesland <- dictStateId[as.integer(state),1]

weatherDataAll <- rbind(weatherDataAll, weatherData)
}

#Converting the weather data to an "outdoor fraction"
#Below a certain temperature, everything happens indoords, above a certain temperature everything happens outdoors and in between we linearize
weatherDataAll <- mutate(weatherDataAll, outdoorFraction = case_when(22.5 >= tmax & tmax >= 12.5 ~ 1 - 0.1*(22.5 - tmax),
                                                                   tmax < 12.5 ~ 0, #I believe this is what could be a cause for the horizontal line?
                                                                  tmax > 22.5 ~ 1))
#Joining the data frames
joinedDataFrame <- inner_join(joinedDataFrame, weatherDataAll)
joinedDataFrame <- joinedDataFrame[,c("Date", "Bundesland", "Incidence", "changeOfIncidence", "outOfHomeDuration","tmax", "outdoorFraction")]
joinedDataFrame <- mutate(joinedDataFrame, explanatory = outOfHomeDuration*outOfHomeDuration*(1-outdoorFraction))


joinedDataFrame$Incidence <- as.double(joinedDataFrame$Incidence)

#Simply plotting the data points against one another
ggplot(data=joinedDataFrame, aes(x=explanatory, y=changeOfIncidence, color=Bundesland))+
  geom_point()+
  facet_wrap(~Bundesland)
#Looks rather horizontal to me? See comment line 129


#Performing the regression
joinedDataFrame.lm <- lm(changeOfIncidence ~ explanatory, data=joinedDataFrame)

#All of the following is no use, if the plot in line 140 f. looks so horizontal
# joinedDataFrame <- mutate(joinedDataFrame,regressionResult = coefficients(joinedDataFrame.lm)["(Intercept)"]+coefficients(joinedDataFrame.lm)["explanatory"]*explanatory)
# 
# ggplot(data=joinedDataFrame, aes(x=regressionResult, y=changeOfIncidence))+
#   geom_point()+
#   stat_smooth(method='lm', formula = y~x)+
#   theme_minimal()+
#   scale_y_log10()+
#   xlab("C'' + gamma * (aushActDur)^2 *outdoorFraction") +
#   ylab("Hospitalisation Incidence")+
#   facet_wrap(~ Bundesland)
# 
# ggplot(data=joinedDataFrame, aes(x=regressionResult, y=changeOfIncidence, color=Bundesland))+
#   geom_point()+
#  # stat_smooth(method='lm', formula = y~x)+
#   theme_minimal()+
#   scale_y_log10()+
#   xlab("C'' + gamma * (aushActDur)^2 *outdoorFraction") +
#   ylab("Hospitalisation Incidence")

#ggsave(paste0("All16States14DayFacetsLogScale",".png"))



# #Doing analysis for every state(!) separately
# for(state in unique(dictStateId$MeldeLandkreisBundesland)){
#   
# #For now, only looking at it nationally, not differentiating between federal states
# hospitalizationDataState <- filter(hospitalizationData,MeldeLandkreisBundesland == state)
# 
# #Reading in Mobility data
# mobilityData <- read_delim("https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/episim/mobilityData/bundeslaender/mobilityData_OverviewBL_weekly.csv")
# 
# #For now, only looking at it nationally
# mobilityData <- filter(mobilityData, BundeslandID==state)
# #mobilityData$date <- character(mobilityData$date)
# mobilityData$date <- paste(substr(mobilityData$date, start = 1, stop = 4), substr(mobilityData$date, start = 5, stop = 6), as.character(substr(mobilityData$date, start = 7, stop = 8)), sep ="-" )
# mobilityData$date <- as.Date(mobilityData$date)
# colnames(mobilityData)[1] <- "Date"
# 
# joinedDataFrame <- inner_join(hospitalizationDataState, mobilityData)
# 
# #Weather data 
# 
# ID <- dictStateId[dictStateId$MeldeLandkreisBundesland==state,2]
# weatherData <- read_delim(paste0("https://bulk.meteostat.net/daily/", dictStateId[dictStateId$MeldeLandkreisBundesland==state,2], ".csv.gz"))
# colnames(weatherData) <- c("Date", "tavg", "tmin", "tmax", "prcp", "snow", "wdir", "wspd", "wpgt", "pres", "tsun")
#   
# weatherData$Date <- as.Date(weatherData$Date)
# weatherData <-weatherData[,c("Date", "tavg")]
# 
# joinedDataFrame <- inner_join(joinedDataFrame, weatherData)
# joinedDataFrame <- joinedDataFrame[,c("Date","hospitalizationIncidence","outOfHomeDuration","tavg")]
# joinedDataFrame$hospitalizationIncidence <- as.double(joinedDataFrame$hospitalizationIncidence)
# joinedDataFrame <- mutate(joinedDataFrame, explanatory =outOfHomeDuration*outOfHomeDuration*tavg*tavg)
# 
# 
# joinedDataFrame.lm <- lm(hospitalizationIncidence ~ explanatory, data=joinedDataFrame)
# 
# joinedDataFrame <- mutate(joinedDataFrame,regressionResult = coefficients(joinedDataFrame.lm)["(Intercept)"]+coefficients(joinedDataFrame.lm)["explanatory"]*explanatory)
# 
# ggplot(data=joinedDataFrame, aes(x=regressionResult, y=hospitalizationIncidence))+
#   geom_point()+
#   stat_smooth(method='lm', formula = y~poly(x,2))+
#   theme_minimal()+
#   xlab("C'' + gamma * (aushActDur * tavg)^2") +
#   ylab("Hospitalisation Incidence")
# 
# ggsave(paste0(state,".png"))
# }
# 
# 
