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
incidenceDataArchiv <- incidenceDataArchiv %>% filter(Bundesland == "Gesamt")

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
incidenceData <- incidenceData %>% filter(Bundesland == "Gesamt")

incidenceData2 <- incidenceData 
incidenceData <- rbind(incidenceDataArchiv, incidenceData)
incidenceData$Incidence <- as.double(incidenceData$Incidence)
incidenceData <- filter(incidenceData, Date < as.Date("2021-01-01") & Date > as.Date("2020-05-11"))

incidenceData <- incidenceData %>%
  mutate(weekMon = cut(Date, "week")) %>% #Weeks start on Monday
  mutate(weekTue = cut(Date - 1, "week")) %>% #Weeks start on Tuesday
  mutate(weekWed = cut(Date - 2, "week")) %>% #Weeks start on Wednesday
  mutate(weekThu = cut(Date - 3, "week")) %>% #Weeks start on Thursday
  mutate(weekFri = cut(Date - 4, "week")) %>% #Weeks start on Friday
  mutate(weekSat = cut(Date - 5, "week")) %>% #Weeks start on Saturday
  mutate(weekSun = cut(Date - 6, "week")) %>% #Weeks start on Sunday
  mutate(year = year(Date))

incidenceData <- incidenceData[-1,]

incidenceDataMon <- incidenceData %>%  group_by(year, weekMon, Bundesland) %>%
  summarise(Date = min(Date)+6, IncidenceMon = mean(Incidence))
incidenceDataTue <- incidenceData %>%  group_by(year, weekTue, Bundesland) %>%
  summarise(Date = min(Date)+5, IncidenceTue = mean(Incidence))
incidenceDataWed <- incidenceData %>%  group_by(year, weekWed, Bundesland) %>%
  summarise(Date = min(Date)+4, IncidenceWed = mean(Incidence))
incidenceDataThu <- incidenceData %>%  group_by(year, weekThu, Bundesland) %>%
  summarise(Date = min(Date)+3, IncidenceThu = mean(Incidence))
incidenceDataFri <- incidenceData %>%  group_by(year, weekFri, Bundesland) %>%
  summarise(Date = min(Date)+2, IncidenceFri = mean(Incidence))
incidenceDataSat <- incidenceData %>%  group_by(year, weekSat, Bundesland) %>%
  summarise(Date = min(Date)+1, IncidenceSat = mean(Incidence))
incidenceDataSun <- incidenceData %>%  group_by(year, weekSun, Bundesland) %>%
  summarise(Date = min(Date), IncidenceSun = mean(Incidence))

incidenceData <- inner_join(incidenceDataMon, incidenceDataTue, by = "Date")
incidenceData <- inner_join(incidenceData, incidenceDataWed, by = "Date")
incidenceData <- inner_join(incidenceData, incidenceDataThu, by = "Date")
incidenceData <- inner_join(incidenceData, incidenceDataFri, by = "Date")
incidenceData <- inner_join(incidenceData, incidenceDataSat, by = "Date")
incidenceData <- inner_join(incidenceData, incidenceDataSun, by = "Date")

incidenceData <- select(incidenceData, c("Date", "year", "IncidenceMon", "IncidenceTue", "IncidenceWed", "IncidenceThu", "IncidenceFri", "IncidenceSat", "IncidenceSun"))
#Adding the change of incidence to the data frame
incidenceData[, "changeOfIncidenceMon"] <- as.double(incidenceData$IncidenceMon/lag(incidenceData$IncidenceMon))
incidenceData[, "changeOfIncidenceTue"] <- incidenceData$IncidenceTue/lag(incidenceData$IncidenceTue)
incidenceData[, "changeOfIncidenceWed"] <- incidenceData$IncidenceWed/lag(incidenceData$IncidenceWed)
incidenceData[, "changeOfIncidenceThu"] <- incidenceData$IncidenceThu/lag(incidenceData$IncidenceThu)
incidenceData[, "changeOfIncidenceFri"] <- incidenceData$IncidenceFri/lag(incidenceData$IncidenceFri)
incidenceData[, "changeOfIncidenceSat"] <- incidenceData$IncidenceSat/lag(incidenceData$IncidenceSat)
incidenceData[, "changeOfIncidenceSun"] <- incidenceData$IncidenceSun/lag(incidenceData$IncidenceSun)

## Estimation for all states simultaneously

#Reading in Mobility data on federal state level, using weekly data
mobilityData <- read_delim("https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/episim/mobilityData/bundeslaender/mobilityData_OverviewBL_weekly.csv")

#Little bit of cleaning, turning date column into right format, renaming columns
mobilityData$date <- paste(substr(mobilityData$date, start = 1, stop = 4), substr(mobilityData$date, start = 5, stop = 6), as.character(substr(mobilityData$date, start = 7, stop = 8)), sep ="-" )
mobilityData$date <- as.Date(mobilityData$date)
colnames(mobilityData)[2] <- "Bundesland"
colnames(mobilityData)[1] <- "Date"

mobilityData <- mobilityData %>% filter(Bundesland == "Deutschland") %>% select(-Bundesland)

joinedDataFrame <- inner_join(incidenceData, mobilityData, by = "Date")

joinedDataFrame <- ungroup(joinedDataFrame)

 joinedDataFrame <- mutate(joinedDataFrame, changeOfIncidencelaggedMon = lead(changeOfIncidenceMon))
 joinedDataFrame <- mutate(joinedDataFrame, changeOfIncidencelaggedMon2 = lead(changeOfIncidencelaggedMon))
 joinedDataFrame <- mutate(joinedDataFrame, changeOfIncidencelaggedTue = lead(changeOfIncidenceTue))
 joinedDataFrame <- mutate(joinedDataFrame, changeOfIncidencelaggedTue2 = lead(changeOfIncidencelaggedTue))
 joinedDataFrame <- mutate(joinedDataFrame, changeOfIncidencelaggedWed = lead(changeOfIncidenceWed))
 joinedDataFrame <- mutate(joinedDataFrame, changeOfIncidencelaggedWed2 = lead(changeOfIncidencelaggedWed))
 joinedDataFrame <- mutate(joinedDataFrame, changeOfIncidencelaggedThu = lead(changeOfIncidenceThu))
 joinedDataFrame <- mutate(joinedDataFrame, changeOfIncidencelaggedThu2 = lead(changeOfIncidencelaggedThu))
 joinedDataFrame <- mutate(joinedDataFrame, changeOfIncidencelaggedFri = lead(changeOfIncidenceFri))
 joinedDataFrame <- mutate(joinedDataFrame, changeOfIncidencelaggedFri2 = lead(changeOfIncidencelaggedFri))
 joinedDataFrame <- mutate(joinedDataFrame, changeOfIncidencelaggedSat = lead(changeOfIncidenceSat))
 joinedDataFrame <- mutate(joinedDataFrame, changeOfIncidencelaggedSat2 = lead(changeOfIncidencelaggedSat))
 joinedDataFrame <- mutate(joinedDataFrame, changeOfIncidencelaggedSun = lead(changeOfIncidenceSun))
 joinedDataFrame <- mutate(joinedDataFrame, changeOfIncidencelaggedSun2 = lead(changeOfIncidencelaggedSun))

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
  group_by(year, week) %>%
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
joinedDataFrame <- left_join(joinedDataFrame, weatherDataAll, by = "Date")
joinedDataFrame <- mutate(joinedDataFrame, explanatory = outOfHomeDuration*outOfHomeDuration*(outdoorFraction)) #What am I using this one for?

#Only for 2020 (and a bit of 2021, as not too many people were vaccinated and alpha wasn't dominant yet)
joinedDataFrame <- filter(joinedDataFrame, Date < "2021-02-22") #Not quite sure why we are filtering again

#Plotting changeOfIncidence over time and outOfHomeDuration over time and OutdoorFactor over time
nestedplotlist <- list()

incidencePlot <- ggplot(data=joinedDataFrame, aes(x=Date, y=Incidence)) +
  geom_point() +
  theme_minimal()
changeOfIncidencePlot <- ggplot(data=joinedDataFrame, aes(x=Date, y=changeOfIncidence)) +
  geom_point() +
  theme_minimal()
mobilityPlot <- ggplot(data=joinedDataFrame, aes(x=Date, y=outOfHomeDuration)) +
  geom_point() +
  theme_minimal()
tempPlot <- ggplot(data=joinedDataFrame, aes(x=Date, y=tmax)) +
  geom_point() +
  geom_hline(yintercept = 22.5) +
  geom_hline(yintercept = 12.5) +
  theme_minimal() +
  ylab("Tmax")
outOfHomevsIncidence <- ggplot(data=joinedDataFrame, aes(x=outOfHomeDuration, y=changeOfIncidence)) +
  geom_point() +
  theme_minimal()
outOfHomeSquaredvsIncidence <- ggplot(data=joinedDataFrame, aes(x=outOfHomeDuration*outOfHomeDuration, y=changeOfIncidence)) +
  geom_point() +
  theme_minimal()
outOfHomePolymvsIncidence <- ggplot(data=joinedDataFrame, aes(x=outOfHomeDuration*outOfHomeDuration+outOfHomeDuration, y=changeOfIncidence)) +
  geom_point() +
  theme_minimal()
outOfHomePolymOutdoorvsIncidence <- ggplot(data=joinedDataFrame, aes(x=outOfHomeDuration*outOfHomeDuration+outdoorFraction, y=changeOfIncidence)) +
  geom_point() +
  theme_minimal()
outOfHomeOutdoorIncidence <- ggplot(data=joinedDataFrame, aes(x=outOfHomeDuration+outdoorFraction2, y=changeOfIncidence)) +
  geom_point() +
  theme_minimal()
outdoorIncidence <- ggplot(data=joinedDataFrame, aes(x=outdoorFraction, y=changeOfIncidence)) +
  geom_point() +
  theme_minimal()
tmaxIncidence <- ggplot(data=joinedDataFrame, aes(x=tmax, y=changeOfIncidence)) +
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
grid.arrange(nestedplotlist[["incidencePlot"]], nestedplotlist[["changeOfIncidencePlot"]], nestedplotlist[["mobilityPlot"]], nestedplotlist[["tempPlot"]], nrow=4)

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
 
# 1) D vs I
#Performing linear regression
nestedplotlist <- list()

joinedDataFrame <- joinedDataFrame[-nrow(joinedDataFrame), ]

weekdays <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
for(weekday in weekdays){
weekdayString <- paste0("changeOfIncidencelagged",weekday,"2")
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ outOfHomeDuration")
DvsI.lm <- lm(formula=formula.lm , data=joinedDataFrame)
plot22 <- ggplot(data=joinedDataFrame) +
geom_point(aes(x=outOfHomeDuration, y = .data[[weekdayString]])) +
geom_smooth(aes(x= outOfHomeDuration, y = .data[[weekdayString]]), method = "lm") +
ggtitle(weekday) +
theme_minimal()
nestedplotlist[[paste0("Regression_DvsI_", weekday)]] <- DvsI.lm
nestedplotlist[[paste0("Plot_DvsI_", weekday)]] <- plot22
}

grid.arrange(nestedplotlist[["Plot_DvsI_Mon"]],nestedplotlist[["Plot_DvsI_Tue"]],nestedplotlist[["Plot_DvsI_Wed"]], nestedplotlist[["Plot_DvsI_Thu"]], nestedplotlist[["Plot_DvsI_Fri"]], nestedplotlist[["Plot_DvsI_Sat"]], nestedplotlist[["Plot_DvsI_Sun"]], nrow=3)

# 2) D^2 vs I
for(weekday in weekdays){
joinedDataFrame <- joinedDataFrame %>% mutate(outOfHomeDurationSquared = outOfHomeDuration*outOfHomeDuration)
weekdayString <- paste0("changeOfIncidencelagged",weekday,"2")
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ outOfHomeDurationSquared")
D2vsI.lm <- lm(formula = formula.lm, data=filter(joinedDataFrame))
plot22 <- ggplot(joinedDataFrame) +
geom_point(aes(x=outOfHomeDurationSquared, y = .data[[weekdayString]])) +
geom_smooth(aes(x= outOfHomeDurationSquared, y = .data[[weekdayString]]), method = "lm") +
ggtitle(weekday) +
theme_minimal()
nestedplotlist[[paste0("Regression_D2vsI", weekday)]] <- D2vsI.lm
nestedplotlist[[paste0("Plot_D2vsI", weekday)]] <- plot22
}

grid.arrange(nestedplotlist[["Plot_D2vsIMon"]],nestedplotlist[["Plot_D2vsITue"]],nestedplotlist[["Plot_D2vsIWed"]], nestedplotlist[["Plot_D2vsIThu"]], nestedplotlist[["Plot_D2vsIFri"]], nestedplotlist[["Plot_D2vsISat"]], nestedplotlist[["Plot_D2vsISun"]], nrow=3)


# 3) D + D^2 vs I
for(weekday in weekdays){
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ outOfHomeDurationSquared + outOfHomeDuration")
joinedDataFrame <- joinedDataFrame %>% mutate(outOfHomeDurationSquared = outOfHomeDuration*outOfHomeDuration)
DplusD2vsI.lm <- lm(formula=formula.lm, data=joinedDataFrame) #Examplary regression for Bayern
plot22 <- ggplot(data=joinedDataFrame, aes(x=outOfHomeDurationSquared + outOfHomeDuration, y = .data[[weekdayString]])) +
geom_point() +
geom_smooth(method="lm") +
ggtitle(weekday) +
theme_minimal()
nestedplotlist[[paste0("Regression_DplusD2vsI", weekday)]] <- DplusD2vsI.lm
nestedplotlist[[paste0("Plot_DplusD2vsI", weekday)]] <- plot22
}

grid.arrange(nestedplotlist[["Plot_DplusD2vsIMon"]],nestedplotlist[["Plot_DplusD2vsITue"]],nestedplotlist[["Plot_DplusD2vsIWed"]], nestedplotlist[["Plot_DplusD2vsIThu"]], nestedplotlist[["Plot_DplusD2vsIFri"]], nestedplotlist[["Plot_DplusD2vsISat"]], nestedplotlist[["Plot_DplusD2vsISun"]], nrow=3)


# 4a) D + tmax vs I
for(weekday in weekdays){
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ outOfHomeDuration + tmax")
DplustmaxvsI.lm <- lm(formula=formula.lm, data=joinedDataFrame) #Examplary regression for Bayern
plot22 <- ggplot(data=joinedDataFrame, aes(x = outOfHomeDuration + tmax, y = .data[[weekdayString]])) +
geom_point() +
geom_smooth(method = "lm") +
ggtitle(weekday) +
theme_minimal()
nestedplotlist[[paste0("Regression_DplustmaxvsI", weekday)]] <- DplustmaxvsI.lm
nestedplotlist[[paste0("Plot_DplustmaxvsI", weekday)]] <- plot22
}
grid.arrange(nestedplotlist[["Plot_DplustmaxvsIMon"]],nestedplotlist[["Plot_DplustmaxvsITue"]],nestedplotlist[["Plot_DplustmaxvsIWed"]], nestedplotlist[["Plot_DplustmaxvsIThu"]], nestedplotlist[["Plot_DplustmaxvsIFri"]], nestedplotlist[["Plot_DplustmaxvsISat"]], nestedplotlist[["Plot_DplustmaxvsISun"]], nrow=3)


# 4b) D + tavg vs I
for(weekday in weekdays){
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ outOfHomeDuration + tavg")
DplustavgvsI.lm <- lm(formula = formula.lm, data=joinedDataFrame)
plot22 <- ggplot(data=joinedDataFrame) +
geom_point(aes(x=outOfHomeDuration+tavg, y = .data[[weekdayString]])) +
geom_smooth(aes(x= outOfHomeDuration+tavg, y = .data[[weekdayString]]), method = "lm") +
ggtitle(weekday) +
theme_minimal()
nestedplotlist[[paste0("Regression_DplustavgvsI", weekday)]] <- DplustavgvsI.lm
nestedplotlist[[paste0("Plot_DplustavgvsI", weekday)]] <- plot22
}

grid.arrange(nestedplotlist[["Plot_DplustavgvsIMon"]],nestedplotlist[["Plot_DplustavgvsITue"]],nestedplotlist[["Plot_DplustavgvsIWed"]], nestedplotlist[["Plot_DplustavgvsIThu"]], nestedplotlist[["Plot_DplustavgvsIFri"]], nestedplotlist[["Plot_DplustavgvsISat"]], nestedplotlist[["Plot_DplustavgvsISun"]], nrow=3)


# 5a) D*tmax vs I
for(weekday in weekdays){
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ outOfHomeDuration*tmax")
DtimestmaxvsI.lm <- lm(formula = formula.lm, data=joinedDataFrame)
plot22 <- ggplot(data=joinedDataFrame) +
geom_point(aes(x=outOfHomeDuration*tmax, y = .data[[weekdayString]])) +
geom_smooth(aes(x= outOfHomeDuration*tmax, y = .data[[weekdayString]]), method = "lm") +
theme_minimal()
nestedplotlist[[paste0("Regression_DtimestmaxvsI", weekday)]] <- DtimestmaxvsI.lm
nestedplotlist[[paste0("Plot_DtimestmaxvsI", weekday)]] <- plot22
}

grid.arrange(nestedplotlist[["Plot_DtimestmaxvsIMon"]],nestedplotlist[["Plot_DtimestmaxvsITue"]],nestedplotlist[["Plot_DtimestmaxvsIWed"]], nestedplotlist[["Plot_DtimestmaxvsIThu"]], nestedplotlist[["Plot_DtimestmaxvsIFri"]], nestedplotlist[["Plot_DtimestmaxvsISat"]], nestedplotlist[["Plot_DtimestmaxvsISun"]], nrow=3)


# 5b) D*tavg vs I
for(weekday in weekdays){
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ outOfHomeDuration*tavg")
DtimestavgvsI.lm <- lm(formula=formula.lm, data=joinedDataFrame)
plot22 <- ggplot(data=joinedDataFrame) +
geom_point(aes(x=outOfHomeDuration*tavg, y = .data[[weekdayString]])) +
geom_smooth(aes(x= outOfHomeDuration*tavg, y = .data[[weekdayString]]), method = "lm") +
theme_minimal()
nestedplotlist[[paste0("Regression_DtimestavgvsI", weekday)]] <- DtimestavgvsI.lm
nestedplotlist[[paste0("Plot_DtimestavgvsI", weekday)]] <- plot22
}

grid.arrange(nestedplotlist[["Plot_DtimestavgvsIMon"]],nestedplotlist[["Plot_DtimestavgvsITue"]],nestedplotlist[["Plot_DtimestavgvsIWed"]], nestedplotlist[["Plot_DtimestavgvsIThu"]], nestedplotlist[["Plot_DtimestavgvsIFri"]], nestedplotlist[["Plot_DtimestavgvsISat"]], nestedplotlist[["Plot_DtimestavgvsISun"]], nrow=3)


# 6a) D + out vs I
for(weekday in weekdays){
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ outOfHomeDuration + outdoorFraction")
DplusoutvsI.lm <- lm(formula=formula.lm, data=joinedDataFrame)
plot22 <- ggplot(data=joinedDataFrame) +
geom_point(aes(x=outOfHomeDuration+outdoorFraction, y = .data[[weekdayString]])) +
geom_smooth(aes(x= outOfHomeDuration+outdoorFraction, y = .data[[weekdayString]]), method = "lm") +
theme_minimal()
nestedplotlist[[paste0("Regression_DplusoutvsI", weekday)]] <- DplusoutvsI.lm
nestedplotlist[[paste0("Plot_DplusoutvsI", weekday)]] <- plot22
}

grid.arrange(nestedplotlist[["Plot_DplusoutvsIMon"]],nestedplotlist[["Plot_DplusoutvsITue"]],nestedplotlist[["Plot_DplusoutvsIWed"]], nestedplotlist[["Plot_DplusoutvsIThu"]], nestedplotlist[["Plot_DplusoutvsIFri"]], nestedplotlist[["Plot_DplusoutvsISat"]], nestedplotlist[["Plot_DplusoutvsISun"]], nrow=3)


# 6b) D + out2 vs I
for(weekday in weekdays){
    formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ outOfHomeDuration + outdoorFraction2")
Dplusout2vsI.lm <- lm(formula = formula.lm, data=joinedDataFrame)
plot22 <- ggplot(data=joinedDataFrame) +
geom_point(aes(x=outOfHomeDuration+outdoorFraction2, y = .data[[weekdayString]])) +
geom_smooth(aes(x= outOfHomeDuration+outdoorFraction2, y = .data[[weekdayString]]), method = "lm") +
theme_minimal()
nestedplotlist[[paste0("Regression_Dplusout2vsI", weekday)]] <- DplusoutvsI.lm
nestedplotlist[[paste0("Plot_Dplusout2vsI", weekday)]] <- plot22
}

grid.arrange(nestedplotlist[["Plot_Dplusout2vsIMon"]],nestedplotlist[["Plot_Dplusout2vsITue"]],nestedplotlist[["Plot_Dplusout2vsIWed"]], nestedplotlist[["Plot_Dplusout2vsIThu"]], nestedplotlist[["Plot_Dplusout2vsIFri"]], nestedplotlist[["Plot_Dplusout2vsISat"]], nestedplotlist[["Plot_Dplusout2vsISun"]], nrow=3)


# 7a) D * out vs I
for(weekday in weekdays){
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ outOfHomeDuration * outdoorFraction")
DtimesoutvsI.lm <- lm(formula = formula.lm, data=joinedDataFrame)
plot22 <- ggplot(data=joinedDataFrame) +
geom_point(aes(x=outOfHomeDuration*outdoorFraction+outOfHomeDuration+outdoorFraction, y = .data[[weekdayString]])) +
geom_smooth(aes(x= outOfHomeDuration*outdoorFraction+outOfHomeDuration+outdoorFraction, y = .data[[weekdayString]]), method = "lm") +
theme_minimal()
nestedplotlist[[paste0("Regression_DtimesoutvsI", weekday)]] <- plot22
nestedplotlist[[paste0("Plot_DtimesoutvsI", weekday)]] <- plot22
}

grid.arrange(nestedplotlist[["Plot_DtimesoutvsIMon"]],nestedplotlist[["Plot_DtimesoutvsITue"]],nestedplotlist[["Plot_DtimesoutvsIWed"]], nestedplotlist[["Plot_DtimesoutvsIThu"]], nestedplotlist[["Plot_DtimesoutvsIFri"]], nestedplotlist[["Plot_DtimesoutvsISat"]], nestedplotlist[["Plot_DtimesoutvsISun"]], nrow=3)


# 7b) D * out2 vs I
for(weekday in weekdays){
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ outOfHomeDuration * outdoorFraction2")
Dtimesout2vsI.lm <- lm(formula = formula.lm, data=joinedDataFrame)
plot22 <- ggplot(data=joinedDataFrame) +
geom_point(aes(x=outOfHomeDuration*outdoorFraction2+outOfHomeDuration+outdoorFraction2, y = .data[[weekdayString]])) +
geom_smooth(aes(x= outOfHomeDuration*outdoorFraction2+outOfHomeDuration+outdoorFraction2, y = .data[[weekdayString]]), method = "lm") +
theme_minimal()
nestedplotlist[[paste0("Regression_Dtimesout2vsI", weekday)]] <- plot22
nestedplotlist[[paste0("Plot_Dtimesout2vsI", weekday)]] <- plot22
}

grid.arrange(nestedplotlist[["Plot_Dtimesout2vsIMon"]],nestedplotlist[["Plot_Dtimesout2vsITue"]],nestedplotlist[["Plot_Dtimesout2vsIWed"]], nestedplotlist[["Plot_Dtimesout2vsIThu"]], nestedplotlist[["Plot_Dtimesout2vsIFri"]], nestedplotlist[["Plot_Dtimesout2vsISat"]], nestedplotlist[["Plot_Dtimesout2vsISun"]], nrow=3)


# 8) D + prcp
for(weekday in weekdays){
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ outOfHomeDuration + prcp")
DplusprcpvsI.lm <- lm(formula = formula.lm, data=joinedDataFrame) #Examplary regression for Bayern
plot22 <- ggplot(data=joinedDataFrame) +
geom_point(aes(x=outOfHomeDuration+prcp, y = .data[[weekdayString]])) +
geom_smooth(aes(x= outOfHomeDuration+prcp, y = .data[[weekdayString]]), method = "lm") +
theme_minimal()
nestedplotlist[[paste0("Regression_DplusprcpvsI", weekday)]] <- DplustmaxvsI.lm
nestedplotlist[[paste0("Plot_DplusprcpvsI", weekday)]] <- plot22
}

grid.arrange(nestedplotlist[["Plot_DplusprcpvsIMon"]],nestedplotlist[["Plot_DplusprcpvsITue"]],nestedplotlist[["Plot_DplusprcpvsIWed"]], nestedplotlist[["Plot_DplusprcpvsIThu"]], nestedplotlist[["Plot_DplusprcpvsIFri"]], nestedplotlist[["Plot_DplusprcpvsISat"]], nestedplotlist[["Plot_DplusprcpvsISun"]], nrow=3)

# 9) D + tmax + prcp
for(weekday in weekdays){
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ outOfHomeDuration * tmax * prcp")
DplustmaxprcpvsI.lm <- lm(formula = formula.lm, data=joinedDataFrame) 
plot22 <- ggplot(data=joinedDataFrame) +
geom_point(aes(x=outOfHomeDuration*tmax*prcp, y = .data[[weekdayString]])) +
geom_smooth(aes(x= outOfHomeDuration*tmax*prcp, y = .data[[weekdayString]]), method = "lm") +
theme_minimal()
nestedplotlist[[paste0("Regression_DplustmaxprcpvsI", weekday)]] <- DplustmaxprcpvsI.lm
nestedplotlist[[paste0("Plot_DplustmaxprcpvsI", weekday)]] <- plot22
}

grid.arrange(nestedplotlist[["Plot_DplustmaxprcpvsIMon"]],nestedplotlist[["Plot_DplustmaxprcpvsITue"]],nestedplotlist[["Plot_DplustmaxprcpvsIWed"]], nestedplotlist[["Plot_DplustmaxprcpvsIThu"]], nestedplotlist[["Plot_DplustmaxprcpvsIFri"]], nestedplotlist[["Plot_DplustmaxprcpvsISat"]], nestedplotlist[["Plot_DplustmaxprcpvsISun"]], nrow=3)


# 9) D + out + prcp
for(weekday in weekdays){
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ outOfHomeDuration + outdoorFraction + prcp")
DplusoutplusprcpvsI.lm <- lm(formula = formula.lm, data=joinedDataFrame) #Examplary regression for Bayern
plot22 <- ggplot(data=joinedDataFrame) +
geom_point(aes(x=outOfHomeDuration+outdoorFraction+prcp, y = .data[[weekdayString]])) +
geom_smooth(aes(x= outOfHomeDuration+outdoorFraction+prcp, y = .data[[weekdayString]]), method = "lm") +
theme_minimal()
nestedplotlist[[paste0("Regressio_DplusoutplusprcpvsI", weekday)]] <- DplusoutplusprcpvsI.lm
nestedplotlist[[paste0("Plot_DplusoutplusprcpvsI", weekday)]] <- plot22
}

grid.arrange(nestedplotlist[["Plot_DplusoutplusprcpvsIMon"]],nestedplotlist[["Plot_DplusoutplusprcpvsITue"]],nestedplotlist[["Plot_DplusoutplusprcpvsIWed"]], nestedplotlist[["Plot_DplusoutplusprcpvsIThu"]], nestedplotlist[["Plot_DplusoutplusprcpvsIFri"]], nestedplotlist[["Plot_DplusoutplusprcpvsISat"]], nestedplotlist[["Plot_DplusoutplusprcpvsISun"]], nrow=3)


# 10) D + D:out2 + D:prcp
for(weekday in weekdays){
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ outOfHomeDuration+outOfHomeDuration:outdoorFraction2+outOfHomeDuration:prcp")
DplusoutplusprcpvsI.lm <- lm(formula = formula.lm, data=joinedDataFrame) #Examplary regression for Bayern
plot22 <- ggplot(data=joinedDataFrame) +
geom_point(aes(x=outOfHomeDuration+outOfHomeDuration:outdoorFraction+outOfHomeDuration:prcp, y = .data[[weekdayString]])) +
geom_smooth(aes(x= outOfHomeDuration+outOfHomeDuration:outdoorFraction+outOfHomeDuration:prcp, y =.data[[weekdayString]]), method = "lm") +
theme_minimal()
nestedplotlist[[paste0("Regression_DplusoutplusprcpvsI", weekday)]] <- DplusoutplusprcpvsI.lm 
nestedplotlist[[paste0("Plot_DplusoutplusprcpvsI", weekday)]] <- plot22
}

grid.arrange(nestedplotlist[["Plot_DplusoutplusprcpvsIMon"]],nestedplotlist[["Plot_DplusoutplusprcpvsITue"]],nestedplotlist[["Plot_DplusoutplusprcpvsIWed"]], nestedplotlist[["Plot_DplusoutplusprcpvsIThu"]], nestedplotlist[["Plot_DplusoutplusprcpvsIFri"]], nestedplotlist[["Plot_DplusoutplusprcpvsISat"]], nestedplotlist[["Plot_DplusoutplusprcpvsISun"]], nrow=3)


#Performing multiple polynomial regression

#joinedDataFrame.pm1 <- lm(changeOfIncidence ~ polym(outOfHomeDuration, tmax, degree=2), data=filter(joinedDataFrame, changeOfIncidencelagged2 < 1.6)) #ToDo: How to visualize this?

#fpoly2<-function(x1,x2){
#  return(joinedDataFrame.pm1$coefficients["(Intercept)"]+
#  joinedDataFrame.pm1$coefficients["polym(outOfHomeDuration, outdoorFraction2, degree = 2)1.0"]*x1+
#  joinedDataFrame.pm1$coefficients["polym(outOfHomeDuration, outdoorFraction2, degree = 2)2.0"]*x1*x1+
#  joinedDataFrame.pm1$coefficients["polym(outOfHomeDuration, outdoorFraction2, degree = 2)0.1"]*x2+
#  joinedDataFrame.pm1$coefficients["polym(outOfHomeDuration, outdoorFraction2, degree = 2)1.1"]*x1*x2+
#  joinedDataFrame.pm1$coefficients["polym(outOfHomeDuration, outdoorFraction2, degree = 2)0.2"]*x2*x2)
#}

#joinedDataFrameBayern <- filter(joinedDataFrame, , changeOfIncidencelagged2 < 1.6)
#x1x2_poly <- c(joinedDataFrameBayern$outOfHomeDuration, joinedDataFrameBayern$outdoorFraction)
#y_poly <- poly(x1x2_poly)
#dataframex1x2y <- data.frame(x1x2_poly, y_poly)

#ggplot() +
#geom_point(data=dataframex1x2y, aes(x= x1x2_poly, y = X1), color= "blue") +
#geom_point(data=joinedDataFrameBayern, aes(x=))
#theme_minimal()

