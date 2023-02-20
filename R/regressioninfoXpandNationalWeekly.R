library(tidyverse)
library(lubridate)
library(readxl)
library(httr)
library(gridExtra)
library(ggiraphExtra)
library(leaps)

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
dict_state_id <- data.frame(matrix(nrow = 0, ncol = 2))
colnames(dict_state_id) <- c("Bundesland", "ID")
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Baden-Württemberg", 10738)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Bayern", 10865)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Berlin", 10382)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Brandenburg", 10379)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Bremen", 10224)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Hamburg", 10147)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Hessen", 10633)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Mecklenburg-Vorpommern", 10162)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Niedersachsen", 10338)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Nordrhein-Westfalen", 10400)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Rheinland-Pfalz", "D3137")
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Saarland", "D6217")
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Sachsen", "D1051")
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Sachsen-Anhalt", 10361)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Schleswig-Holstein", 10044)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Thüringen", 10554)

#Reading incidence data in, data hereby comes from RKI
url1 <-'https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Kum_Tab_Archiv.xlsx?__blob=publicationFile'
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
incidence_data_archiv <- read_excel(tf, 3)
colnames(incidence_data_archiv) <- incidence_data_archiv[4, ]
colnames(incidence_data_archiv)[1] <- "Bundesland"
incidence_data_archiv <- incidence_data_archiv[-(1:4), ]
incidence_data_archiv <- pivot_longer(incidence_data_archiv, names_to="Date", values_to="Incidence", cols=colnames(incidence_data_archiv)[2:ncol(incidence_data_archiv)])
incidence_data_archiv$Date <- as.integer(incidence_data_archiv$Date)
incidence_data_archiv$Date <- as.Date(incidence_data_archiv$Date,origin="1899-12-30")
incidence_data_archiv <- incidence_data_archiv %>% filter(Bundesland == "Gesamt")

url1 <- 'https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Kum_Tab_aktuell.xlsx?__blob=publicationFile'
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
incidence_data <- read_excel(tf, 4)
colnames(incidence_data) <- incidence_data[4, ]
colnames(incidence_data)[1] <- "Bundesland"
incidence_data <- incidence_data[-(1:4),] #Removing the 1st line, as this solely contains the column names
# hospitalizationData <- mutate(hospitalizationData, BundeslandId=c("08", "09", "11", "12", "04", "02", "06", "13", "03", "05", "07", "10", "14", "15", "01", "16", "00"))
incidence_data <- pivot_longer(incidence_data, names_to="Date", values_to="Incidence", cols=colnames(incidence_data)[2:ncol(incidence_data)])
incidence_data$Date <- paste(substr(incidence_data$Date, start = 7, stop = 10), substr(incidence_data$Date, start = 4, stop = 5), as.character(substr(incidence_data$Date, start = 1, stop = 2)), sep ="-" )
incidence_data$Date <- as.Date(incidence_data$Date)
incidence_data$Date <- incidence_data$Date
incidence_data <- incidence_data %>% filter(Bundesland == "Gesamt")

# incidence_data2 <- incidence_data
incidence_data <- rbind(incidence_data_archiv, incidence_data)
incidence_data$Incidence <- as.double(incidence_data$Incidence)
incidence_data <- filter(incidence_data, Date < as.Date("2021-01-01") & Date > as.Date("2020-05-11"))

incidence_data <- incidence_data %>%
  mutate(weekMon = cut(Date, "week")) %>% #Weeks start on Monday
  mutate(weekTue = cut(Date - 1, "week")) %>% #Weeks start on Tuesday
  mutate(weekWed = cut(Date - 2, "week")) %>% #Weeks start on Wednesday
  mutate(weekThu = cut(Date - 3, "week")) %>% #Weeks start on Thursday
  mutate(weekFri = cut(Date - 4, "week")) %>% #Weeks start on Friday
  mutate(weekSat = cut(Date - 5, "week")) %>% #Weeks start on Saturday
  mutate(weekSun = cut(Date - 6, "week")) %>% #Weeks start on Sunday
  mutate(year = year(Date))

incidence_data <- incidence_data[-1, ]

incidence_dataMon <- incidence_data %>%  group_by(year, weekMon, Bundesland) %>%
  summarise(Date = min(Date) + 6, IncidenceMon = mean(Incidence))
incidence_dataTue <- incidence_data %>%  group_by(year, weekTue, Bundesland) %>%
  summarise(Date = min(Date) + 5, IncidenceTue = mean(Incidence))
incidence_dataWed <- incidence_data %>%  group_by(year, weekWed, Bundesland) %>%
  summarise(Date = min(Date) + 4, IncidenceWed = mean(Incidence))
incidence_dataThu <- incidence_data %>%  group_by(year, weekThu, Bundesland) %>%
  summarise(Date = min(Date) + 3, IncidenceThu = mean(Incidence))
incidence_dataFri <- incidence_data %>%  group_by(year, weekFri, Bundesland) %>%
  summarise(Date = min(Date) + 2, IncidenceFri = mean(Incidence))
incidence_dataSat <- incidence_data %>%  group_by(year, weekSat, Bundesland) %>%
  summarise(Date = min(Date) + 1, IncidenceSat = mean(Incidence))
incidence_dataSun <- incidence_data %>%  group_by(year, weekSun, Bundesland) %>%
  summarise(Date = min(Date), IncidenceSun = mean(Incidence))

incidence_data <- inner_join(incidence_dataMon, incidence_dataTue, by = "Date")
incidence_data <- inner_join(incidence_data, incidence_dataWed, by = "Date")
incidence_data <- inner_join(incidence_data, incidence_dataThu, by = "Date")
incidence_data <- inner_join(incidence_data, incidence_dataFri, by = "Date")
incidence_data <- inner_join(incidence_data, incidence_dataSat, by = "Date")
incidence_data <- inner_join(incidence_data, incidence_dataSun, by = "Date")

incidence_data <- select(incidence_data, c("Date", "year", "IncidenceMon", "IncidenceTue", "IncidenceWed", "IncidenceThu", "IncidenceFri", "IncidenceSat", "IncidenceSun"))
#Adding the change of incidence to the data frame
incidence_data[, "changeOfIncidenceMon"] <- as.double(incidence_data$IncidenceMon/lag(incidence_data$IncidenceMon))
incidence_data[, "changeOfIncidenceTue"] <- incidence_data$IncidenceTue/lag(incidence_data$IncidenceTue)
incidence_data[, "changeOfIncidenceWed"] <- incidence_data$IncidenceWed/lag(incidence_data$IncidenceWed)
incidence_data[, "changeOfIncidenceThu"] <- incidence_data$IncidenceThu/lag(incidence_data$IncidenceThu)
incidence_data[, "changeOfIncidenceFri"] <- incidence_data$IncidenceFri/lag(incidence_data$IncidenceFri)
incidence_data[, "changeOfIncidenceSat"] <- incidence_data$IncidenceSat/lag(incidence_data$IncidenceSat)
incidence_data[, "changeOfIncidenceSun"] <- incidence_data$IncidenceSun/lag(incidence_data$IncidenceSun)

## Estimation for all states simultaneously

#Reading in Mobility data on federal state level, using weekly data
mobility_data <- read_delim("https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/episim/mobilityData/bundeslaender/mobilityData_OverviewBL_weekly.csv")

#Little bit of cleaning, turning date column into right format, renaming columns
mobility_data$date <- paste(substr(mobility_data$date, start = 1, stop = 4), substr(mobility_data$date, start = 5, stop = 6), as.character(substr(mobility_data$date, start = 7, stop = 8)), sep ="-" )
mobility_data$date <- as.Date(mobility_data$date)
colnames(mobility_data)[2] <- "Bundesland"
colnames(mobility_data)[1] <- "Date"

mobility_data <- mobility_data %>% filter(Bundesland == "Deutschland") %>% select(-Bundesland)

joinedDataFrame <- inner_join(incidence_data, mobility_data, by = "Date")

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
weather_data_all <- data.frame(matrix(nrow = 0, ncol = 3))
for (state in 1:length(dict_state_id$Bundesland)){
ID <- dict_state_id[as.integer(state), 2]
weather_data <- read_delim(paste0("https://bulk.meteostat.net/daily/", ID, ".csv.gz"))
colnames(weather_data) <- c("Date", "tavg", "tmin", "tmax", "prcp", "snow", "wdir", "wspd", "wpgt", "pres", "tsun")

weather_data$Date <- as.Date(weather_data$Date)
weather_data <- weather_data[, c("Date", "tmax", "tavg", "prcp")]
weather_data$Bundesland <- dict_state_id[as.integer(state), 1]

weather_data_all <- rbind(weather_data_all, weather_data)
}

weather_data_all <- filter(weather_data_all, Date < "2021-01-01") %>%
filter(Date > "2020-01-01") %>%
  mutate(week = week(Date)) %>%
  mutate(year = year(Date)) %>%
  group_by(year, week) %>%
  summarise(Date = min(Date)+4, tmax = mean(tmax), tavg = mean(tavg), prcp = mean(prcp))


#Converting the weather data to an "outdoor fraction"
#Below a certain temperature, everything happens indoords, above a certain temperature everything happens outdoors and in between we linearize
weather_data_all <- mutate(weather_data_all, outdoorFraction = case_when(22.5 >= tmax & tmax >= 12.5 ~ 2 - 0.1*(22.5 - tmax),
                                                                   tmax < 12.5 ~ 2,
                                                                  tmax > 22.5 ~ 1))
#Alternative to compute outdoor fraction
weather_data_all <- mutate(weather_data_all, outdoorFraction2 = case_when(22.5 >= tmax & tmax >= 12.5 ~ 0.1*(22.5 - tmax),
                                                                   tmax < 12.5 ~ 0,
                                                                  tmax > 22.5 ~ 1))                                                                 
#Joining the data frames
joinedDataFrame <- left_join(joinedDataFrame, weather_data_all, by = "Date")
joinedDataFrame <- mutate(joinedDataFrame, explanatory = outOfHomeDuration*outOfHomeDuration*(outdoorFraction)) #What am I using this one for?

#Only for 2020 (and a bit of 2021, as not too many people were vaccinated and alpha wasn't dominant yet)
joinedDataFrame <- filter(joinedDataFrame, Date < "2021-02-22") #Not quite sure why we are filtering again

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

# 1) D vs I
#Performing linear regression
nestedplotlist <- list()

joinedDataFrame <- joinedDataFrame[-nrow(joinedDataFrame), ]

weekdays <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Mon_1week_lag")
for (weekday in weekdays){
if (weekday == "Mon_1week_lag") {
weekdayString <- "changeOfIncidencelaggedMon"
formula.lm <- "changeOfIncidencelaggedMon ~ outOfHomeDuration"
} else {
weekdayString <- paste0("changeOfIncidencelagged", weekday, "2")
formula.lm <- paste0("changeOfIncidencelagged", weekday, "2", " ~ outOfHomeDuration")
}
DvsI.lm <- lm(formula = formula.lm, data = joinedDataFrame) #Regression
if (weekday == "Mon") {
title <- "14 Day lag"
} else if (weekday == "Tue") {
title <- "13 Day lag"
} else if (weekday == "Wed") {
title <- "12 Day lag"
} else if (weekday == "Thu") {
title <- "11 Day lag"
} else if (weekday == "Fri") {
title <- "10 Day lag"
} else if (weekday == "Sat") {
title <- "9 Day lag"
} else if (weekday == "Sun") {
title <- "8 Day lag"
} else if (weekday == "Mon_1week_lag") {
title <- "7 Day lag"
}
plot22 <- ggplot(data = joinedDataFrame) + #First plot; x = outOfHomeDuration, y = changeOfIncidence
geom_point(aes(x = outOfHomeDuration, y = .data[[weekdayString]])) +
geom_smooth(aes(x= outOfHomeDuration, y = .data[[weekdayString]]), method = "lm") +
ggtitle(title) +
theme_minimal()

plot23 <- ggplot(data = joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
geom_smooth(aes(x=coefficients(DvsI.lm)["(Intercept)"] + coefficients(DvsI.lm)["outOfHomeDuration"] * outOfHomeDuration, y = .data[[weekdayString]], color="lightgrey"), method="lm", se=FALSE, size = 2) +
ggtitle(title) +
xlab("Intercept + a * outOfHomeDuration ") +
ylab("changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
scale_color_identity(labels = c("x=y", "Regression line"), guide="legend") +
theme_minimal() +
theme(legend.position = "bottom", legend.title = element_blank()) +
geom_point(data = joinedDataFrame, aes(x=coefficients(DvsI.lm)["(Intercept)"] + coefficients(DvsI.lm)["outOfHomeDuration"] * outOfHomeDuration, y = .data[[weekdayString]], fill=tmax), size=3, shape=21)


nestedplotlist[[paste0("Regression_DvsI", weekday)]] <- DvsI.lm
nestedplotlist[[paste0("Plot_DvsI_", weekday)]] <- plot22
nestedplotlist[[paste0("ResvsFittedValues_DvsI", weekday)]] <- function(){
plot(DvsI.lm, which=1)
}
nestedplotlist[[paste0("Qqplot_DvsI", weekday)]] <- function(){
plot(DvsI.lm, which=2)
}
nestedplotlist[[paste0("ScaleLoc_DvsI", weekday)]] <- function(){
plot(DvsI.lm, which=3)
}
nestedplotlist[[paste0("Cooksdist_DvsI", weekday)]] <- function(){
plot(DvsI.lm, which=4)
}
nestedplotlist[[paste0("ActualvsEstimation_DvsI_", weekday)]] <- plot23
}

grid.arrange(nestedplotlist[["Plot_DvsIMon_1week_lag"]],nestedplotlist[["Plot_DvsISun"]],nestedplotlist[["Plot_DvsISat"]],nestedplotlist[["Plot_DvsIFri"]], nestedplotlist[["Plot_DvsIThu"]], nestedplotlist[["Plot_DvsIWed"]], nestedplotlist[["Plot_DvsITue"]], nestedplotlist[["Plot_DvsIMon"]], nrow=3)
grid.arrange(nestedplotlist[["ActualvsEstimation_DvsI_Mon_1week_lag"]],nestedplotlist[["ActualvsEstimation_DvsI_Sun"]],nestedplotlist[["ActualvsEstimation_DvsI_Sat"]],nestedplotlist[["ActualvsEstimation_DvsI_Fri"]], nestedplotlist[["ActualvsEstimation_DvsI_Thu"]], nestedplotlist[["ActualvsEstimation_DvsI_Wed"]], nestedplotlist[["ActualvsEstimation_DvsI_Tue"]], nestedplotlist[["ActualvsEstimation_DvsI_Mon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimation_DvsI_Mon_1week_lag"]],nestedplotlist[["ActualvsEstimation_DvsI_Sun"]],nestedplotlist[["ActualvsEstimation_DvsI_Sat"]],nestedplotlist[["ActualvsEstimation_DvsI_Fri"]], nestedplotlist[["ActualvsEstimation_DvsI_Thu"]], nestedplotlist[["ActualvsEstimation_DvsI_Wed"]], nestedplotlist[["ActualvsEstimation_DvsI_Tue"]], nestedplotlist[["ActualvsEstimation_DvsI_Mon"]], nrow=3)
#g <- arrangeGrob(nestedplotlist[["Plot_DvsI_Mon_1week_lag"]],nestedplotlist[["Plot_DvsI_Sun"]],nestedplotlist[["Plot_DvsI_Sat"]],nestedplotlist[["Plot_DvsI_Fri"]], nestedplotlist[["Plot_DvsI_Thu"]], nestedplotlist[["Plot_DvsI_Wed"]], nestedplotlist[["Plot_DvsI_Tue"]], nestedplotlist[["Plot_DvsI_Mon"]], nrow=3)


# 2) D^2 vs I
for (weekday in weekdays){
joinedDataFrame <- joinedDataFrame %>% mutate(outOfHomeDurationSquared = outOfHomeDuration*outOfHomeDuration)
if (weekday == "Mon_1week_lag") {
weekdayString <- "changeOfIncidencelaggedMon"
formula.lm <- "changeOfIncidencelaggedMon ~ outOfHomeDurationSquared"
} else {weekdayString <- paste0("changeOfIncidencelagged", weekday, "2")
formula.lm <- paste0("changeOfIncidencelagged", weekday, "2", " ~ outOfHomeDurationSquared")
weekdayString <- paste0("changeOfIncidencelagged", weekday, "2")
}
D2vsI.lm <- lm(formula = formula.lm, data = filter(joinedDataFrame)) # Regression
if (weekday == "Mon") {
title <- "14 Day lag"
} else if (weekday == "Tue") {
title <- "13 Day lag"
} else if (weekday == "Wed") {
title <- "12 Day lag"
} else if (weekday == "Thu") {
title <- "11 Day lag"
} else if (weekday == "Fri") {
title <- "10 Day lag"
} else if (weekday == "Sat") {
title <- "9 Day lag"
} else if (weekday == "Sun") {
title <- "8 Day lag"
} else if (weekday == "Mon_1week_lag") {
title <- "7 Day lag"
}
plot22 <- ggplot(joinedDataFrame) + #1st plot; x = outOfHomeDurationSquared, y = changeOfIncidence
geom_point(aes(x = outOfHomeDurationSquared, y = .data[[weekdayString]])) +
geom_smooth(aes(x= outOfHomeDurationSquared, y = .data[[weekdayString]]), method = "lm") +
ggtitle(title) +
theme_minimal()

plot23 <- ggplot(data = joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
geom_smooth(aes(x=coefficients(D2vsI.lm)["(Intercept)"] + coefficients(D2vsI.lm)["outOfHomeDurationSquared"] * outOfHomeDurationSquared, y = .data[[weekdayString]], color="lightgrey"), method="lm", se=FALSE, size = 2) +
ggtitle(title) +
xlab("Intercept + a * outOfHomeDuration^2") +
ylab("changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
scale_color_identity(labels = c("x=y", "Regression line"), guide="legend") +
theme_minimal() +
theme(legend.position = "bottom", legend.title = element_blank()) +
geom_point(aes(x=coefficients(D2vsI.lm)["(Intercept)"] + coefficients(D2vsI.lm)["outOfHomeDurationSquared"] * outOfHomeDurationSquared, y = .data[[weekdayString]], fill = tmax), size=3, shape = 21) 

nestedplotlist[[paste0("Regression_D2vsI", weekday)]] <- D2vsI.lm
nestedplotlist[[paste0("Plot_D2vsI", weekday)]] <- plot22
nestedplotlist[[paste0("ResvsFittedValues_D2vsI", weekday)]] <- function() {
plot(D2vsI.lm, which=1)
}
nestedplotlist[[paste0("Qqplot_D2vsI", weekday)]] <- function() {
plot(D2vsI.lm, which=2)
}
nestedplotlist[[paste0("ScaleLoc_D2vsI", weekday)]] <- function() {
plot(D2vsI.lm, which=3)
}
nestedplotlist[[paste0("Cooksdist_D2vsI", weekday)]] <- function() {
plot(D2vsI.lm, which=4)
}
nestedplotlist[[paste0("ActualvsEstimation_D2vsI", weekday)]] <- plot23
}

grid.arrange(nestedplotlist[["Plot_D2vsIMon_1week_lag"]],nestedplotlist[["Plot_D2vsISun"]],nestedplotlist[["Plot_D2vsISat"]],nestedplotlist[["Plot_D2vsIFri"]], nestedplotlist[["Plot_D2vsIThu"]], nestedplotlist[["Plot_D2vsIWed"]], nestedplotlist[["Plot_D2vsITue"]], nestedplotlist[["Plot_D2vsIMon"]], nrow=3)
grid.arrange(nestedplotlist[["ActualvsEstimation_D2vsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_D2vsISun"]],nestedplotlist[["ActualvsEstimation_D2vsISat"]],nestedplotlist[["ActualvsEstimation_D2vsIFri"]], nestedplotlist[["ActualvsEstimation_D2vsIThu"]], nestedplotlist[["ActualvsEstimation_D2vsIWed"]], nestedplotlist[["ActualvsEstimation_D2vsITue"]], nestedplotlist[["ActualvsEstimation_D2vsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimation_D2vsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_D2vsISun"]],nestedplotlist[["ActualvsEstimation_D2vsISat"]],nestedplotlist[["ActualvsEstimation_D2vsIFri"]], nestedplotlist[["ActualvsEstimation_D2vsIThu"]], nestedplotlist[["ActualvsEstimation_D2vsIWed"]], nestedplotlist[["ActualvsEstimation_D2vsITue"]], nestedplotlist[["ActualvsEstimation_D2vsIMon"]], nrow=3)
#g <- arrangeGrob(nestedplotlist[["Plot_D2vsIMon_1week_lag"]],nestedplotlist[["Plot_D2vsISun"]],nestedplotlist[["Plot_D2vsISat"]],nestedplotlist[["Plot_D2vsIFri"]], nestedplotlist[["Plot_D2vsIThu"]], nestedplotlist[["Plot_D2vsIWed"]], nestedplotlist[["Plot_D2vsITue"]], nestedplotlist[["Plot_D2vsIMon"]], nrow=3)


# 3) D + D^2 vs I
for (weekday in weekdays){
if (weekday == "Mon_1week_lag") {
weekdayString <- "changeOfIncidencelaggedMon"
formula.lm <- "changeOfIncidencelaggedMon ~ outOfHomeDurationSquared + outOfHomeDuration"
} else {
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ outOfHomeDurationSquared + outOfHomeDuration")
weekdayString <- paste0("changeOfIncidencelagged", weekday, "2")
}
joinedDataFrame <- joinedDataFrame %>% mutate(outOfHomeDurationSquared = outOfHomeDuration*outOfHomeDuration)
DplusD2vsI.lm <- lm(formula=formula.lm, data=joinedDataFrame) #Regression
if (weekday == "Mon") {
title <- "14 Day lag"
} else if (weekday == "Tue") {
title <- "13 Day lag"
} else if (weekday == "Wed") {
title <- "12 Day lag"
} else if (weekday == "Thu") {
title <- "11 Day lag"
} else if (weekday == "Fri") {
title <- "10 Day lag"
} else if (weekday == "Sat") {
title <- "9 Day lag"
} else if (weekday == "Sun") {
title <- "8 Day lag"
} else if (weekday == "Mon_1week_lag") {
title <- "7 Day lag"
}

#1st plot; x = outOfHomeDurationSquared, y = changeOfIncidence
plot22 <- ggplot(data=joinedDataFrame, aes(x=outOfHomeDurationSquared + outOfHomeDuration, y = .data[[weekdayString]])) + 
geom_point() +
geom_smooth(method = "lm") +
ggtitle(title) +
theme_minimal()

plot23 <- ggplot(data = joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
geom_smooth(aes(x=coefficients(DplusD2vsI.lm)["(Intercept)"] + coefficients(DplusD2vsI.lm)["outOfHomeDurationSquared"] * outOfHomeDurationSquared + coefficients(DplusD2vsI.lm)["outOfHomeDuration"] * outOfHomeDuration, y = .data[[weekdayString]], color="lightgrey"), method="lm", se=FALSE, size = 2) +
ggtitle(title) +
xlab("Intercept + a * outOfHomeDuration^2 + b * outOfHomeDuration") +
ylab("changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
theme_minimal() +
scale_color_identity(labels = c("x=y", "Regression line"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank()) +
geom_point(aes(x=coefficients(DplusD2vsI.lm)["(Intercept)"] + coefficients(DplusD2vsI.lm)["outOfHomeDurationSquared"] * outOfHomeDurationSquared + coefficients(DplusD2vsI.lm)["outOfHomeDuration"] * outOfHomeDuration, y = .data[[weekdayString]], fill = tmax), shape = 21, size =3) 


nestedplotlist[[paste0("Regression_DplusD2vsI", weekday)]] <- DplusD2vsI.lm

nestedplotlist[[paste0("ResvsFittedValues_DplusD2vsI", weekday)]] <- function(){
plot(DplusD2vsI.lm, which=1)
}
nestedplotlist[[paste0("Qqplot_DplusD2vsI", weekday)]] <- function(){
plot(DplusD2vsI.lm, which=2)
}
nestedplotlist[[paste0("ScaleLoc_DplusD2vsI", weekday)]] <- function(){
plot(DplusD2vsI.lm, which=3)
}
nestedplotlist[[paste0("Cooksdist_DplusD2vsI", weekday)]] <- function(){
plot(DplusD2vsI.lm, which=4)
}
nestedplotlist[[paste0("Plot_DplusD2vsI", weekday)]] <- plot22
nestedplotlist[[paste0("ActualvsEstimation_DplusD2vsI", weekday)]] <- plot23
}

grid.arrange(nestedplotlist[["Plot_DplusD2vsIMon_1week_lag"]],nestedplotlist[["Plot_DplusD2vsISun"]],nestedplotlist[["Plot_DplusD2vsISat"]],nestedplotlist[["Plot_DplusD2vsIFri"]], nestedplotlist[["Plot_DplusD2vsIThu"]], nestedplotlist[["Plot_DplusD2vsIWed"]], nestedplotlist[["Plot_DplusD2vsITue"]], nestedplotlist[["Plot_DplusD2vsIMon"]], nrow=3)
grid.arrange(nestedplotlist[["ActualvsEstimation_DplusD2vsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_DplusD2vsISun"]],nestedplotlist[["ActualvsEstimation_DplusD2vsISat"]],nestedplotlist[["ActualvsEstimation_DplusD2vsIFri"]], nestedplotlist[["ActualvsEstimation_DplusD2vsIThu"]], nestedplotlist[["ActualvsEstimation_DplusD2vsIWed"]], nestedplotlist[["ActualvsEstimation_DplusD2vsITue"]], nestedplotlist[["ActualvsEstimation_DplusD2vsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimation_DplusD2vsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_DplusD2vsISun"]],nestedplotlist[["ActualvsEstimation_DplusD2vsISat"]],nestedplotlist[["ActualvsEstimation_DplusD2vsIFri"]], nestedplotlist[["ActualvsEstimation_DplusD2vsIThu"]], nestedplotlist[["ActualvsEstimation_DplusD2vsIWed"]], nestedplotlist[["ActualvsEstimation_DplusD2vsITue"]], nestedplotlist[["ActualvsEstimation_DplusD2vsIMon"]], nrow=3)
#g <- arrangeGrob(nestedplotlist[["Plot_DplusD2vsIMon_1week_lag"]],nestedplotlist[["Plot_DplusD2vsISun"]],nestedplotlist[["Plot_DplusD2vsISat"]],nestedplotlist[["Plot_DplusD2vsIFri"]], nestedplotlist[["Plot_DplusD2vsIThu"]], nestedplotlist[["Plot_DplusD2vsIWed"]], nestedplotlist[["Plot_DplusD2vsITue"]], nestedplotlist[["Plot_DplusD2vsIMon"]], nrow=3)


# 4a) D + tmax vs I
for (weekday in weekdays){
if (weekday == "Mon_1week_lag") {
weekdayString <- "changeOfIncidencelaggedMon"
formula.lm <- "changeOfIncidencelaggedMon ~ outOfHomeDuration + tmax"
} else {
formula.lm <- paste0("changeOfIncidencelagged", weekday,"2", " ~ outOfHomeDuration + tmax")
weekdayString <- paste0("changeOfIncidencelagged", weekday, "2")
}
DplustmaxvsI.lm <- lm(formula = formula.lm, data = joinedDataFrame) #Regression
if (weekday == "Mon") {
title <- "14 Day lag"
} else if (weekday == "Tue") {
title <- "13 Day lag"
} else if (weekday == "Wed") {
title <- "12 Day lag"
} else if (weekday == "Thu") {
title <- "11 Day lag"
} else if (weekday == "Fri") {
title <- "10 Day lag"
} else if (weekday == "Sat") {
title <- "9 Day lag"
} else if (weekday == "Sun") {
title <- "8 Day lag"
} else if (weekday == "Mon_1week_lag") {
title <- "7 Day lag"
}

#1st plot; x = outOfHomeDuration, y = changeOfIncidence, color = tmax
plot22 <- ggplot(data=joinedDataFrame, aes(x = outOfHomeDuration, color= tmax, y = .data[[weekdayString]])) +
geom_point() +
geom_smooth(method = "lm") +
ggtitle(title) +
theme_minimal()

plot23 <- ggplot(data = joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
geom_smooth(aes(x=coefficients(DplustmaxvsI.lm)["(Intercept)"] + coefficients(DplustmaxvsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DplustmaxvsI.lm)["tmax"] * tmax, y = .data[[weekdayString]], color="lightgrey"), method="lm", se=FALSE, size = 2) +
geom_point(aes(x=coefficients(DplustmaxvsI.lm)["(Intercept)"] + coefficients(DplustmaxvsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DplustmaxvsI.lm)["tmax"] * tmax, y = .data[[weekdayString]], fill = tmax), size = 3, shape =21) +
ggtitle(title) +
xlab("Intercept + a * outOfHomeDuration + b * tmax") +
ylab("changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
theme_minimal() +
scale_color_identity(labels = c("x=y", "Regression line"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank())


nestedplotlist[[paste0("Regression_DplustmaxvsI", weekday)]] <- DplustmaxvsI.lm
nestedplotlist[[paste0("Plot_DplustmaxvsI", weekday)]] <- plot22
nestedplotlist[[paste0("ResvsFittedValues_DplustmaxvsI", weekday)]] <- function(){
plot(DplustmaxvsI.lm, which=1)
}
nestedplotlist[[paste0("Qqplot_DplustmaxvsI", weekday)]] <- function(){
plot(DplustmaxvsI.lm, which=2)
}
nestedplotlist[[paste0("ScaleLoc_DplustmaxvsI", weekday)]] <- function(){
plot(DplustmaxvsI.lm, which=3)
}
nestedplotlist[[paste0("Cooksdist_DplustmaxvsI", weekday)]] <- function(){
plot(DplustmaxvsI.lm, which=4)
}
nestedplotlist[[paste0("ActualvsEstimation_DplustmaxvsI", weekday)]] <- plot23
}
grid.arrange(nestedplotlist[["Plot_DplustmaxvsIMon_1week_lag"]],nestedplotlist[["Plot_DplustmaxvsISun"]],nestedplotlist[["Plot_DplustmaxvsISat"]],nestedplotlist[["Plot_DplustmaxvsIFri"]], nestedplotlist[["Plot_DplustmaxvsIThu"]], nestedplotlist[["Plot_DplustmaxvsIWed"]], nestedplotlist[["Plot_DplustmaxvsITue"]], nestedplotlist[["Plot_DplustmaxvsIMon"]], nrow=3)
grid.arrange(nestedplotlist[["ActualvsEstimation_DplustmaxvsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_DplustmaxvsISun"]],nestedplotlist[["ActualvsEstimation_DplustmaxvsISat"]],nestedplotlist[["ActualvsEstimation_DplustmaxvsIFri"]], nestedplotlist[["ActualvsEstimation_DplustmaxvsIThu"]], nestedplotlist[["ActualvsEstimation_DplustmaxvsIWed"]], nestedplotlist[["ActualvsEstimation_DplustmaxvsITue"]], nestedplotlist[["ActualvsEstimation_DplustmaxvsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimation_DplustmaxvsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_DplustmaxvsISun"]],nestedplotlist[["ActualvsEstimation_DplustmaxvsISat"]],nestedplotlist[["ActualvsEstimation_DplustmaxvsIFri"]], nestedplotlist[["ActualvsEstimation_DplustmaxvsIThu"]], nestedplotlist[["ActualvsEstimation_DplustmaxvsIWed"]], nestedplotlist[["ActualvsEstimation_DplustmaxvsITue"]], nestedplotlist[["ActualvsEstimation_DplustmaxvsIMon"]], nrow=3)
#g <- arrangeGrob(nestedplotlist[["Plot_DplustmaxvsIMon_1week_lag"]],nestedplotlist[["Plot_DplustmaxvsISun"]],nestedplotlist[["Plot_DplustmaxvsISat"]],nestedplotlist[["Plot_DplustmaxvsIFri"]], nestedplotlist[["Plot_DplustmaxvsIThu"]], nestedplotlist[["Plot_DplustmaxvsIWed"]], nestedplotlist[["Plot_DplustmaxvsITue"]], nestedplotlist[["Plot_DplustmaxvsIMon"]], nrow=3)

# 4b) D + tavg vs I
for (weekday in weekdays){
if (weekday == "Mon_1week_lag") {
weekdayString <- "changeOfIncidencelaggedMon"
formula.lm <- "changeOfIncidencelaggedMon ~ outOfHomeDuration + tavg"
} else {
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ outOfHomeDuration + tavg")
weekdayString <- paste0("changeOfIncidencelagged", weekday, "2")
}
DplustavgvsI.lm <- lm(formula = formula.lm, data = joinedDataFrame) #Regression
if (weekday == "Mon") {
title <- "14 Day lag"
} else if (weekday == "Tue") {
title <- "13 Day lag"
} else if (weekday == "Wed") {
title <- "12 Day lag"
} else if (weekday == "Thu") {
title <- "11 Day lag"
} else if (weekday == "Fri") {
title <- "10 Day lag"
} else if (weekday == "Sat") {
title <- "9 Day lag"
} else if (weekday == "Sun") {
title <- "8 Day lag"
} else if (weekday == "Mon_1week_lag") {
title <- "7 Day lag"
}

#1st plot; x = outOfHomeDuration, y = changeOfIncidence, color = tavg
plot22 <- ggplot(data=joinedDataFrame, aes(x=outOfHomeDuration, color = tavg, y = .data[[weekdayString]])) +
geom_point() +
geom_smooth(method = "lm") +
ggtitle(title) +
theme_minimal()

plot23 <- ggplot(data = joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
geom_smooth(aes(x=coefficients(DplustavgvsI.lm)["(Intercept)"] + coefficients(DplustavgvsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DplustavgvsI.lm)["tavg"] * tavg, y = .data[[weekdayString]], color="lightgrey"), method="lm", se=FALSE, size = 2) +
geom_point(aes(x=coefficients(DplustavgvsI.lm)["(Intercept)"] + coefficients(DplustavgvsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DplustavgvsI.lm)["tavg"] * tavg, y = .data[[weekdayString]], fill = tavg), size = 3, shape = 21) +
ggtitle(title) +
xlab("Intercept + a * outOfHomeDuration + b * tavg") +
ylab("changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
theme_minimal() +
scale_color_identity(labels = c("x=y", "Regression line"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank())


nestedplotlist[[paste0("Regression_DplustavgvsI", weekday)]] <- DplustavgvsI.lm
nestedplotlist[[paste0("Plot_DplustavgvsI", weekday)]] <- plot22
nestedplotlist[[paste0("ResvsFittedValues_DplustavgvsI", weekday)]] <- function(){
plot(DplustavgvsI.lm, which=1)
}
nestedplotlist[[paste0("Qqplot_DplustavgvsI", weekday)]] <- function(){
plot(DplustavgvsI.lm, which=2)
}
nestedplotlist[[paste0("ScaleLoc_DplustavgvsI", weekday)]] <- function(){
plot(DplustavgvsI.lm, which=3)
}
nestedplotlist[[paste0("Cooksdist_DplustavgvsI", weekday)]] <- function(){
plot(DplustavgvsI.lm, which=4)
}
nestedplotlist[[paste0("ActualvsEstimation_DplustavgvsI", weekday)]] <- plot23
}

grid.arrange(nestedplotlist[["Plot_DplustavgvsIMon_1week_lag"]],nestedplotlist[["Plot_DplustavgvsISun"]],nestedplotlist[["Plot_DplustavgvsISat"]],nestedplotlist[["Plot_DplustavgvsIFri"]], nestedplotlist[["Plot_DplustavgvsIThu"]], nestedplotlist[["Plot_DplustavgvsIWed"]], nestedplotlist[["Plot_DplustavgvsITue"]], nestedplotlist[["Plot_DplustavgvsIMon"]], nrow=3)
grid.arrange(nestedplotlist[["ActualvsEstimation_DplustavgvsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_DplustavgvsISun"]],nestedplotlist[["ActualvsEstimation_DplustavgvsISat"]],nestedplotlist[["ActualvsEstimation_DplustavgvsIFri"]], nestedplotlist[["ActualvsEstimation_DplustavgvsIThu"]], nestedplotlist[["ActualvsEstimation_DplustavgvsIWed"]], nestedplotlist[["ActualvsEstimation_DplustavgvsITue"]], nestedplotlist[["ActualvsEstimation_DplustavgvsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimation_DplustavgvsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_DplustavgvsISun"]],nestedplotlist[["ActualvsEstimation_DplustavgvsISat"]],nestedplotlist[["ActualvsEstimation_DplustavgvsIFri"]], nestedplotlist[["ActualvsEstimation_DplustavgvsIThu"]], nestedplotlist[["ActualvsEstimation_DplustavgvsIWed"]], nestedplotlist[["ActualvsEstimation_DplustavgvsITue"]], nestedplotlist[["ActualvsEstimation_DplustavgvsIMon"]], nrow=3)
#g <- arrangeGrob(nestedplotlist[["Plot_DplustavgvsIMon_1week_lag"]],nestedplotlist[["Plot_DplustavgvsISun"]],nestedplotlist[["Plot_DplustavgvsISat"]],nestedplotlist[["Plot_DplustavgvsIFri"]], nestedplotlist[["Plot_DplustavgvsIThu"]], nestedplotlist[["Plot_DplustavgvsIWed"]], nestedplotlist[["Plot_DplustavgvsITue"]], nestedplotlist[["Plot_DplustavgvsIMon"]], nrow=3)

# 5a) D*tmax vs I
for (weekday in weekdays){
if (weekday == "Mon_1week_lag") {
weekdayString <- "changeOfIncidencelaggedMon"
formula.lm <- "changeOfIncidencelaggedMon ~ outOfHomeDuration * tmax"
} else {
formula.lm <- paste0("changeOfIncidencelagged", weekday, "2", " ~ outOfHomeDuration*tmax")
weekdayString <- paste0("changeOfIncidencelagged", weekday, "2")
}
DtimestmaxvsI.lm <- lm(formula = formula.lm, data = joinedDataFrame) #Regression
if (weekday == "Mon") {
title <- "14 Day lag"
} else if (weekday == "Tue") {
title <- "13 Day lag"
} else if (weekday == "Wed") {
title <- "12 Day lag"
} else if (weekday == "Thu") {
title <- "11 Day lag"
} else if (weekday == "Fri") {
title <- "10 Day lag"
} else if (weekday == "Sat") {
title <- "9 Day lag"
} else if (weekday == "Sun") {
title <- "8 Day lag"
} else if (weekday == "Mon_1week_lag") {
title <- "7 Day lag"
}
#1st plot; x = outOfHomeDuration, y = changeOfIncidence, color = tmax
plot22 <- ggplot(data=joinedDataFrame, aes(y = .data[[weekdayString]], x = outOfHomeDuration, color = tmax)) +
geom_point() +
geom_smooth(method = "lm") +
ggtitle(title) +
theme_minimal()

plot23 <- ggplot(data = joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
geom_smooth(aes(x=coefficients(DtimestmaxvsI.lm)["(Intercept)"] + coefficients(DtimestmaxvsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DtimestmaxvsI.lm)["tmax"] * tmax + coefficients(DtimestmaxvsI.lm)["outOfHomeDuration:tmax"] * outOfHomeDuration * tmax , y = .data[[weekdayString]], color="lightgrey"), size=2, method = "lm", se=FALSE) +
ggtitle(title) +
xlab("Intercept + a * outOfHomeDuration + b * tmax + c * oOHD * tmax") +
ylab("changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
theme_minimal() +
scale_color_identity(labels = c("x=y", "Regression line"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank()) +
geom_point(aes(x=coefficients(DtimestmaxvsI.lm)["(Intercept)"] + coefficients(DtimestmaxvsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DtimestmaxvsI.lm)["tmax"] * tmax + coefficients(DtimestmaxvsI.lm)["outOfHomeDuration:tmax"] * outOfHomeDuration * tmax , y = .data[[weekdayString]], fill = tmax), size = 3, shape = 21)


nestedplotlist[[paste0("Regression_DtimestmaxvsI", weekday)]] <- DtimestmaxvsI.lm
nestedplotlist[[paste0("Plot_DtimestmaxvsI", weekday)]] <- plot22
nestedplotlist[[paste0("ResvsFittedValues_DtimestmaxvsI", weekday)]] <- function(){
plot(DtimestmaxvsI.lm, which=1)
}
nestedplotlist[[paste0("Qqplot_DtimestmaxvsI", weekday)]] <- function(){
plot(DtimestmaxvsI.lm, which=2)
}
nestedplotlist[[paste0("ScaleLoc_DtimestmaxvsI", weekday)]] <- function(){
plot(DtimestmaxvsI.lm, which=3)
}
nestedplotlist[[paste0("Cooksdist_DtimestmaxvsI", weekday)]] <- function(){
plot(DtimestmaxvsI.lm, which=4)
}
nestedplotlist[[paste0("ActualvsEstimation_DtimestmaxvsI", weekday)]] <- plot23
}

grid.arrange(nestedplotlist[["Plot_DtimestmaxvsIMon_1week_lag"]],nestedplotlist[["Plot_DtimestmaxvsISun"]],nestedplotlist[["Plot_DtimestmaxvsISat"]],nestedplotlist[["Plot_DtimestmaxvsIFri"]], nestedplotlist[["Plot_DtimestmaxvsIThu"]], nestedplotlist[["Plot_DtimestmaxvsIWed"]], nestedplotlist[["Plot_DtimestmaxvsITue"]], nestedplotlist[["Plot_DtimestmaxvsIMon"]], nrow=3)
grid.arrange(nestedplotlist[["ActualvsEstimation_DtimestmaxvsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_DtimestmaxvsISun"]],nestedplotlist[["ActualvsEstimation_DtimestmaxvsISat"]],nestedplotlist[["ActualvsEstimation_DtimestmaxvsIFri"]], nestedplotlist[["ActualvsEstimation_DtimestmaxvsIThu"]], nestedplotlist[["ActualvsEstimation_DtimestmaxvsIWed"]], nestedplotlist[["ActualvsEstimation_DtimestmaxvsITue"]], nestedplotlist[["ActualvsEstimation_DtimestmaxvsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimation_DtimestmaxvsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_DtimestmaxvsISun"]],nestedplotlist[["ActualvsEstimation_DtimestmaxvsISat"]],nestedplotlist[["ActualvsEstimation_DtimestmaxvsIFri"]], nestedplotlist[["ActualvsEstimation_DtimestmaxvsIThu"]], nestedplotlist[["ActualvsEstimation_DtimestmaxvsIWed"]], nestedplotlist[["ActualvsEstimation_DtimestmaxvsITue"]], nestedplotlist[["ActualvsEstimation_DtimestmaxvsIMon"]], nrow=3)
#g <- arrangeGrob(nestedplotlist[["Plot_DtimestmaxvsIMon_1week_lag"]],nestedplotlist[["Plot_DtimestmaxvsISun"]],nestedplotlist[["Plot_DtimestmaxvsISat"]],nestedplotlist[["Plot_DtimestmaxvsIFri"]], nestedplotlist[["Plot_DtimestmaxvsIThu"]], nestedplotlist[["Plot_DtimestmaxvsIWed"]], nestedplotlist[["Plot_DtimestmaxvsITue"]], nestedplotlist[["Plot_DtimestmaxvsIMon"]], nrow=3)

# 5b) D*tavg vs I
for (weekday in weekdays){

if (weekday == "Mon_1week_lag") {
weekdayString <- "changeOfIncidencelaggedMon"
formula.lm <- "changeOfIncidencelaggedMon ~ outOfHomeDuration * tavg"
} else {
formula.lm <- paste0("changeOfIncidencelagged", weekday, "2", " ~ outOfHomeDuration*tavg")
weekdayString <- paste0("changeOfIncidencelagged", weekday, "2")
}
DtimestavgvsI.lm <- lm(formula = formula.lm, data = joinedDataFrame) #Regression
if (weekday == "Mon") {
title <- "14 Day lag"
} else if (weekday == "Tue") {
title <- "13 Day lag"
} else if (weekday == "Wed") {
title <- "12 Day lag"
} else if (weekday == "Thu") {
title <- "11 Day lag"
} else if (weekday == "Fri") {
title <- "10 Day lag"
} else if (weekday == "Sat") {
title <- "9 Day lag"
} else if (weekday == "Sun") {
title <- "8 Day lag"
} else if (weekday == "Mon_1week_lag") {
title <- "7 Day lag"
}
#1st plot; x = outOfHomeDuration, y = changeOfIncidence, color = tavg
plot22 <- ggplot(data=joinedDataFrame, aes(y = .data[[weekdayString]], x = outOfHomeDuration, color = tavg))  +
geom_point() +
geom_smooth(method = "lm") +
ggtitle(title) +
theme_minimal() +
scale_color_identity(labels = c("Regression line", "x=y"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank())


plot23 <- ggplot(data = joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
geom_smooth(aes(x=coefficients(DtimestavgvsI.lm)["(Intercept)"] + coefficients(DtimestavgvsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DtimestavgvsI.lm)["tavg"] * tavg + coefficients(DtimestavgvsI.lm)["outOfHomeDuration:tavg"] * outOfHomeDuration * tavg , y = .data[[weekdayString]], color="lightgrey"), size=2, method = "lm", se=FALSE) +
geom_point(aes(x=coefficients(DtimestavgvsI.lm)["(Intercept)"] + coefficients(DtimestavgvsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DtimestavgvsI.lm)["tavg"] * tavg + coefficients(DtimestavgvsI.lm)["outOfHomeDuration:tavg"] * outOfHomeDuration * tavg , y = .data[[weekdayString]], fill = tavg), shape = 21, size =3) +
ggtitle(title) +
xlab("Intercept + a * outOfHomeDuration + b * tavg + c * oOHD * tavg") +
ylab("changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
theme_minimal() +
scale_color_identity(labels = c("x=y", "Regression line"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank())

nestedplotlist[[paste0("Regression_DtimestavgvsI", weekday)]] <- DtimestavgvsI.lm
nestedplotlist[[paste0("Plot_DtimestavgvsI", weekday)]] <- plot22

nestedplotlist[[paste0("ResvsFittedValues_DtimetavgvsI", weekday)]] <- function(){
plot(DtimestavgvsI.lm, which=1)
}
nestedplotlist[[paste0("Qqplot_DtimestavgvsI", weekday)]] <- function(){
plot(DtimestavgvsI.lm, which=2)
}
nestedplotlist[[paste0("ScaleLoc_DtimestavgvsI", weekday)]] <- function(){
plot(DtimestavgvsI.lm, which=3)
}
nestedplotlist[[paste0("Cooksdist_DtimestavgvsI", weekday)]] <- function(){
plot(DtimestavgvsI.lm, which=4)
}
nestedplotlist[[paste0("ActualvsEstimation_DtimestavgvsI", weekday)]] <- plot23
}

grid.arrange(nestedplotlist[["Plot_DtimestavgvsIMon_1week_lag"]],nestedplotlist[["Plot_DtimestavgvsISun"]],nestedplotlist[["Plot_DtimestavgvsISat"]],nestedplotlist[["Plot_DtimestavgvsIFri"]], nestedplotlist[["Plot_DtimestavgvsIThu"]], nestedplotlist[["Plot_DtimestavgvsIWed"]], nestedplotlist[["Plot_DtimestavgvsITue"]], nestedplotlist[["Plot_DtimestavgvsIMon"]], nrow=3)
grid.arrange(nestedplotlist[["ActualvsEstimation_DtimestavgvsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_DtimestavgvsISun"]],nestedplotlist[["ActualvsEstimation_DtimestavgvsISat"]],nestedplotlist[["ActualvsEstimation_DtimestavgvsIFri"]], nestedplotlist[["ActualvsEstimation_DtimestavgvsIThu"]], nestedplotlist[["ActualvsEstimation_DtimestavgvsIWed"]], nestedplotlist[["ActualvsEstimation_DtimestavgvsITue"]], nestedplotlist[["ActualvsEstimation_DtimestavgvsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimation_DtimestavgvsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_DtimestavgvsISun"]],nestedplotlist[["ActualvsEstimation_DtimestavgvsISat"]],nestedplotlist[["ActualvsEstimation_DtimestavgvsIFri"]], nestedplotlist[["ActualvsEstimation_DtimestavgvsIThu"]], nestedplotlist[["ActualvsEstimation_DtimestavgvsIWed"]], nestedplotlist[["ActualvsEstimation_DtimestavgvsITue"]], nestedplotlist[["ActualvsEstimation_DtimestavgvsIMon"]], nrow=3)
#g <- arrangeGrob(nestedplotlist[["Plot_DtimestavgvsIMon_1week_lag"]],nestedplotlist[["Plot_DtimestavgvsISun"]],nestedplotlist[["Plot_DtimestavgvsISat"]],nestedplotlist[["Plot_DtimestavgvsIFri"]], nestedplotlist[["Plot_DtimestavgvsIThu"]], nestedplotlist[["Plot_DtimestavgvsIWed"]], nestedplotlist[["Plot_DtimestavgvsITue"]], nestedplotlist[["Plot_DtimestavgvsIMon"]], nrow=3)

# 6a) D + out vs I
for (weekday in weekdays) {
if (weekday == "Mon_1week_lag") {
weekdayString <- "changeOfIncidencelaggedMon"
formula.lm <- "changeOfIncidencelaggedMon ~ outOfHomeDuration + outdoorFraction"
} else {
formula.lm <- paste0("changeOfIncidencelagged", weekday, "2", " ~ outOfHomeDuration + outdoorFraction")
weekdayString <- paste0("changeOfIncidencelagged", weekday, "2")
}
DplusoutvsI.lm <- lm(formula = formula.lm, data = joinedDataFrame) #Regression
if (weekday == "Mon") {
title <- "14 Day lag"
} else if (weekday == "Tue") {
title <- "13 Day lag"
} else if (weekday == "Wed") {
title <- "12 Day lag"
} else if (weekday == "Thu") {
title <- "11 Day lag"
} else if (weekday == "Fri") {
title <- "10 Day lag"
} else if (weekday == "Sat") {
title <- "9 Day lag"
} else if (weekday == "Sun") {
title <- "8 Day lag"
} else if (weekday == "Mon_1week_lag") {
title <- "7 Day lag"
}
#1st plot; x = outOfHomeDuration, y = changeOfIncidence, color = outdoorFraction
plot22 <- ggplot(data=joinedDataFrame, aes(x=outOfHomeDuration, color =outdoorFraction, y = .data[[weekdayString]])) +
geom_point() +
geom_smooth(method = "lm") +
ggtitle(title) +
theme_minimal()

plot23 <- ggplot(data=joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
geom_smooth(aes(x=coefficients(DplusoutvsI.lm )["(Intercept)"] + coefficients(DplusoutvsI.lm )["outOfHomeDuration"] * outOfHomeDuration + coefficients(DplusoutvsI.lm )["outdoorFraction"] * outdoorFraction, y = .data[[weekdayString]], color="lightgrey"), size=2, method = "lm", se=FALSE) +
geom_point(aes(x=coefficients(DplusoutvsI.lm )["(Intercept)"] + coefficients(DplusoutvsI.lm )["outOfHomeDuration"] * outOfHomeDuration + coefficients(DplusoutvsI.lm )["outdoorFraction"] * outdoorFraction , y = .data[[weekdayString]], fill = tavg), shape = 21, size =3) +
ggtitle(title) +
xlab("Intercept + a * outOfHomeDuration + b * outdoorFraction") +
ylab("changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
theme_minimal() +
scale_color_identity(labels = c("x=y", "Regression line"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank())


nestedplotlist[[paste0("Regression_DplusoutvsI", weekday)]] <- DplusoutvsI.lm
nestedplotlist[[paste0("Plot_DplusoutvsI", weekday)]] <- plot22
nestedplotlist[[paste0("ResvsFittedValues_DplusoutvsI", weekday)]] <- function(){
plot(DplusoutvsI.lm, which=1)
}
nestedplotlist[[paste0("Qqplot_DplusoutvsI", weekday)]] <- function(){
plot(DplusoutvsI.lm, which=2)
}
nestedplotlist[[paste0("ScaleLoc_DplusoutvsI", weekday)]] <- function(){
plot(DplusoutvsI.lm, which=3)
}
nestedplotlist[[paste0("Cooksdist_DplusoutvsI", weekday)]] <- function(){
plot(DplusoutvsI.lm, which=4)
}
nestedplotlist[[paste0("ActualvsEstimation_DplusoutvsI", weekday)]] <- plot23
}

grid.arrange(nestedplotlist[["Plot_DplusoutvsIMon_1week_lag"]],nestedplotlist[["Plot_DplusoutvsISun"]],nestedplotlist[["Plot_DplusoutvsISat"]],nestedplotlist[["Plot_DplusoutvsIFri"]], nestedplotlist[["Plot_DplusoutvsIThu"]], nestedplotlist[["Plot_DplusoutvsIWed"]], nestedplotlist[["Plot_DplusoutvsITue"]], nestedplotlist[["Plot_DplusoutvsIMon"]], nrow=3)
grid.arrange(nestedplotlist[["ActualvsEstimation_DplusoutvsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_DplusoutvsISun"]],nestedplotlist[["ActualvsEstimation_DplusoutvsISat"]],nestedplotlist[["ActualvsEstimation_DplusoutvsIFri"]], nestedplotlist[["ActualvsEstimation_DplusoutvsIThu"]], nestedplotlist[["ActualvsEstimation_DplusoutvsIWed"]], nestedplotlist[["ActualvsEstimation_DplusoutvsITue"]], nestedplotlist[["ActualvsEstimation_DplusoutvsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimation_DplusoutvsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_DplusoutvsISun"]],nestedplotlist[["ActualvsEstimation_DplusoutvsISat"]],nestedplotlist[["ActualvsEstimation_DplusoutvsIFri"]], nestedplotlist[["ActualvsEstimation_DplusoutvsIThu"]], nestedplotlist[["ActualvsEstimation_DplusoutvsIWed"]], nestedplotlist[["ActualvsEstimation_DplusoutvsITue"]], nestedplotlist[["ActualvsEstimation_DplusoutvsIMon"]], nrow=3)
#g <- arrangeGrob(nestedplotlist[["Plot_DplusoutvsIMon_1week_lag"]],nestedplotlist[["Plot_DplusoutvsISun"]],nestedplotlist[["Plot_DplusoutvsISat"]],nestedplotlist[["Plot_DplusoutvsIFri"]], nestedplotlist[["Plot_DplusoutvsIThu"]], nestedplotlist[["Plot_DplusoutvsIWed"]], nestedplotlist[["Plot_DplusoutvsITue"]], nestedplotlist[["Plot_DplusoutvsIMon"]], nrow=3)

# 6b) D + out2 vs I
for (weekday in weekdays) {
if (weekday == "Mon_1week_lag") {
weekdayString <- "changeOfIncidencelaggedMon"
formula.lm <- "changeOfIncidencelaggedMon ~ outOfHomeDuration + outdoorFraction2"
} else {
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ outOfHomeDuration + outdoorFraction2")
weekdayString <- paste0("changeOfIncidencelagged", weekday, "2")
}
Dplusout2vsI.lm <- lm(formula = formula.lm, data = joinedDataFrame) #Regression
if (weekday == "Mon") {
title <- "14 Day lag"
} else if (weekday == "Tue") {
title <- "13 Day lag"
} else if (weekday == "Wed") {
title <- "12 Day lag"
} else if (weekday == "Thu") {
title <- "11 Day lag"
} else if (weekday == "Fri") {
title <- "10 Day lag"
} else if (weekday == "Sat") {
title <- "9 Day lag"
} else if (weekday == "Sun") {
title <- "8 Day lag"
} else if (weekday == "Mon_1week_lag") {
title <- "7 Day lag"
}
#1st plot; x = outOfHomeDuration, y = changeOfIncidence, color = outdoorFraction2
plot22 <- ggplot(data=joinedDataFrame, aes(x=outOfHomeDuration, color =outdoorFraction2, y = .data[[weekdayString]])) +
geom_point() +
geom_smooth(method = "lm") +
ggtitle(title) +
theme_minimal()

plot23 <- ggplot(data=joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
geom_smooth(aes(x=coefficients(Dplusout2vsI.lm )["(Intercept)"] + coefficients(Dplusout2vsI.lm )["outOfHomeDuration"] * outOfHomeDuration + coefficients(Dplusout2vsI.lm )["outdoorFraction2"] * outdoorFraction2 , y = .data[[weekdayString]], color="lightgrey"), size=2, method = "lm", se=FALSE) +
geom_point(aes(x=coefficients(Dplusout2vsI.lm )["(Intercept)"] + coefficients(Dplusout2vsI.lm )["outOfHomeDuration"] * outOfHomeDuration + coefficients(Dplusout2vsI.lm )["outdoorFraction2"] * outdoorFraction2 , y = .data[[weekdayString]], fill = tavg), size = 3, shape = 21) +
ggtitle(title) +
xlab("Intercept + a * outOfHomeDuration + b * outdoorFraction2") +
ylab("changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
theme_minimal() +
scale_color_identity(labels = c("x=y","Regression line"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank())


nestedplotlist[[paste0("Regression_Dplusout2vsI", weekday)]] <- Dplusout2vsI.lm
nestedplotlist[[paste0("Plot_Dplusout2vsI", weekday)]] <- plot22
nestedplotlist[[paste0("ResvsFittedValues_Dplusout2vsI", weekday)]] <- function(){
plot(Dplusout2vsI.lm, which=1)
}
nestedplotlist[[paste0("Qqplot_Dplusout2vsI", weekday)]] <- function(){
plot(Dplusout2vsI.lm, which=2)
}
nestedplotlist[[paste0("ScaleLoc_Dplusout2vsI", weekday)]] <- function(){
plot(Dplusout2vsI.lm, which=3)
}
nestedplotlist[[paste0("Cooksdist_Dplusout2vsI", weekday)]] <- function(){
plot(Dplusout2vsI.lm, which=4)
}
nestedplotlist[[paste0("ActualvsEstimation_Dplusout2vsI", weekday)]] <- plot23
}

grid.arrange(nestedplotlist[["ActualvsEstimation_Dplusout2vsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_Dplusout2vsISun"]],nestedplotlist[["ActualvsEstimation_Dplusout2vsISat"]],nestedplotlist[["ActualvsEstimation_Dplusout2vsIFri"]], nestedplotlist[["ActualvsEstimation_Dplusout2vsIThu"]], nestedplotlist[["ActualvsEstimation_Dplusout2vsIWed"]], nestedplotlist[["ActualvsEstimation_Dplusout2vsITue"]], nestedplotlist[["ActualvsEstimation_Dplusout2vsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimation_Dplusout2vsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_Dplusout2vsISun"]],nestedplotlist[["ActualvsEstimation_Dplusout2vsISat"]],nestedplotlist[["ActualvsEstimation_Dplusout2vsIFri"]], nestedplotlist[["ActualvsEstimation_Dplusout2vsIThu"]], nestedplotlist[["ActualvsEstimation_Dplusout2vsIWed"]], nestedplotlist[["ActualvsEstimation_Dplusout2vsITue"]], nestedplotlist[["ActualvsEstimation_Dplusout2vsIMon"]], nrow=3)
#g <- arrangeGrob(nestedplotlist[["Plot_Dplusout2vsIMon_1week_lag"]],nestedplotlist[["Plot_Dplusout2vsISun"]],nestedplotlist[["Plot_Dplusout2vsISat"]],nestedplotlist[["Plot_Dplusout2vsIFri"]], nestedplotlist[["Plot_Dplusout2vsIThu"]], nestedplotlist[["Plot_Dplusout2vsIWed"]], nestedplotlist[["Plot_Dplusout2vsITue"]], nestedplotlist[["Plot_Dplusout2vsIMon"]], nrow=3)

# 7a) D * out vs I
for (weekday in weekdays) {
if (weekday == "Mon_1week_lag") {
weekdayString <- "changeOfIncidencelaggedMon"
formula.lm <- "changeOfIncidencelaggedMon ~ outOfHomeDuration * outdoorFraction"
} else {
formula.lm <- paste0("changeOfIncidencelagged", weekday, "2", " ~ outOfHomeDuration * outdoorFraction")
weekdayString <- paste0("changeOfIncidencelagged", weekday, "2")
}
DtimesoutvsI.lm <- lm(formula = formula.lm, data = joinedDataFrame) #Regression
if (weekday == "Mon") {
title <- "14 Day lag"
} else if (weekday == "Tue") {
title <- "13 Day lag"
} else if (weekday == "Wed") {
title <- "12 Day lag"
} else if (weekday == "Thu") {
title <- "11 Day lag"
} else if (weekday == "Fri") {
title <- "10 Day lag"
} else if (weekday == "Sat") {
title <- "9 Day lag"
} else if (weekday == "Sun") {
title <- "8 Day lag"
} else if (weekday == "Mon_1week_lag") {
title <- "7 Day lag"
}
#1st plot; x = outOfHomeDuration, y = changeOfIncidence, color = outdoorFraction
plot22 <- ggplot(data=joinedDataFrame, aes(x=outOfHomeDuration, color =outdoorFraction, y = .data[[weekdayString]])) +
geom_point() +
geom_smooth(method = "lm") +
ggtitle(title) +
theme_minimal()

plot23 <- ggplot(data = joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
geom_smooth(aes(x=coefficients(DtimesoutvsI.lm)["(Intercept)"] + coefficients(DtimesoutvsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DtimesoutvsI.lm)["outdoorFraction"] * outdoorFraction + coefficients(DtimesoutvsI.lm)["outOfHomeDuration:outdoorFraction"] * outOfHomeDuration * outdoorFraction, y = .data[[weekdayString]], color="lightgrey"), size=2, method = "lm", se=FALSE) +
geom_point(aes(x=coefficients(DtimesoutvsI.lm)["(Intercept)"] + coefficients(DtimesoutvsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DtimesoutvsI.lm)["outdoorFraction"] * outdoorFraction + coefficients(DtimesoutvsI.lm)["outOfHomeDuration:outdoorFraction"] * outOfHomeDuration * outdoorFraction, y = .data[[weekdayString]], fill = tavg), shape = 21, size = 3) +
ggtitle(title) +
xlab("Intercept + a*outOfHomeDuration + b*outdoorFraction + c*oOHD*oF") +
ylab("changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
theme_minimal() +
scale_color_identity(labels = c("x=y", "Regression line"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank())

nestedplotlist[[paste0("Regression_DtimesoutvsI", weekday)]] <- DtimesoutvsI.lm
nestedplotlist[[paste0("Plot_DtimesoutvsI", weekday)]] <- plot22

nestedplotlist[[paste0("ResvsFittedValues_DtimesoutvsI", weekday)]] <- function(){
plot(DtimesoutvsI.lm, which=1)
}
nestedplotlist[[paste0("Qqplot_DtimesoutvsI", weekday)]] <- function(){
plot(DtimesoutvsI.lm, which=2)
}
nestedplotlist[[paste0("ScaleLoc_DtimesoutvsI", weekday)]] <- function(){
plot(DtimesoutvsI.lm, which=3)
}
nestedplotlist[[paste0("Cooksdist_DtimesoutvsI", weekday)]] <- function(){
plot(DtimesoutvsI.lm, which=4)
}
nestedplotlist[[paste0("ActualvsEstimation_DtimesoutvsI", weekday)]] <- plot23
}

grid.arrange(nestedplotlist[["Plot_DtimesoutvsIMon_1week_lag"]], nestedplotlist[["Plot_DtimesoutvsISun"]],nestedplotlist[["Plot_DtimesoutvsISat"]],nestedplotlist[["Plot_DtimesoutvsIFri"]], nestedplotlist[["Plot_DtimesoutvsIThu"]], nestedplotlist[["Plot_DtimesoutvsIWed"]], nestedplotlist[["Plot_DtimesoutvsITue"]], nestedplotlist[["Plot_DtimesoutvsIMon"]], nrow=3)
grid.arrange(nestedplotlist[["ActualvsEstimation_DtimesoutvsIMon_1week_lag"]], nestedplotlist[["ActualvsEstimation_DtimesoutvsISun"]],nestedplotlist[["ActualvsEstimation_DtimesoutvsISat"]],nestedplotlist[["ActualvsEstimation_DtimesoutvsIFri"]], nestedplotlist[["ActualvsEstimation_DtimesoutvsIThu"]], nestedplotlist[["ActualvsEstimation_DtimesoutvsIWed"]], nestedplotlist[["ActualvsEstimation_DtimesoutvsITue"]], nestedplotlist[["ActualvsEstimation_DtimesoutvsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimation_DtimesoutvsIMon_1week_lag"]], nestedplotlist[["ActualvsEstimation_DtimesoutvsISun"]],nestedplotlist[["ActualvsEstimation_DtimesoutvsISat"]],nestedplotlist[["ActualvsEstimation_DtimesoutvsIFri"]], nestedplotlist[["ActualvsEstimation_DtimesoutvsIThu"]], nestedplotlist[["ActualvsEstimation_DtimesoutvsIWed"]], nestedplotlist[["ActualvsEstimation_DtimesoutvsITue"]], nestedplotlist[["ActualvsEstimation_DtimesoutvsIMon"]], nrow=3)
#g <- arrangeGrob(nestedplotlist[["Plot_DtimesoutvsIMon_1week_lag"]], nestedplotlist[["Plot_DtimesoutvsISun"]],nestedplotlist[["Plot_DtimesoutvsISat"]],nestedplotlist[["Plot_DtimesoutvsIFri"]], nestedplotlist[["Plot_DtimesoutvsIThu"]], nestedplotlist[["Plot_DtimesoutvsIWed"]], nestedplotlist[["Plot_DtimesoutvsITue"]], nestedplotlist[["Plot_DtimesoutvsIMon"]], nrow=3)

# 7b) D * out2 vs I
for (weekday in weekdays){
if (weekday == "Mon_1week_lag") {
weekdayString <- "changeOfIncidencelaggedMon"
formula.lm <- "changeOfIncidencelaggedMon ~ outOfHomeDuration * outdoorFraction2"
} else {
formula.lm <- paste0("changeOfIncidencelagged", weekday, "2", " ~ outOfHomeDuration * outdoorFraction2")
weekdayString <- paste0("changeOfIncidencelagged", weekday, "2")
}
Dtimesout2vsI.lm <- lm(formula = formula.lm, data = joinedDataFrame) #Regression
if (weekday == "Mon") {
title <- "14 Day lag"
} else if (weekday == "Tue") {
title <- "13 Day lag"
} else if (weekday == "Wed") {
title <- "12 Day lag"
} else if (weekday == "Thu") {
title <- "11 Day lag"
} else if (weekday == "Fri") {
title <- "10 Day lag"
} else if (weekday == "Sat") {
title <- "9 Day lag"
} else if (weekday == "Sun") {
title <- "8 Day lag"
} else if (weekday == "Mon_1week_lag") {
title <- "7 Day lag"
}
#1st plot; x = outOfHomeDuration, y = changeOfIncidence, color = outdoorFraction2
plot22 <- ggplot(data=joinedDataFrame, aes(x=outOfHomeDuration, color = outdoorFraction2, y = .data[[weekdayString]])) +
geom_point() +
geom_smooth(method = "lm") +
ggtitle(title) +
theme_minimal()

plot23 <- ggplot(data = joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
geom_smooth(aes(x=coefficients(Dtimesout2vsI.lm)["(Intercept)"] + coefficients(Dtimesout2vsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(Dtimesout2vsI.lm)["outdoorFraction2"] * outdoorFraction2 + coefficients(Dtimesout2vsI.lm)["outOfHomeDuration:outdoorFraction2"] * outOfHomeDuration * outdoorFraction2, y = .data[[weekdayString]], color="lightgrey"), size=2, method = "lm", se=FALSE) +
geom_point(aes(x=coefficients(Dtimesout2vsI.lm)["(Intercept)"] + coefficients(Dtimesout2vsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(Dtimesout2vsI.lm)["outdoorFraction2"] * outdoorFraction2 + coefficients(Dtimesout2vsI.lm)["outOfHomeDuration:outdoorFraction2"] * outOfHomeDuration * outdoorFraction2, y = .data[[weekdayString]], fill = tavg), shape = 21, size =3) +
ggtitle(title) +
xlab("Intercept + a * outOfHomeDuration + b * outdoorFraction2 + c * oOHD * oF2") +
ylab("changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
theme_minimal() +
scale_color_identity(labels = c("x=y", "Regression line"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank())


nestedplotlist[[paste0("Regression_Dtimesout2vsI", weekday)]] <- Dtimesout2vsI.lm
nestedplotlist[[paste0("Plot_Dtimesout2vsI", weekday)]] <- plot22
nestedplotlist[[paste0("ResvsFittedValues_Dtimesout2vsI", weekday)]] <- function(){
plot(Dtimesout2vsI.lm, which=1)
}
nestedplotlist[[paste0("Qqplot_Dtimesout2vsI", weekday)]] <- function(){
plot(Dtimesout2vsI.lm, which=2)
}
nestedplotlist[[paste0("ScaleLoc_Dtimesout2vsI", weekday)]] <- function(){
plot(Dtimesout2vsI.lm, which=3)
}
nestedplotlist[[paste0("Cooksdist_Dtimesout2vsI", weekday)]] <- function(){
plot(Dtimesout2vsI.lm, which=4)
}
nestedplotlist[[paste0("ActualvsEstimation_Dtimesout2vsI", weekday)]] <- plot23
}

grid.arrange(nestedplotlist[["Plot_Dtimesout2vsIMon_1week_lag"]],nestedplotlist[["Plot_Dtimesout2vsISun"]],nestedplotlist[["Plot_Dtimesout2vsISat"]],nestedplotlist[["Plot_Dtimesout2vsIFri"]], nestedplotlist[["Plot_Dtimesout2vsIThu"]], nestedplotlist[["Plot_Dtimesout2vsIWed"]], nestedplotlist[["Plot_Dtimesout2vsITue"]], nestedplotlist[["Plot_Dtimesout2vsIMon"]], nrow=3)
grid.arrange(nestedplotlist[["ActualvsEstimation_Dtimesout2vsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_Dtimesout2vsISun"]],nestedplotlist[["ActualvsEstimation_Dtimesout2vsISat"]],nestedplotlist[["ActualvsEstimation_Dtimesout2vsIFri"]], nestedplotlist[["ActualvsEstimation_Dtimesout2vsIThu"]], nestedplotlist[["ActualvsEstimation_Dtimesout2vsIWed"]], nestedplotlist[["ActualvsEstimation_Dtimesout2vsITue"]], nestedplotlist[["ActualvsEstimation_Dtimesout2vsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimation_Dtimesout2vsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_Dtimesout2vsISun"]],nestedplotlist[["ActualvsEstimation_Dtimesout2vsISat"]],nestedplotlist[["ActualvsEstimation_Dtimesout2vsIFri"]], nestedplotlist[["ActualvsEstimation_Dtimesout2vsIThu"]], nestedplotlist[["ActualvsEstimation_Dtimesout2vsIWed"]], nestedplotlist[["ActualvsEstimation_Dtimesout2vsITue"]], nestedplotlist[["ActualvsEstimation_Dtimesout2vsIMon"]], nrow=3)
#g <- arrangeGrob(nestedplotlist[["Plot_Dtimesout2vsIMon_1week_lag"]],nestedplotlist[["Plot_Dtimesout2vsISun"]],nestedplotlist[["Plot_Dtimesout2vsISat"]],nestedplotlist[["Plot_Dtimesout2vsIFri"]], nestedplotlist[["Plot_Dtimesout2vsIThu"]], nestedplotlist[["Plot_Dtimesout2vsIWed"]], nestedplotlist[["Plot_Dtimesout2vsITue"]], nestedplotlist[["Plot_Dtimesout2vsIMon"]], nrow=3)

# 8) D + prcp
for(weekday in weekdays){
if(weekday == "Mon_1week_lag") {
weekdayString <- "changeOfIncidencelaggedMon"
formula.lm <- "changeOfIncidencelaggedMon ~ outOfHomeDuration + prcp"
} else {
formula.lm <- paste0("changeOfIncidencelagged", weekday, "2", " ~ outOfHomeDuration + prcp")
weekdayString <- paste0("changeOfIncidencelagged", weekday, "2")
}
DplusprcpvsI.lm <- lm(formula = formula.lm, data=joinedDataFrame) #Regression
if(weekday == "Mon") {
title <- "14 Day lag"
} else if (weekday == "Tue") {
title <- "13 Day lag"
} else if (weekday == "Wed") {
title <- "12 Day lag"
} else if (weekday == "Thu") {
title <- "11 Day lag"
} else if (weekday == "Fri") {
title <- "10 Day lag"
} else if (weekday == "Sat") {
title <- "9 Day lag"
} else if (weekday == "Sun") {
title <- "8 Day lag"
} else if(weekday == "Mon_1week_lag"){
title <- "7 Day lag"
}
#1st plot; x = outOfHomeDuration, y = changeOfIncidence, color = prcp
plot22 <- ggplot(data=joinedDataFrame, aes(x=outOfHomeDuration, color = prcp, y = .data[[weekdayString]])) +
geom_point() +
geom_smooth(method = "lm") +
ggtitle(title) +
theme_minimal()

plot23 <- ggplot(data=joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
geom_smooth(aes(x=coefficients(DplusprcpvsI.lm)["(Intercept)"] + coefficients(DplusprcpvsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DplusprcpvsI.lm)["prcp"] * prcp, y = .data[[weekdayString]], color="lightgrey"), size=2, method = "lm", se=FALSE) +
geom_point(aes(x=coefficients(DplusprcpvsI.lm)["(Intercept)"] + coefficients(DplusprcpvsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DplusprcpvsI.lm)["prcp"] * prcp, y = .data[[weekdayString]], fill = tavg), shape = 21, size = 3) +
ggtitle(title) +
xlab("Intercept + a * outOfHomeDuration + b * prcp") +
ylab("changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
theme_minimal() +
scale_color_identity(labels = c("x=y", "Regression line"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank())


nestedplotlist[[paste0("Regression_DplusprcpvsI", weekday)]] <- DplusprcpvsI.lm
nestedplotlist[[paste0("Plot_DplusprcpvsI", weekday)]] <- plot22
nestedplotlist[[paste0("ResvsFittedValues_DplusprcpvsI", weekday)]] <- function(){
plot(DplusprcpvsI.lm, which=1)
}
nestedplotlist[[paste0("Qqplot_DplusprcpvsI", weekday)]] <- function(){
plot(DplusprcpvsI.lm, which=2)
}
nestedplotlist[[paste0("ScaleLoc_DplusprcpvsI", weekday)]] <- function(){
plot(DplusprcpvsI.lm, which=3)
}
nestedplotlist[[paste0("Cooksdist_DplusprcpvsI", weekday)]] <- function(){
plot(DplusprcpvsI.lm, which=4)
}
nestedplotlist[[paste0("ActualvsEstimation_DplusprcpvsI", weekday)]] <- plot23
}

grid.arrange(nestedplotlist[["Plot_DplusprcpvsIMon_1week_lag"]],nestedplotlist[["Plot_DplusprcpvsISun"]],nestedplotlist[["Plot_DplusprcpvsISat"]],nestedplotlist[["Plot_DplusprcpvsIFri"]], nestedplotlist[["Plot_DplusprcpvsIThu"]], nestedplotlist[["Plot_DplusprcpvsIWed"]], nestedplotlist[["Plot_DplusprcpvsITue"]], nestedplotlist[["Plot_DplusprcpvsIMon"]], nrow=3)
grid.arrange(nestedplotlist[["ActualvsEstimation_DplusprcpvsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_DplusprcpvsISun"]],nestedplotlist[["ActualvsEstimation_DplusprcpvsISat"]],nestedplotlist[["ActualvsEstimation_DplusprcpvsIFri"]], nestedplotlist[["ActualvsEstimation_DplusprcpvsIThu"]], nestedplotlist[["ActualvsEstimation_DplusprcpvsIWed"]], nestedplotlist[["ActualvsEstimation_DplusprcpvsITue"]], nestedplotlist[["ActualvsEstimation_DplusprcpvsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimation_DplusprcpvsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_DplusprcpvsISun"]],nestedplotlist[["ActualvsEstimation_DplusprcpvsISat"]],nestedplotlist[["ActualvsEstimation_DplusprcpvsIFri"]], nestedplotlist[["ActualvsEstimation_DplusprcpvsIThu"]], nestedplotlist[["ActualvsEstimation_DplusprcpvsIWed"]], nestedplotlist[["ActualvsEstimation_DplusprcpvsITue"]], nestedplotlist[["ActualvsEstimation_DplusprcpvsIMon"]], nrow=3)
#g <- arrangeGrob(nestedplotlist[["Plot_DplusprcpvsIMon_1week_lag"]],nestedplotlist[["Plot_DplusprcpvsISun"]],nestedplotlist[["Plot_DplusprcpvsISat"]],nestedplotlist[["Plot_DplusprcpvsIFri"]], nestedplotlist[["Plot_DplusprcpvsIThu"]], nestedplotlist[["Plot_DplusprcpvsIWed"]], nestedplotlist[["Plot_DplusprcpvsITue"]], nestedplotlist[["Plot_DplusprcpvsIMon"]], nrow=3)


# 9a) D + tmax + prcp
joinedDataFrame <- joinedDataFrame %>% mutate(prcpRound = round(prcp))
for(weekday in weekdays){
if(weekday == "Mon_1week_lag"){
weekdayString <- "changeOfIncidencelaggedMon"
formula.lm <- "changeOfIncidencelaggedMon ~ outOfHomeDuration + tmax + prcpRound" 
} else {
formula.lm <- paste0("changeOfIncidencelagged", weekday, "2", " ~ outOfHomeDuration + tmax + prcpRound")
weekdayString <- paste0("changeOfIncidencelagged", weekday, "2")
}
DplustmaxplusprcpvsI.lm <- lm(formula = formula.lm, data=joinedDataFrame) #Regression
if(weekday == "Mon") {
title <- "14 Day lag" 
} else if (weekday == "Tue") {
title <- "13 Day lag" 
} else if (weekday == "Wed") {
title <- "12 Day lag"
} else if (weekday == "Thu") {
title <- "11 Day lag"
} else if (weekday == "Fri") {
title <- "10 Day lag"
} else if (weekday == "Sat") {
title <- "9 Day lag"
} else if (weekday == "Sun") {
title <- "8 Day lag"
} else if (weekday == "Mon_1week_lag") {
title <- "7 Day lag"   
}
plot22 <- ggPredict(DplustmaxplusprcpvsI.lm, interactive=TRUE)

plot23 <- ggplot(data=joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
geom_smooth(aes(x=coefficients(DplustmaxplusprcpvsI.lm)["(Intercept)"] + coefficients(DplustmaxplusprcpvsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DplustmaxplusprcpvsI.lm)["tmax"] * tmax + coefficients(DplustmaxplusprcpvsI.lm)["prcpRound"] * prcpRound, y = .data[[weekdayString]], color="lightgrey"), size=2, method = "lm", se=FALSE) +
geom_point(aes(x=coefficients(DplustmaxplusprcpvsI.lm)["(Intercept)"] + coefficients(DplustmaxplusprcpvsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DplustmaxplusprcpvsI.lm)["tmax"] * tmax + coefficients(DplustmaxplusprcpvsI.lm)["prcpRound"] * prcpRound, y = .data[[weekdayString]], fill = tavg), shape = 21, size = 3) +
ggtitle(title) +
xlab("Intercept + a * outOfHomeDuration + b * tmax + c * prcp") +
ylab("changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
theme_minimal() +
scale_color_identity(labels = c("x=y", "Regression line"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank())


nestedplotlist[[paste0("Regression_DplustmaxplusprcpvsI", weekday)]] <- DplustmaxplusprcpvsI.lm
nestedplotlist[[paste0("Plot_DplustmaxplusprcpvsI", weekday)]] <- plot22
nestedplotlist[[paste0("ResvsFittedValues_DplustmaxplusprcpvsI", weekday)]] <- function(){
plot(DplustmaxplusprcpvsI.lm, which=1)
}
nestedplotlist[[paste0("Qqplot_DplustmaxplusprcpvsI", weekday)]] <- function(){
plot(DplustmaxplusprcpvsI.lm, which=2)
}
nestedplotlist[[paste0("ScaleLoc_DplustmaxplusprcpvsI", weekday)]] <- function(){
plot(DplustmaxplusprcpvsI.lm, which=3)
}
nestedplotlist[[paste0("Cooksdist_DplustmaxplusprcpvsI", weekday)]] <- function(){
plot(DplustmaxplusprcpvsI.lm, which=4)
}
nestedplotlist[[paste0("ActualvsEstimation_DplustmaxplusprcpvsI", weekday)]] <- plot23
}

#grid.arrange(nestedplotlist[["Plot_DplustmaxplusprcpvsIMon_1week_lag"]],nestedplotlist[["Plot_DplustmaxplusprcpvsISun"]],nestedplotlist[["Plot_DplustmaxplusprcpvsISat"]],nestedplotlist[["Plot_DplustmaxplusprcpvsIFri"]], nestedplotlist[["Plot_DplustmaxplusprcpvsIThu"]], nestedplotlist[["Plot_DplustmaxplusprcpvsIWed"]], nestedplotlist[["Plot_DplustmaxplusprcpvsITue"]], nestedplotlist[["Plot_DplustmaxplusprcpvsIMon"]], nrow=3)
grid.arrange(nestedplotlist[["ActualvsEstimation_DplustmaxplusprcpvsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_DplustmaxplusprcpvsISun"]],nestedplotlist[["ActualvsEstimation_DplustmaxplusprcpvsISat"]],nestedplotlist[["ActualvsEstimation_DplustmaxplusprcpvsIFri"]], nestedplotlist[["ActualvsEstimation_DplustmaxplusprcpvsIThu"]], nestedplotlist[["ActualvsEstimation_DplustmaxplusprcpvsIWed"]], nestedplotlist[["ActualvsEstimation_DplustmaxplusprcpvsITue"]], nestedplotlist[["ActualvsEstimation_DplustmaxplusprcpvsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimation_DplustmaxplusprcpvsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_DplustmaxplusprcpvsISun"]],nestedplotlist[["ActualvsEstimation_DplustmaxplusprcpvsISat"]],nestedplotlist[["ActualvsEstimation_DplustmaxplusprcpvsIFri"]], nestedplotlist[["ActualvsEstimation_DplustmaxplusprcpvsIThu"]], nestedplotlist[["ActualvsEstimation_DplustmaxplusprcpvsIWed"]], nestedplotlist[["ActualvsEstimation_DplustmaxplusprcpvsITue"]], nestedplotlist[["ActualvsEstimation_DplustmaxplusprcpvsIMon"]], nrow=3)
#g <- arrangeGrob(nestedplotlist[["Plot_DplustmaxplusprcpvsIMon_1week_lag"]],nestedplotlist[["Plot_DplustmaxplusprcpvsISun"]],nestedplotlist[["Plot_DplustmaxplusprcpvsISat"]],nestedplotlist[["Plot_DplustmaxplusprcpvsIFri"]], nestedplotlist[["Plot_DplustmaxplusprcpvsIThu"]], nestedplotlist[["Plot_DplustmaxplusprcpvsIWed"]], nestedplotlist[["Plot_DplustmaxplusprcpvsITue"]], nestedplotlist[["Plot_DplustmaxplusprcpvsIMon"]], nrow=3)

# 9b) D + out + prcp
for(weekday in weekdays){
if(weekday == "Mon_1week_lag"){
weekdayString <- "changeOfIncidencelaggedMon"
formula.lm <- "changeOfIncidencelaggedMon ~ outOfHomeDuration + outdoorFraction + prcpRound" 
} else {
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ outOfHomeDuration + outdoorFraction + prcpRound")
weekdayString <- paste0("changeOfIncidencelagged",weekday,"2")
}
DplusoutplusprcpvsI.lm <- lm(formula = formula.lm, data=joinedDataFrame) #Regression
if(weekday == "Mon") {
title <- "14 Day lag" 
} else if(weekday=="Tue") {
title <- "13 Day lag" 
} else if(weekday=="Wed") {
title <- "12 Day lag"
} else if(weekday=="Thu") {
title <- "11 Day lag"
} else if(weekday=="Fri") {
title <- "10 Day lag"
} else if(weekday=="Sat") {
title <- "9 Day lag"
} else if(weekday == "Sun") {
title <- "8 Day lag"
} else if(weekday == "Mon_1week_lag") {
title <- "7 Day lag"   
}
plot22 <- ggPredict(DplustmaxplusprcpvsI.lm, interactive=TRUE)

plot23 <- ggplot(data=joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
geom_smooth(aes(x=coefficients(DplusoutplusprcpvsI.lm)["(Intercept)"] + coefficients(DplusoutplusprcpvsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DplusoutplusprcpvsI.lm)["outdoorFraction"] * outdoorFraction + coefficients(DplusoutplusprcpvsI.lm)["prcpRound"] * prcpRound, y = .data[[weekdayString]], color="lightgrey"), size=2, method = "lm", se=FALSE) +
geom_point(aes(x=coefficients(DplusoutplusprcpvsI.lm)["(Intercept)"] + coefficients(DplusoutplusprcpvsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DplusoutplusprcpvsI.lm)["outdoorFraction"] * outdoorFraction + coefficients(DplusoutplusprcpvsI.lm)["prcpRound"] * prcpRound, y = .data[[weekdayString]], fill = tavg), shape = 21, size = 3) +
ggtitle(title) +
xlab("Intercept + a * outOfHomeDuration + b * outdoorFraction + c * prcp") +
ylab("changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
theme_minimal() +
scale_color_identity(labels = c("x=y", "Regression line"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank())

nestedplotlist[[paste0("Regression_DplusoutplusprcpvsI", weekday)]] <- DplusoutplusprcpvsI.lm
nestedplotlist[[paste0("Plot_DplusoutplusprcpvsI", weekday)]] <- plot22
nestedplotlist[[paste0("ResvsFittedValues_DplusoutplusprcpvsI", weekday)]] <- function(){
plot(DplusoutplusprcpvsI.lm, which=1)
}
nestedplotlist[[paste0("Qqplot_DplusoutplusprcpvsI", weekday)]] <- function(){
plot(DplusoutplusprcpvsI.lm, which=2)
}
nestedplotlist[[paste0("ScaleLoc_DplusoutplusprcpvsI", weekday)]] <- function(){
plot(DplusoutplusprcpvsI.lm, which=3)
}
nestedplotlist[[paste0("Cooksdist_DplusoutplusprcpvsI", weekday)]] <- function(){
plot(DplusoutplusprcpvsI.lm, which=4)
}
nestedplotlist[[paste0("ActualvsEstimate_DplusoutplusprcpvsI", weekday)]] <- plot23
}

grid.arrange(nestedplotlist[["Plot_DplusoutplusprcpvsIMon_1week_lag"]],nestedplotlist[["Plot_DplusoutplusprcpvsISun"]],nestedplotlist[["Plot_DplusoutplusprcpvsISat"]],nestedplotlist[["Plot_DplusoutplusprcpvsIFri"]], nestedplotlist[["Plot_DplusoutplusprcpvsIThu"]], nestedplotlist[["Plot_DplusoutplusprcpvsIWed"]], nestedplotlist[["Plot_DplusoutplusprcpvsITue"]], nestedplotlist[["Plot_DplusoutplusprcpvsIMon"]], nrow=3)
grid.arrange(nestedplotlist[["ActualvsEstimate_DplusoutplusprcpvsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimate_DplusoutplusprcpvsISun"]],nestedplotlist[["ActualvsEstimate_DplusoutplusprcpvsISat"]],nestedplotlist[["ActualvsEstimate_DplusoutplusprcpvsIFri"]], nestedplotlist[["ActualvsEstimate_DplusoutplusprcpvsIThu"]], nestedplotlist[["ActualvsEstimate_DplusoutplusprcpvsIWed"]], nestedplotlist[["ActualvsEstimate_DplusoutplusprcpvsITue"]], nestedplotlist[["ActualvsEstimate_DplusoutplusprcpvsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimate_DplusoutplusprcpvsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimate_DplusoutplusprcpvsISun"]],nestedplotlist[["ActualvsEstimate_DplusoutplusprcpvsISat"]],nestedplotlist[["ActualvsEstimate_DplusoutplusprcpvsIFri"]], nestedplotlist[["ActualvsEstimate_DplusoutplusprcpvsIThu"]], nestedplotlist[["ActualvsEstimate_DplusoutplusprcpvsIWed"]], nestedplotlist[["ActualvsEstimate_DplusoutplusprcpvsITue"]], nestedplotlist[["ActualvsEstimate_DplusoutplusprcpvsIMon"]], nrow=3)
#g <- arrangeGrob(nestedplotlist[["Plot_DplusoutplusprcpvsIMon_1week_lag"]],nestedplotlist[["Plot_DplusoutplusprcpvsISun"]],nestedplotlist[["Plot_DplusoutplusprcpvsISat"]],nestedplotlist[["Plot_DplusoutplusprcpvsIFri"]], nestedplotlist[["Plot_DplusoutplusprcpvsIThu"]], nestedplotlist[["Plot_DplusoutplusprcpvsIWed"]], nestedplotlist[["Plot_DplusoutplusprcpvsITue"]], nestedplotlist[["Plot_DplusoutplusprcpvsIMon"]], nrow=3)


# 10) D + D:out2 + D:prcp
for(weekday in weekdays){
if(weekday == "Mon_1week_lag"){
weekdayString <- "changeOfIncidencelaggedMon"
formula.lm <- "changeOfIncidencelaggedMon ~ outOfHomeDuration+outOfHomeDuration:outdoorFraction2+outOfHomeDuration:prcpRound" 
} else {
formula.lm <- paste0("changeOfIncidencelagged", weekday, "2", " ~ outOfHomeDuration+outOfHomeDuration:outdoorFraction2+outOfHomeDuration:prcpRound")
}
DplusDtimesoutplusDtimesprcp.lm <- lm(formula = formula.lm, data=joinedDataFrame) #Regression
if(weekday == "Mon") {
title <- "14 Day lag"
} else if(weekday == "Tue") {
title <- "13 Day lag"
} else if(weekday == "Wed") {
title <- "12 Day lag"
} else if(weekday == "Thu") {
title <- "11 Day lag"
} else if(weekday == "Fri") {
title <- "10 Day lag"
} else if(weekday == "Sat") {
title <- "9 Day lag"
} else if(weekday == "Sun") {
title <- "8 Day lag"
} else if(weekday == "Mon_1week_lag"){
title <- "7 Day lag"
}
plot22 <- ggPredict(DplusDtimesoutplusDtimesprcp.lm, interactive = TRUE)

plot23 <- ggplot(data=joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
geom_smooth(aes(x=coefficients(DplusDtimesoutplusDtimesprcp.lm)["(Intercept)"] + coefficients(DplusDtimesoutplusDtimesprcp.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DplusDtimesoutplusDtimesprcp.lm)["outOfHomeDuration:outdoorFraction2"] * outOfHomeDuration* outdoorFraction2 + coefficients(DplusDtimesoutplusDtimesprcp.lm)["outOfHomeDuration:prcpRound"] * outOfHomeDuration * prcpRound, y = .data[[weekdayString]], color="lightgrey"), size=2, method = "lm", se=FALSE) +
geom_point(aes(x=coefficients(DplusDtimesoutplusDtimesprcp.lm)["(Intercept)"] + coefficients(DplusDtimesoutplusDtimesprcp.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DplusDtimesoutplusDtimesprcp.lm)["outOfHomeDuration:outdoorFraction2"] * outOfHomeDuration* outdoorFraction2 + coefficients(DplusDtimesoutplusDtimesprcp.lm)["outOfHomeDuration:prcpRound"] * outOfHomeDuration * prcpRound, y = .data[[weekdayString]], fill = tavg), shape = 21, size = 3) +
ggtitle(title) +
xlab("Intercept + a * outOfHomeDuration + b * oOHD * oF2 + c * oOHD * prcp") +
ylab("changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
theme_minimal() +
scale_color_identity(labels = c("x=y", "Regression line"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank())


nestedplotlist[[paste0("Regression_D:out2:prcpvsI", weekday)]] <- DplusDtimesoutplusDtimesprcp.lm 
nestedplotlist[[paste0("Plot_D:out2:prcpvsI", weekday)]] <- plot22
nestedplotlist[[paste0("ResvsFittedValues_D:out2:prcpvsI", weekday)]] <- function(){
plot(DplusDtimesoutplusDtimesprcp.lm , which=1)
}
nestedplotlist[[paste0("Qqplot_D:out2:prcpvsI", weekday)]] <- function(){
plot(DplusDtimesoutplusDtimesprcp.lm , which=2)
}
nestedplotlist[[paste0("ScaleLoc_D:out2:prcpvsI", weekday)]] <- function(){
plot(DplusDtimesoutplusDtimesprcp.lm , which=3)
}
nestedplotlist[[paste0("Cooksdist_D:out2:prcpvsI", weekday)]] <- function(){
plot(DplusDtimesoutplusDtimesprcp.lm , which=4)
}
nestedplotlist[[paste0("ActualvsEstimate_D:out2:prcpvsI", weekday)]] <- plot23
}

grid.arrange(nestedplotlist[["Plot_D:out2:prcpvsIMon_1week_lag"]],nestedplotlist[["Plot_D:out2:prcpvsISun"]],nestedplotlist[["Plot_D:out2:prcpvsISat"]],nestedplotlist[["Plot_D:out2:prcpvsIFri"]], nestedplotlist[["Plot_D:out2:prcpvsIThu"]], nestedplotlist[["Plot_D:out2:prcpvsIWed"]], nestedplotlist[["Plot_D:out2:prcpvsITue"]], nestedplotlist[["Plot_D:out2:prcpvsIMon"]], nrow=3)
grid.arrange(nestedplotlist[["ActualvsEstimate_D:out2:prcpvsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimate_D:out2:prcpvsISun"]],nestedplotlist[["ActualvsEstimate_D:out2:prcpvsISat"]],nestedplotlist[["ActualvsEstimate_D:out2:prcpvsIFri"]], nestedplotlist[["ActualvsEstimate_D:out2:prcpvsIThu"]], nestedplotlist[["ActualvsEstimate_D:out2:prcpvsIWed"]], nestedplotlist[["ActualvsEstimate_D:out2:prcpvsITue"]], nestedplotlist[["ActualvsEstimate_D:out2:prcpvsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimate_D:out2:prcpvsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimate_D:out2:prcpvsISun"]],nestedplotlist[["ActualvsEstimate_D:out2:prcpvsISat"]],nestedplotlist[["ActualvsEstimate_D:out2:prcpvsIFri"]], nestedplotlist[["ActualvsEstimate_D:out2:prcpvsIThu"]], nestedplotlist[["ActualvsEstimate_D:out2:prcpvsIWed"]], nestedplotlist[["ActualvsEstimate_D:out2:prcpvsITue"]], nestedplotlist[["ActualvsEstimate_D:out2:prcpvsIMon"]], nrow=3)
#g <- arrangeGrob(nestedplotlist[["Plot_DplusDtimesoutplusDtimesprcpMon_1week_lag"]],nestedplotlist[["Plot_DplusDtimesoutplusDtimesprcpSun"]],nestedplotlist[["Plot_DplusDtimesoutplusDtimesprcpSat"]],nestedplotlist[["Plot_DplusDtimesoutplusDtimesprcpFri"]], nestedplotlist[["Plot_DplusDtimesoutplusDtimesprcpThu"]], nestedplotlist[["Plot_DplusDtimesoutplusDtimesprcpWed"]], nestedplotlist[["Plot_DplusDtimesoutplusDtimesprcpTue"]], nestedplotlist[["Plot_DplusDtimesoutplusDtimesprcpMon"]], nrow=3)


# 11) D^2*out vs I 
for(weekday in weekdays){
if(weekday == "Mon_1week_lag"){
formula.lm <- "changeOfIncidencelaggedMon ~ outOfHomeDurationSquared * outdoorFraction" 
weekdayString <- "changeOfIncidencelaggedMon"
} else {
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ outOfHomeDurationSquared * outdoorFraction")
weekdayString <- paste0("changeOfIncidencelagged",weekday,"2")
}
DSquaredtimesoutvsI.lm <- lm(formula = formula.lm, data=joinedDataFrame) #Regression
if(weekday == "Mon") {
title <- "14 Day lag"
} else if(weekday == "Tue") {
title <- "13 Day lag"
} else if(weekday == "Wed") {
title <- "12 Day lag"
} else if(weekday == "Thu") {
title <- "11 Day lag"
} else if(weekday == "Fri") {
title <- "10 Day lag"
} else if(weekday == "Sat") {
title <- "9 Day lag"
} else if(weekday == "Sun") {
title <- "8 Day lag"
} else if(weekday == "Mon_1week_lag") {
title <- "7 Day lag"
}
#1st plot; x = outOfHomeDurationSquared, y = changeOfIncidence, color = outdoorFraction
plot22 <- ggplot(data=joinedDataFrame, aes(x=outOfHomeDurationSquared, color =outdoorFraction, y = .data[[weekdayString]])) +
geom_point() +
geom_smooth(method = "lm") +
ggtitle(title) +
theme_minimal() +
scale_color_identity(labels = c("Regression line", "x=y"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank())


plot23 <- ggplot(data=joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
geom_smooth(aes(x=coefficients(DSquaredtimesoutvsI.lm)["(Intercept)"] + coefficients(DSquaredtimesoutvsI.lm)["outOfHomeDurationSquared"] * outOfHomeDurationSquared + coefficients(DSquaredtimesoutvsI.lm)["outdoorFraction"] * outdoorFraction + coefficients(DSquaredtimesoutvsI.lm)["outOfHomeDurationSquared:outdoorFraction"] * outOfHomeDurationSquared * outdoorFraction, y = .data[[weekdayString]], color="lightgrey"), size=2, method = "lm", se=FALSE) +
geom_point(aes(x=coefficients(DSquaredtimesoutvsI.lm)["(Intercept)"] + coefficients(DSquaredtimesoutvsI.lm)["outOfHomeDurationSquared"] * outOfHomeDurationSquared + coefficients(DSquaredtimesoutvsI.lm)["outdoorFraction"] * outdoorFraction + coefficients(DSquaredtimesoutvsI.lm)["outOfHomeDurationSquared:outdoorFraction"] * outOfHomeDurationSquared * outdoorFraction, y = .data[[weekdayString]], fill = tavg), shape = 21, size = 3) +
ggtitle(title) +
xlab("Intercept + a * outOfHomeDuration^2 * outdoorFraction") +
ylab("changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
theme_minimal() +
scale_color_identity(labels = c("x=y", "Regression line"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank())


nestedplotlist[[paste0("Regression_D2timesoutvsI", weekday)]] <- DSquaredtimesoutvsI.lm
nestedplotlist[[paste0("Plot_D2timesoutvsI", weekday)]] <- plot22
nestedplotlist[[paste0("ResvsFittedValues_D2timesoutvsI", weekday)]] <- function(){
plot(DSquaredtimesoutvsI.lm , which=1)
}
nestedplotlist[[paste0("Qqplot_D2timesoutvsI", weekday)]] <- function(){
plot(DSquaredtimesoutvsI.lm , which=2)
}
nestedplotlist[[paste0("ScaleLoc_D2timesoutvsI", weekday)]] <- function(){
plot(DSquaredtimesoutvsI.lm , which=3)
}
nestedplotlist[[paste0("Cooksdist_D2timesoutvsI", weekday)]] <- function(){
plot(DSquaredtimesoutvsI.lm , which=4)
}
nestedplotlist[[paste0("ActualvsEstimate_D2timesoutvsI", weekday)]] <- plot23
}


grid.arrange(nestedplotlist[["Plot_D2timesoutvsIMon_1week_lag"]], nestedplotlist[["Plot_D2timesoutvsISun"]],nestedplotlist[["Plot_D2timesoutvsISat"]],nestedplotlist[["Plot_D2timesoutvsIFri"]], nestedplotlist[["Plot_D2timesoutvsIThu"]], nestedplotlist[["Plot_D2timesoutvsIWed"]], nestedplotlist[["Plot_D2timesoutvsITue"]], nestedplotlist[["Plot_D2timesoutvsIMon"]], nrow=3)
grid.arrange(nestedplotlist[["ActualvsEstimate_D2timesoutvsIMon_1week_lag"]], nestedplotlist[["ActualvsEstimate_D2timesoutvsISun"]],nestedplotlist[["ActualvsEstimate_D2timesoutvsISat"]],nestedplotlist[["ActualvsEstimate_D2timesoutvsIFri"]], nestedplotlist[["ActualvsEstimate_D2timesoutvsIThu"]], nestedplotlist[["ActualvsEstimate_D2timesoutvsIWed"]], nestedplotlist[["ActualvsEstimate_D2timesoutvsITue"]], nestedplotlist[["ActualvsEstimate_D2timesoutvsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimate_D2timesoutvsIMon_1week_lag"]], nestedplotlist[["ActualvsEstimate_D2timesoutvsISun"]],nestedplotlist[["ActualvsEstimate_D2timesoutvsISat"]],nestedplotlist[["ActualvsEstimate_D2timesoutvsIFri"]], nestedplotlist[["ActualvsEstimate_D2timesoutvsIThu"]], nestedplotlist[["ActualvsEstimate_D2timesoutvsIWed"]], nestedplotlist[["ActualvsEstimate_D2timesoutvsITue"]], nestedplotlist[["ActualvsEstimate_D2timesoutvsIMon"]], nrow=3)
#g <- arrangeGrob(nestedplotlist[["Plot_D2timesoutvsIMon_1week_lag"]], nestedplotlist[["Plot_D2timesoutvsISun"]],nestedplotlist[["Plot_D2timesoutvsISat"]],nestedplotlist[["Plot_D2timesoutvsIFri"]], nestedplotlist[["Plot_D2timesoutvsIThu"]], nestedplotlist[["Plot_D2timesoutvsIWed"]], nestedplotlist[["Plot_D2timesoutvsITue"]], nestedplotlist[["Plot_D2timesoutvsIMon"]], nrow=3)


#12) D * tmax^2 vs I 
joinedDataFrame <- joinedDataFrame %>% mutate(tmaxSquared = tmax * tmax)
for(weekday in weekdays){
if(weekday == "Mon_1week_lag"){
formula.lm <- "changeOfIncidencelaggedMon ~ outOfHomeDuration * tmaxSquared" 
weekdayString <- "changeOfIncidencelaggedMon"
} else {
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ outOfHomeDuration * tmaxSquared")
weekdayString <- paste0("changeOfIncidencelagged",weekday,"2")
}
DtimestmaxSquaredvsI.lm <- lm(formula = formula.lm, data=joinedDataFrame) #Regression
if(weekday == "Mon") {
title <- "14 Day lag" 
} else if (weekday == "Tue") {
title <- "13 Day lag" 
} else if (weekday == "Wed") {
title <- "12 Day lag"
} else if (weekday == "Thu") {
title <- "11 Day lag"
} else if (weekday == "Fri") {
title <- "10 Day lag"
} else if (weekday == "Sat") {
title <- "9 Day lag"
} else if (weekday == "Sun") {
title <- "8 Day lag"
} else if (weekday == "Mon_1week_lag") {
title <- "7 Day lag"   
}
#1st plot; x = outOfHomeDuration, y = changeOfIncidence, color = tmaxSquared
plot22 <- ggplot(data=joinedDataFrame, aes(x=outOfHomeDuration, color =tmaxSquared, y = .data[[weekdayString]])) +
geom_point() +
geom_smooth(method = "lm") +
ggtitle(title) +
theme_minimal()


plot23 <- ggplot(data=joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
geom_smooth(aes(x=coefficients(DtimestmaxSquaredvsI.lm)["(Intercept)"] + coefficients(DtimestmaxSquaredvsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DtimestmaxSquaredvsI.lm)["tmaxSquared"] * tmaxSquared + coefficients(DtimestmaxSquaredvsI.lm)["outOfHomeDuration:tmaxSquared"] * outOfHomeDuration * tmaxSquared, y = .data[[weekdayString]], color="lightgrey"), size=2, method = "lm", se=FALSE) +
geom_point(aes(x=coefficients(DtimestmaxSquaredvsI.lm)["(Intercept)"] + coefficients(DtimestmaxSquaredvsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DtimestmaxSquaredvsI.lm)["tmaxSquared"] * tmaxSquared + coefficients(DtimestmaxSquaredvsI.lm)["outOfHomeDuration:tmaxSquared"] * outOfHomeDuration * tmaxSquared, y = .data[[weekdayString]], fill = tavg), shape = 21, size = 3) +
ggtitle(title) +
xlab("Intercept + a*outOfHomeDuration + b*tmax^2 + c*oOHD*tmax^2") +
ylab("changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
theme_minimal() +
scale_color_identity(labels = c("x=y","Regression line"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank())


nestedplotlist[[paste0("Regression_DtimestmaxSquaredvsI", weekday)]] <- DtimestmaxSquaredvsI.lm 
nestedplotlist[[paste0("Plot_DtimestmaxSquaredvsI", weekday)]] <- plot22
nestedplotlist[[paste0("ResvsFittedValues_DtimestmaxSquaredvsI", weekday)]] <- function(){
plot(DtimestmaxSquaredvsI.lm , which=1)
}
nestedplotlist[[paste0("Qqplot_DtimestmaxSquaredvsI", weekday)]] <- function(){
plot(DtimestmaxSquaredvsI.lm , which=2)
}
nestedplotlist[[paste0("ScaleLoc_DtimestmaxSquaredvsI", weekday)]] <- function(){
plot(DtimestmaxSquaredvsI.lm , which=3)
}
nestedplotlist[[paste0("Cooksdist_DtimestmaxSquaredvsI", weekday)]] <- function(){
plot(DtimestmaxSquaredvsI.lm , which=4)
}
nestedplotlist[[paste0("ActualvsEstimate_DtimestmaxSquaredvsI", weekday)]] <- plot23
}


grid.arrange(nestedplotlist[["Plot_DtimestmaxSquaredvsIMon_1week_lag"]], nestedplotlist[["Plot_DtimestmaxSquaredvsISun"]],nestedplotlist[["Plot_DtimestmaxSquaredvsISat"]],nestedplotlist[["Plot_DtimestmaxSquaredvsIFri"]], nestedplotlist[["Plot_DtimestmaxSquaredvsIThu"]], nestedplotlist[["Plot_DtimestmaxSquaredvsIWed"]], nestedplotlist[["Plot_DtimestmaxSquaredvsITue"]], nestedplotlist[["Plot_DtimestmaxSquaredvsIMon"]], nrow=3)
grid.arrange(nestedplotlist[["ActualvsEstimate_DtimestmaxSquaredvsIMon_1week_lag"]], nestedplotlist[["ActualvsEstimate_DtimestmaxSquaredvsISun"]],nestedplotlist[["ActualvsEstimate_DtimestmaxSquaredvsISat"]],nestedplotlist[["ActualvsEstimate_DtimestmaxSquaredvsIFri"]], nestedplotlist[["ActualvsEstimate_DtimestmaxSquaredvsIThu"]], nestedplotlist[["ActualvsEstimate_DtimestmaxSquaredvsIWed"]], nestedplotlist[["ActualvsEstimate_DtimestmaxSquaredvsITue"]], nestedplotlist[["ActualvsEstimate_DtimestmaxSquaredvsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimate_DtimestmaxSquaredvsIMon_1week_lag"]], nestedplotlist[["ActualvsEstimate_DtimestmaxSquaredvsISun"]],nestedplotlist[["ActualvsEstimate_DtimestmaxSquaredvsISat"]],nestedplotlist[["ActualvsEstimate_DtimestmaxSquaredvsIFri"]], nestedplotlist[["ActualvsEstimate_DtimestmaxSquaredvsIThu"]], nestedplotlist[["ActualvsEstimate_DtimestmaxSquaredvsIWed"]], nestedplotlist[["ActualvsEstimate_DtimestmaxSquaredvsITue"]], nestedplotlist[["ActualvsEstimate_DtimestmaxSquaredvsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["Plot_DtimestmaxSquaredvsIMon_1week_lag"]], nestedplotlist[["Plot_DtimestmaxSquaredvsISun"]],nestedplotlist[["Plot_DtimestmaxSquaredvsISat"]],nestedplotlist[["Plot_DtimestmaxSquaredvsIFri"]], nestedplotlist[["Plot_DtimestmaxSquaredvsIThu"]], nestedplotlist[["Plot_DtimestmaxSquaredvsIWed"]], nestedplotlist[["Plot_DtimestmaxSquaredvsITue"]], nestedplotlist[["Plot_DtimestmaxSquaredvsIMon"]], nrow=3)


# 13)  D * tmax * prcp
joinedDataFrame <- joinedDataFrame %>% mutate(tmaxSquared = tmax * tmax)
for(weekday in weekdays){
if(weekday == "Mon_1week_lag"){
formula.lm <- "changeOfIncidencelaggedMon ~ outOfHomeDuration * tmaxSquared * prcp" 
weekdayString <- "changeOfIncidencelaggedMon"
} else {
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ outOfHomeDuration * tmaxSquared * prcp")
weekdayString <- paste0("changeOfIncidencelagged",weekday,"2")
}
DtimestmaxtimesprcpvsI.lm <- lm(formula = formula.lm, data=joinedDataFrame) #Regression
if(weekday == "Mon") {
title <- "14 Day lag" 
} else if (weekday == "Tue") {
title <- "13 Day lag" 
} else if (weekday == "Wed") {
title <- "12 Day lag"
} else if (weekday == "Thu") {
title <- "11 Day lag"
} else if (weekday == "Fri") {
title <- "10 Day lag"
} else if (weekday == "Sat") {
title <- "9 Day lag"
} else if (weekday == "Sun") {
title <- "8 Day lag"
} else if (weekday == "Mon_1week_lag") {
title <- "7 Day lag"   
}
#1st plot; x = outOfHomeDuration, y = changeOfIncidence, color = tmaxSquared
plot22 <- ggplot(data=joinedDataFrame, aes(x=outOfHomeDuration, color =tmaxSquared, y = .data[[weekdayString]])) +
geom_point() +
geom_smooth(method = "lm") +
ggtitle(title) + 
theme_minimal()

plot23 <- ggplot(data=joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
geom_smooth(aes(x=coefficients(DtimestmaxtimesprcpvsI.lm)["(Intercept)"] + 
  coefficients(DtimestmaxtimesprcpvsI.lm)["outOfHomeDuration"] * outOfHomeDuration + 
  coefficients(DtimestmaxtimesprcpvsI.lm)["tmaxSquared"] * tmaxSquared + 
  coefficients(DtimestmaxtimesprcpvsI.lm)["prcp"] * prcp + 
  coefficients(DtimestmaxtimesprcpvsI.lm)["outOfHomeDuration:tmaxSquared"] * outOfHomeDuration * tmaxSquared + 
  coefficients(DtimestmaxtimesprcpvsI.lm)["outOfHomeDuration:prcp"] * outOfHomeDuration * prcp +
  coefficients(DtimestmaxtimesprcpvsI.lm)["tmaxSquared:prcp"] * prcp * tmaxSquared + 
  coefficients(DtimestmaxtimesprcpvsI.lm)["outOfHomeDuration:tmaxSquared:prcp"] * outOfHomeDuration * tmaxSquared * prcp, y = .data[[weekdayString]], color="lightgrey"), size=2, method = "lm", se=FALSE) +
geom_point(aes(x=coefficients(DtimestmaxtimesprcpvsI.lm)["(Intercept)"] + 
  coefficients(DtimestmaxtimesprcpvsI.lm)["outOfHomeDuration"] * outOfHomeDuration + 
  coefficients(DtimestmaxtimesprcpvsI.lm)["tmaxSquared"] * tmaxSquared + 
  coefficients(DtimestmaxtimesprcpvsI.lm)["prcp"] * prcp + 
  coefficients(DtimestmaxtimesprcpvsI.lm)["outOfHomeDuration:tmaxSquared"] * outOfHomeDuration * tmaxSquared + 
  coefficients(DtimestmaxtimesprcpvsI.lm)["outOfHomeDuration:prcp"] * outOfHomeDuration * prcp +
  coefficients(DtimestmaxtimesprcpvsI.lm)["tmaxSquared:prcp"] * prcp * tmaxSquared + 
  coefficients(DtimestmaxtimesprcpvsI.lm)["outOfHomeDuration:tmaxSquared:prcp"] * outOfHomeDuration * tmaxSquared * prcp, y = .data[[weekdayString]], fill = tavg), shape = 21, size = 3) +
ggtitle(title) +
xlab("Estimate") +
ylab("Actual changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
theme_minimal() +
scale_color_identity(labels = c("x=y", "Regression line"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank())


nestedplotlist[[paste0("Regression_DtimestmaxtimesprcpvsI", weekday)]] <- DtimestmaxtimesprcpvsI.lm 
nestedplotlist[[paste0("Plot_DtimestmaxtimesprcpvsI", weekday)]] <- plot22
nestedplotlist[[paste0("ResvsFittedValues_DtimestmaxtimesprcpvsI", weekday)]] <- function(){
plot(DtimestmaxtimesprcpvsI.lm, which=1)
}
nestedplotlist[[paste0("Qqplot_DtimestmaxtimesprcpvsI", weekday)]] <- function(){
plot(DtimestmaxtimesprcpvsI.lm, which=2)
}
nestedplotlist[[paste0("ScaleLoc_DtimestmaxtimesprcpvsI", weekday)]] <- function(){
plot(DtimestmaxtimesprcpvsI.lm, which=3)
}
nestedplotlist[[paste0("Cooksdist_DtimestmaxtimesprcpvsI", weekday)]]<- function(){
plot(DtimestmaxtimesprcpvsI.lm, which=4)
}
nestedplotlist[[paste0("ActualvsEstimate_DtimestmaxtimesprcpvsI", weekday)]] <- plot23
}

grid.arrange(nestedplotlist[["Plot_DtimestmaxtimesprcpvsIMon_1week_lag"]], nestedplotlist[["Plot_DtimestmaxtimesprcpvsISun"]],nestedplotlist[["Plot_DtimestmaxtimesprcpvsISat"]],nestedplotlist[["Plot_DtimestmaxtimesprcpvsIFri"]], nestedplotlist[["Plot_DtimestmaxtimesprcpvsIThu"]], nestedplotlist[["Plot_DtimestmaxtimesprcpvsIWed"]], nestedplotlist[["Plot_DtimestmaxtimesprcpvsITue"]], nestedplotlist[["Plot_DtimestmaxtimesprcpvsIMon"]], nrow=3)
grid.arrange(nestedplotlist[["ActualvsEstimate_DtimestmaxtimesprcpvsIMon_1week_lag"]], nestedplotlist[["ActualvsEstimate_DtimestmaxtimesprcpvsISun"]],nestedplotlist[["ActualvsEstimate_DtimestmaxtimesprcpvsISat"]],nestedplotlist[["ActualvsEstimate_DtimestmaxtimesprcpvsIFri"]], nestedplotlist[["ActualvsEstimate_DtimestmaxtimesprcpvsIThu"]], nestedplotlist[["ActualvsEstimate_DtimestmaxtimesprcpvsIWed"]], nestedplotlist[["ActualvsEstimate_DtimestmaxtimesprcpvsITue"]], nestedplotlist[["ActualvsEstimate_DtimestmaxtimesprcpvsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimate_DtimestmaxtimesprcpvsIMon_1week_lag"]], nestedplotlist[["ActualvsEstimate_DtimestmaxtimesprcpvsISun"]],nestedplotlist[["ActualvsEstimate_DtimestmaxtimesprcpvsISat"]],nestedplotlist[["ActualvsEstimate_DtimestmaxtimesprcpvsIFri"]], nestedplotlist[["ActualvsEstimate_DtimestmaxtimesprcpvsIThu"]], nestedplotlist[["ActualvsEstimate_DtimestmaxtimesprcpvsIWed"]], nestedplotlist[["ActualvsEstimate_DtimestmaxtimesprcpvsITue"]], nestedplotlist[["ActualvsEstimate_DtimestmaxtimesprcpvsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["Plot_DtimestmaxtimesprcpvsIMon_1week_lag"]], nestedplotlist[["Plot_DtimestmaxtimesprcpvsISun"]],nestedplotlist[["Plot_DtimestmaxtimesprcpvsISat"]],nestedplotlist[["Plot_DtimestmaxtimesprcpvsIFri"]], nestedplotlist[["Plot_DtimestmaxtimesprcpvsIThu"]], nestedplotlist[["Plot_DtimestmaxtimesprcpvsIWed"]], nestedplotlist[["Plot_DtimestmaxtimesprcpvsITue"]], nestedplotlist[["Plot_DtimestmaxtimesprcpvsIMon"]], nrow=3)


# 14) log(D) + log(tmax) vs I
joinedDataFrame <- joinedDataFrame %>% mutate(logOutOfHomeDuration = log(outOfHomeDuration)) %>%
 mutate(logtmax = log(tmax))

for(weekday in weekdays){
if(weekday == "Mon_1week_lag"){
formula.lm <- "changeOfIncidencelaggedMon ~ logOutOfHomeDuration + logtmax" 
weekdayString <- "changeOfIncidencelaggedMon"
} else {
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ logOutOfHomeDuration + logtmax")
weekdayString <- paste0("changeOfIncidencelagged",weekday,"2")
}
logDpluslogtmaxvsI.lm <- lm(formula = formula.lm, data=joinedDataFrame) #Regression
if(weekday == "Mon") {
title <- "14 Day lag" 
} else if (weekday == "Tue") {
title <- "13 Day lag" 
} else if (weekday == "Wed") {
title <- "12 Day lag"
} else if (weekday == "Thu") {
title <- "11 Day lag"
} else if (weekday == "Fri") {
title <- "10 Day lag"
} else if (weekday == "Sat") {
title <- "9 Day lag"
} else if (weekday == "Sun") {
title <- "8 Day lag"
} else if (weekday == "Mon_1week_lag") {
title <- "7 Day lag"   
}
#1st plot; x = outOfHomeDuration, y = changeOfIncidence, color = tmaxSquared
plot22 <- ggplot(data=joinedDataFrame, aes(x=logoutOfHomeDuration, color =logtmax, y = .data[[weekdayString]])) +
geom_point() +
geom_smooth(method = "lm") +
ggtitle(title) + 
theme_minimal()

plot23 <- ggplot(data=joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
geom_smooth(aes(x=coefficients(logDpluslogtmaxvsI.lm)["(Intercept)"] + 
  coefficients(logDpluslogtmaxvsI.lm)["logOutOfHomeDuration"] * logOutOfHomeDuration + 
  coefficients(logDpluslogtmaxvsI.lm)["logtmax"] * logtmax, y = .data[[weekdayString]], color="lightgrey"), size=2, method = "lm", se=FALSE) +
geom_point(aes(x=coefficients(logDpluslogtmaxvsI.lm)["(Intercept)"] + 
  coefficients(logDpluslogtmaxvsI.lm)["logOutOfHomeDuration"] * logOutOfHomeDuration + 
  coefficients(logDpluslogtmaxvsI.lm)["logtmax"] * logtmax, y = .data[[weekdayString]], fill =logtmax), shape = 21, size = 3) + 
ggtitle(title) +
xlab("Estimate") +
ylab("Actual changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
theme_minimal() +
scale_color_identity(labels = c("x=y", "Regression line"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank())


nestedplotlist[[paste0("Regression_logDpluslogtmaxvsI", weekday)]] <- logDpluslogtmaxvsI.lm
nestedplotlist[[paste0("Plot_logDpluslogtmaxvsI", weekday)]] <- plot22
nestedplotlist[[paste0("ResvsFittedValues_logDpluslogtmaxvsI", weekday)]] <- function(){
plot(logDpluslogtmaxvsI.lm , which=1)
}
nestedplotlist[[paste0("Qqplot_logDpluslogtmaxvsI", weekday)]] <- function(){
plot(logDpluslogtmaxvsI.lm , which=2)
}
nestedplotlist[[paste0("ScaleLoc_logDpluslogtmaxvsI", weekday)]] <- function(){
plot(logDpluslogtmaxvsI.lm , which=3)
}
nestedplotlist[[paste0("Cooksdist_logDpluslogtmaxvsI", weekday)]] <- function(){
plot(logDpluslogtmaxvsI.lm , which=4)
}
nestedplotlist[[paste0("ActualvsEstimate_logDpluslogtmaxvsI", weekday)]] <- plot23
}

grid.arrange(nestedplotlist[["Plot_logDpluslogtmaxvsIMon_1week_lag"]], nestedplotlist[["Plot_logDpluslogtmaxvsISun"]],nestedplotlist[["Plot_logDpluslogtmaxvsISat"]],nestedplotlist[["Plot_logDpluslogtmaxvsIFri"]], nestedplotlist[["Plot_logDpluslogtmaxvsIThu"]], nestedplotlist[["Plot_logDpluslogtmaxvsIWed"]], nestedplotlist[["Plot_logDpluslogtmaxvsITue"]], nestedplotlist[["Plot_logDpluslogtmaxvsIMon"]], nrow=3)
grid.arrange(nestedplotlist[["ActualvsEstimate_logDpluslogtmaxvsIMon_1week_lag"]], nestedplotlist[["ActualvsEstimate_logDpluslogtmaxvsISun"]],nestedplotlist[["ActualvsEstimate_logDpluslogtmaxvsISat"]],nestedplotlist[["ActualvsEstimate_logDpluslogtmaxvsIFri"]], nestedplotlist[["ActualvsEstimate_logDpluslogtmaxvsIThu"]], nestedplotlist[["ActualvsEstimate_logDpluslogtmaxvsIWed"]], nestedplotlist[["ActualvsEstimate_logDpluslogtmaxvsITue"]], nestedplotlist[["ActualvsEstimate_logDpluslogtmaxvsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimate_logDpluslogtmaxvsIMon_1week_lag"]], nestedplotlist[["ActualvsEstimate_logDpluslogtmaxvsISun"]],nestedplotlist[["ActualvsEstimate_logDpluslogtmaxvsISat"]],nestedplotlist[["ActualvsEstimate_logDpluslogtmaxvsIFri"]], nestedplotlist[["ActualvsEstimate_logDpluslogtmaxvsIThu"]], nestedplotlist[["ActualvsEstimate_logDpluslogtmaxvsIWed"]], nestedplotlist[["ActualvsEstimate_logDpluslogtmaxvsITue"]], nestedplotlist[["ActualvsEstimate_logDpluslogtmaxvsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["Plot_logDpluslogtmaxvsIMon_1week_lag"]], nestedplotlist[["Plot_logDpluslogtmaxvsISun"]],nestedplotlist[["Plot_logDpluslogtmaxvsISat"]],nestedplotlist[["Plot_logDpluslogtmaxvsIFri"]], nestedplotlist[["Plot_logDpluslogtmaxvsIThu"]], nestedplotlist[["Plot_logDpluslogtmaxvsIWed"]], nestedplotlist[["Plot_logDpluslogtmaxvsITue"]], nestedplotlist[["Plot_logDpluslogtmaxvsIMon"]], nrow=3)


# 15) 1+percChange + tmax vs log(I)
joinedDataFrame <- joinedDataFrame %>% mutate(onePlusPercChange = 1 + percentageChangeComparedToBeforeCorona/100) 

for(weekday in weekdays){
if(weekday == "Mon_1week_lag"){
formula.lm <- "log(changeOfIncidencelaggedMon) ~ onePlusPercChange * tmax" 
weekdayString <- "changeOfIncidencelaggedMon"
} else {
formula.lm <- paste0("log(changeOfIncidencelagged",weekday,"2)", " ~ onePlusPercChange * tmax")
weekdayString <- paste0("changeOfIncidencelagged",weekday,"2")
}
onePlusPercChangeplustmaxvslogI.lm <- lm(formula = formula.lm, data=joinedDataFrame) #Regression
if(weekday == "Mon") {
title <- "14 Day lag" 
} else if (weekday == "Tue") {
title <- "13 Day lag" 
} else if (weekday == "Wed") {
title <- "12 Day lag"
} else if (weekday == "Thu") {
title <- "11 Day lag"
} else if (weekday == "Fri") {
title <- "10 Day lag"
} else if (weekday == "Sat") {
title <- "9 Day lag"
} else if (weekday == "Sun") {
title <- "8 Day lag"
} else if (weekday == "Mon_1week_lag") {
title <- "7 Day lag"   
}
#1st plot; x = onePlusPercChange, y = changeOfIncidence, color = tmax
plot22 <- ggplot(data=joinedDataFrame, aes(x=onePlusPercChange, color =tmax, y = log(.data[[weekdayString]]))) +
geom_point() +
geom_smooth(method = "lm") +
ggtitle(title) + 
theme_minimal()

plot23 <- ggplot(data=joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
geom_smooth(aes(x=coefficients(onePlusPercChangeplustmaxvslogI.lm)["(Intercept)"] + 
  coefficients(onePlusPercChangeplustmaxvslogI.lm)["onePlusPercChange"] * onePlusPercChange + 
  coefficients(onePlusPercChangeplustmaxvslogI.lm)["tmax"] * tmax +
  coefficients(onePlusPercChangeplustmaxvslogI.lm)["onePlusPercChange:tmax"] * onePlusPercChange * tmax, y = log(.data[[weekdayString]]), color="lightgrey"), size=2, method = "lm", se=FALSE) +
geom_point(aes(x=coefficients(onePlusPercChangeplustmaxvslogI.lm)["(Intercept)"] + 
  coefficients(onePlusPercChangeplustmaxvslogI.lm)["onePlusPercChange"] * onePlusPercChange + 
  coefficients(onePlusPercChangeplustmaxvslogI.lm)["tmax"] * tmax +
  coefficients(onePlusPercChangeplustmaxvslogI.lm)["onePlusPercChange:tmax"] * onePlusPercChange * tmax, y = log(.data[[weekdayString]]), fill =tmax), shape = 21, size = 3) + 
ggtitle(title) +
xlab("Estimate") +
ylab("Actual changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
theme_minimal() +
scale_color_identity(labels = c("x=y", "Regression line"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank())


nestedplotlist[[paste0("Regression_onePlusPercChangeplustmaxvslogI", weekday)]] <- onePlusPercChangeplustmaxvslogI.lm 
nestedplotlist[[paste0("Plot_onePlusPercChangeplustmaxvslogI", weekday)]] <- plot22
nestedplotlist[[paste0("ResvsFittedValues_onePlusPercChangeplustmaxvslogI", weekday)]] <- function(){
plot(onePlusPercChangeplustmaxvslogI.lm , which=1)
}
nestedplotlist[[paste0("Qqplot_onePlusPercChangeplustmaxvslogI", weekday)]] <- function(){
plot(onePlusPercChangeplustmaxvslogI.lm , which=2)
}
nestedplotlist[[paste0("ScaleLoc_onePlusPercChangeplustmaxvslogI", weekday)]] <- function(){
plot(onePlusPercChangeplustmaxvslogI.lm , which=3)
}
nestedplotlist[[paste0("Cooksdist_onePlusPercChangeplustmaxvslogI", weekday)]] <- function(){
plot(onePlusPercChangeplustmaxvslogI.lm , which=4)
}
nestedplotlist[[paste0("ActualvsEstimate_onePlusPercChangeplustmaxvslogI", weekday)]] <- plot23
}
v
grid.arrange(nestedplotlist[["Plot_onePlusPercChangeplustmaxvslogIMon_1week_lag"]], nestedplotlist[["Plot_onePlusPercChangeplustmaxvslogISun"]],nestedplotlist[["Plot_onePlusPercChangeplustmaxvslogISat"]],nestedplotlist[["Plot_onePlusPercChangeplustmaxvslogIFri"]], nestedplotlist[["Plot_onePlusPercChangeplustmaxvslogIThu"]], nestedplotlist[["Plot_onePlusPercChangeplustmaxvslogIWed"]], nestedplotlist[["Plot_onePlusPercChangeplustmaxvslogITue"]], nestedplotlist[["Plot_onePlusPercChangeplustmaxvslogIMon"]], nrow=3)
grid.arrange(nestedplotlist[["ActualvsEstimate_onePlusPercChangeplustmaxvslogIMon_1week_lag"]], nestedplotlist[["ActualvsEstimate_onePlusPercChangeplustmaxvslogISun"]],nestedplotlist[["ActualvsEstimate_onePlusPercChangeplustmaxvslogISat"]],nestedplotlist[["ActualvsEstimate_onePlusPercChangeplustmaxvslogIFri"]], nestedplotlist[["ActualvsEstimate_onePlusPercChangeplustmaxvslogIThu"]], nestedplotlist[["ActualvsEstimate_onePlusPercChangeplustmaxvslogIWed"]], nestedplotlist[["ActualvsEstimate_onePlusPercChangeplustmaxvslogITue"]], nestedplotlist[["ActualvsEstimate_onePlusPercChangeplustmaxvslogIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimate_onePlusPercChangeplustmaxvslogIMon_1week_lag"]], nestedplotlist[["ActualvsEstimate_onePlusPercChangeplustmaxvslogISun"]],nestedplotlist[["ActualvsEstimate_onePlusPercChangeplustmaxvslogISat"]],nestedplotlist[["ActualvsEstimate_onePlusPercChangeplustmaxvslogIFri"]], nestedplotlist[["ActualvsEstimate_onePlusPercChangeplustmaxvslogIThu"]], nestedplotlist[["ActualvsEstimate_onePlusPercChangeplustmaxvslogIWed"]], nestedplotlist[["ActualvsEstimate_onePlusPercChangeplustmaxvslogITue"]], nestedplotlist[["ActualvsEstimate_onePlusPercChangeplustmaxvslogIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["Plot_onePlusPercChangeplustmaxvslogIMon_1week_lag"]], nestedplotlist[["Plot_onePlusPercChangeplustmaxvslogISun"]],nestedplotlist[["Plot_onePlusPercChangeplustmaxvslogISat"]],nestedplotlist[["Plot_onePlusPercChangeplustmaxvslogIFri"]], nestedplotlist[["Plot_onePlusPercChangeplustmaxvslogIThu"]], nestedplotlist[["Plot_onePlusPercChangeplustmaxvslogIWed"]], nestedplotlist[["Plot_onePlusPercChangeplustmaxvslogITue"]], nestedplotlist[["Plot_onePlusPercChangeplustmaxvslogIMon"]], nrow=3)



#16) NEEDS TO BE REWORKED! NEED TO INTRODUCE A SECOND INDICATOR VARIABLE
#Let's try another by adding a categorical variable which tells us the different phases of 2020
#D * tmax + cat vs I
joinedDataFrame <- joinedDataFrame %>% mutate(pointInTime = case_when(Date < "2020-05-10" ~ 1, 
                                                            Date < "2020-10-15" ~ 2, 
                                                            Date < "2022-01-01" ~ 3
                                                            ))


for (weekday in weekdays){
if (weekday == "Mon_1week_lag") {
weekdayString <- "changeOfIncidencelaggedMon"
formula.lm <- "changeOfIncidencelaggedMon ~ outOfHomeDuration * tmax + factor(pointInTime)"
} else {
formula.lm <- paste0("changeOfIncidencelagged", weekday, "2", " ~ outOfHomeDuration*tmax + factor(pointInTime)")
weekdayString <- paste0("changeOfIncidencelagged", weekday, "2")
}
DtimestmaxpluspointInTimevsI.lm <- lm(formula = formula.lm, data = joinedDataFrame) #Regression
if (weekday == "Mon") {
title <- "14 Day lag"
} else if (weekday == "Tue") {
title <- "13 Day lag"
} else if (weekday == "Wed") {
title <- "12 Day lag"
} else if (weekday == "Thu") {
title <- "11 Day lag"
} else if (weekday == "Fri") {
title <- "10 Day lag"
} else if (weekday == "Sat") {
title <- "9 Day lag"
} else if (weekday == "Sun") {
title <- "8 Day lag"
} else if (weekday == "Mon_1week_lag") {
title <- "7 Day lag"
}
#1st plot; x = outOfHomeDuration, y = changeOfIncidence, color = tmax
plot22 <- ggplot(data=joinedDataFrame, aes(y = .data[[weekdayString]], x = outOfHomeDuration, color = tmax)) +
geom_point() +
geom_smooth(method = "lm") +
ggtitle(title) +
theme_minimal()

plot23 <- ggplot(data = joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
geom_smooth(aes(x=coefficients(DtimestmaxpluspointInTimevsI.lm)["(Intercept)"] + coefficients(DtimestmaxpluspointInTimevsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DtimestmaxpluspointInTimevsI.lm)["tmax"] * tmax + coefficients(DtimestmaxpluspointInTimevsI.lm)["outOfHomeDuration:tmax"] * outOfHomeDuration * tmax + coefficients(DtimestmaxpluspointInTimevsI.lm)["factor(pointInTime)3"] * pointInTime , y = .data[[weekdayString]], color="lightgrey"), size=2, method = "lm", se=FALSE) +
ggtitle(title) +
xlab("Intercept + a * outOfHomeDuration + b * tmax + c * oOHD * tmax") +
ylab("changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
theme_minimal() +
scale_color_identity(labels = c("x=y", "Regression line"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank()) +
geom_point(aes(x=coefficients(DtimestmaxpluspointInTimevsI.lm)["(Intercept)"] + coefficients(DtimestmaxpluspointInTimevsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DtimestmaxpluspointInTimevsI.lm)["tmax"] * tmax + coefficients(DtimestmaxpluspointInTimevsI.lm)["outOfHomeDuration:tmax"] * outOfHomeDuration * tmax + coefficients(DtimestmaxpluspointInTimevsI.lm)["factor(pointInTime)3"] * pointInTime , y = .data[[weekdayString]], fill = tmax), size = 3, shape = 21)


nestedplotlist[[paste0("Regression_DtimestmaxpluspITvsI", weekday)]] <- DtimestmaxpluspointInTimevsI.lm
nestedplotlist[[paste0("Plot_DtimestmaxpluspITvsI", weekday)]] <- plot22
nestedplotlist[[paste0("ResvsFittedValues_DtimestmaxpluspITvsI", weekday)]] <- function(){
plot(DtimestmaxpluspointInTimevsI.lm, which=1)
}
nestedplotlist[[paste0("Qqplot_DtimestmaxpluspITvsI", weekday)]] <- function(){
plot(DtimestmaxpluspointInTimevsI.lm, which=2)
}
nestedplotlist[[paste0("ScaleLoc_DtimestmaxpluspITvsI", weekday)]] <- function(){
plot(DtimestmaxpluspointInTimevsI.lm, which=3)
}
nestedplotlist[[paste0("Cooksdist_DtimestmaxpluspITvsI", weekday)]] <- function(){
plot(DtimestmaxpluspointInTimevsI.lm, which=4)
}
nestedplotlist[[paste0("ActualvsEstimation_DtimestmaxpluspITvsI", weekday)]] <- plot23
}

grid.arrange(nestedplotlist[["Plot_DtimestmaxpluspITvsIMon_1week_lag"]],nestedplotlist[["Plot_DtimestmaxpluspITvsISun"]],nestedplotlist[["Plot_DtimestmaxpluspITvsISat"]],nestedplotlist[["Plot_DtimestmaxpluspITvsIFri"]], nestedplotlist[["Plot_DtimestmaxpluspITvsIThu"]], nestedplotlist[["Plot_DtimestmaxpluspITvsIWed"]], nestedplotlist[["Plot_DtimestmaxpluspITvsITue"]], nestedplotlist[["Plot_DtimestmaxpluspITvsIMon"]], nrow=3)
grid.arrange(nestedplotlist[["ActualvsEstimation_DtimestmaxpluspITvsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_DtimestmaxpluspITvsISun"]],nestedplotlist[["ActualvsEstimation_DtimestmaxpluspITvsISat"]],nestedplotlist[["ActualvsEstimation_DtimestmaxpluspITvsIFri"]], nestedplotlist[["ActualvsEstimation_DtimestmaxpluspITvsIThu"]], nestedplotlist[["ActualvsEstimation_DtimestmaxpluspITvsIWed"]], nestedplotlist[["ActualvsEstimation_DtimestmaxpluspITvsITue"]], nestedplotlist[["ActualvsEstimation_DtimestmaxpluspITvsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimation_DtimestmaxpluspITvsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_DtimestmaxpluspITvsISun"]],nestedplotlist[["ActualvsEstimation_DtimestmaxpluspITvsISat"]],nestedplotlist[["ActualvsEstimation_DtimestmaxpluspITvsIFri"]], nestedplotlist[["ActualvsEstimation_DtimestmaxpluspITvsIThu"]], nestedplotlist[["ActualvsEstimation_DtimestmaxpluspITvsIWed"]], nestedplotlist[["ActualvsEstimation_DtimestmaxpluspITvsITue"]], nestedplotlist[["ActualvsEstimation_DtimestmaxpluspITvsIMon"]], nrow=3)
#g <- arrangeGrob(nestedplotlist[["Plot_DtimestmaxpluspITvsIMon_1week_lag"]],nestedplotlist[["Plot_DtimestmaxpluspITvsISun"]],nestedplotlist[["Plot_DtimestmaxpluspITvsISat"]],nestedplotlist[["Plot_DtimestmaxpluspITvsIFri"]], nestedplotlist[["Plot_DtimestmaxpluspITvsIThu"]], nestedplotlist[["Plot_DtimestmaxpluspITvsIWed"]], nestedplotlist[["Plot_DtimestmaxpluspITvsITue"]], nestedplotlist[["Plot_DtimestmaxpluspITvsIMon"]], nrow=3)

#17) D*tmax vs I, where D is split into 5 parts
for (weekday in weekdays){
if (weekday == "Mon_1week_lag") {
weekdayString <- "changeOfIncidencelaggedMon"
formula.lm <- "changeOfIncidencelaggedMon ~ outOfHomeDuration + s(tmax,2)"
} else {
formula.lm <- paste0("changeOfIncidencelagged", weekday, "2", " ~ outOfHomeDuration + s(tmax,2)")
weekdayString <- paste0("changeOfIncidencelagged", weekday, "2")
}
DtimestmaxvsI.lm <- gam(formula = formula.lm, data = joinedDataFrame) #Regression
if (weekday == "Mon") {
title <- "14 Day lag"
} else if (weekday == "Tue") {
title <- "13 Day lag"
} else if (weekday == "Wed") {
title <- "12 Day lag"
} else if (weekday == "Thu") {
title <- "11 Day lag"
} else if (weekday == "Fri") {
title <- "10 Day lag"
} else if (weekday == "Sat") {
title <- "9 Day lag"
} else if (weekday == "Sun") {
title <- "8 Day lag"
} else if (weekday == "Mon_1week_lag") {
title <- "7 Day lag"
}
#1st plot; x = outOfHomeDuration, y = changeOfIncidence, color = tmax
plot22 <- ggplot(data=joinedDataFrame, aes(y = .data[[weekdayString]], x = outOfHomeDuration, color = tmax)) +
geom_point() +
geom_smooth(method = "lm") +
ggtitle(title) +
theme_minimal()

plot23 <- ggplot(data = joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
geom_smooth(aes(x=coefficients(DtimestmaxvsI.lm)["(Intercept)"] + coefficients(DtimestmaxvsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DtimestmaxvsI.lm)["tmax"] * tmax + coefficients(DtimestmaxvsI.lm)["outOfHomeDuration:tmax"] * outOfHomeDuration * tmax , y = .data[[weekdayString]], color="lightgrey"), size=2, method = "lm", se=FALSE) +
ggtitle(title) +
xlab("Intercept + a * outOfHomeDuration + b * tmax + c * oOHD * tmax") +
ylab("changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
theme_minimal() +
scale_color_identity(labels = c("x=y", "Regression line"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank()) +
geom_point(aes(x=coefficients(DtimestmaxvsI.lm)["(Intercept)"] + coefficients(DtimestmaxvsI.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DtimestmaxvsI.lm)["tmax"] * tmax + coefficients(DtimestmaxvsI.lm)["outOfHomeDuration:tmax"] * outOfHomeDuration * tmax , y = .data[[weekdayString]], fill = tmax), size = 3, shape = 21)


nestedplotlist[[paste0("Regression_DtimestmaxvsI", weekday)]] <- DtimestmaxvsI.lm
nestedplotlist[[paste0("Plot_DtimestmaxvsI", weekday)]] <- plot22
nestedplotlist[[paste0("ResvsFittedValues_DtimestmaxvsI", weekday)]] <- function(){
plot(DtimestmaxvsI.lm, which=1)
}
nestedplotlist[[paste0("Qqplot_DtimestmaxvsI", weekday)]] <- function(){
plot(DtimestmaxvsI.lm, which=2)
}
nestedplotlist[[paste0("ScaleLoc_DtimestmaxvsI", weekday)]] <- function(){
plot(DtimestmaxvsI.lm, which=3)
}
nestedplotlist[[paste0("Cooksdist_DtimestmaxvsI", weekday)]] <- function(){
plot(DtimestmaxvsI.lm, which=4)
}
nestedplotlist[[paste0("ActualvsEstimation_DtimestmaxvsI", weekday)]] <- plot23
}

grid.arrange(nestedplotlist[["Plot_DtimestmaxvsIMon_1week_lag"]],nestedplotlist[["Plot_DtimestmaxvsISun"]],nestedplotlist[["Plot_DtimestmaxvsISat"]],nestedplotlist[["Plot_DtimestmaxvsIFri"]], nestedplotlist[["Plot_DtimestmaxvsIThu"]], nestedplotlist[["Plot_DtimestmaxvsIWed"]], nestedplotlist[["Plot_DtimestmaxvsITue"]], nestedplotlist[["Plot_DtimestmaxvsIMon"]], nrow=3)
grid.arrange(nestedplotlist[["ActualvsEstimation_DtimestmaxvsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_DtimestmaxvsISun"]],nestedplotlist[["ActualvsEstimation_DtimestmaxvsISat"]],nestedplotlist[["ActualvsEstimation_DtimestmaxvsIFri"]], nestedplotlist[["ActualvsEstimation_DtimestmaxvsIThu"]], nestedplotlist[["ActualvsEstimation_DtimestmaxvsIWed"]], nestedplotlist[["ActualvsEstimation_DtimestmaxvsITue"]], nestedplotlist[["ActualvsEstimation_DtimestmaxvsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimation_DtimestmaxvsIMon_1week_lag"]],nestedplotlist[["ActualvsEstimation_DtimestmaxvsISun"]],nestedplotlist[["ActualvsEstimation_DtimestmaxvsISat"]],nestedplotlist[["ActualvsEstimation_DtimestmaxvsIFri"]], nestedplotlist[["ActualvsEstimation_DtimestmaxvsIThu"]], nestedplotlist[["ActualvsEstimation_DtimestmaxvsIWed"]], nestedplotlist[["ActualvsEstimation_DtimestmaxvsITue"]], nestedplotlist[["ActualvsEstimation_DtimestmaxvsIMon"]], nrow=3)
#g <- arrangeGrob(nestedplotlist[["Plot_DtimestmaxvsIMon_1week_lag"]],nestedplotlist[["Plot_DtimestmaxvsISun"]],nestedplotlist[["Plot_DtimestmaxvsISat"]],nestedplotlist[["Plot_DtimestmaxvsIFri"]], nestedplotlist[["Plot_DtimestmaxvsIThu"]], nestedplotlist[["Plot_DtimestmaxvsIWed"]], nestedplotlist[["Plot_DtimestmaxvsITue"]], nestedplotlist[["Plot_DtimestmaxvsIMon"]], nrow=3)
