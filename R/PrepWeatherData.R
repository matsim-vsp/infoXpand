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
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Gesamt", 10382)

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
  group_by(year, week, Bundesland) %>%
  summarise(Bundesland = Bundesland, Date = min(Date)+4, tmax = mean(tmax), tavg = mean(tavg), prcp = mean(prcp)) %>% distinct()

#The method above sets the temp for "Gesamt" equal to the temp for Berlin.
for(date in unique(weather_data_all$Date)){
filtered <- filter(weather_data_all, Date == date) %>%
            filter(Bundesland != "Gesamt")

weather_data_all$tmax[weather_data_all$Date == date & weather_data_all$Bundesland == "Gesamt"] <- mean(filtered$tmax)
weather_data_all$tavg[weather_data_all$Date == date & weather_data_all$Bundesland == "Gesamt"] <- mean(filtered$tavg)
weather_data_all$prcp[weather_data_all$Date == date & weather_data_all$Bundesland == "Gesamt"] <- mean(filtered$prcp)
}

#Converting the weather data to an "outdoor fraction"
#Below a certain temperature, everything happens indoords, above a certain temperature everything happens outdoors and in between we linearize
#Compare https://doi.org/10.1371/journal.pone.0259037 for computation of outdoor fraction
weather_data_all <- weather_data_all %>% mutate(TStar = case_when(Date < "2020-03-01" ~ 17.5, 
                                                                  Date >= "2020-03-01" & Date <= "2020-10-01" ~ as.numeric(Date-as.Date("2020-03-01"))/7*7.5/31 + 17.5,
                                                                  Date > "2020-10-01" ~ 25))

weather_data_all <- mutate(weather_data_all, outdoorFraction = case_when(TStar + 5 >= tmax & tmax >= TStar - 5 ~ -1/(10.5) * tmax + (TStar+5)/10.5 + 1,
                                                                   tmax < TStar - 5 ~ 2,
                                                                   tmax > TStar + 5 ~ 1))
#Alternative to compute outdoor fraction
weather_data_all <- weather_data_all %>% mutate(outdoorFraction2 = case_when(TStar + 5 >= tmax & tmax >= TStar - 5 ~  (tmax - (TStar-5))/10,
                                                                   tmax < TStar - 5 ~ 0,
                                                                  tmax > TStar + 5 ~ 1))                                                                 
