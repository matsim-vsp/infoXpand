library(tidyverse)
library(lubridate)
library(readxl)
library(httr)
library(gridExtra)
library(ggiraphExtra)
library(leaps)
library(stats)

#### This r script reads in and prepares climate/weather data on a national level
#### The prepared data is then used for further analysis
#### Author: S. Paltra @ TU Berlin

# We read in data on a federal state level, to then compute a temperature to represent Germany on a national level
# Here are the  16 states and their capitals. Weather IDs are taken from meteostat and were extracted manually
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

# Setting up da data frame containing the states and the corresponding weather IDs
dict_state_id <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(dict_state_id) <- c("Bundesland", "ID", "EinwohnerInnen")
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Baden-Württemberg", 10738, 11124642)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Bayern", 10865, 13176989)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Berlin", 10382, 3677472)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Brandenburg", 10379, 2537868)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Bremen", 10224, 676463)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Hamburg", 10147, 1853935)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Hessen", 10633, 6295017)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Mecklenburg-Vorpommern", 10162, 1611160)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Niedersachsen", 10338, 8027031)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Nordrhein-Westfalen", 10400, 17924591)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Rheinland-Pfalz", "D3137", 4106485)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Saarland", "D6217", 982348)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Sachsen", "D1051", 4043002)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Sachsen-Anhalt", 10361, 2169253)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Schleswig-Holstein", 10044, 2922005)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Thüringen", 10554, 2108863)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Gesamt", 10382, 83237124)

# Reading in weather data
# For now, the national weather ("Gesamt") is set equal to the weather in Berlin
weather_data_all <- data.frame(matrix(nrow = 0, ncol = 5))
for (state in 1 : 17) {
ID <- dict_state_id[as.integer(state), 2]
weather_data <- read_delim(paste0("https://bulk.meteostat.net/daily/", ID, ".csv.gz"))
colnames(weather_data) <- c("Date", "tavg", "tmin", "tmax", "prcp", "snow", "wdir", "wspd", "wpgt", "pres", "tsun")

weather_data$Date <- as.Date(weather_data$Date)
weather_data <- weather_data[, c("Date", "tmax", "tavg", "prcp")]
weather_data$Bundesland <- dict_state_id[as.integer(state), 1]
weather_data$EinwohnerInnen <- dict_state_id[as.integer(state), 3]
weather_data$EinwohnerInnenRelativ <- as.integer(dict_state_id[as.integer(state), 3])/83237124


weather_data_all <- rbind(weather_data_all, weather_data)
}

# Computing weekly averages
weather_data_all <- filter(weather_data_all, Date < "2021-01-01") %>%
filter(Date > "2020-01-01") %>%
  mutate(week = week(Date)) %>%
  mutate(year = year(Date)) %>%
  group_by(year, week, Bundesland) %>%
  summarise(Bundesland = Bundesland, Date = min(Date)+4, tmax = mean(tmax), tavg = mean(tavg), prcp = mean(prcp), EinwohnerInnen, EinwohnerInnenRelativ) %>% distinct()

# A weighted average is computed for the national weather and replaces the read-in value
for(date in unique(weather_data_all$Date)){
filtered <- filter(weather_data_all, Date == date) %>%
            filter(Bundesland != "Gesamt")

weather_data_all$tmax[weather_data_all$Date == date & weather_data_all$Bundesland == "Gesamt"] <- weighted.mean(filtered$tmax, filtered$EinwohnerInnenRelativ)
weather_data_all$tavg[weather_data_all$Date == date & weather_data_all$Bundesland == "Gesamt"] <- weighted.mean(filtered$tavg, filtered$EinwohnerInnenRelativ)
weather_data_all$prcp[weather_data_all$Date == date & weather_data_all$Bundesland == "Gesamt"] <- weighted.mean(filtered$prcp, filtered$EinwohnerInnenRelativ)
}

# Temperature is converted to the so-called "outdoor fraction" (Fraction of leisure activities the population performs outside)
# It is assumed that the outdoor effect saturates. Between the two saturation ends, we linearize
# See https://doi.org/10.1371/journal.pone.0259037 for computation of outdoor fraction
weather_data_all <- weather_data_all %>% mutate(TStar = case_when(Date < "2020-03-01" ~ 17.5, 
                                                                  Date >= "2020-03-01" & Date <= "2020-10-01" ~ as.numeric(Date-as.Date("2020-03-01"))/7*7.5/31 + 17.5,
                                                                  Date > "2020-10-01" ~ 25))

weather_data_all <- mutate(weather_data_all, outdoorFraction = case_when(TStar + 5 >= tmax & tmax >= TStar - 5 ~ -1/(10.5) * tmax + (TStar+5)/10.5 + 1,
                                                                   tmax < TStar - 5 ~ 2,
                                                                   tmax > TStar + 5 ~ 1))
# Alternative to compute outdoor fraction
weather_data_all <- weather_data_all %>% mutate(outdoorFraction2 = case_when(TStar + 5 >= tmax & tmax >= TStar - 5 ~  (tmax - (TStar-5))/10,
                                                                   tmax < TStar - 5 ~ 0,
                                                                  tmax > TStar + 5 ~ 1))  

# Computation of indoo fraction (1- outdoor fraction = indoor fraction)
weather_data_all <- weather_data_all %>% mutate(indoorFraction = case_when(TStar + 5 >= tmax & tmax >= TStar - 5 ~  1-(tmax - (TStar-5))/10,
                                                                   tmax < TStar - 5 ~ 1,
                                                                  tmax > TStar + 5 ~ 0))                                                                                                                                   
