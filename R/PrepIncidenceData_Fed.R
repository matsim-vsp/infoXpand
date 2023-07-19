library(tidyverse)
library(lubridate)
library(readxl)
library(httr)
library(gridExtra)
library(ggiraphExtra)
library(leaps)

#Using R 4.1.1

#Reading incidence data in, data hereby comes from RKI
#Note: Older versions of this code use multiple data sources as I was not aware of this repository
incidence_data <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland/main/COVID-19-Faelle_7-Tage-Inzidenz_Bundeslaender.csv")
incidence_data <- incidence_data %>%
                filter(Altersgruppe == "00+")
incidence_data <- incidence_data[, c("Meldedatum", "Inzidenz_7-Tage", "Bundesland_id")]
colnames(incidence_data) <- c("Date", "Incidence", "Bundesland_id")

# Here, we are filtering only for 2020
incidence_data <- incidence_data %>% filter(Date < as.Date("2021-03-01")) %>%
                  mutate(year = year(Date)) %>%
                  mutate(week = isoweek(Date)) %>%
                  mutate(weekday = wday(Date, week_start = 1))

incidence_data <- incidence_data %>%  group_by(year, week, Bundesland_id) %>%
  summarise(Date = max(Date), Incidence = mean(Incidence))

incidence_data  <- incidence_data  %>%
                    ungroup() %>%
                    arrange(Bundesland_id)

incidence_data <- incidence_data %>%
                    mutate(cOI = as.double(lead(incidence_data$Incidence)/incidence_data$Incidence)) %>% 
                    mutate(cOI_1weekbefore = lead(cOI)) %>%
                    mutate(cOI_2weeksbefore = lead(cOI_1weekbefore)) %>%
                    mutate(cOI_3weeksbefore = lead(cOI_2weeksbefore)) %>%
                    mutate(cOI_4weeksbefore = lead(cOI_3weeksbefore)) %>%
                    mutate(cOI_5weeksbefore = lead(cOI_4weeksbefore)) %>%
                    filter(Date < as.Date("2020-12-31")) %>%
                    filter(Date > as.Date("2020-02-27"))
