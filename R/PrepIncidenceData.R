library(tidyverse)
library(lubridate)
library(readxl)
library(httr)
library(gridExtra)
library(ggiraphExtra)
library(leaps)

#Using R 4.1.1

#Reading incidence data in, data hereby comes from RKI
#Note: Older versions of this code use multiple data sources as I was not aware
incidence_data <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland/main/COVID-19-Faelle_7-Tage-Inzidenz_Deutschland.csv")
incidence_data <- incidence_data %>%
                filter(Altersgruppe == "00+") %>%
                mutate(Bundesland = "Gesamt")
incidence_data <- incidence_data[, c("Meldedatum", "Inzidenz_7-Tage", "Bundesland")]
colnames(incidence_data) <- c("Date", "Incidence", "Bundesland")

# Here, we are filtering only for 2020
incidence_data <- filter(incidence_data, Date < as.Date("2021-01-01")) %>%
                  mutate(year = year(Date)) %>%
                  mutate(week = isoweek(Date)) %>%
                  mutate(weekday = wday(Date, week_start = 1))

incidence_data <- incidence_data %>%  group_by(year, week, Bundesland) %>%
  summarise(Date = max(Date), Incidence = mean(Incidence))

incidence_data  <- incidence_data  %>% ungroup()

incidence_data <- incidence_data %>%
                    mutate(cOI = as.double(lead(incidence_data$Incidence)/incidence_data$Incidence)) %>% 
                    mutate(cOI_1weekbefore = lead(cOI)) %>%
                    mutate(cOI_2weeksbefore = lead(cOI_1weekbefore)) %>%
                    mutate(cOI_3weeksbefore = lead(cOI_2weeksbefore)) %>%
                    mutate(cOI_4weeksbefore = lead(cOI_3weeksbefore)) %>%
                    mutate(cOI_5weeksbefore = lead(cOI_4weeksbefore))
