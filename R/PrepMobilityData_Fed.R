library(tidyverse)
library(lubridate)
library(readxl)
library(httr)
library(gridExtra)
library(ggiraphExtra)
library(leaps)

#Reading in Mobility data on federal state level, using weekly data
mobility_data <- read_delim("https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/episim/mobilityData/bundeslaender/mobilityData_OverviewBL_weekly.csv")

#Little bit of cleaning, turning date column into right format, renaming columns
mobility_data$date <- paste(substr(mobility_data$date, start = 1, stop = 4), substr(mobility_data$date, start = 5, stop = 6), as.character(substr(mobility_data$date, start = 7, stop = 8)), sep ="-" )
mobility_data$date <- as.Date(mobility_data$date)
colnames(mobility_data)[2] <- "Bundesland"
colnames(mobility_data)[1] <- "Date"

mobility_data <- mobility_data %>% filter(Bundesland != "Deutschland") %>%
                 mutate(Bundesland_id = case_when(Bundesland == "Schleswig-Holstein" ~ "01",
                                                                    Bundesland == "Hamburg" ~ "02",
                                                                    Bundesland == "Niedersachsen" ~ "03",
                                                                    Bundesland == "Bremen" ~ "04",
                                                                    Bundesland == "Nordrhein-Westfalen" ~ "05",
                                                                    Bundesland == "Hessen" ~ "06",
                                                                    Bundesland == "Rheinland-Pfalz" ~ "07",
                                                                    Bundesland == "Baden-Württemberg" ~ "08",
                                                                    Bundesland == "Bayern" ~ "09",
                                                                    Bundesland == "Saarland" ~ "10", 
                                                                    Bundesland == "Berlin" ~ "11", 
                                                                    Bundesland == "Brandenburg" ~ "12",
                                                                    Bundesland == "Mecklenburg-Vorpommern" ~ "13",
                                                                    Bundesland == "Sachsen" ~ "14",
                                                                    Bundesland == "Sachsen-Anhalt" ~ "15",
                                                                    Bundesland == "Thüringen" ~ "16"))
