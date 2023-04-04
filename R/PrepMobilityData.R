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

mobility_data$Bundesland[mobility_data$Bundesland == "Deutschland"] <- "Gesamt"

