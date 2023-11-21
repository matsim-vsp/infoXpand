library(tidyverse)
library(lubridate)
library(readxl)
library(httr)
library(gridExtra)
library(ggiraphExtra)
library(leaps)

#### This r script reads in and prepares mobility data (daily "out of home duration" in hours per person) on a national level
#### The prepared data is then used for further analysis
#### Author: S. Paltra @ TU Berlin

# Reading in mobility data on federal state level, weekly data
mobility_data <- read_delim("https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/episim/mobilityData/bundeslaender/mobilityData_OverviewBL_weekly.csv")

# Cleaning, turning date column into right format, renaming columns
mobility_data$date <- paste(substr(mobility_data$date, start = 1, stop = 4), substr(mobility_data$date, start = 5, stop = 6), as.character(substr(mobility_data$date, start = 7, stop = 8)), sep ="-" )
mobility_data$date <- as.Date(mobility_data$date)
colnames(mobility_data)[2] <- "Bundesland"
colnames(mobility_data)[1] <- "Date"

# Filtering for national data
mobility_data$Bundesland[mobility_data$Bundesland == "Deutschland"] <- "Gesamt"