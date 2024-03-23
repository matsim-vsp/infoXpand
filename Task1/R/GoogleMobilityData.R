library(tidyverse)
library(lubridate)

### This r scripts reads in the google mobility data, filters the data for germany, aggregates it on a weekly level and creates a scatter plot containing the mobility data for the 16 federal states
#### Author: S. Paltra @ TU Berlin

#Read in Google mobility data
#The google mobility data can be found here: https://www.google.com/covid19/mobility/
google_mobility_data <- read_csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", col_select=c("sub_region_1", "country_region_code", "country_region", "date", "retail_and_recreation_percent_change_from_baseline", "grocery_and_pharmacy_percent_change_from_baseline", "parks_percent_change_from_baseline", "transit_stations_percent_change_from_baseline", "workplaces_percent_change_from_baseline", "residential_percent_change_from_baseline"))

#Filter for Germany
google_mobility_data <- filter(google_mobility_data, country_region == "Germany")
google_mobility_data <- google_mobility_data %>% mutate(sub_region_1 = ifelse(is.na(sub_region_1), "Germany", sub_region_1))

#Data is provided on a daily level, but we look at it on a weekly level
google_mobility_data <- mutate(google_mobility_data, year = year(date))
google_mobility_data <- mutate(google_mobility_data, week = isoweek(date))
google_mobility_data_weekly <- google_mobility_data %>% group_by(week, year, sub_region_1) %>%
  summarise(date = max(date),
            retail_and_recreation = mean(retail_and_recreation_percent_change_from_baseline, na.rm = TRUE),
            grocery_and_pharmacy = mean(grocery_and_pharmacy_percent_change_from_baseline, na.rm = TRUE),
            parks = mean(parks_percent_change_from_baseline, na.rm = TRUE),
            transit_stations = mean(transit_stations_percent_change_from_baseline, na.rm = TRUE),
            workplaces = mean(workplaces_percent_change_from_baseline, na.rm = TRUE),
            residential = mean(residential_percent_change_from_baseline, na.rm = TRUE))

google_mobility_data_weekly <- google_mobility_data_weekly %>% filter(sub_region_1 == "Germany") %>% filter(date > "2020-03-01") %>% filter(date < "2020-12-31")

#Turning the data into tidy format
google_mobility_data_weekly <- pivot_longer(google_mobility_data_weekly , cols = c("retail_and_recreation", "grocery_and_pharmacy", "parks", "transit_stations", "workplaces", "residential"), names_to="category", values_to = "changeFromBaseline")

#Plotting the data for 2020 for all 16 federal German states and for all of Germany
google_mobility_data_weekly %>% filter(date < "2021-01-01") %>% # filter(category!="parks") %>%
ggplot() +
  geom_point(mapping = aes(x = date, y = changeFromBaseline, col = category)) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom") +
  ylab("Percentage Change From Baseline") +
  xlab("Date") +
  facet_wrap(vars(sub_region_1))
