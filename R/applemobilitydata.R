library(tidyverse)
library(lubridate)

# In this script we are using mobility data which was collected and provided by Apple from April 2020 until April 2022.
# As the data is no longer provided on Apple's website, we are obtaining the data from https://github.com/ActiveConclusion/COVID19_mobility

#Reading in the data, filtering for Germany and removing the first date as this is representing baseline
apple_mobility_data <- read_csv("https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/apple_reports/apple_mobility_report.csv") %>%
filter(country == "Germany") %>%
filter(date != min(date))

#Computing weekly averages
apple_mobility_data <- apple_mobility_data %>%
    mutate(week = week(date)) %>%
    mutate(year = year(date)) %>%
    group_by(`sub-region`, week, year) %>%
    summarise(date = max(date), weekly_change_driving = mean(driving), weekly_change_transit = mean(transit, na.rm=TRUE), weekly_change_walking = mean(walking))

#Plotting the weekly changes for the different federal states
    cols <- c("driving" = "red", "transit" = "purple", "walking" = "blue")
    ggplot(data = apple_mobility_data) +
    geom_line(aes(x = date, y = weekly_change_driving, color = "driving")) +
    geom_line(aes(x = date, y = weekly_change_transit, color = "transit")) +
    geom_line(aes(x = date, y = weekly_change_walking, color = "walking")) +
    scale_color_manual(values = cols) +
    labs(title = "Weekly average change in % compared to baseline (January 13th, 2020)",
        x = "Date",
        y = "Change in %") +
    theme_minimal() +
    theme(legend.position = "bottom", axis.text.x = element_text(angle=45, hjust = 1), legend.title = element_blank()) +
    facet_wrap(vars(`sub-region`)) +
    scale_x_date(breaks = "6 months")