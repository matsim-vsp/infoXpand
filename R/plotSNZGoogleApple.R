library(tidyverse)
library(lubridate)
library


#First step: setwd(".../R")
source("PrepMobilityData.R") #Results are saved as mobility_data

mobility_data %>% filter(Bundesland != "Gesamt") %>%
ggplot(aes(x = Date, y = outOfHomeDuration)) +
geom_line(size = 1.5) +
facet_wrap(vars(Bundesland), nrow = 4) +
theme_bw() +
ylab("outOfHomeDuration/Mobility")


source("AppleMobilityData.R")
view(apple_mobility_data)
colnames(apple_mobility_data)[1] <- "Bundesland"
apple_mobility_data <- apple_mobility_data %>% 
                        mutate(Bundesland = case_when(Bundesland == "Baden-Württemberg" ~ "Baden-Württemberg",
                                                      Bundesland == "Bavaria" ~ "Bayern",
                                                      Bundesland == "Berlin" ~ "Berlin",
                                                      Bundesland == "Brandenburg" ~ "Brandenburg",
                                                      Bundesland == "Bremen (state)" ~ "Bremen",
                                                      Bundesland == "Hamburg" ~ "Hamburg",
                                                      Bundesland == "Hesse" ~ "Hessen",
                                                      Bundesland == "Lower Saxony" ~ "Niedersachsen",
                                                      Bundesland == "Mecklenburg-Vorpommern" ~ "Mecklenburg-Vorpommern",
                                                      Bundesland == "North Rhine-Westphalia" ~ "Nordrhein-Westfalen",
                                                      Bundesland == "Rhineland-Palatinate" ~ "Rheinland-Pfalz",
                                                      Bundesland == "Saarland" ~ "Saarland",
                                                      Bundesland == "Saxony" ~ "Sachsen",
                                                      Bundesland == "Saxony-Anhalt" ~ "Sachsen-Anhalt",
                                                      Bundesland == "Schleswig-Holstein" ~ "Schleswig-Holstein",
                                                      Bundesland == "Thuringia" ~ "Thüringen",
                                                      Bundesland == "Total" ~ "Gesamt"))
cols <- c("Driving" = "blueviolet", "Transit" = "hotpink3", "Walking" = "darkorange2")

apple_mobility_data %>% mutate(date = as.Date(date)) %>% filter(Bundesland != "Gesamt") %>% #ToDo: Doublecheck what columns actually mean and what you've done in the applemobilityscript. Also check, if numbers for e.g. Hamburg, transit, april 2022 ware plausible
ggplot(aes(x = date)) +
geom_line(aes(y = weekly_change_driving, colour = "Driving"), size = 1.2) +
geom_line(aes(y = weekly_change_transit, colour = "Transit"), size = 1.2) +
geom_line(aes(y = weekly_change_walking, colour = "Walking"), size = 1.2) +
facet_wrap(vars(Bundesland), nrow = 4) +
theme_bw() +
scale_colour_manual(name = "",
  values = cols,
) +
scale_x_date(date_labels = "%Y", breaks = date_breaks("1 year")) +
theme(legend.position = "bottom") +
ylab("Weekly change in perentage")
