library(tidyverse)
library(lubridate)
library(scales)
library(ggokabeito)

#First step: setwd(".../R")

### SNZ data
source("PrepMobilityData.R") #Results are saved as mobility_data
colnames(mobility_data)[2] <- "Bundesland_GER"
mobility_data <- mobility_data %>% mutate(Bundesland_ENG = case_when(Bundesland_GER == "Baden-Württemberg" ~ "Baden-Württemberg",
                                                                    Bundesland_GER == "Bayern" ~ "Bavaria",
                                                                    Bundesland_GER == "Berlin" ~  "Berlin",
                                                                    Bundesland_GER == "Brandenburg" ~ "Brandenburg",
                                                                    Bundesland_GER == "Bremen" ~ "Bremen",
                                                                    Bundesland_GER == "Gesamt" ~ "Germany",
                                                                    Bundesland_GER == "Hamburg" ~ "Hamburg",
                                                                    Bundesland_GER == "Hessen" ~ "Hessen",
                                                                    Bundesland_GER == "Mecklenburg-Vorpommern" ~ "Mecklenburg-Vorpommern",
                                                                    Bundesland_GER == "Niedersachsen" ~ "Lower Saxony",
                                                                    Bundesland_GER == "Nordrhein-Westfalen" ~ "North Rhine-Westphalia",
                                                                    Bundesland_GER == "Rheinland-Pfalz" ~ "Rhineland-Palatinate",
                                                                    Bundesland_GER == "Saarland" ~ "Saarland" ,
                                                                    Bundesland_GER == "Sachsen" ~ "Saxony",
                                                                    Bundesland_GER == "Sachsen-Anhalt" ~ "Saxony-Anhalt",
                                                                    Bundesland_GER == "Schleswig-Holstein" ~ "Schleswig-Holstein",
                                                                    Bundesland_GER == "Thüringen" ~ "Thuringia"))

mobility_data <- mobility_data %>% mutate(atHomeDuration = 24 - outOfHomeDuration) %>% 
                  group_by(Bundesland_GER) %>%
                  mutate(percentageChangeatHome = (atHomeDuration - atHomeDuration[1])/atHomeDuration[1] * 100)


mobility_data %>% filter(Bundesland_GER != "Gesamt") %>%
ggplot(aes(x = Date, y = percentageChangeComparedToBeforeCorona)) +
geom_line(size = 1.5, color = "blueviolet") +
facet_wrap(vars(Bundesland_ENG), nrow = 4) +
theme_bw() +
ylab("Percentage Change compared to baseline")



### Apple mobility data
source("AppleMobilityData.R") #Results are saved as apple_mobility_data
colnames(apple_mobility_data)[1] <- "Bundesland"
apple_mobility_data <- apple_mobility_data %>%
                        mutate(Bundesland_GER = case_when(Bundesland == "Baden-Württemberg" ~ "Baden-Württemberg",
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

apple_mobility_data %>% mutate(date = as.Date(date)) %>% filter(Bundesland != "Gesamt") %>% #ToDo: Doublecheck what columns actually mean and what you've done in the applemobilityscript. Also check, if numbers for e.g. Hamburg, transit, april 2022 ware plausible, read https://pubmed.ncbi.nlm.nih.gov/33772501/
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

### Google mobility data
source("GoogleMobilityData.R")
google_mobility_data <- google_mobility_data_weekly
colnames(google_mobility_data)[3] <- "Bundesland_ENG"
google_mobility_data <- google_mobility_data %>%
          mutate(Bundesland_GER = case_when(Bundesland_ENG == "Baden-Württemberg" ~ "Baden-Württemberg",
                                        Bundesland_ENG == "Bavaria" ~ "Bayern",
                                        Bundesland_ENG == "Berlin" ~ "Berlin",
                                        Bundesland_ENG == "Brandenburg" ~ "Brandenburg",
                                        Bundesland_ENG == "Bremen" ~ "Bremen",
                                        Bundesland_ENG == "Germany" ~ "Gesamt",
                                        Bundesland_ENG == "Hamburg" ~ "Hamburg",
                                        Bundesland_ENG == "Hessen" ~ "Hessen",
                                        Bundesland_ENG == "Lower Saxony" ~ "Niedersachsen",
                                        Bundesland_ENG == "Mecklenburg-Vorpommern" ~ "Mecklenburg-Vorpommern",
                                        Bundesland_ENG == "North Rhine-Westphalia" ~ "Nordrhein_Westfalen",
                                        Bundesland_ENG == "Rhineland-Palatinate" ~ "Rheinland-Pfalz",
                                        Bundesland_ENG == "Saarland" ~ "Saarland",
                                        Bundesland_ENG == "Saxony" ~ "Sachsen",
                                        Bundesland_ENG == "Saxony-Anhalt" ~ "Sachsen-Anhalt",
                                        Bundesland_ENG == "Schleswig-Holstein" ~ "Schleswig-Holstein",
                                        Bundesland_ENG == "Thuringia" ~ "Thüringen"))

google_mobility_data <- google_mobility_data %>%
  mutate(timeAtHome = (100+changeFromBaseline)/100*15.7) %>% #15.7 comes from https://www.sciencedirect.com/science/article/abs/pii/S1438463905000635?via%3Dihub 
  mutate(timeOutsideHome = 24 - timeAtHome)

google_mobility_data %>% filter(Bundesland_ENG != "Germany") %>%
  ggplot() +
  geom_line(aes(x = date, y = changeFromBaseline, col = category)) +
  facet_wrap(vars(Bundesland_ENG), nrow = 4) +
  theme_bw() +
  ylab("Average weekly change in percentage") +
  xlab("Date") +
  theme(legend.position = "bottom")

### Only looking at "residential" category
google_mobility_data %>% filter(Bundesland_ENG != "Germany") %>%
  filter(category == "residential") %>%
  mutate(mobility = 100 - changeFromBaseline) %>%
  ggplot() +
  geom_line(aes(x = date, y = mobility, col = category), size = 1.2) +
  facet_wrap(vars(Bundesland_ENG), nrow = 4) +
  theme_bw() +
  ylab("Average weekly change in percentage") +
  xlab("Date") +
  theme(legend.position = "none")

### Plotting both snz and google data in one plot (federal state level)
google_mobility_data <- google_mobility_data %>% filter(Bundesland_ENG != "Germany") %>%
  filter(category == "residential")

mobility_data <- mobility_data %>% filter(Bundesland_ENG != "Germany")

cols <- c("Senozon" = "#666666", "Google Mobility Report" = "darkorange")
ggplot() +
geom_line(data = mobility_data, aes(x=Date, y = percentageChangeatHome, colour="SNZ"), size = 1.2) +
geom_line(data = google_mobility_data, aes(x=date, y = changeFromBaseline, colour = "Google"), size = 1.2) +
facet_wrap(vars(Bundesland_ENG), nrow = 4) +
theme_bw() +
ylab("Average weekly change in percentage") +
xlab("Date") +
ggtitle("Change time at home") +
scale_colour_manual(name = "",
  values = cols,
) +
scale_x_date(date_labels = "%Y", breaks = scales::date_breaks("1 year")) +
theme(text = element_text(size = 13), legend.position = "bottom", legend.title = element_blank()) +
theme(axis.ticks.x = element_line(),
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))

#Plotting both snz and google data in one plot (national level)
google_mobility_data <- google_mobility_data %>% filter(Bundesland_ENG == "Germany") %>%
  filter(category == "residential") %>%
  filter(date < "2020-12-31") %>% filter(date > "2020-03-01")

mobility_data <- mobility_data %>% filter(Bundesland_ENG == "Germany") %>% filter(Date < "2021-01-01")

ggplot() +
geom_line(data = mobility_data, aes(x=Date, y = percentageChangeatHome, colour="SNZ"), size = 1.2) +
geom_line(data = google_mobility_data, aes(x=date, y = changeFromBaseline, colour = "Google"), size = 1.2) +
facet_wrap(vars(Bundesland_ENG), nrow = 4) +
theme_bw() +
ylab("Average weekly change in percentage") +
xlab("Date") +
ggtitle("Change time at home") +
scale_colour_manual(name = "",
  values = cols,
) +
scale_x_date(date_labels = "%Y", breaks = scales::date_breaks("1 year")) +
theme(legend.position = "bottom")

ggplot() +
geom_line(data = mobility_data, aes(x=Date, y = outOfHomeDuration, color="Senozon"), size = 1.2) +
geom_line(data = google_mobility_data, aes(x=date, y = timeOutsideHome, color = "Google Mobility Report"), size = 1.2) +
ylab("Daily Out Of Home \nDuration/Person") +
xlab("Date") +
scale_colour_manual(name = "",
  values = cols) +
theme_minimal() +
scale_x_date(date_breaks = "1 month", date_labels = "%d/%b/%y") +
theme(text = element_text(size = 13), legend.position = "bottom", legend.title = element_blank()) +
theme(axis.ticks.x = element_line(),
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))
