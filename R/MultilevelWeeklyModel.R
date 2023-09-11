library(tidyverse)
library(lme4)
library(lmerTest)

#Need to read in incidence_data on a federal state level
incidence_data_fedState <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland/main/COVID-19-Faelle_7-Tage-Inzidenz_Bundeslaender.csv")
incidence_data_fedState <- incidence_data_fedState %>%
                filter(Altersgruppe == "00+")
incidence_data_fedState <- incidence_data_fedState[, c("Meldedatum", "Inzidenz_7-Tage", "Bundesland_id")]
colnames(incidence_data_fedState) <- c("Date", "Incidence", "Bundesland")

# Here, we are filtering only for 2020
incidence_data_fedState <- filter(incidence_data_fedState, Date < as.Date("2021-01-01")) %>%
                  mutate(year = year(Date)) %>%
                  mutate(week = isoweek(Date)) %>%
                  mutate(weekday = wday(Date, week_start = 1))

incidence_data_fedState <- incidence_data_fedState %>%  group_by(year, week, Bundesland) %>%
  summarise(Date = max(Date), Incidence = mean(Incidence))

incidence_data_fedState <- incidence_data_fedState %>% ungroup()

incidence_data_fedState <- incidence_data_fedState[order(incidence_data_fedState$Bundesland,decreasing=TRUE),]

incidence_data_fedState <- incidence_data_fedState %>%
                    mutate(cOI = as.double(lead(incidence_data_fedState$Incidence)/incidence_data_fedState$Incidence)) %>% 
                    mutate(cOI_1weekbefore = lead(cOI)) %>%
                    mutate(cOI_2weeksbefore = lead(cOI_1weekbefore)) %>%
                    mutate(cOI_3weeksbefore = lead(cOI_2weeksbefore)) %>%
                    mutate(cOI_4weeksbefore = lead(cOI_3weeksbefore)) %>%
                    mutate(cOI_5weeksbefore = lead(cOI_4weeksbefore))

incidence_data_fedState <- incidence_data_fedState %>% mutate(Bundesland = case_when(Bundesland == "01" ~ "Schleswig-Holstein",
                                                                      Bundesland == "02" ~ "Hamburg",
                                                                      Bundesland == "03" ~ "Niedersachsen",
                                                                      Bundesland == "04" ~ "Bremen",
                                                                      Bundesland == "05" ~ "Nordrhein-Westfalen",
                                                                      Bundesland == "06" ~ "Hessen",
                                                                      Bundesland == "07" ~ "Rheinland-Pfalz",
                                                                      Bundesland == "08" ~ "Baden-Württemberg",
                                                                      Bundesland == "09" ~ "Bayern",
                                                                      Bundesland == "10" ~ "Saarland",
                                                                      Bundesland == "11" ~ "Berlin",
                                                                      Bundesland == "12" ~ "Brandenburg",
                                                                      Bundesland == "13" ~ "Mecklenburg-Vorpommern",
                                                                      Bundesland == "14" ~ "Sachsen",
                                                                      Bundesland == "15" ~ "Sachsen-Anhalt",
                                                                      Bundesland == "16" ~ "Thüringen"))

source("PrepIncidenceData.R")

incidence_data <- left_join(incidence_data_fedState, incidence_data, by = "Date")

incidence_data <- select(incidence_data, year.x, week.x, Bundesland.x, Date, Incidence.x, cOI.x, cOI_1weekbefore.x, cOI_2weeksbefore.x, cOI_3weeksbefore.x, cOI_4weeksbefore.x, cOI_5weeksbefore.x, Incidence.y, cOI.y, cOI_1weekbefore.y, cOI_2weeksbefore.y, cOI_3weeksbefore.y, cOI_4weeksbefore.y, cOI_5weeksbefore.y)
colnames(incidence_data) <- c("year", "week", "Bundesland", "Date", "Incidence_Fed", "cOI_Fed", "cOI_1weekbefore_Fed", "cOI_2weeksbefore_Fed", "cOI_3weeksbefore_Fed", "cOI_4weeksbefore_Fed", "cOI_5weeksbefore_Fed", "Incidence_Nat", "cOI_Nat", "cOI_1weekbefore_Nat", "cOI_2weeksbefore_Nat", "cOI_3weeksbefore_Nat", "cOI_4weeksbefore_Nat", "cOI_5weeksbefore_Nat")

source("PrepMobilityData.R")

mobility_data_Nat <- mobility_data %>% filter(Bundesland == "Gesamt")
mobility_data <- mobility_data %>% filter(Bundesland != "Gesamt")
mobility_data <- left_join(mobility_data, mobility_data_Nat, by = "Date")
mobility_data <- select(mobility_data, -Bundesland.y)
colnames(mobility_data) <- c("Date", "Bundesland", "outOfHomeDuration_Fed", "percChange_Fed", "outOfHomeDuration_Nat", "percChange_Nat")

joinedDataFrame <- inner_join(incidence_data, mobility_data, by = c("Date", "Bundesland"))

source("PrepWeatherData.R")

joinedDataFrame <- left_join(joinedDataFrame, weather_data_all, by = c("Date", "Bundesland"))

joinedDataFrame <- joinedDataFrame %>% filter(Bundesland != "Gesamt")

#Before starting the analysis, let's plot the data with regression lines for the different federal states
joinedDataFrame %>% filter(cOI_2weeksbefore_Fed < 2) %>%
ggplot(aes(x = outOfHomeDuration_Fed, y = cOI_2weeksbefore_Fed, col = as.factor(Bundesland))) + geom_point(size= 1,
             alpha    = .7, position = "jitter") +
  geom_smooth(method = lm, se = FALSE, size = 1.5, linetype = 1, alpha = .7) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title    = "Linear Relationship Between outOfHomeDuration and changeOfIncidence", 
       subtitle = "by federal State") +
  scale_color_manual(name   = "Federal State",
                     labels = c("BW", "BY", "BE", "BB", "HB", "HH", "HE", "MP", "NI", "NW", "RP", "SL", "SN", "ST", "SH", "TH"),
                     values = c("aquamarine", "lightblue", "pink", "brown1", "darkgoldenrod1", "chartreuse4", "darkgreen", "deepskyblue", "darkmagenta", "darkred", "deeppink", "darkslateblue", "lightpink3", "darkgray","peachpuff", "seagreen4", "plum"))


#InterceptOnlyModel
interceptonlymodel <- lmer(formula = cOI_2weeksbefore_Fed ~ 1 + (1 | Date),
                         data = joinedDataFrame)

summary(interceptonlymodel)

#First level predictors
firstlevelmodel <- lmer(formula = cOI_2weeksbefore_Fed ~ 1 + outOfHomeDuration_Fed + outdoorFraction + (1|Date), 
               data    = joinedDataFrame)

summary(firstlevelmodel)

#Second level predictors
#First: We need to manipulate the dataframe
firstsecondlevelmodel <- lmer(formula = cOI_2weeksbefore_Fed ~ 1 + outOfHomeDuration_Fed + outdoorFraction + outOfHomeDuration_Nat + (1|Date), 
               data    = joinedDataFrame)

summary(firstsecondlevelmodel)
