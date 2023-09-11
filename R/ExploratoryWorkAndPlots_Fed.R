library(tidyverse)
library(lubridate)
library(readxl)
library(httr)
library(grid)
library(gtable)
library(gridExtra)
library(ggiraphExtra)
library(leaps)
library(RColorBrewer)

# Incidence data from RKI is read in on a federal state level
incidence_data_fedState <- read_csv("https://raw.githubusercontent.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland/main/COVID-19-Faelle_7-Tage-Inzidenz_Bundeslaender.csv")
incidence_data_fedState <- incidence_data_fedState %>%
                filter(Altersgruppe == "00+")
incidence_data_fedState <- incidence_data_fedState[, c("Meldedatum", "Inzidenz_7-Tage", "Bundesland_id")]
colnames(incidence_data_fedState) <- c("Date", "Incidence", "Bundesland")

# Prepping incidence data on a national level
source("PrepIncidenceData.R")

incidence_data <- left_join(incidence_data_fedState, incidence_data, by = "Date")

incidence_data <- dplyr::select(incidence_data, year.x, week.x, Bundesland.x, Date, Incidence.x, cOI.x, cOI_1weekbefore.x, cOI_2weeksbefore.x, cOI_3weeksbefore.x, cOI_4weeksbefore.x, cOI_5weeksbefore.x, Incidence.y, cOI.y, cOI_1weekbefore.y, cOI_2weeksbefore.y, cOI_3weeksbefore.y, cOI_4weeksbefore.y, cOI_5weeksbefore.y)
colnames(incidence_data) <- c("year", "week", "Bundesland", "Date", "Incidence_Fed", "cOI_Fed", "cOI_1weekbefore_Fed", "cOI_2weeksbefore_Fed", "cOI_3weeksbefore_Fed", "cOI_4weeksbefore_Fed", "cOI_5weeksbefore_Fed", "Incidence_Nat", "cOI_Nat", "cOI_1weekbefore_Nat", "cOI_2weeksbefore_Nat", "cOI_3weeksbefore_Nat", "cOI_4weeksbefore_Nat", "cOI_5weeksbefore_Nat")

# Prepping mobility data on a national level
source("PrepMobilityData.R")

mobility_data_Nat <- mobility_data %>% filter(Bundesland == "Gesamt")
mobility_data <- mobility_data %>% filter(Bundesland != "Gesamt")
mobility_data <- left_join(mobility_data, mobility_data_Nat, by = "Date")
mobility_data <- dplyr::select(mobility_data, -Bundesland.y)
colnames(mobility_data) <- c("Date", "Bundesland", "outOfHomeDuration_Fed", "percChange_Fed", "outOfHomeDuration_Nat", "percChange_Nat")

joinedDataFrame <- inner_join(incidence_data, mobility_data, by = c("Date", "Bundesland"))

# Prepping weather data on a national level
source("PrepWeatherData.R")


joinedDataFrame <- left_join(joinedDataFrame, weather_data_all, by = c("Date", "Bundesland"))


# Filtering for 2020
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


#Plotting growthMultiplier, outOfHomeDuration, OutdoorFactor over time (for the federal states)
nestedplotlist <- list()

joinedDataFrame <- joinedDataFrame %>% filter(cOI_2weeksbefore_Fed < 2)
for (state in unique(joinedDataFrame$Bundesland)) {
incidencePlot <- ggplot(data = joinedDataFrame %>% filter(Bundesland == state), aes(x = Date, y = Incidence_Fed)) +
  geom_line(color = "cornflowerblue") +
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  ggtitle(toString(state)) 
changeOfIncidencePlot <- ggplot(data = joinedDataFrame %>% filter(Bundesland == state), aes(x = Date, y = cOI_2weeksbefore_Fed)) +
  geom_line(color = "cornflowerblue") +
  #ylab("Change of Incidence") +
  theme_minimal() +
  ylim(0, 2)+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  ggtitle("")
mobilityPlot <- ggplot(data = joinedDataFrame %>% filter(Bundesland == state), aes(x = Date, y = outOfHomeDuration_Fed)) +
  geom_line(color = "cornflowerblue") +
  #ylab("Out of home duration per person (in hours)") +
  theme_minimal() +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    ggtitle("") +
    ylim(5,9)
tempPlot <- ggplot(data = joinedDataFrame %>% filter(Bundesland == state), aes(x = Date, y = tmax)) +
  geom_line(color = "cornflowerblue") +
  theme_minimal() +
  #ylab("Maximal Temperature (in C°)") +
    ylim(0, 36)+
    theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
    ggtitle("")
outdoorFractionPlot <- ggplot(data = joinedDataFrame %>% filter(Bundesland == state), aes(x = Date, y = outdoorFraction2)) +
  geom_line(color = "cornflowerblue") +
  theme_minimal() +
  #ylab("Share Of Activities Performed Outside") +
  theme_minimal() +
  ylim(0,1)+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank()) +
  ggtitle("")
outOfHomevsIncidence <- ggplot(data = joinedDataFrame %>% filter(Bundesland == state), aes(x=outOfHomeDuration_Fed, y=cOI_2weeksbefore_Fed)) +
  geom_line(color = "cornflowerblue") +
  theme_minimal() +
theme(axis.title.x=element_blank(), axis.title.y=element_blank())
outOfHomeSquaredvsIncidence <- ggplot(data = joinedDataFrame %>% filter(Bundesland == state), aes(x=outOfHomeDuration_Fed*outOfHomeDuration_Fed, y=cOI_2weeksbefore_Fed)) +
  geom_line(color = "cornflowerblue") +
  theme_minimal()
outOfHomePolymvsIncidence <- ggplot(data = joinedDataFrame %>% filter(Bundesland == state), aes(x=outOfHomeDuration_Fed*outOfHomeDuration_Fed+outOfHomeDuration_Fed, y=cOI_2weeksbefore_Fed)) +
  geom_line(color = "cornflowerblue") +
  theme_minimal()
outOfHomePolymOutdoorvsIncidence <- ggplot(data = joinedDataFrame %>% filter(Bundesland == state), aes(x=outOfHomeDuration_Fed*outOfHomeDuration_Fed+outdoorFraction, y=cOI_2weeksbefore_Fed)) +
  geom_line(color = "cornflowerblue") +
  theme_minimal()
outOfHomeOutdoorIncidence <- ggplot(data = joinedDataFrame %>% filter(Bundesland == state), aes(x=outOfHomeDuration_Fed+outdoorFraction2, y=cOI_2weeksbefore_Fed)) +
  geom_line(color = "cornflowerblue") +
  theme_minimal()
outdoorIncidence <- ggplot(data = joinedDataFrame %>% filter(Bundesland == state), aes(x=outdoorFraction, y=cOI_2weeksbefore_Fed)) +
  geom_line(color = "cornflowerblue") +
  theme_minimal()
tmaxIncidence <- ggplot(data = joinedDataFrame %>% filter(Bundesland == state), aes(x=tmax, y=cOI_2weeksbefore_Fed)) +
  geom_line(color = "cornflowerblue") +
  theme_minimal()
nestedplotlist[[paste0("incidencePlot_", state)]] <- incidencePlot
nestedplotlist[[paste0("changeOfIncidencePlot_", state)]] <- changeOfIncidencePlot
nestedplotlist[[paste0("mobilityPlot_", state)]] <- mobilityPlot
nestedplotlist[[paste0("tempPlot_", state)]] <- tempPlot
nestedplotlist[[paste0("outdoorFractionPlot_", state)]] <- outdoorFractionPlot
nestedplotlist[[paste0("outOfHomevsIncidence_", state)]] <- outOfHomevsIncidence
nestedplotlist[[paste0("outOfHomeSquaredvsIncidence_", state)]] <- outOfHomeSquaredvsIncidence
nestedplotlist[[paste0("outOfHomePolymvsIncidence_", state)]] <- outOfHomePolymvsIncidence
nestedplotlist[[paste0("outOfHomePolymOutdoorvsIncidence_", state)]] <- outOfHomePolymOutdoorvsIncidence
nestedplotlist[[paste0("outOfHomeOutdoorvsIncidence_", state)]] <- outOfHomeOutdoorIncidence
nestedplotlist[[paste0("outdoorIncidence_", state)]] <- outdoorIncidence
nestedplotlist[[paste0("tmaxvsIncidence_", state)]] <- tmaxIncidence

}

g <- (arrangeGrob(nestedplotlist[["incidencePlot_Baden-Württemberg"]], nestedplotlist[["incidencePlot_Bayern"]], nestedplotlist[["incidencePlot_Berlin"]], nestedplotlist[["incidencePlot_Brandenburg"]],
    nestedplotlist[["incidencePlot_Bremen"]], nestedplotlist[["incidencePlot_Hamburg"]], nestedplotlist[["incidencePlot_Hessen"]], nestedplotlist[["incidencePlot_Mecklenburg-Vorpommern"]],
    nestedplotlist[["incidencePlot_Niedersachsen"]], nestedplotlist[["incidencePlot_Nordrhein-Westfalen"]], nestedplotlist[["incidencePlot_Rheinland-Pfalz"]], nestedplotlist[["incidencePlot_Saarland"]],
    nestedplotlist[["incidencePlot_Sachsen-Anhalt"]], nestedplotlist[["incidencePlot_Sachsen"]], nestedplotlist[["incidencePlot_Schleswig-Holstein"]], nestedplotlist[["incidencePlot_Thüringen"]],
    nrow=16,
    left = textGrob("7-Day-Incidence/100,000", rot = 90, vjust = 1)))
ggsave("ExploratoryAnalysis_Incidence.png", g, w = 2.75, h = 16, dpi = 300)

g <- (arrangeGrob(nestedplotlist[["changeOfIncidencePlot_Baden-Württemberg"]], nestedplotlist[["changeOfIncidencePlot_Bayern"]], nestedplotlist[["changeOfIncidencePlot_Berlin"]], nestedplotlist[["changeOfIncidencePlot_Brandenburg"]],
    nestedplotlist[["changeOfIncidencePlot_Bremen"]], nestedplotlist[["changeOfIncidencePlot_Hamburg"]], nestedplotlist[["changeOfIncidencePlot_Hessen"]], nestedplotlist[["changeOfIncidencePlot_Mecklenburg-Vorpommern"]],
    nestedplotlist[["changeOfIncidencePlot_Niedersachsen"]], nestedplotlist[["changeOfIncidencePlot_Nordrhein-Westfalen"]], nestedplotlist[["changeOfIncidencePlot_Rheinland-Pfalz"]], nestedplotlist[["changeOfIncidencePlot_Saarland"]],
    nestedplotlist[["changeOfIncidencePlot_Sachsen-Anhalt"]], nestedplotlist[["changeOfIncidencePlot_Sachsen"]], nestedplotlist[["changeOfIncidencePlot_Schleswig-Holstein"]], nestedplotlist[["changeOfIncidencePlot_Thüringen"]],
    nrow=16,
    left = textGrob("Change Of Incidence", rot = 90, vjust = 1)))

ggsave("ExploratoryAnalysis_changeOfIncidence.png", g, w = 2.75, h = 16, dpi = 300)

g <- (arrangeGrob(nestedplotlist[["mobilityPlot_Baden-Württemberg"]], nestedplotlist[["mobilityPlot_Bayern"]], nestedplotlist[["mobilityPlot_Berlin"]], nestedplotlist[["mobilityPlot_Brandenburg"]],
    nestedplotlist[["mobilityPlot_Bremen"]], nestedplotlist[["mobilityPlot_Hamburg"]], nestedplotlist[["mobilityPlot_Hessen"]], nestedplotlist[["mobilityPlot_Mecklenburg-Vorpommern"]],
    nestedplotlist[["mobilityPlot_Niedersachsen"]], nestedplotlist[["mobilityPlot_Nordrhein-Westfalen"]], nestedplotlist[["mobilityPlot_Rheinland-Pfalz"]], nestedplotlist[["mobilityPlot_Saarland"]],
    nestedplotlist[["mobilityPlot_Sachsen-Anhalt"]], nestedplotlist[["mobilityPlot_Sachsen"]], nestedplotlist[["mobilityPlot_Schleswig-Holstein"]], nestedplotlist[["mobilityPlot_Thüringen"]],
    nrow=16,
    left = textGrob("Out Of Home Duration", rot = 90, vjust = 1)))

ggsave("ExploratoryAnalysis_Mobility.png", g, w = 2.75, h = 16, dpi = 300)

g <- (arrangeGrob(nestedplotlist[["tempPlot_Baden-Württemberg"]], nestedplotlist[["tempPlot_Bayern"]], nestedplotlist[["tempPlot_Berlin"]], nestedplotlist[["tempPlot_Brandenburg"]],
    nestedplotlist[["tempPlot_Bremen"]], nestedplotlist[["tempPlot_Hamburg"]], nestedplotlist[["tempPlot_Hessen"]], nestedplotlist[["tempPlot_Mecklenburg-Vorpommern"]],
    nestedplotlist[["tempPlot_Niedersachsen"]], nestedplotlist[["tempPlot_Nordrhein-Westfalen"]], nestedplotlist[["tempPlot_Rheinland-Pfalz"]], nestedplotlist[["tempPlot_Saarland"]],
    nestedplotlist[["tempPlot_Sachsen-Anhalt"]], nestedplotlist[["tempPlot_Sachsen"]], nestedplotlist[["tempPlot_Schleswig-Holstein"]], nestedplotlist[["tempPlot_Thüringen"]],
    nrow=16,
    left = textGrob("Maximal Temperature", rot = 90, vjust = 1)))

ggsave("ExploratoryAnalysis_Temp.png", g, w = 2.75, h = 16, dpi = 300)

g <- (arrangeGrob(nestedplotlist[["outdoorFractionPlot_Baden-Württemberg"]], nestedplotlist[["outdoorFractionPlot_Bayern"]], nestedplotlist[["outdoorFractionPlot_Berlin"]], nestedplotlist[["outdoorFractionPlot_Brandenburg"]],
    nestedplotlist[["outdoorFractionPlot_Bremen"]], nestedplotlist[["outdoorFractionPlot_Hamburg"]], nestedplotlist[["outdoorFractionPlot_Hessen"]], nestedplotlist[["outdoorFractionPlot_Mecklenburg-Vorpommern"]],
    nestedplotlist[["outdoorFractionPlot_Niedersachsen"]], nestedplotlist[["outdoorFractionPlot_Nordrhein-Westfalen"]], nestedplotlist[["outdoorFractionPlot_Rheinland-Pfalz"]], nestedplotlist[["outdoorFractionPlot_Saarland"]],
    nestedplotlist[["outdoorFractionPlot_Sachsen-Anhalt"]], nestedplotlist[["outdoorFractionPlot_Sachsen"]], nestedplotlist[["outdoorFractionPlot_Schleswig-Holstein"]], nestedplotlist[["outdoorFractionPlot_Thüringen"]],
    nrow=16,
    left = textGrob("Share of Activities Performed Outside", rot = 90, vjust = 1)))

ggsave("ExploratoryAnalysis_oF.png", g, w = 2.75, h = 16, dpi = 300)


g <- (arrangeGrob(nestedplotlist[["outdoorFractionPlot_Baden-Württemberg"]], nestedplotlist[["outdoorFractionPlot_Bayern"]], nestedplotlist[["outdoorFractionPlot_Berlin"]], nestedplotlist[["outdoorFractionPlot_Brandenburg"]],
    nestedplotlist[["outdoorFractionPlot_Bremen"]], nestedplotlist[["outdoorFractionPlot_Hamburg"]], nestedplotlist[["outdoorFractionPlot_Hessen"]], nestedplotlist[["outdoorFractionPlot_Mecklenburg-Vorpommern"]],
    nestedplotlist[["outdoorFractionPlot_Niedersachsen"]], nestedplotlist[["outdoorFractionPlot_Nordrhein-Westfalen"]], nestedplotlist[["outdoorFractionPlot_Rheinland-Pfalz"]], nestedplotlist[["outdoorFractionPlot_Saarland"]],
    nestedplotlist[["outdoorFractionPlot_Sachsen-Anhalt"]], nestedplotlist[["outdoorFractionPlot_Sachsen"]], nestedplotlist[["outdoorFractionPlot_Schleswig-Holstein"]], nestedplotlist[["outdoorFractionPlot_Thüringen"]],
    nrow=16,
    left = textGrob("Share of Activities Performed Outside", rot = 90, vjust = 1)))
