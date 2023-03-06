library(tidyverse)
library(lubridate)
library(readxl)
library(httr)
library(gridExtra)
library(ggiraphExtra)
library(leaps)

#Using R 4.1.1

#The following script combines three data sources to obtain incidences on a federal state as well as national level
#This data set can then be used for further analysis

#Setting up da data frame containing the states and the corresponding weather IDs
dict_state_id <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(dict_state_id) <- c("Bundesland", "ID", "Einwohner")
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Baden-Württemberg", 10738, 11069533)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Bayern", 10865, 13076721)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Berlin", 10382, 3644826)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Brandenburg", 10379, 2511917)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Bremen", 10224, 682986)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Hamburg", 10147, 1841179)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Hessen", 10633, 6265809)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Mecklenburg-Vorpommern", 10162, 1609675)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Niedersachsen", 10338, 7982448)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Nordrhein-Westfalen", 10400, 17932651)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Rheinland-Pfalz", "D3137", 4084844)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Saarland", "D6217", 990509)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Sachsen", "D1051", 4077937)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Sachsen-Anhalt", 10361, 2208321)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Schleswig-Holstein", 10044, 2896712)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Thüringen", 10554,  2143145)
dict_state_id[nrow(dict_state_id) + 1, ] <- c("Gesamt", 00000, 83200000)

#Reading incidence data in, data hereby comes from RKI

#Incidence from Januar 2020 until May 2020
#Data starting Jan 2020
url1 <- 'https://svn.vsp.tu-berlin.de/repos/public-svn/matsim/scenarios/countries/de/episim/original-data/Fallzahlen/RKI/bundesland-cases.csv'
GET(url1, write_disk(tf <- tempfile(fileext = ".csv")))
incidence_data_archiv <- read_csv(tf)
incidence_data_archiv <- incidence_data_archiv %>% mutate(Date = make_date(year = year, month = month, day = day))

incidence_data_seven_days <- data.frame(matrix(nrow = 0, ncol = 3))
colnames(incidence_data_seven_days) <- c("Date", "Cases", "Bundesland")

datum <- min(incidence_data_archiv$Date)+6
federalStates <- c("Baden-Württemberg", "Bayern", "Berlin", "Brandenburg", "Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen",
                    "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen", "Sachsen-Anhalt", "Schleswig-Holstein", "Thüringen")
while(datum < as.Date("2020-05-06")){
  filtered <- filter(incidence_data_archiv, Date <= datum) %>% filter(Date > datum-7)
  incidence_data_seven_days[nrow(incidence_data_seven_days)+1,1] <- datum
  incidence_data_seven_days[nrow(incidence_data_seven_days), 2] <- sum(filtered$cases)
  incidence_data_seven_days[nrow(incidence_data_seven_days), 3] <- "Gesamt"
    for(fedstate in federalStates){
        filtered2 <- filter(filtered, Bundesland == fedstate)
          incidence_data_seven_days[nrow(incidence_data_seven_days)+1,1] <- datum
          incidence_data_seven_days[nrow(incidence_data_seven_days), 2] <- sum(filtered2$cases)
          incidence_data_seven_days[nrow(incidence_data_seven_days), 3] <- fedstate
    }
  datum <- datum + 1
}

incidence_data_seven_days$Date <- as.Date(incidence_data_seven_days$Date, origin="1970-01-01")
incidence_data_seven_days <- incidence_data_seven_days %>% mutate(Incidence = case_when(
                                                                    Bundesland == "Baden-Württemberg" ~ Cases / 11069533 * 100000,
                                                                    Bundesland == "Bayern" ~ Cases / 13076721 * 100000,
                                                                    Bundesland == "Berlin" ~ Cases / 3644826 * 100000,
                                                                    Bundesland == "Brandenburg" ~ Cases / 2511917 * 100000,
                                                                    Bundesland == "Bremen" ~ Cases / 682986 * 100000,
                                                                    Bundesland == "Hamburg" ~ Cases / 1841179 * 100000,
                                                                    Bundesland == "Hessen" ~ Cases / 6265809 * 100000,
                                                                    Bundesland == "Mecklenburg-Vorpommern" ~ Cases / 1609675 * 100000,
                                                                    Bundesland == "Niedersachsen" ~ Cases / 7982448 * 100000,
                                                                    Bundesland == "Nordrhein-Westfalen" ~ Cases / 17932651 * 100000,
                                                                    Bundesland == "Rheinland-Pfalz" ~ Cases / 4084844 * 100000,
                                                                    Bundesland == "Saarland" ~ Cases / 990509 * 100000,
                                                                    Bundesland == "Sachsen" ~ Cases / 4077937 * 100000,
                                                                    Bundesland == "Sachsen-Anhalt" ~ Cases / 2208321 * 100000,
                                                                    Bundesland == "Schleswig-Holstein" ~ Cases / 2896712 * 100000,
                                                                    Bundesland == "Thüringen" ~ Cases / 2143145 * 100000,
                                                                    Bundesland == "Gesamt" ~ Cases / 83200000 * 100000))
incidence_data_seven_days <- incidence_data_seven_days[, c("Date", "Bundesland", "Incidence")]

#Incidence from May 2020 until September 2021
url1 <-'https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Kum_Tab_Archiv.xlsx?__blob=publicationFile'
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
incidence_data_archiv <- read_excel(tf, 3)
colnames(incidence_data_archiv) <- incidence_data_archiv[4, ]
colnames(incidence_data_archiv)[1] <- "Bundesland"
incidence_data_archiv <- incidence_data_archiv[-(1:4), ]
incidence_data_archiv <- pivot_longer(incidence_data_archiv, names_to="Date", values_to="Incidence", cols=colnames(incidence_data_archiv)[2:ncol(incidence_data_archiv)])
incidence_data_archiv$Date <- as.integer(incidence_data_archiv$Date)
incidence_data_archiv$Date <- as.Date(incidence_data_archiv$Date,origin="1899-12-30")

#Incidence from September 2021 onwards
url1 <- 'https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Daten/Fallzahlen_Kum_Tab_aktuell.xlsx?__blob=publicationFile'
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
incidence_data <- read_excel(tf, 4)
colnames(incidence_data) <- incidence_data[4, ]
colnames(incidence_data)[1] <- "Bundesland"
incidence_data <- incidence_data[-(1:4),] #Removing the 1st line, as this solely contains the column names
# hospitalizationData <- mutate(hospitalizationData, BundeslandId=c("08", "09", "11", "12", "04", "02", "06", "13", "03", "05", "07", "10", "14", "15", "01", "16", "00"))
incidence_data <- pivot_longer(incidence_data, names_to="Date", values_to="Incidence", cols=colnames(incidence_data)[2:ncol(incidence_data)])
incidence_data$Date <- paste(substr(incidence_data$Date, start = 7, stop = 10), substr(incidence_data$Date, start = 4, stop = 5), as.character(substr(incidence_data$Date, start = 1, stop = 2)), sep ="-" )
incidence_data$Date <- as.Date(incidence_data$Date)
incidence_data$Date <- incidence_data$Date

incidence_data_archiv <- rbind(incidence_data_seven_days,incidence_data_archiv)
incidence_data <- rbind(incidence_data_archiv, incidence_data)
incidence_data$Incidence <- as.double(incidence_data$Incidence)
# Here, we are filtering only for 2020
incidence_data <- filter(incidence_data, Date < as.Date("2021-01-01"))

incidence_data <- incidence_data %>%
  mutate(weekMon = cut(Date, "week")) %>% #Weeks start on Monday
  mutate(weekTue = cut(Date - 1, "week")) %>% #Weeks start on Tuesday
  mutate(weekWed = cut(Date - 2, "week")) %>% #Weeks start on Wednesday
  mutate(weekThu = cut(Date - 3, "week")) %>% #Weeks start on Thursday
  mutate(weekFri = cut(Date - 4, "week")) %>% #Weeks start on Friday
  mutate(weekSat = cut(Date - 5, "week")) %>% #Weeks start on Saturday
  mutate(weekSun = cut(Date - 6, "week")) %>% #Weeks start on Sunday
  mutate(year = year(Date))

# incidence_data <- incidence_data[-1, ]

incidence_dataMon <- incidence_data %>%  group_by(year, weekMon, Bundesland) %>%
  summarise(Date = min(Date) + 6, IncidenceMon = mean(Incidence))
incidence_dataTue <- incidence_data %>%  group_by(year, weekTue, Bundesland) %>%
  summarise(Date = min(Date) + 5, IncidenceTue = mean(Incidence))
incidence_dataWed <- incidence_data %>%  group_by(year, weekWed, Bundesland) %>%
  summarise(Date = min(Date) + 4, IncidenceWed = mean(Incidence))
incidence_dataThu <- incidence_data %>%  group_by(year, weekThu, Bundesland) %>%
  summarise(Date = min(Date) + 3, IncidenceThu = mean(Incidence))
incidence_dataFri <- incidence_data %>%  group_by(year, weekFri, Bundesland) %>%
  summarise(Date = min(Date) + 2, IncidenceFri = mean(Incidence))
incidence_dataSat <- incidence_data %>%  group_by(year, weekSat, Bundesland) %>%
  summarise(Date = min(Date) + 1, IncidenceSat = mean(Incidence))
incidence_dataSun <- incidence_data %>%  group_by(year, weekSun, Bundesland) %>%
  summarise(Date = min(Date), IncidenceSun = mean(Incidence))

incidence_data <- inner_join(incidence_dataMon, incidence_dataTue, by = c("Date", "Bundesland"))
incidence_data <- inner_join(incidence_data, incidence_dataWed, by = c("Date", "Bundesland"))
incidence_data <- inner_join(incidence_data, incidence_dataThu, by = c("Date", "Bundesland"))
incidence_data <- inner_join(incidence_data, incidence_dataFri, by = c("Date", "Bundesland"))
incidence_data <- inner_join(incidence_data, incidence_dataSat, by = c("Date", "Bundesland"))
incidence_data <- inner_join(incidence_data, incidence_dataSun, by = c("Date", "Bundesland"))

incidence_data <- ungroup(incidence_data)

incidence_data <- select(incidence_data, c("Date", "Bundesland", "year", "IncidenceMon", "IncidenceTue", "IncidenceWed", "IncidenceThu", "IncidenceFri", "IncidenceSat", "IncidenceSun"))
incidence_data <- incidence_data[order(incidence_data$Bundesland), ]

#Adding the change of incidence to the data frame
incidence_data[, "changeOfIncidenceMon"] <- as.double(incidence_data$IncidenceMon/lag(incidence_data$IncidenceMon))
incidence_data[, "changeOfIncidenceTue"] <- incidence_data$IncidenceTue/lag(incidence_data$IncidenceTue)
incidence_data[, "changeOfIncidenceWed"] <- incidence_data$IncidenceWed/lag(incidence_data$IncidenceWed)
incidence_data[, "changeOfIncidenceThu"] <- incidence_data$IncidenceThu/lag(incidence_data$IncidenceThu)
incidence_data[, "changeOfIncidenceFri"] <- incidence_data$IncidenceFri/lag(incidence_data$IncidenceFri)
incidence_data[, "changeOfIncidenceSat"] <- incidence_data$IncidenceSat/lag(incidence_data$IncidenceSat)
incidence_data[, "changeOfIncidenceSun"] <- incidence_data$IncidenceSun/lag(incidence_data$IncidenceSun)

 incidence_data <- mutate(incidence_data, changeOfIncidencelaggedMon = lead(changeOfIncidenceMon))
 incidence_data <- mutate(incidence_data, changeOfIncidencelaggedMon2 = lead(changeOfIncidencelaggedMon))
 incidence_data <- mutate(incidence_data, changeOfIncidencelaggedTue = lead(changeOfIncidenceTue))
 incidence_data <- mutate(incidence_data, changeOfIncidencelaggedTue2 = lead(changeOfIncidencelaggedTue))
 incidence_data <- mutate(incidence_data, changeOfIncidencelaggedWed = lead(changeOfIncidenceWed))
 incidence_data <- mutate(incidence_data, changeOfIncidencelaggedWed2 = lead(changeOfIncidencelaggedWed))
 incidence_data <- mutate(incidence_data, changeOfIncidencelaggedThu = lead(changeOfIncidenceThu))
 incidence_data <- mutate(incidence_data, changeOfIncidencelaggedThu2 = lead(changeOfIncidencelaggedThu))
 incidence_data <- mutate(incidence_data, changeOfIncidencelaggedFri = lead(changeOfIncidenceFri))
 incidence_data <- mutate(incidence_data, changeOfIncidencelaggedFri2 = lead(changeOfIncidencelaggedFri))
 incidence_data <- mutate(incidence_data, changeOfIncidencelaggedSat = lead(changeOfIncidenceSat))
 incidence_data <- mutate(incidence_data, changeOfIncidencelaggedSat2 = lead(changeOfIncidencelaggedSat))
 incidence_data <- mutate(incidence_data, changeOfIncidencelaggedSun = lead(changeOfIncidenceSun))
 incidence_data <- mutate(incidence_data, changeOfIncidencelaggedSun2 = lead(changeOfIncidencelaggedSun))
