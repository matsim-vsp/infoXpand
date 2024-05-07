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

#### This r script performs exploratory data analysis by visualizing the data frames created in PrepIncidenceData.R, PrepMobilityData.R, PrepWeatherData.R
#### Author: S. Paltra @ TU Berlin)

# Prepping incidence data
source("PrepIncidenceData.R")
incidence_data <- incidence_data %>% filter(Date > "2020-03-15") %>% filter(Date < "2021-01-01") %>%
                                      mutate(year = year(Date))
incidence_data <- incidence_data %>% mutate(Welle = case_when(Date < "2020-05-17" ~ "1st Wave",
                                                              Date >= "2020-09-28" ~ "Second wave",
                                                               Date >= "2020-05-17" ~ "Summer Period"))

sd <- sd(incidence_data$Incidence)

incidence_data <- incidence_data %>% mutate(leadInc = lead(incidence_data$Incidence))
sd2 <- sd(incidence_data$leadInc, na.rm=TRUE)

covariance <- cov(incidence_data$Incidence, incidence_data$leadInc, use = "pairwise.complete.obs")
 
incidence_data <- incidence_data[-nrow(incidence_data),]

incidence_data <- incidence_data %>% mutate(ErrorcOI = sqrt((1/sqrt(Incidence))^2 + (1/sqrt(leadInc))^2))
incidence_data <- incidence_data %>% mutate(ErrorIncidence = 1/sqrt(Incidence))

date_breaks <- data.frame(start = c(as.Date("2020-03-22"), as.Date("2020-05-17"), as.Date("2020-09-28")),
                          end = c(as.Date("2020-05-17"), as.Date("2020-09-28"), as.Date("2021-01-01")),
                          colors = c("First Wave", "Summer Period", "Second Wave"))
date_breaks$colors <- factor(date_breaks$colors, levels = c("First Wave", "Summer Period", "Second Wave"))

# Plot of national 7-day incidence over time
p1 <- ggplot(incidence_data) +
ylab("7-day-Incidence \nper 100,000") +
xlab("") +
scale_y_log10(breaks=c(1,10,100),labels=c(1,10,100)) +
geom_rect(data = date_breaks,
            aes(xmin = start,
                xmax = end,
                ymin = 0,
                ymax = Inf,
                fill = colors),
            alpha = 0.3) +
scale_fill_manual(values = c("#1B9E77",
                               "#7570B3", "#D95F02")) +
geom_point(aes(x = Date, y = Incidence), color = "#333333", size = 2.5) +
geom_errorbar(aes(x= Date, ymin=Incidence-ErrorIncidence, ymax=Incidence+ErrorIncidence), width=1, color="#333333") +
theme_minimal() +
scale_x_date(date_breaks = "1 month", date_labels = "%b") +
theme(text = element_text(size = 25), legend.position = "bottom", legend.title=element_blank()) +
   theme(axis.ticks.x = element_line(),
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt")) +
 theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none")

# Plot of national growth multiplier over time
p2 <- ggplot(incidence_data) +
ylab("Growth \nMultiplier") +
xlab("2020") +
geom_rect(data = date_breaks,
            aes(xmin = start,
                xmax = end,
                ymin = - Inf,
                ymax = Inf,
                fill = colors),
            alpha = 0.3) +
scale_fill_manual(values = c("#1B9E77",
                               "#7570B3", "#D95F02")) +
geom_point(aes(x = Date, y = cOI), color="#333333", size = 2.5) +
geom_errorbar(aes(x = Date, ymin=cOI-ErrorcOI, ymax=cOI+ErrorcOI), width=1, color="#333333") +
#coord_cartesian(ylim = c(0,20)) +
theme_minimal() +
scale_x_date(date_breaks = "1 month", date_labels = "%b") +
#theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
theme(text = element_text(size = 25), legend.position = "bottom", legend.title=element_blank()) +
   theme(axis.ticks.x = element_line(),
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))

# Prepping mobility data
date_breaks <- data.frame(start = c(as.Date("2020-03-08"), as.Date("2020-03-23"), as.Date("2020-05-10"), as.Date("2020-11-02"), as.Date("2020-12-16")),
                          end = c(as.Date("2020-03-23"), as.Date("2020-05-10"), as.Date("2020-11-02"), as.Date("2020-12-16"), as.Date("2021-01-03")),
                          colors = c("No Restrictions", "First Contact Restrictions", "Relaxation Period", "Lockdown Light", "Tighter Contact Restrictions"))
date_breaks$colors <- factor(date_breaks$colors, levels = c("No Restrictions", "First Contact Restrictions", "Relaxation Period", "Lockdown Light", "Tighter Contact Restrictions"))

source("PrepMobilityData.R")

# Plot of mobility data (national level)
p3 <- mobility_data %>% filter (Bundesland == "Gesamt") %>% filter(Date < "2021-01-04") %>%
ggplot() +
ylab("Daily Out Of Home \nDuration/Person") +
xlab("2020") +
geom_rect(data = date_breaks,
            aes(xmin = start,
                xmax = end,
                ymin = - Inf,
                ymax = Inf,
                fill = colors),
            alpha = 0.3) +
scale_fill_manual(values = c("#B3B3B3", "#E7298A",
                               "#E6AB02", "#66A61E", "#A6761D")) +
theme_minimal() +
geom_point(aes(x = Date, y = outOfHomeDuration), color = "#333333", size = 2.5) +
scale_x_date(date_breaks = "1 month", date_labels = "%b") +
#theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
theme(text = element_text(size = 25), legend.position = "bottom", legend.title=element_blank()) +
   theme(axis.ticks.x = element_line(),
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt")) +
guides(fill = guide_legend(nrow = 2))

ggsave("OutOfHomeDuration.pdf", p3, dpi = 500, w = 12, h = 6)
ggsave("OutOfHomeDuration.png", p3, dpi = 500, w = 12, h = 6)


# Plot of temperature data over time
source("PrepWeatherData.R")
p4 <- weather_data_all %>% filter(Bundesland == "Gesamt") %>% filter(Date > "2020-03-01") %>% ggplot(aes(x = Date, y = tmax)) +
geom_point(color = "#333333", size = 2.5) +
ylab("Maximum Temperature \nin C°") +
xlab("") +
theme_minimal() +
#scale_x_date(date_breaks = "1 month", date_labels = "%d/%b/%y") +
#theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
theme(text = element_text(size = 25), legend.position = "none") +
theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none") +
theme(axis.ticks.x = element_line(),
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))

# Plot of outdoor fraction over time
p5 <- weather_data_all %>% filter(Bundesland == "Gesamt") %>% filter(Date > "2020-03-01") %>% ggplot(aes(x = Date, y = outdoorFraction2)) +
geom_point(color = "#333333", size = 2.5) +
ylab("Share Of Activities \nPerformed Outside") +
theme_minimal() +
xlab("2020") +
scale_x_date(date_breaks = "1 month", date_labels = "%b") +
#theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
theme(text = element_text(size = 25), legend.position = "bottom", legend.title=element_blank()) +
theme(axis.ticks.x = element_line(), 
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))

#Plot of T^{\star} over time (see manuscript for definition)
ggplot(weather_data_all %>% filter(Date > "2020-02-01" & Date < "2020-12-15")) +
geom_line(aes(x = Date, y = TStar), color = "#666666", size = 1.2) +
theme_minimal() +
xlab("2020") +
ylab("Temperature in C°") +
scale_x_date(date_breaks = "1 month", date_labels = "%b") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
theme(text = element_text(size = 25), legend.position = "bottom", legend.title=element_blank()) +
theme(axis.ticks.x = element_line(),
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))

ggsave("TStarOverTime.pdf", w = 12, h = 4.75, dpi = 500)
ggsave("TStarOverTime.png", w = 12, h = 4.75, dpi = 500)                   

p <- arrangeGrob(p1, p2, p3, p4, p5, nrow = 5)
p <- arrangeGrob(p1, p2, nrow=2, heights = c(2,3.5))
ggsave("IncidenceChangeOfIncidence.pdf", p, w = 12, h = 10, dpi = 500)
ggsave("IncidenceChangeOfIncidence.png", p, w = 12, h = 10, dpi = 500)
p <- arrangeGrob(p4, p5, nrow = 2, heights = c(3.5,3.5))
ggsave("weatherplots.pdf", p, w = 12, h = 8, dpi = 500)
ggsave("weatherplots.png", p, w = 12, h = 8, dpi = 500)

# Scatter plot: growth mutiplier vs outdoor fraction
p2 <- joinedDataFrame_reduced %>% filter(cOI_2weeksbefore < 2) %>%
ggplot(aes(x = outdoorFraction2, y = cOI_2weeksbefore)) +
geom_point(color = "#666666", size = 2) +
theme_minimal() +
xlab("Outdoor Fraction") +
ylab("Growth Multiplier") +
theme(text = element_text(size = 25))

#ggsave("oOHoutdoorFractionvsChangeOfIncidence.png", dpi = 500, w = 9, h = 6)

# Scatter plot : growth multiplier vs oOH*outdoorfraction
joinedDataFrame_reduced %>% filter(cOI_2weeksbefore < 2) %>%
ggplot(aes(x = outOfHomeDuration*outdoorFraction2, y = cOI_2weeksbefore)) +
geom_point(color = "#666666", size = 2) +
theme_minimal() +
xlab("Daily Out Of Home Duration/Person*outdoorFraction") +
ylab("Change Of Incidence") +
theme(text = element_text(size = 13))

picture <- arrangeGrob(p,p2, nrow=1)

# Creating plot of regression line of the linear/quadratic/cubic mobility-only model
#To create these plots, one must first run regressionAnalysis_Nat.R
oOH_x <- seq(-0.5, 10, length.out = 40)
oOH_y <- resultsList[["oOH"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[1]] + resultsList[["oOH"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[2]]*oOH_x

oOH2_x <- seq(-0.5, 10, length.out = 40)
oOH2_y <- resultsList[["oOH2"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[1]] + resultsList[["oOH2"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[2]]*oOH2_x^2

oOH3_x <- seq(-0.5, 10, length.out = 40)
oOH3_y <- resultsList[["oOH3"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[1]] + resultsList[["oOH3"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[2]]*oOH3_x^3

joinedDataFrame_reduced <- joinedDataFrame %>% filter(cOI_2weeksbefore < 2) %>% filter(Date + 14 < "2021-01-01")
joinedDataFrame_reduced <- joinedDataFrame_reduced %>% mutate(labeled = case_when(cOI_2weeksbefore> 1.5 ~ as.character(joinedDataFrame_reduced$Date+14),
                                                                  cOI_2weeksbefore <= 1.5 ~ ""))
colors <- c("linear (1)" = "#e6ab02", "quadratic (2)" = "#e7298a", "cubic (3)" = "#7570b3", "Nouvellet" = "#666666", "Dainton" = "#ff0000", "OuldSetti" = "#0000ff")

# Plot of these regression lines as well as scatter plot of growth multiplier vs oOH
p <- joinedDataFrame_reduced %>%
ggplot(aes(x = outOfHomeDuration, y = cOI_2weeksbefore)) +
theme_minimal() +
#geom_line(aes(oOH_x, oOH_y, color = "linear (1)"), size = 2) +
#geom_line(aes(oOH2_x, oOH2_y, color = "quadratic (2)"), size = 2) +
#geom_line(aes(oOH3_x, oOH3_y, , color = "cubic (3)"), size = 2) +
scale_color_manual(values = colors, breaks = names(colors)[c(1, 2, 3, 4, 5, 6)]) +
#geom_text_repel(aes(label = labeled), size = 5) +
geom_point(color = "#666666", size = 2.5) +
xlab("Daily Out Of Home Duration per Person") +
#xlim(-0.55, 9) +
#ylim(-1.1, 1.9) +
ylab("Growth Multiplier") +
theme(text = element_text(size = 25), legend.position = "bottom", legend.title=element_blank()) +
theme(axis.ticks.x = element_line(),
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))

# Creating regression line for the linear/quadratic/cubic mobility-only model VS R VALUE
# To create these plots, one must first run regressionAnalysis_Nat.R
# Our model is compared to models found in the literature


# oOH_x <- seq(-0.5, 10, length.out = 40)
# oOH_y <- resultsList[["oOH"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[1]] + resultsList[["oOH"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[2]]*oOH_x
# oOH_y <- oOH_y^(5/7)
# oOH_y <- ifelse(is.nan(oOH_y), 0, oOH_y)

oOH2_x <- seq(-0.5, 10, length.out = 40)
oOH2_y <- resultsList[["oOH2+oOH2:out2_noInt"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[1]]*oOH2_x^2 + resultsList[["oOH2+oOH2:out2_noInt"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[2]]
oOH2_y <- oOH2_y^(5/7)
oOH2_y <- ifelse(is.nan(oOH2_y), 0, oOH2_y)

# oOH3_x <- seq(-0.5, 10, length.out = 40)
# oOH3_y <- resultsList[["oOH3"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[1]] + resultsList[["oOH3"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[2]]*oOH3_x^3
# oOH3_y <- oOH3_y^(5/7)
# oOH3_y <- ifelse(is.nan(oOH3_y), 0, oOH3_y)

#Nouvellet at al.
Nouv_x <- seq(0, 1, length.out = 40)
Nouv_y <- log(0.97) - 1.51 * (1 - Nouv_x)
Nouv_y <- exp(Nouv_y)
#Nouv_y <- Nouv_y^(7 / 5)
Nouv_x <- 8 * Nouv_x
#Nouv_y <- ifelse(is.nan(Nouv_y), 0, Nouv_y)

#Dainton et al.
Dain_x <- seq(0, 50, length.out = 40)
Dain_y <- 1.4 - 0.035 * Dain_x
Dain_x <- Dain_x * (-8/50) + 8
#Dain_y <- Dain_y^(7/5)
#Dain_y <- ifelse(is.nan(Dain_y), 0, Dain_y)

#Ould-Setti et al.
Ould_x <- seq(0, 50, length.out = 40)
Ould_y <- 1.2 - 0.021 * Ould_x
Ould_x <- Ould_x * (-8/50) + 8
#Ould_y <- Ould_y^(7/5)
#Ould_y <- ifelse(is.nan(Ould_y), 0, Ould_y)

#Noland
Nol_x <- seq(0, 50, length.out = 40)
Nol_y <- log(1.4) - 0.0236*(Nol_x)
Nol_y <- exp(Nol_y)
Nol_x <- Nol_x * (-8/50) + 8 #Converting the percentage change of "time spent at home" to hours spent outside one's home



joinedDataFrame_reduced <- joinedDataFrame %>% filter(cOI_2weeksbefore < 2) %>% filter(Date + 14 < "2021-01-01")
joinedDataFrame_reduced <- joinedDataFrame_reduced %>% mutate(labeled = case_when(cOI_2weeksbefore> 1.5 ~ as.character(joinedDataFrame_reduced$Date+14),
                                                                  cOI_2weeksbefore <= 1.5 ~ ""))
colors <- c("linear" = "#e6ab02", "Our Model (9)" = "#e7298a", "cubic" = "#7570b3", "Nouvellet et al." = "#a6d854", "Dainton et al." = "#b3b3b3", "Setti and Tollis" = "#ffd92f", "Noland" = "#e5c494")

# Plot of these regression lines as well as scatter plot of growth multiplier vs oOH
p <- joinedDataFrame_reduced %>%
ggplot(aes(x = outOfHomeDuration, y = cOI_2weeksbefore^(5/7))) +
theme_minimal() +
#geom_line(aes(oOH_x, oOH_y, color = "linear")) +
#geom_line(aes(oOH3_x, oOH3_y, , color = "cubic")) +
geom_line(aes(Nouv_x, Nouv_y, color = "Nouvellet et al."), size = 2) +
geom_line(aes(Dain_x, Dain_y, color = "Dainton et al."), size = 2) +
geom_line(aes(Nol_x, Nol_y, color = "Noland"), size = 2) +
geom_line(aes(Ould_x, Ould_y, color = "Setti and Tollis"), size = 2) +
geom_line(aes(oOH2_x, oOH2_y, color = "Our Model (9)"), size = 2) +
scale_color_manual(values = colors, breaks = names(colors)[c(1, 2, 3, 4, 5, 6, 7)]) +
#geom_text_repel(aes(label = labeled)) +
geom_point(color = "#666666", size = 2.5) +
xlab("Daily Out Of Home Duration per Person") +
xlim(-0.20, 9) +
ylim(-0.5, 1.6) +
ylab("R-Value") +
theme(text = element_text(size = 25), legend.position = "right", legend.title=element_blank()) +
theme(axis.ticks.x = element_line(), 
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt")) +
guides(fill=guide_legend(nrow=2,byrow=TRUE))

ggsave("oOHvsRValueModelComparison.pdf", p, w = 9, h = 7, dpi = 500)
