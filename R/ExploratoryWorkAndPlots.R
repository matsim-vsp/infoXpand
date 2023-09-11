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
                                                               Date >= "2020-05-17" ~ "Summer break",
                                                               Date >= "2020-09-28" ~ "Second wave"))

date_breaks <- data.frame(start = c(as.Date("2020-03-22"), as.Date("2020-05-17"), as.Date("2020-09-28")),
                          end = c(as.Date("2020-05-17"), as.Date("2020-09-28"), as.Date("2021-01-01")),
                          colors = c("First Wave", "Summer Break", "Second Wave"))
date_breaks$colors <- factor(date_breaks$colors, levels = c("First Wave", "Summer Break", "Second Wave"))

# Plot of national 7-day incidence over time
p1 <- ggplot(incidence_data) +
ylab("7-day-Incidence \nper 100,000") +
xlab("") +
geom_rect(data = date_breaks,
            aes(xmin = start,
                xmax = end,
                ymin = - Inf,
                ymax = Inf,
                fill = colors),
            alpha = 0.3) +
scale_fill_manual(values = c("#1B9E77",
                               "#7570B3", "#D95F02")) +
geom_line(aes(x=Date, y =Incidence),color = "#666666", size = 1.2) +
theme_minimal() +
scale_x_date(date_breaks = "1 month", date_labels = "%d/%b/%y") +
theme(text = element_text(size = 13), legend.position = "none") +
   theme(axis.ticks.x = element_line(), 
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))

# Plot of national growth multiplier over time 
p2 <- ggplot(incidence_data) +
ylab("Growth \nMultiplier") +
xlab("") +
geom_rect(data = date_breaks,
            aes(xmin = start,
                xmax = end,
                ymin = - Inf,
                ymax = Inf,
                fill = colors),
            alpha = 0.3) +
scale_fill_manual(values = c("#1B9E77",
                               "#7570B3", "#D95F02")) +
geom_line(aes(x = Date, y = cOI), color="#666666", size = 1.2) +
theme_minimal() +
scale_x_date(date_breaks = "1 month", date_labels = "%d/%b/%y") +
theme(text = element_text(size = 13), legend.position = "bottom", legend.title = element_blank()) +
   theme(axis.ticks.x = element_line(), 
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))

# Prepping mobility data
date_breaks <- data.frame(start = c(as.Date("2020-03-23"), as.Date("2020-05-10"), as.Date("2020-11-02"), as.Date("2020-12-16")),
                          end = c(as.Date("2020-05-10"), as.Date("2020-11-02"), as.Date("2020-12-16"), as.Date("2021-01-03")),
                          colors = c("First Contact Restrictions", "Relaxation Period", "Lockdown Light", "Tighter Contact Restrctions"))
date_breaks$colors <- factor(date_breaks$colors, levels = c("First Contact Restrictions", "Relaxation Period", "Lockdown Light", "Tighter Contact Restrctions"))

source("PrepMobilityData.R")

# Plot of mobility data (national level)
p3 <- mobility_data %>% filter (Bundesland == "Gesamt") %>% filter(Date < "2021-01-04") %>%
ggplot() +
ylab("Daily Out Of Home \nDuration/Person") +
xlab("") +
geom_rect(data = date_breaks,
            aes(xmin = start,
                xmax = end,
                ymin = - Inf,
                ymax = Inf,
                fill = colors),
            alpha = 0.3) +
scale_fill_manual(values = c("#E7298A",
                               "#E6AB02", "#66A61E", "#A6761D")) +
#endregiongeom_vline(aes(xintercept = (as.Date("2020-03-16")),
#                colour = "Beginning School Closure"), size=1, linetype = "dotted") +
#scale_color_manual(values = c("Beginning School Closure" = "gray40")) +
#geom_vline(xintercept = (as.Date("2020-03-23")),
#                color = "#009E73", size=1) +
#geom_vline(xintercept = (as.Date("2020-05-10")),
#                color = "#D55E00", size=1) +
#geom_vline(xintercept = (as.Date("2020-11-02")),
#               color = "#CC79A7", size=1) +
#geom_vline(xintercept = (as.Date("2020-12-13")),
#                color = "#56B4E9", size=1) +
theme_minimal() +
geom_line(aes(x = Date, y = outOfHomeDuration), color = "#666666", size = 1.2) +
#annotate(geom = "text",
#        label = c("School closure", "Contact restrictions", "Relaxation Period", "Lockdown light", "2nd Lockdown"),
#        x = c(as.Date("2020-03-16"), as.Date("2020-03-23"), as.Date("2020-05-10"), as.Date("2020-11-02"), as.Date("2020-12-13")),
#        y = c(6.5,6.5,6.5,6.5,6.5),
#        angle = 90,
#        vjust = 1.5) +
scale_x_date(date_breaks = "1 month", date_labels = "%d/%b/%y") +
theme(text = element_text(size = 13), legend.position = "bottom", legend.title=element_blank()) +
   theme(axis.ticks.x = element_line(), 
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))

# Plot of temperature data over time
source("PrepWeatherData.R")
p4 <- weather_data_all %>% filter(Bundesland == "Gesamt") %>% filter(Date > "2020-03-01") %>% ggplot(aes(x = Date, y = tmax)) +
geom_line(color = "#666666", size = 1.2) +
ylab("Maximum Temperature \nin C°") +
xlab("") +
theme_minimal() +
scale_x_date(date_breaks = "1 month", date_labels = "%d/%b/%y") +
theme(text = element_text(size = 13)) +
theme(axis.ticks.x = element_line(), 
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))

# Plot of outdoor fraction over time
p5 <- weather_data_all %>% filter(Bundesland == "Gesamt") %>% filter(Date > "2020-03-01") %>% ggplot(aes(x = Date, y = outdoorFraction2)) +
geom_line(color="#666666", size = 1.2) +
ylab("Share Of Activities \nPerformed Outside") +
theme_minimal() +
xlab("") +
scale_x_date(date_breaks = "1 month", date_labels = "%d/%b/%y") +
theme(text = element_text(size = 13)) +
theme(axis.ticks.x = element_line(), 
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))

p <- arrangeGrob(p1,p2,p3,p4,p5, nrow=5)
p <- arrangeGrob(p1,p2, nrow=2)
p <- arrangeGrob(p4,p5, nrow=2)

# Creating regression line for the linear/quadratic/cubic mobility-only model
oOH_x <- seq(-0.5, 10, length.out = 39)
oOH_y <- resultsList[["oOH"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[1]] + resultsList[["oOH"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[2]]*oOH_x

oOH2_x <- seq(-0.5, 10, length.out = 39)
oOH2_y <- resultsList[["oOH2"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[1]] + resultsList[["oOH2"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[2]]*oOH2_x^2

oOH3_x <- seq(-0.5, 10, length.out = 39)
oOH3_y <- resultsList[["oOH3"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[1]] + resultsList[["oOH3"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[2]]*oOH3_x^3

joinedDataFrame_reduced <- joinedDataFrame %>% filter(cOI_2weeksbefore < 2) %>% filter(Date + 14 < "2021-01-01")
joinedDataFrame_reduced <- joinedDataFrame_reduced %>% mutate(labeled = case_when(cOI_2weeksbefore> 1.5 ~ as.character(joinedDataFrame_reduced$Date+14),
                                                                  cOI_2weeksbefore <= 1.5 ~ ""))

colors <- c("linear" = "#e6ab02", "quadratic" = "#e7298a", "cubic" = "#7570b3")
# Plot of these regression lines as well as scatter plot of growth multiplier vs oOH
p <- joinedDataFrame_reduced %>%
ggplot(aes(x = outOfHomeDuration, y = cOI_2weeksbefore)) +
theme_minimal() +
#geom_line(aes(oOH_x, oOH_y, color = "linear")) +
#geom_line(aes(oOH2_x, oOH2_y, color = "quadratic")) +
#geom_line(aes(oOH3_x, oOH3_y, , color = "cubic")) +
#scale_color_manual(values = colors, breaks = names(colors)[c(1, 2, 3)]) +
#geom_text_repel(aes(label = labeled)) +
geom_point(color = "#666666", size = 2) +
xlab("Daily Out Of Home Duration per Person") +
#xlim(-0.55, 9) +
#ylim(-1.1, 1.9) +
ylab("Growth Multiplier") +
theme(text = element_text(size = 13), legend.position = "bottom", legend.title=element_blank()) +
theme(axis.ticks.x = element_line(), 
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))

ggsave("oOHvsChangeOfIncidence.pdf", w = 9, h = 4.5, dpi = 500)

# Scatter plot: growth mutiplier vs outdoor fraction
p2 <- joinedDataFrame_reduced %>% filter(cOI_2weeksbefore < 2) %>%
ggplot(aes(x = outdoorFraction2, y = cOI_2weeksbefore)) +
geom_point(color = "#666666", size = 2) +
theme_minimal() +
xlab("Outdoor Fraction") +
ylab("Growth Multiplier") +
theme(text = element_text(size = 13))

ggsave("oOHoutdoorFractionvsChangeOfIncidence.png", dpi = 500, w = 9, h = 6)

# Scatter plot : growth multiplier vs oOH*outdoorfraction
joinedDataFrame_reduced %>% filter(cOI_2weeksbefore < 2) %>%
ggplot(aes(x = outOfHomeDuration*outdoorFraction2, y = cOI_2weeksbefore)) +
geom_point(color = "#666666", size = 2) +
theme_minimal() +
xlab("Daily Out Of Home Duration/Person*outdoorFraction") +
ylab("Change Of Incidence") +
theme(text = element_text(size = 13))
