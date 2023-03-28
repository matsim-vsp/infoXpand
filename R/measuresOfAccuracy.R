library(tidyverse)
library(lubridate)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(arm)


models <- c("oOH", "oOH2", "oOH+oOH2", "oOH+tmax", "oOH+tavg",
          "oOH*tmax", "oOH*tavg", "oOH+out", "oOH+out2",
          "oOH+prcp", "oOH+tmax+prcp", "oOH+out+prcp", "oOH:out2:prcpvsI",
          "oOH*out", "oOH*tmax2", "oOH*tmax*prcp", "logoOH+logtmax")

#weekdays <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Sun_2week_lag")
weekdays <- c("Sun", "Sun_2week_lag")


accuracy_measures <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(accuracy_measures) <- c("Model", "Weekday", "LagDurationChangeOfIncidence", "Fstatistic", "RSE", "Rsquared", "AdjRSquared")

#Iterating through all our models and weekdays to extract the following measures of model accuracy
#F-statistic
#RSE
#R Squared
#Adjusted RSquared

for (model in models) {
    for (weekday in weekdays) {

accuracy_measures[nrow(accuracy_measures)+1, 1] <- model
accuracy_measures[nrow(accuracy_measures), 2] <- weekday
if (weekday == "Mon") {
accuracy_measures[nrow(accuracy_measures), 3] <- "13 Day lag"
} else if (weekday == "Tue") {
accuracy_measures[nrow(accuracy_measures), 3] <- "12 Day lag"
} else if (weekday == "Wed") {
accuracy_measures[nrow(accuracy_measures), 3] <- "11 Day lag"
} else if (weekday == "Thu") {
accuracy_measures[nrow(accuracy_measures), 3] <- "10 Day lag"
} else if (weekday == "Fri") {
accuracy_measures[nrow(accuracy_measures), 3] <- "9 Day lag"
} else if (weekday == "Sat") {
accuracy_measures[nrow(accuracy_measures), 3] <- "8 Day lag"
} else if (weekday == "Sun") {
accuracy_measures[nrow(accuracy_measures), 3] <- "7 Day lag"
} else if (weekday == "Sun_2week_lag") {
accuracy_measures[nrow(accuracy_measures), 3] <- "14 Day lag"
}
wording <- paste0("Regression_", model, weekday)

#F-Statistic -> needs to be >> 1
accuracy_measures[nrow(accuracy_measures), 4] <- summary(resultsList[[model]][[weekday]][["Model"]])$fstatistic[1]

#RSE -> needs to be small
accuracy_measures[nrow(accuracy_measures), 5] <- summary(resultsList[[model]][[weekday]][["Model"]])$sigma

#R^2 -> needs to be close to 1
accuracy_measures[nrow(accuracy_measures), 6] <- summary(resultsList[[model]][[weekday]][["Model"]])$r.squared

#Adj. R^2 -> needs to be close to 1, accounts for the fact that more variables -> better fit
accuracy_measures[nrow(accuracy_measures), 7] <- summary(resultsList[[model]][[weekday]][["Model"]])$adj.r.squared

    }
}

#Visualizations of these measures of accuracy
ggplot(accuracy_measures, aes(x = LagDurationChangeOfIncidence, fill=LagDurationChangeOfIncidence, y=AdjRSquared)) + 
    geom_bar(position="dodge", stat="identity") +
    facet_wrap(~Model) +
    theme_bw() + 
    xlab("") +
    theme(legend.position = "bottom")
