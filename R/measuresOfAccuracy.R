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
          "oOH*out", "oOH*out2", "oOH*tmax2", "oOH*tmax*prcp", "logoOH+logtmax")

lags <- c("cOI", "cOI_1weekbefore", "cOI_2weeksbefore", "cOI_3weeksbefore", "cOI_4weeksbefore")

accuracy_measures <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(accuracy_measures) <- c("Model", "lag", "Fstatistic", "RSE", "Rsquared", "AdjRSquared")

#Iterating through all our models and lags to extract the following measures of model accuracy
    #F-statistic
    #RSE
    #R Squared
    #Adjusted RSquared

for (model in models) {
    for (lag in lags) {

accuracy_measures[nrow(accuracy_measures) + 1, 1] <- model
accuracy_measures[nrow(accuracy_measures), 2] <- lag

#F-Statistic -> needs to be >> 1
accuracy_measures[nrow(accuracy_measures), 3] <- summary(resultsList[[model]][[lag]][["Model"]])$fstatistic[1]

#RSE -> needs to be small
accuracy_measures[nrow(accuracy_measures), 4] <- summary(resultsList[[model]][[lag]][["Model"]])$sigma

#R^2 -> needs to be close to 1
accuracy_measures[nrow(accuracy_measures), 5] <- summary(resultsList[[model]][[lag]][["Model"]])$r.squared

#Adj. R^2 -> needs to be close to 1, accounts for the fact that more variables -> better fit
accuracy_measures[nrow(accuracy_measures), 6] <- summary(resultsList[[model]][[lag]][["Model"]])$adj.r.squared

    }
}

colnames(accuracy_measures) <- c("Model", "lag", "Fstatistic", "RSE", "Rsquared", "AdjRSquared")

#Visualization of adjusted R^2 for different models
ggplot(accuracy_measures, aes(x = lag, fill = lag, y = AdjRSquared)) +
    geom_bar(position = "dodge", stat = "identity") +
    ggtitle("Adjusted R Squared for different models") +
    facet_wrap(~Model) +
    theme_bw() +
    xlab("") +
    theme(legend.position = "bottom") #For all models: Maximum reached when considering a 2 week lag

#Visualization of RSE for different models
ggplot(accuracy_measures, aes(x = lag, fill = lag, y = RSE)) +
    geom_bar(position = "dodge", stat = "identity") +
    ggtitle("Adjusted R Squared for different models") +
    facet_wrap(~Model) +
    theme_bw() +
    xlab("") +
    theme(legend.position = "bottom") #For all models: Minimum reached when considering a 2 week lag

#Visualization of F-Statistic for different models
ggplot(accuracy_measures, aes(x = lag, fill = lag, y = Fstatistic)) +
    geom_bar(position = "dodge", stat = "identity") +
    ggtitle("Adjusted R Squared for different models") +
    facet_wrap(~Model) +
    theme_bw() +
    xlab("") +
    theme(legend.position = "bottom") #For most models: Maximum reached when considering a 2 week lag, exceptions: oOH, oOH2


#Judging from the plots above, a 2 week lag seems to be optimal. Hence, let us create a bar chart of the different models, comparing their adj R^2 for 2 week lag
accuracy_measures %>% filter(lag == "cOI_2weeksbefore") %>%
    ggplot(aes(x = fct_reorder(Model, AdjRSquared), y = AdjRSquared)) +
    geom_bar(stat = "identity", fill = "plum2") +
    coord_flip() +
    theme_minimal() +
    xlab("Model") +
    ylab("Adjusted R^2") +
    ggtitle("Comparison Of Adj R^2 for different models (considering a 2 week lag)")
