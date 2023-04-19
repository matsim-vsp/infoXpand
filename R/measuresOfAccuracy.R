library(tidyverse)
library(lubridate)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(arm)
library(ggokabeito)


ids <- c("oOH", "oOH2", "oOH+oOH2", "oOH+tmax", "oOH+tavg",
          "oOH*tmax", "oOH*tavg", "oOH+out", "oOH+out2", "oOH*out", "oOH*out2",
          "oOH+prcp", "oOH+tmax+prcp", "oOH+tavg+prcp", "oOH+out+prcp", "oOH+out2+prcp",
          "oOH:tmax:prcp", "oOH:tavg:prcp","oOH:out:prcp","oOH:out2:prcp",
          "oOH2*out", "oOH2*out2", "oOH*tmax2", "oOH*tavg2",
          "oOH*tmax*prcp", "oOH*tavg*prcp", "oOH*out*prcp", "oOH*out2*prcp", "logoOH+logtmax")

lags <- c("cOI", "cOI_1weekbefore", "cOI_2weeksbefore", "cOI_3weeksbefore", "cOI_4weeksbefore")

accuracy_measures <- data.frame(matrix(ncol = 9, nrow = 0))
colnames(accuracy_measures) <- c("Model", "lag", "Fstatistic", "RSE", "Rsquared", "AdjRSquared", "AIC", "BIC", "LOOCV_RMSE", "LOOCV_R2", "LOOCV_MAE")

#Iterating through all our models and lags to extract the following measures of id accuracy
    #F-statistic
    #RSE
    #R Squared
    #Adjusted RSquared

for (id in ids) {
    for (lag in lags) {
        accuracy_measures[nrow(accuracy_measures) + 1, 1] <- id
        accuracy_measures[nrow(accuracy_measures), 2] <- lag

        #F-Statistic -> needs to be >> 1
        accuracy_measures[nrow(accuracy_measures), 3] <- summary(resultsList[[id]][[lag]][["Model"]])$fstatistic[1]

        #RSE -> needs to be small
        accuracy_measures[nrow(accuracy_measures), 4] <- summary(resultsList[[id]][[lag]][["Model"]])$sigma

        #R^2 -> needs to be close to 1
        accuracy_measures[nrow(accuracy_measures), 5] <- summary(resultsList[[id]][[lag]][["Model"]])$r.squared

        #Adj. R^2 -> needs to be close to 1, accounts for the fact that more variables -> better fit
        accuracy_measures[nrow(accuracy_measures), 6] <- summary(resultsList[[id]][[lag]][["Model"]])$adj.r.squared

        #AIC -> Should be small
        accuracy_measures[nrow(accuracy_measures), 7] <- AIC(resultsList[[id]][[lag]][["Model"]])

        #BIC -> Should be small
        accuracy_measures[nrow(accuracy_measures), 8] <- BIC(resultsList[[id]][[lag]][["Model"]])

        #RMSE after LOOCV
        accuracy_measures[nrow(accuracy_measures), 9] <- resultsList[[id]][[lag]][["Model_LOOCV"]][[4]][[2]]

        #Adjuested Rsquared LOOCV
        accuracy_measures[nrow(accuracy_measures), 10] <- resultsList[[id]][[lag]][["Model_LOOCV"]][[4]][[3]]

        #MAE LOOCV (lees sensitive to outliers than RMSE)
        accuracy_measures[nrow(accuracy_measures), 11] <- resultsList[[id]][[lag]][["Model_LOOCV"]][[4]][[4]]
    }
}

colnames(accuracy_measures) <- c("Model", "lag", "Fstatistic", "RSE", "Rsquared", "AdjRSquared", "AIC", "BIC", "LOOCV_RMSE", "LOOCV_R2", "LOOCV_MAE")

accuracy_measures <- accuracy_measures %>% mutate(lag_plot = case_when(lag =="cOI" ~ "0 lag",
                                                                    lag == "cOI_1weekbefore" ~ "1 week lag",
                                                                    lag == "cOI_2weeksbefore" ~ "2 weeks lag",
                                                                    lag == "cOI_3weeksbefore" ~ "3 weeks lag",
                                                                    lag == "cOI_4weeksbefore" ~ "4 weeks lag")) %>%
                                            mutate(model_no = case_when(Model == "oOH" ~ "1",
                                                                        Model == "oOH2" ~ "2", 
                                                                        Model == "oOH+oOH2" ~ "3",
                                                                        Model == "oOH+tmax" ~ "4a",
                                                                        Model == "oOH+tavg" ~ "4b",
                                                                        Model == "oOH*tmax" ~ "5a",
                                                                        Model == "oOH*tavg" ~ "5b",
                                                                        Model == "oOH+out" ~ "6a",
                                                                        Model == "oOH+out2" ~ "6b",
                                                                        Model == "oOH*out" ~ "7a",
                                                                        Model == "oOH*out2" ~ "7b",
                                                                        Model == "oOH+prcp" ~ "8",
                                                                        Model == "oOH+tmax+prcp" ~ "9a",
                                                                        Model == "oOH+tavg+prcp" ~ "9b",
                                                                        Model == "oOH+out+prcp" ~ "10a",
                                                                        Model == "oOH+out2+prcp" ~ "10b",
                                                                        Model == "oOH:tmax:prcp" ~ "11a",
                                                                        Model == "oOH:tavg:prcp" ~ "11b",
                                                                        Model == "oOH:out:prcp" ~ "12a",
                                                                        Model == "oOH:out2:prcp" ~ "12b",
                                                                        Model == "oOH2*out" ~ "13a",
                                                                        Model == "oOH2*out2" ~ "13b",
                                                                        Model == "oOH*tmax2" ~ "14a",
                                                                        Model == "oOH*tavg2" ~ "14b",
                                                                        Model == "oOH*tmax*prcp" ~ "15a",
                                                                        Model == "oOH*tavg*prcp" ~ "15b",
                                                                        Model == "oOH*out*prcp" ~ "16a",
                                                                        Model == "oOH*out2*prcp" ~ "16b",
                                                                        Model == "logoOH+logtmax" ~ "17"))

accuracy_measures$model_no <- factor(accuracy_measures$model_no, levels = c("1", "2", "3", "4a", "4b", "5a", "5b", "6a", "6b", "7a", "7b", "8", "9a", "9b", "10a", "10b", "11a", "11b", "12a", "12b", "13a", "13b", "14a", "14b", "15a", "15b", "16a", "16b", "17"))

# To find the most accurate model, we first look for the most adequate model for each lag and for the most adequate lag for each model
# Finding the most adequate model for each lag:
comparing_measures_eachlag <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(comparing_measures_eachlag) <- c("lag", "measure", "model", "model_no", "value")

for (chosen_lag in unique(accuracy_measures$lag)) {
    filtered <- filter(accuracy_measures, lag == chosen_lag)
    filtered$model_no <- as.character(filtered$model_no)
    comparing_measures_eachlag[nrow(comparing_measures_eachlag) + 1, 1] <- chosen_lag
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 2] <- "AIC"
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 3] <- filtered[which.min(filtered$AIC), 1]
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 4] <- filtered[which.min(filtered$AIC), 13]
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 5] <- filtered[which.min(filtered$AIC), 7]
    
    comparing_measures_eachlag[nrow(comparing_measures_eachlag) + 1, 1] <- chosen_lag
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 2] <- "BIC"
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 3] <- filtered[which.min(filtered$BIC), 1]
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 4] <- filtered[which.min(filtered$BIC), 13]
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 5] <- filtered[which.min(filtered$BIC), 8]

    comparing_measures_eachlag[nrow(comparing_measures_eachlag) + 1, 1] <- chosen_lag
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 2] <- "AdjRSquared"
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 3] <- filtered[which.max(filtered$AdjRSquared), 1]
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 4] <- filtered[which.max(filtered$AdjRSquared), 13]
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 5] <- filtered[which.max(filtered$AdjRSquared), 6]

    comparing_measures_eachlag[nrow(comparing_measures_eachlag) + 1, 1] <- chosen_lag
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 2] <- "LOOCV_RMSE"
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 3] <- filtered[which.min(filtered$LOOCV_RMSE), 1]
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 4] <- filtered[which.min(filtered$LOOCV_RMSE), 13]
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 5] <- filtered[which.min(filtered$LOOCV_RMSE), 9]

    comparing_measures_eachlag[nrow(comparing_measures_eachlag) + 1, 1] <- chosen_lag
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 2] <- "LOOCV_R2"
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 3] <- filtered[which.max(filtered$LOOCV_R2), 1]
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 4] <- filtered[which.max(filtered$LOOCV_R2), 13]
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 5] <- filtered[which.max(filtered$LOOCV_R2), 10]
    
    comparing_measures_eachlag[nrow(comparing_measures_eachlag) + 1, 1] <- chosen_lag
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 2] <- "LOOCV_MAE"
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 3] <- filtered[which.min(filtered$LOOCV_MAE), 1]
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 4] <- filtered[which.min(filtered$LOOCV_MAE), 13]
    comparing_measures_eachlag[nrow(comparing_measures_eachlag), 5] <- filtered[which.min(filtered$LOOCV_MAE), 11]
}

# Finding the most accurate lag for each model :
comparing_measures_eachmodel <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(comparing_measures_eachmodel) <- c("model", "model_no", "criterion", "lag", "value")

for (chosen_model in unique(accuracy_measures$Model)){
    filtered <- filter(accuracy_measures, Model == chosen_model)
    filtered$model_no <- as.character(filtered$model_no)

    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel) + 1, 1] <- chosen_model
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 2] <- filtered[which.min(filtered$AIC), 13]
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 3] <- "AIC"
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 4] <- filtered[which.min(filtered$AIC), 2]
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 5] <- filtered[which.min(filtered$AIC), 7]

    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel) + 1, 1] <- chosen_model
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 2] <- filtered[which.min(filtered$BIC), 13]
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 3] <- "BIC"
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 4] <- filtered[which.min(filtered$BIC), 2]
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 5] <- filtered[which.min(filtered$BIC), 8]

    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel) + 1, 1] <- chosen_model
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 2] <- filtered[which.max(filtered$AdjRSquared), 13]
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 3] <- "AdjRSquared"
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 4] <- filtered[which.max(filtered$AdjRSquared), 2]
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 5] <- filtered[which.max(filtered$AdjRSquared), 6]

    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel) + 1, 1] <- chosen_model
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 2] <- filtered[which.min(filtered$LOOCV_RMSE), 13]
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 3] <- "LOOCV_RMSE"
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 4] <- filtered[which.min(filtered$LOOCV_RMSE), 2]
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 5] <- filtered[which.min(filtered$LOOCV_RMSE), 9]

    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel) + 1, 1] <- chosen_model
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 2] <- filtered[which.min(filtered$LOOCV_MAE), 13]
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 3] <- "LOOCV_MAE"
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 4] <- filtered[which.min(filtered$LOOCV_MAE), 2]
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 5] <- filtered[which.min(filtered$LOOCV_MAE), 11]

    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel) + 1, 1] <- chosen_model
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 2] <- filtered[which.max(filtered$LOOCV_R2), 13]
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 3] <- "LOOCV_R2"
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 4] <- filtered[which.max(filtered$LOOCV_R2), 2]
    comparing_measures_eachmodel[nrow(comparing_measures_eachmodel), 5] <- filtered[which.max(filtered$LOOCV_R2), 10]

}

#Visualization of adjusted R^2 for different models
ggplot(accuracy_measures, aes(x = lag_plot, fill = lag_plot, y = AdjRSquared)) +
    geom_bar(position = "dodge", stat = "identity") +
    #ggtitle("Adjusted R Squared for different models") +
    facet_wrap(~model_no) +
    theme_bw() +
    xlab("") +
    ylab("Adjusted R^2") +
    scale_fill_okabe_ito() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "none") #For all models: Maximum reached when considering a 2 week lag

#Visualization of RSE for different models
ggplot(accuracy_measures, aes(x = lag_plot, fill = lag_plot, y = RSE)) +
    geom_bar(position = "dodge", stat = "identity") +
   # ggtitle("Adjusted R Squared for different models") +
    facet_wrap(~model_no) +
    theme_bw() +
    xlab("") +
    ylab("RSE") + 
    scale_fill_okabe_ito() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "none") #For all models: Minimum reached when considering a 2 week lag

#Visualization of F-Statistic for different models
ggplot(accuracy_measures, aes(x = lag_plot, fill = lag_plot, y = Fstatistic)) +
    geom_bar(position = "dodge", stat = "identity") +
  #  ggtitle("Adjusted R Squared for different models") +
    facet_wrap(~model_no) +
    theme_bw() +
    xlab("") +
    ylab("F-Statistic") +
    scale_fill_okabe_ito() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "none") #For most models: Maximum reached when considering a 2 week lag, exceptions: oOH, oOH2


#Visualization of AIC or different models
ggplot(accuracy_measures, aes(x = lag_plot, fill = lag_plot, y = AIC)) +
    geom_bar(position = "dodge", stat = "identity") +
  #  ggtitle("Adjusted R Squared for different models") +
    facet_wrap(~model_no) +
    theme_bw() +
    xlab("") +
    ylab("AIC") +
    scale_fill_okabe_ito() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "none") #For most models: Maximum reached when considering a 2 week lag, exceptions: oOH, oOH2

#Visualization for BIC for different models
ggplot(accuracy_measures, aes(x = lag_plot, fill = lag_plot, y = BIC)) +
    geom_bar(position = "dodge", stat = "identity") +
  #  ggtitle("Adjusted R Squared for different models") +
    facet_wrap(~model_no) +
    theme_bw() +
    xlab("") +
    ylab("BIC") +
    scale_fill_okabe_ito() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "none") #For most models: Maximum reached when considering a 2 week lag, exceptions: oOH, oOH2


#Visualization of LOOCV RMSE for different models
ggplot(accuracy_measures, aes(x = lag_plot, fill = lag_plot, y = LOOCV_RMSE)) +
    geom_bar(position = "dodge", stat = "identity") +
   # ggtitle("RMSE for different models") +
    facet_wrap(~model_no) +
    theme_bw() +
    xlab("") +
    ylab("RMSE") +
    scale_fill_okabe_ito() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "none")

#Visualization of LOOCV R2 for different models
ggplot(accuracy_measures, aes(x = lag, fill = lag_plot, y = LOOCV_R2)) +
    geom_bar(position = "dodge", stat = "identity") +
   # ggtitle("R Squared for different models") +
    facet_wrap(~Model) +
    theme_bw() +
    xlab("") +
    ylab("Adjuested R^2") +
    scale_fill_okabe_ito() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "none")

#Visualization of LOOCV MAE for different models
ggplot(accuracy_measures, aes(x = lag, fill = lag_plot, y = LOOCV_MAE)) +
    geom_bar(position = "dodge", stat = "identity") +
   # ggtitle("MAE for different models") +
    facet_wrap(~Model) +
    theme_bw() +
    xlab("") +
    ylab("MAE") +
    scale_fill_okabe_ito() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "none")

#Judging from the plots above, a 2 week lag seems to be optimal. Hence, let us create a bar chart of the different models, comparing their adj R^2 for 2 week lag
accuracy_measures %>% filter(lag == "cOI_2weeksbefore") %>%
    ggplot(aes(x = fct_reorder(Model, AdjRSquared), y = AdjRSquared)) +
    geom_bar(stat = "identity", fill = "plum2") +
    coord_flip() +
    theme_minimal() +
    xlab("Model") +
    ylab("Adjusted R^2") +
    ggtitle("Comparison Of Adj R^2 for different models (considering a 2 week lag)")
