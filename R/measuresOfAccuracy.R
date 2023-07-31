library(tidyverse)
library(lubridate)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(arm)
library(ggokabeito)
library(lindia)

#Different model names
ids <- c("oOH", "oOH2", "oOH3", "oOH+oOH2", "oOH+tmax", "oOH+tavg",
          "oOH*tmax", "oOH*tavg", "oOH+oOH2+out", "oOH+oOH2+out2", "oOH+out", "oOH+out2", "oOH2+out", "oOH2+out2", "oOH*out", "oOH*out2",
          "oOH+prcp", "oOH+tmax+prcp", "oOH+tavg+prcp", "oOH+out+prcp", "oOH+out2+prcp",
          "oOH:tmax:prcp", "oOH:tavg:prcp", "oOH:out:prcp", "oOH:out2:prcp",
          "oOH2*out", "oOH2*out2", "oOH*tmax2", "oOH*tavg2",
          "oOH*tmax*prcp", "oOH*tavg*prcp", "oOH*out*prcp", "oOH*out2*prcp", "logoOH+logtmax",
          "c_oOH", "c_oOH+c_tmax", "c_oOH+c_tavg", "c_oOH+c_out", "c_oOH+c_out2",
          "c_oOH*c_tmax", "c_oOH*c_tavg", "c_oOH*c_out", "c_oOH*c_out2", "oOH+oOH2+out2", "oOH+oOH2+out2_0Intercept", "oOH+oOH2+out2+gradtmax",
          "polyoOH+polyout2", "oOH*out2+oOH2", "oOH+oOH2*out2", "polyoOH3+polyout23", "polyoOH2+polyout23", "polyoOH2+out2+out23", "oOH2+out2_noInt", "oOH2*out2_noInt",
          "oOH2+tmax_noInt", "oOH2+tavg_noInt", "oOH2+out2+prcp_noInt", "logoOH+oOH2+out2_0Intercept", "oOH2+indoor", "polyoOH2+polyindoor_step1", "polyoOH2+polyindoor_step2", "polyoOH2+polyindoor_step3", "polyoOH2+polyindoor_step4", "polyoOH2+polyindoor_step5",
          "oOH+indoor", "oOH+oOH2+indoor", "oOH2+indoor", "oOH2*indoor", "oOH3+indoor", "oOH4+indoor", "oOH3+out2",
          "oOH2+oOH2:indoor", "oOH2+oOH2:indoor_noInt", "indoor+oOH2:indoor", "oOH2:indoor", "oOH2+oOH2:tmax", "oOH2+oOH2:tavg",
          "oOH+oOH:indoor", "indoor+oOH:indoor", "oOH:indoor", "out2_noInt",
          "oOH3+oOH3:indoor", "indoor+oOH3:indoor", "oOH3:indoor", "oOH4+oOH4:indoor", "oOH5+oOH5:indoor", "oOH6+oOH6:indoor",
          "oOH212+oOH212:indoor", "oOH213+oOH213:indoor", "oOH223+oOH223:indoor",
          "log_oOH", "log_oOH_noInt", "polyoOH3", "test", "test2", "test3", "oOH2+oOH2:out2", "oOH2+oOH2:out2_noInt",
          "oOH2+oOH2:tmax", "oOH2+oOH2:tmax_noInt", "oOH2+oOH2:tavg", "oOH2+oOH2:tavg_noInt", "oOH2_noInt",  "oOH3+oOHout+oOH2out")

#Different lags (0 to 5 week lag)
lags <- c("cOI", "cOI_1weekbefore", "cOI_2weeksbefore", "cOI_3weeksbefore", "cOI_4weeksbefore", "cOI_5weeksbefore")

#Creation of data frame, which will contain the different statistics
accuracy_measures <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(accuracy_measures) <- c("Model", "lag", "Fstatistic", "RSE", "Rsquared", "AdjRSquared", "AIC", "BIC")

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
    }
}

#Visualization of adjusted R^2 for different models
ggplot(accuracy_measures, aes(x = lag, fill = lag, y = AdjRSquared)) +
    geom_bar(position = "dodge", stat = "identity") +
    #ggtitle("Adjusted R Squared for different models") +
    facet_wrap(~Model) +
    theme_bw() +
    xlab("") +
    ylab("Adjusted R^2") +
    scale_fill_okabe_ito() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "none")

#Visualization of RSE for different models
ggplot(accuracy_measures, aes(x = lag, fill = lag, y = RSE)) +
    geom_bar(position = "dodge", stat = "identity") +
   # ggtitle("RSE for different models") +
    facet_wrap(~Model) +
    theme_bw() +
    xlab("") +
    ylab("RSE") + 
    scale_fill_okabe_ito() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "none") 

#Visualization of F-Statistic for different models
ggplot(accuracy_measures, aes(x = lag, fill = lag, y = Fstatistic)) +
    geom_bar(position = "dodge", stat = "identity") +
  #  ggtitle("F Statistic for different models") +
    facet_wrap(~Model) +
    theme_bw() +
    xlab("") +
    ylab("F-Statistic") +
    scale_fill_okabe_ito() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "none") 

#Visualization of AIC or different models
ggplot(accuracy_measures, aes(x = lag, fill = lag, y = AIC)) +
    geom_bar(position = "dodge", stat = "identity") +
  #  ggtitle("AIC for different models") +
    facet_wrap(~Model) +
    theme_bw() +
    xlab("") +
    ylab("AIC") +
    scale_fill_okabe_ito() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "none")

#Visualization for BIC for different models
ggplot(accuracy_measures, aes(x = lag, fill = lag, y = BIC)) +
    geom_bar(position = "dodge", stat = "identity") +
  #  ggtitle("BIC") +
    facet_wrap(~Model) +
    theme_bw() +
    xlab("") +
    ylab("BIC") +
    scale_fill_okabe_ito() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.position = "none") #For most models: Maximum reached when considering a 2 week lag, exceptions: oOH, oOH2

# Sec. 5.1 : Diagnostic plots for "oOH2+oOH2:out2_noInt"
# If one's interested in a different model, then line 130 must be adapted
# This came in extremely handy: https://rpubs.com/therimalaya/43190

joinedDataFrame_residuals <- joinedDataFrame %>% filter(cOI_2weeksbefore < 2) %>% filter(Date + 14 < "2021-01-01") %>%
        dplyr::select(Date, cOI_2weeksbefore, outOfHomeDuration, outOfHomeDurationSquared, indoorFraction) %>%
        mutate(predicted = predict(resultsList[["oOH2+oOH2:out2_noInt"]][["cOI_2weeksbefore"]][["Model"]])) %>% 
        mutate(residuals = cOI_2weeksbefore - predicted) %>%
        mutate(standardizesRes = scale(residuals)) %>%
        mutate(Date = Date + 14) %>%
        filter(Date < "2021-01-01")

# Plot of Standardized Residuals vs. predicted values
joinedDataFrame_residuals %>% 
  gather(key = "iv", value = "x", -cOI_2weeksbefore, -predicted, -standardizesRes, -residuals, -Date) %>%  # Get data into shape
  ggplot(aes(x = predicted, y = standardizesRes)) +  # Note use of `x` here and next line
  geom_point(color = "#7570B3", shape = 1, size = 2.5, stroke = 1.5) +
  #scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = FALSE) +
  theme_minimal() +
  ylab("Standardized Residuals") +
  xlab("Predicted Values") +
  geom_smooth(method ="loess", se = FALSE, linetype = "dashed", color = "#666666", size = 1.5) +
  theme(text = element_text(size = 13), axis.ticks.x = element_line(), 
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))

# Plot of Residuals vs. predicted values
joinedDataFrame_residuals %>% 
  gather(key = "iv", value = "x", -cOI_2weeksbefore, -predicted, -standardizesRes, -residuals, -Date) %>%  # Get data into shape
  ggplot(aes(x = predicted, y = residuals)) +  # Note use of `x` here and next line
  geom_point(color = "#7570B3", shape = 1, size = 2.5, stroke = 1.5) +
  #scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = FALSE) +
  theme_minimal() +
  ylab("Residuals") +
  xlab("Predicted Values") +
  geom_smooth(method ="loess", se = FALSE, linetype = "dashed", color = "#666666", size = 1.5) +
  theme(text = element_text(size = 13), axis.ticks.x = element_line(),
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))

#Plot of sqrt(abs(Standardized Residuals)) vs. predicted values
joinedDataFrame_residuals %>% 
  gather(key = "iv", value = "x", -cOI_2weeksbefore, -predicted, -standardizesRes, -residuals, -Date) %>%  # Get data into shape
  ggplot(aes(x = predicted, y = sqrt(abs(standardizesRes)))) +  # Note use of `x` here and next line
  geom_point(color = "#7570B3", shape = 1, size = 2.5, stroke = 1.5) +
  guides(color = FALSE) +
  theme_minimal() +
  ylab(expression(sqrt("|Standardized residuals|"))) +
  xlab("Predicted Values") +
  geom_smooth(method ="loess", se = FALSE, linetype = "dashed", color = "#666666", size = 1.5) +
theme(text = element_text(size = 13), axis.ticks.x = element_line(), 
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))

#Breusch-Pagan-Test to formally test for heteroskedasticity
lmtest::bptest(resultsList[["oOH2+oOH2:out2_noInt"]][["cOI_2weeksbefore"]][["Model"]])

#Qqplot
ggplot(joinedDataFrame_residuals, aes(sample=standardizesRes)) +
  stat_qq(shape = 1, size = 2.5, stroke = 1.5, color = "#7570B3") +
  stat_qq_line(color = "#666666", linetype = "dashed", size = 1.5) +
  theme_minimal() +
  xlab("Theoretical Quantiles") +
  ylab("Model Residual Quantiles") +
  theme(text = element_text(size = 13), axis.ticks.x = element_line(),
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))

#Cook's distance
ggplot(resultsList[["oOH2+oOH2:out2_noInt"]][["cOI_2weeksbefore"]][["Model"]], aes(seq_along(.cooksd)+1, .cooksd)) + 
    geom_bar(stat = "identity", position = "identity", fill = "#7570B3") +
    xlab("Observation Number") +
    ylab("Cook's distance") +
    theme_minimal() +
    theme(text = element_text(size = 13), axis.ticks.x = element_line(), 
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))

max(cooks.distance(resultsList[["oOH2+oOH2:out2_noInt"]][["cOI_2weeksbefore"]][["Model"]]))
which.max(cooks.distance(resultsList[["oOH2+oOH2:out2_noInt"]][["cOI_2weeksbefore"]][["Model"]]))