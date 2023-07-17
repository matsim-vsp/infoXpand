library(tidyverse)

#Computation of r2 for no-intercept models
r2 <- function(data, lag, model){
 if(lag == "cOI") {
    data <- data %>% filter(cOI < 2) %>% filter(Date < "2021-01-01")
 }
  if(lag == "cOI_1weekbefore") {
    data <- data %>% filter(cOI_1weekbefore < 2) %>% filter(Date + 7 < "2021-01-01")
 }
  if(lag == "cOI_2weeksbefore") {
    data <- data %>% filter(cOI_2weeksbefore < 2) %>% filter(Date + 14 < "2021-01-01")
 }
 1 - sum((data[[lag]] - predict(resultsList[[model]][[lag]][["Model"]]))^2)/sum((data[[lag]] - mean(data[[lag]]))^2)
}

r2_weather_noInt_0weeks <- r2(joinedDataFrame, "cOI", "oOH2+oOH2:out2_noInt")
r2_weather_noInt_1week <- r2(joinedDataFrame, "cOI_1weekbefore", "oOH2+oOH2:out2_noInt")
r2_weather_noInt_2weeks <- r2(joinedDataFrame, "cOI_2weeksbefore", "oOH2+oOH2:out2_noInt")
r2_weather_noInt_3weeks <- r2(joinedDataFrame, "cOI_3weeksbefore", "oOH2+oOH2:out2_noInt")
r2_weather_noInt_4weeks <- r2(joinedDataFrame, "cOI_4weeksbefore", "oOH2+oOH2:out2_noInt")

#Computation of adjusted r2 for no-intercept models
adjustedr2 <- function(r2, n, p){
 1 - ((1-r2)*(n-1))/(n-p)
}

adjr2_weather_noInt <- adjustedr2(r2_weather_noInt, 40, 2)
adjr2_weather <- adjustedr2(r2_weather, 40, 2)
adjr2_noweather_noInt <- adjustedr2(r2_noweather_noInt, 40, 1)
adjr2_noweather <- adjustedr2(r2_noweather, 40, 1)

