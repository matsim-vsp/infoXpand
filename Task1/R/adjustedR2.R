library(tidyverse)

#### This r script computes (adjusted r2) for the no-intercept models from regressionAnalysis_Nat.R
#### Author: S. Paltra @ TU Berlin

#Computation of r2 for no-intercept models
r2 <- function(data, lag, model){
    data <- data %>% filter(Date > "2020-03-08") %>% filter(Date < "2020-12-20")
 1 - sum((data[[lag]] - predict(resultsList[[model]][[lag]][["Model"]]))^2)/sum((data[[lag]] - mean(data[[lag]]))^2)
}

#Computation of r2 for quadratic model with no intercept (cOI_Xweeksbefore ~ 0 + oOH^2)
r2_noInt_0weeks <- r2(joinedDataFrame, "cOI", "oOH2_noInt")
r2_noInt_1weeks <- r2(joinedDataFrame, "cOI_1weekbefore", "oOH2_noInt")
r2_noInt_2weeks <- r2(joinedDataFrame, "cOI_2weeksbefore", "oOH2_noInt")
r2_noInt_3weeks <- r2(joinedDataFrame, "cOI_3weeksbefore", "oOH2_noInt")
r2_noInt_4weeks <- r2(joinedDataFrame, "cOI_4weeksbefore", "oOH2_noInt")

#Computation of r2 model including linear out2 (cOI_2weeksbefore ~ 0 + oOH2 + oOH2:outdoorFraction)
r2_oOHtimesout2_noInt_0weeks <- r2(joinedDataFrame, "cOI", "oOH2*out2_noInt")
r2_oOHtimesout2_noInt_1weeks <- r2(joinedDataFrame, "cOI_1weekbefore", "oOH2*out2_noInt")
r2_oOHtimesout2_noInt_2weeks <- r2(joinedDataFrame, "cOI_2weeksbefore", "oOH2*out2_noInt")
r2_oOHtimesout2_noInt_3weeks <- r2(joinedDataFrame, "cOI_3weeksbefore", "oOH2*out2_noInt")
r2_oOHtimesout2_noInt_4weeks <- r2(joinedDataFrame, "cOI_4weeksbefore", "oOH2*out2_noInt")

#Computation of r2 for chosen model (cOI_2weeksbefore ~ 0 + oOH2 + oOH2:outdoorFraction)
r2_weather_noInt_0weeks <- r2(joinedDataFrame, "cOI", "oOH2+oOH2:out2_noInt")
r2_weather_noInt_1week <- r2(joinedDataFrame, "cOI_1weekbefore", "oOH2+oOH2:out2_noInt")
r2_weather_noInt_2weeks <- r2(joinedDataFrame, "cOI_2weeksbefore", "oOH2+oOH2:out2_noInt")
r2_weather_noInt_3weeks <- r2(joinedDataFrame, "cOI_3weeksbefore", "oOH2+oOH2:out2_noInt")
r2_weather_noInt_4weeks <- r2(joinedDataFrame, "cOI_4weeksbefore", "oOH2+oOH2:out2_noInt")
#Computation of r2 for models using tmax/tavg
r2_tmax_noInt_2weeks <- r2(joinedDataFrame, "cOI_2weeksbefore", "oOH2+oOH2*tmax_noInt")
r2_tavg_noInt_2weeks <- r2(joinedDataFrame, "cOI_2weeksbefore", "oOH2+oOH2*tavg_noInt")

r2_tmaxprcp_noInt_2weeks <- r2(joinedDataFrame, "cOI_2weeksbefore", "oOH2:tmax:prcp_noInt")
r2_tavgprcp_noInt_2weeks <- r2(joinedDataFrame, "cOI_2weeksbefore", "oOH2:tavg:prcp_noInt")
r2_out2prcp_noInt_2weeks <- r2(joinedDataFrame, "cOI_2weeksbefore", "oOH2:out2:prcp_noInt")

r2_best_subset <- r2(joinedDataFrame, "cOI_2weeksbefore", "oOH3+oOHout+oOH2out")
adj <- adjustedr2(r2_best_subset, 40, 2)
#Computation of r2 for mobility only model
r2_noInt <- r2(joinedDataFrame, "cOI_2weeksbefore", "oOH2_noInt")

#Computation of r2 when using google parks data
r2_google_noInt_2weeks <- r2(joinedDataFrame, "cOI_2weeksbefore", "oOH2+oOH2:google_noInt")

r2_adjout_noInt_2weeks <- r2(joinedDataFrame, "cOI_2weeksbefore", "oOH2+oOH2:adjout_noInt")


#Computation of r2 for mobility only model with linear, quadratic, and cubic mobility term
r2_allmobility <- r2(joinedDataFrame, "cOI_2weeksbefore", "oOH+oOH2+oOH3_noInt")

#Computation of adjusted r2 for no-intercept models
adjustedr2 <- function(r2, n, p){
 1 - ((1-r2)*(n-1))/(n-p)
}

#Computation of adjusted r2 for chosen model
adjr2_weather_noInt_0weeks <- adjustedr2(r2_weather_noInt_0weeks, 40, 2)
adjr2_weather_noInt_1week <- adjustedr2(r2_weather_noInt_1week, 40, 2)
adjr2_weather_noInt_2weeks <- adjustedr2(r2_weather_noInt_2weeks, 40, 2)
adjr2_weather_noInt_3weeks <- adjustedr2(r2_weather_noInt_3weeks, 40, 2)
adjr2_weather_noInt_4weeks <- adjustedr2(r2_weather_noInt_4weeks, 40, 2)

#Computation of adjusted r2 for models using tmax/tavg
adjr2_tmax_noInt_2weeks <- adjustedr2(r2_tmax_noInt_2weeks, 40, 2)
adjr2_tavg_noInt_2weeks <- adjustedr2(r2_tavg_noInt_2weeks, 40, 2)


adjr2_tmaxprcp_noInt_2weeks <- adjustedr2(r2_tmaxprcp_noInt_2weeks, 40, 2)
adjr2_tavgprcp_noInt_2weeks <- adjustedr2(r2_tavgprcp_noInt_2weeks, 40, 2)
adjr2_out2prcp_noInt_2weeks <- adjustedr2(r2_out2prcp_noInt_2weeks, 40, 2)
 
#Computation of r2 for mobility only model
adjr2_noInt_0weeks <- adjustedr2(r2_noInt_0weeks, 40, 2)
adjr2_noInt_1weeks <- adjustedr2(r2_noInt_1weeks, 40, 2)
adjr2_noInt_2weeks <- adjustedr2(r2_noInt_2weeks, 40, 2)
adjr2_noInt_3weeks <- adjustedr2(r2_noInt_3weeks, 40, 2)
adjr2_noInt_4weeks <- adjustedr2(r2_noInt_4weeks, 40, 2)

#Computation of adj r2 for mobility including linear out
adr2_oOHtimesout2_noInt_0weeks <- adjustedr2(r2_oOHtimesout2_noInt_0weeks, 40, 2)
adr2_oOHtimesout2_noInt_1weeks <- adjustedr2(r2_oOHtimesout2_noInt_1weeks, 40, 2)
adr2_oOHtimesout2_noInt_2weeks <- adjustedr2(r2_oOHtimesout2_noInt_2weeks, 40, 2)
adr2_oOHtimesout2_noInt_3weeks <- adjustedr2(r2_oOHtimesout2_noInt_3weeks, 40, 2)
adr2_oOHtimesout2_noInt_4weeks <- adjustedr2(r2_oOHtimesout2_noInt_4weeks, 40, 2)

#Computation of r2 when using google parks data
adjr2_noInt_2weeks_google <- adjustedr2(r2_google_noInt_2weeks, 40, 2)
adjr2_noInt_2weeks_adjout <- adjustedr2(r2_adjout_noInt_2weeks, 40, 2)

adjr2_allmobility <- adjustedr2(r2_allmobility, 40, 2)
