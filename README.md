# infoXpand

This repository contains all simulation and estimation work done by the [Transport Systems Planning and Transport Telematics group](https://www.tu.berlin/vsp) of [Technische Universität Berlin](https://www.tu-berlin.de) relating to subproject 2 of the infoXpand project.

## Contents of this repository


### Data prep

- **PrepIncidenceData.R**
Script which prepares incidence data.

- **PrepMobilityData.R**
Script which prepares mobility data on national and federal state level (Germany).

- **PrepWeatherData.R**
Script which reads in and cleans weather data from meteostat.org.

### Estimation

- **regressioninfoXpandNationalWeekly.R** : This script combines data import, data cleaning, first exploratory visualizations as well as 17 different regression models. First, incidence data from RKI, aggregated mobility data from our MODUS-Covid project and weather data from meteostat are imported, cleaned and joined into a single data frame. Second, the following measures are plotted over time: Incidence, change of incidence (weekly), out of home duration (in hours per person and day), tmax (in celsius). Additionally, raw corrlations are computed. Finally, 17 regression models are set up and their results are saved within a list. For each regression model, we save the model itself, a scatter plot of a linar combinations of the regressors vs change of incidence, 4 diagnostic plots (residuals vs fitted values, ggplot, scale-loc plot, cook's distance plot), a plot of the predicted vs the actual values (including the regression line),

- **measuresOfAccuracy.R** : The aim of this script is to identify the model which most accurately describes the data. To this end, the following measures of accuracy are considered: F-statistic, RSE, R Squared, Adjusted RSquared.

- **subsetselection.R** : Trying to improve the results of **regressioninfoXpandNationalWeekly.R** and **measuresOfAccuracy.R** by using a subset selection algorithm. This does not improve the results of our manual regression.

- **BeyondLinearityNationalWeekly.R** : The residuals vs fitted values plot suggest a non-linearity in the relationship between changeOfIncidence and outOfHomeDuration*tmax. Hence, this script aims to extend the linear models considered in **regressioninfoXpandNationalWeekly.R**. First, polynomial regression is considered making outOfHomeDuration the only independet variable and considering it up until degree 2. Second, again polynomial regression is considered with outOfHomeDuration and tmax up until degree 2 (no mix terms). Third, a GAM is considered. We use both natural and smoothing splines for outOfHomeDuration and tmax. 

- **autoML.R** : We used autoML to improve the models we have (thus far) set up manually. Hereby, we make use of the automl package as well as the H_2O framework.

- **regressioninfoXpandFederalStateWeekly.R** : Currently on hold, until our work on a national level is finished.

- **regressioninfoXpandFederalStateMonthly.R** : Currently on hold, until our work on a national level is finished.

### Miscellaneous scripts contained in this repository

- **applemobilitydata.R** : Apple mobility data, which was provided by Apple from April 2020 until April 2022 is read and cleaned. Here, we filter for Germany, filter for Germany and plot weekly changes for the different federal states. 

- **googlemobilitydatacleaning.R** : Google mobility data, which was provided by Google until October 2022 is read and cleaned. Here, we filter for Germany, turn daily into weekly values, and plot the data for Germany's 16 federal states.