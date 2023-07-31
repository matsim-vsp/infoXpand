# infoXpand

This repository contains all simulation and estimation work done by the [Transport Systems Planning and Transport Telematics group](https://www.tu.berlin/vsp) of [Technische Universität Berlin](https://www.tu-berlin.de) relating to subproject 2 of the infoXpand project.

## Contents of this repository


### Data prep

- **PrepIncidenceData.R**
 Reads in and prepares [RKI](https://github.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland) COVID-19 incidence at the national level.

- **PrepIncidenceData_Fed.R**
 Reads in and prepares [RKI](https://github.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland) COVID-19 incidence data at the federal state level.

- **PrepMobilityData.R**
Reads in and prepares mobility data (daily "out of home duration" in hours per person) at the national level.

- **PrepMobilityData_Fed.R**
Reads in and prepares mobility data (daily "out of home duration" in hours per person) at the federal state level.

- **PrepWeatherData_Fed.R**
Reads in and prepares climate/weather data from [METEOSTAT](https://meteostat.net/en/) at the federal state level.

- **PrepWeatherData.R**
Reads in and prepares climate/weather data from [METEOSTAT](https://meteostat.net/en/) at the national level.

### Estimation

- **ExploratoryWorkAndPlots.R** : Exploratory data analysis by visualizing the data frames created in PrepIncidenceData.R, PrepMobilityData.R, PrepWeatherData.R . The (in)dependent variables are plotted over time and various scatter plots are created

- **ExploratoryWorkAndPlots_Fed.R** : Exploratory data analysis by visualizing the data frames created in PrepIncidenceData_Fed.R, PrepMobilityData_Fed.R, PrepWeatherData_Fed.R . The (in)dependent variables are plotted over time and various scatter plots are created.

- **regressionAnalysis_Nat.R** : A variety of regression models are set up and their results are saved within a list. For each regression model, we save the model itself, a scatter plot of a linar combinations of the regressors vs change of incidence, 4 diagnostic plots (residuals vs fitted values, ggplot, scale-loc plot, cook's distance plot), a plot of the predicted vs the actual values (including the regression line).

- **adjustedR2.R** : Computes adjusted R^2 for the no-intercept regression models from regressionAnalysis_Nat.R. The functions introduced here may also be applied to the models from regressionAnalysis_Fed.R.

- **measuresOfAccuracy.R** : Helps to identify the model which most accurately describes the data. To this end, the following measures of accuracy are considered: F-statistic, RSE, R Squared, Adjusted RSquared.

### Miscellaneous scripts contained in this repository

- **applemobilitydata.R** : Apple mobility data, which was provided by Apple from April 2020 until April 2022 is read and cleaned. Here, we filter for Germany, and plot weekly changes for the different federal states. 

- **googlemobilitydatacleaning.R** : Google mobility data, which was provided by Google until October 2022 is read and cleaned. Here, we filter for Germany, turn daily into weekly values, and plot the data for Germany's 16 federal states.

- **plotSNZGoogleApple.R** : Creation of plots to compare the three different mobility data sources.