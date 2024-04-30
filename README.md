# infoXpand

This repository contains all simulation and estimation work done by the [Transport Systems Planning and Transport Telematics group](https://www.tu.berlin/vsp) of [Technische Universität Berlin](https://www.tu-berlin.de) relating to subproject 2 of the infoXpand project. Each folder corresponds to one of the tasks of the project.

## Task 1

### R

#### Data prep

- **PrepIncidenceData.R**
 Reads in and prepares [RKI](https://github.com/robert-koch-institut/COVID-19_7-Tage-Inzidenz_in_Deutschland) COVID-19 incidence at the national level.

- **PrepMobilityData.R**
Reads in and prepares mobility data (daily "out of home duration" in hours per person) at the national level.

- **PrepWeatherData.R**
Reads in and prepares climate/weather data from [METEOSTAT](https://meteostat.net/en/) at the national level.

#### Estimation

- **ExploratoryWorkAndPlots.R** : Exploratory data analysis by visualizing the data frames created in PrepIncidenceData.R, PrepMobilityData.R, PrepWeatherData.R . The (in)dependent variables are plotted over time and various scatter plots are created

- **regressionAnalysis_Nat.R** : A variety of regression models are set up and their results are saved within a list. For each regression model, we save the model itself, cross validation results (LOOCV and 10-fold CV), a plot of the observed vs. the predicted values and a plot of predicted/observed values over time. After models are run, we use multiple model selection techniques: Elastic net, 10-fold CV, regular subset selection. Then, the regression lines of the linear/quadratic/cubic model are placed on the scatter plot of the dependent vs. the independent variable. Finally, the regression results are plotted in 3D. The latter half of this script, might be moved to another script. 

- **adjustedR2.R** : Computes adjusted R^2 for the no-intercept regression models from regressionAnalysis_Nat.R. The functions introduced here may also be applied to the models from regressionAnalysis_Fed.R.

- **measuresOfAccuracy.R** : Helps to identify the model which most accurately describes the data. To this end, the following measures of accuracy are considered: F-statistic, RSE, R Squared, Adjusted RSquared.

#### Miscellaneous scripts contained in this folder

- **AppleMobilityFata.R** : Apple mobility data, which was provided by Apple from April 2020 until April 2022 is read and cleaned. Here, we filter for Germany, and plot weekly changes for the different federal states. 

- **GoogleMobilityData.R** : Google mobility data, which was provided by Google until October 2022 is read and cleaned. Here, we filter for Germany, turn daily into weekly values, and plot the data for Germany's 16 federal states.

- **plotSNZGoogleApple.R** : Creation of plots to compare the three different mobility data sources.

### Julia

- **model.jl** : Implemementation of the toy model thats demonstrates the quadratic effect of reduction of mobility on infection growth. The 3 main building blocks are: 1. The construction of the agents, 2. The construction of the environment the agents "live in", 3. The construction of the agents' behavior or in other words: how the infection spreads. 

- **plotting.jl** : Contains the function person_color that colors the agents according to their health status. 

- **interactive.jl** : Allows the user to create an interactive plot of the model. 

- **video.jl** : Creates videos of exemplary model runs. 