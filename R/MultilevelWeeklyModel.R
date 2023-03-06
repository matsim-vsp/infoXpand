library(tidyverse)
library(lme4)
library(lmerTest)

#In order to set up joinedDataFrame one first needs to run PrepIncidenceData.R, PrepMobilityData.R and PrepWeatherData.R
joinedDataFrame <- inner_join(incidence_data, mobility_data, by = c("Date", "Bundesland"))
joinedDataFrame <- left_join(joinedDataFrame, weather_data_all, by = c("Date", "Bundesland"))

joinedDataFrame <- joinedDataFrame %>% filter(Date > min(joinedDataFrame$Date))
joinedDataFrame <- filter(joinedDataFrame, changeOfIncidencelaggedWed2 < 3)

ggplot(data = joinedDataFrame,
       aes(x = outOfHomeDuration, y = changeOfIncidencelaggedWed2, col = as.factor(Bundesland))) + geom_point(size= 1,
             alpha    = .7, position = "jitter") +
  geom_smooth(method = lm, se = FALSE, size = 1.5, linetype = 1, alpha = .7) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title    = "Linear Relationship Between outOfHomeDuration and changeOfIncidence", 
       subtitle = "by federal State") +
  scale_color_manual(name   = "Federal State",
                     labels = c("BW", "BY", "BE", "BB", "HB", "GER", "HH", "HE", "MP", "NI", "NW", "RP", "SL", "SN", "ST", "SH", "TH"),
                     values = c("aquamarine", "lightblue", "pink", "brown1", "darkgoldenrod1", "chartreuse4", "darkgreen", "deepskyblue", "darkmagenta", "darkred", "deeppink", "darkslateblue", "lightpink3", "darkgray","peachpuff", "seagreen4", "plum"))


#InterceptOnlyModel
interceptonlymodel <- lmer(formula = changeOfIncidencelaggedWed2 ~ 1 + (1|Bundesland),
                         data = joinedDataFrame)

summary(interceptonlymodel)

#First level predictors
firstlevelmodel <- lmer(formula = changeOfIncidencelaggedWed2 ~ 1 + outOfHomeDuration * tmax + (1|Bundesland), 
               data    = joinedDataFrame)

summary(firsdtlevelpredictors)

#Second level predictors
#First: We need to manipulate the dataframe
joinedDataFrame <- joinedDataFrame %>% mutate(outOfHomeDurationGesamt = )
firstlevelmodel <- lmer(formula = changeOfIncidencelaggedWed2 ~ 1 + outOfHomeDuration * tmax + (1|Bundesland), 
               data    = joinedDataFrame)

summary(firsdtlevelpredictors)
