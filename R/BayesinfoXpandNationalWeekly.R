library(tidyverse)
library(rethinking)
library(extraDistr)
library(bcp)

setwd("/Users/sydney/git/infoxpand/R")

#Read in different data sources
source("PrepIncidenceData.R")
source("PrepMobilityData.R")
source("PrepWeatherData.R")

#Only filter fot national data
mobility_data <- mobility_data %>%
        filter(Bundesland == "Gesamt")

weather_data_all <- weather_data_all %>%
        filter(Bundesland == "Gesamt") %>%
        filter(Date < "2021-01-01")
#First exploratory plots to infer distribution of parameters on national level
cOI <- incidence_data %>% filter(cOI < 2) %>%
ggplot(aes(x = cOI)) +
geom_histogram(binwidth = 0.1, color = "black", fill = "white") +
theme_minimal() +
ggtitle("change of Incidence")

png("DensityPlotoutOfHomeDuration.png")
M <- mobility_data %>%
ggplot(aes(x = outOfHomeDuration)) +
geom_histogram(binwidth = 0.3, color = "black", fill = "white") +
theme_minimal() +
ggtitle("Out Of Home Duration") +
theme(plot.title = element_text(size = 8))

dens(mobility_data$outOfHomeDuration, main = "Density plot OutOfHome Duration (M)")

M2 <- mobility_data %>% mutate(outOfHomeDurationSquared = outOfHomeDuration * outOfHomeDuration) %>%
ggplot(aes(x = outOfHomeDurationSquared)) +
geom_histogram(binwidth = 4, color = "black", fill = "white") +
theme_minimal() +
ggtitle("Out Of Home Duration Squared") +
theme(plot.title = element_text(size = 8))

dens(mobility_data$outOfHomeDurationSquared, main = "Density plot OutOfHome Duration Squared (M^2)")

tmax <- weather_data_all  %>%
ggplot(aes(x = tmax)) +
geom_histogram(binwidth = 1, color = "black", fill = "white") +
theme_minimal() +
ggtitle("tmax") +
theme(plot.title = element_text(size = 8))

png("DensityPlottmax.png")
dens(weather_data_all$tmax, main = "Density plot tmax")

oF <- weather_data_all  %>%
ggplot(aes(x = outdoorFraction2)) +
geom_histogram(binwidth = 0.3, color = "black", fill = "white") +
theme_minimal() +
ggtitle("outdoorFraction2") +
theme(plot.title = element_text(size = 8))

dens(weather_data_all$outdoorFraction2, main = "Density plot outdoorFraction2")

weather_data_all  %>%
ggplot(aes(x = prcp)) +
geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
theme_minimal() +
ggtitle("Precipitation")

dens(weather_data_all$prcp, main = "Density plot outdoorFraction")


g <- arrangeGrob(cOI, M, tmax, nrow = 1)
g2 <- arrangeGrob(cOI, M, M2, oF, nrow = 1)

#### Now, let's the model for M + M^2 + out2 as a Bayesian model

mobility_data <- mobility_data %>% filter(Bundesland == "Gesamt")
joinedDataFrame <- inner_join(incidence_data, mobility_data, by = "Date")
                                                             
#Joining the data frames
weather_data_all <- weather_data_all %>% filter(Bundesland == "Gesamt")
joinedDataFrame <- left_join(joinedDataFrame, weather_data_all, by = "Date")

#Only for 2020 (and a bit of 2021, as not too many people were vaccinated and alpha wasn't dominant yet)
joinedDataFrame <- filter(joinedDataFrame, Date < "2021-01-01") %>%
                      distinct()

joinedDataFrame <- joinedDataFrame %>% mutate(Welle = case_when(Date < "2020-05-17" ~ "1st Wave",
                                                                Date >= "2020-05-17" & Date <= "2020-09-27" ~ "Summer break",
                                                                Date > "2020-09-27" ~ "2nd Wave")) %>%
                                        mutate(outOfHomeDurationSquared = outOfHomeDuration * outOfHomeDuration) %>% 
                                        filter(cOI_2weeksbefore < 2)


m <- quap(
        alist(
        cOI_2weeksbefore ~ dnorm(mu, sigma) ,
        mu <- beta0 + beta1 * outOfHomeDuration + beta2 * outOfHomeDurationSquared + beta3 * outdoorFraction2,
        sigma ~  dcauchy(0 , 1), #HalfCauchy
        beta0 ~ dlnorm(log(1/4), 0.25),
        beta1 ~ dlnorm(log(0.03), 0.25),
        beta2 ~ dlnorm(log(0.0045), 0.25),
        beta3 ~ dlnorm(0, 0.25)
        ), data = joinedDataFrame
)

#Call link without specifying new data so it uses original data
mu <- link(m)

#summarize sampels across cases
mu_mean <- apply(mu, 2, mean)
mu_PI <- apply(mu, 2, PI)

#Simulate observations
#Again, no new data, using original data
D_sim <- sim(m, n = 1e4)
D_PI <- apply(D_sim, 2, PI)

plot(mu_mean ~ joinedDataFrame$cOI_2weeksbefore, col = rangi2, ylim = range(mu_PI),
        xlab="Observed cOI", ylab = "Predicted cOI")
abline(a = 0, b = 1, lty = 2)
for (i in 1:nrow(joinedDataFrame)) {
        lines(rep(joinedDataFrame$cOI_2weeksbefore[i], 2),
        mu_PI[,i],
        col=rangi2)
}

joinedDataFrame <- cbind(joinedDataFrame, mu_mean)
joinedDataFrame <- cbind(joinedDataFrame, mu_PI[1,])
joinedDataFrame <- cbind(joinedDataFrame, mu_PI[2,])
colnames(joinedDataFrame)[colnames(joinedDataFrame) == "mu_PI[1, ]"] <- "low"
colnames(joinedDataFrame)[colnames(joinedDataFrame) == "mu_PI[2, ]"] <- "high"

joinedDataFrame <- joinedDataFrame %>% mutate(PredictFrequentist = predict(resultsList[["oOH+oOH2+out2"]][["cOI_2weeksbefore"]][["Model"]]))

ggplot(data = joinedDataFrame) +
geom_line(aes(x=Date, y = mu_mean, color = "Predicted cOI (Bayes)"), size = 1.2) +
geom_line(aes(x= Date, y = cOI_2weeksbefore, color = "Actual cOI"), size = 1.2) +
geom_line(aes(x= Date, y = PredictFrequentist, color = "Predict cOI (Frequentist)"), size = 1.2) +
theme_minimal() +
scale_x_date(breaks = "1 week") +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
theme(legend.position = "bottom", legend.title = element_blank()) +
ylab("change Of Incidence") +
geom_ribbon(aes(x=Date, y = mu_mean, ymin = low, ymax = high), fill = "cyan3", alpha = 0.1)


#Looking for change points -> 

#Using bcp to perform a Bayesian analysis of change point problems
bcp_cOI <- bcp(joinedDataFrame$cOI_2weeksbefore, return.mcmc = TRUE)
plot(bcp_cOI)

bcp_sum <- as.data.frame(summary(bcp_cOI))
bcp_sum$id <- 1:length(joinedDataFrame)
(sel <- bcp_sum[which(bcp_cOI$posterior.prob > 0.7), ])
time(joinedDataFrame$cOI_2weeksbefore)[sel$id] #points 25 and 29
#Point 25 corresponds to 2020-09-13
#Point 29 corresponds to 2020-10-11