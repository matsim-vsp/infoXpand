library(tidyverse)
library(ISLR2)
library(lubridate)
library(splines)
library(gam)

#In order to set up joinedDataFrame one first needs to run PrepIncidenceData.R, PrepMobilityData.R and PrepWeatherData.R
joinedDataFrame <- inner_join(incidence_data, mobility_data, by = c("Date", "Bundesland"))
joinedDataFrame <- left_join(joinedDataFrame, weather_data_all, by = c("Date", "Bundesland"))

joinedDataFrame <- joinedDataFrame %>% filter(Bundesland == "Gesamt") %>%
                    filter(changeOfIncidencelaggedMon < 2) %>% 
                    filter(Date < "2021-01-01") %>% distinct()

joinedDataFrame <- joinedDataFrame %>% mutate(Welle = case_when(Date < "2020-06-01" ~ "1. Welle",
                                                                Date >= "2020-06-01" & Date <= "2020-10-01" ~ "Sommer",
                                                                Date > "2020-10-01" ~ "2. Welle"))


#Polynomial regression, but only using outOfHomeDuration
#Result -> Not worth it -> Multiple linear(!) regression model integrating temperature performs much better
lags <- c("cOI", "cOI_1weekbefore", "cOI_2weeksbefore", "cOI_3weeksbefore", "cOI_4weeksbefore", "cOI_5weeksbefore")
degrees <- c(2, 3, 4)

resultsList <- list()

for (degree in degrees) {
    resultsList[[paste0("PolynomReg", degree)]] <- list()

    for (lag in lags){
        formula.lm <- paste0(lag, "~ poly(outOfHomeDuration,", degree, ")")
        model.lm <- lm(formula = formula.lm, data = joinedDataFrame)
        resultsList[[paste0("PolynomReg", degree)]][[lag]][["Model"]] <- model.lm
        
        if (degree == 2) {
            formula.reg <- "y ~ poly(x,2)"

            predicted2 <- predict(model.lm)
            resultsList[["PolynomReg2"]][[lag]][["PlotActualFitted"]] <- ggplot(data = joinedDataFrame, aes(x=predicted2, y = .data[[lag]])) +
            geom_point(aes(fill = Welle), size = 3, shape = 21) +
            geom_smooth(aes(x=predicted2, y = .data[[lag]], color="darkgrey"), method="lm", size = 2) +
            ggtitle(paste0("Polynomial regression of degree ", degree, " lag: ", lag)) +
            ylab("Actual change of incidence") +
            xlab("Prediction") +
            geom_abline(aes(intercept = 0, slope = 1, color = "blue")) +
            scale_color_identity(labels = c("Regression line", "x=y"), guide = "legend") +
            theme_minimal() +
            theme(legend.position = "bottom", legend.title = element_blank())

        } else if (degree == 3) {
            formula.reg <- "y ~ poly(x,3)"
            
            predicted3 <- predict(model.lm)
            resultsList[["PolynomReg3"]][[lag]][["PlotActualFitted"]] <- ggplot(data = joinedDataFrame, aes(x=predicted3, y = .data[[lag]])) +
            geom_point(aes(fill = Welle), size = 3, shape = 21) +
            geom_smooth(aes(x=predicted3, y = .data[[lag]], color="darkgrey"), method="lm", size = 2) +
            ggtitle(paste0("Polynomial regression of degree ", degree, " lag: ", lag)) +
            ylab("Actual change of incidence") +
            xlab("Prediction") +
            geom_abline(aes(intercept = 0, slope = 1, color = "blue")) +
            scale_color_identity(labels = c("Regression line", "x=y"), guide = "legend") +
            theme_minimal() +
            theme(legend.position = "bottom", legend.title = element_blank())

        } else if (degree == 4) {
            formula.reg <- "y ~ poly(x,4)"

            predicted4 <- predict(model.lm)
            resultsList[["PolynomReg4"]][[lag]][["PlotActualFitted"]] <- ggplot(data = joinedDataFrame, aes(x=predicted4, y = .data[[lag]])) +
            geom_point(aes(fill = Welle), size = 3, shape = 21) +
            geom_smooth(aes(x=predicted4, y = .data[[lag]], color="darkgrey"), method="lm", size = 2) +
            ggtitle(paste0("Polynomial regression of degree ", degree, " lag: ", lag)) +
            ylab("Actual change of incidence") +
            xlab("Prediction") +
            geom_abline(aes(intercept = 0, slope = 1, color = "blue")) +
            scale_color_identity(labels = c("Regression line", "x=y"), guide = "legend") +
            theme_minimal() +
            theme(legend.position = "bottom", legend.title = element_blank())

        }
        resultsList[[paste0("PolynomReg", degree)]][[lag]][["PlotDataAndFit"]] <- ggplot(data = joinedDataFrame, aes(x=outOfHomeDuration, y = .data[[lag]])) +
        geom_point(aes(fill = Welle), size = 3, shape = 21) +
        geom_smooth(formula = formula.reg, color="darkgrey", method="lm", size = 2) +
        ggtitle(paste0("Polynomial regression of degree ", degree, " lag: ", lag)) +
        ylab("Actual change of incidence") +
        xlab("Prediction") +
        theme_minimal() +
        theme(legend.position = "bottom", legend.title = element_blank())
    }
}

#Comparing different lags for degree 2
grid.arrange(resultsList[["PolynomReg2"]][["cOI"]][["PlotDataAndFit"]], resultsList[["PolynomReg2"]][["cOI_1weekbefore"]][["PlotDataAndFit"]],
resultsList[["PolynomReg2"]][["cOI_2weeksbefore"]][["PlotDataAndFit"]], resultsList[["PolynomReg2"]][["cOI_3weeksbefore"]][["PlotDataAndFit"]], nrow=2)

grid.arrange(resultsList[["PolynomReg2"]][["cOI"]][["PlotActualFitted"]], resultsList[["PolynomReg2"]][["cOI_1weekbefore"]][["PlotActualFitted"]],
resultsList[["PolynomReg2"]][["cOI_2weeksbefore"]][["PlotActualFitted"]], resultsList[["PolynomReg2"]][["cOI_3weeksbefore"]][["PlotActualFitted"]], nrow=2)

#Comparing different degrees for a 2 week lag
grid.arrange(resultsList[["oOH"]][["cOI_2weeksbefore"]][["PlotDataAndFit"]], resultsList[["PolynomReg2"]][["cOI_2weeksbefore"]][["PlotDataAndFit"]],
resultsList[["PolynomReg3"]][["cOI_2weeksbefore"]][["PlotDataAndFit"]], resultsList[["PolynomReg4"]][["cOI_4weeksbefore"]][["PlotDataAndFit"]], nrow=2)

grid.arrange(resultsList[["oOH"]][["cOI_2weeksbefore"]][["PlotActualFitted"]], resultsList[["PolynomReg2"]][["cOI_2weeksbefore"]][["PlotActualFitted"]],
resultsList[["PolynomReg3"]][["cOI_2weeksbefore"]][["PlotActualFitted"]], resultsList[["PolynomReg4"]][["cOI_2weeksbefore"]][["PlotActualFitted"]], nrow=2)


#Let's try tmax up to degree 2 and outOfHomeDuration up to degree 2
#i.e. changeOfIncidenceLagged ~ outOfHomeDuration + outOfHomeDuration^2 + tmax + tmax^2 -> NO MIX TERM
resultsList[["PolynomRegTmax2"]] <- list()
#i.e. changeOfIncidenceLagged ~ outOfHomeDuration * tmax -> WITH MIX TERM
resultsList[["PolynomRegTmax2MIXED"]] <- list()
resultsList[["PolynomRegTmaxMIXEDstep2"]] <- list()
resultsList[["PolynomRegTmaxMIXEDstep3"]] <- list()
resultsList[["PolynomRegTmaxMIXEDstep4"]] <- list()
resultsList[["PolynomRegTmaxMIXEDstep5"]] <- list()
ids <- c("PolynomRegTmax2", "PolynomRegTmax2MIXED", "PolynomRegTmaxMIXEDstep2",  "PolynomRegTmaxMIXEDstep3", "PolynomRegTmaxMIXEDstep4", "PolynomRegTmaxMIXEDstep5")

joinedDataFrame <- joinedDataFrame %>% mutate(tmaxSquared = tmax * tmax) %>%
                    mutate(outOfHomeDurationSquared = outOfHomeDuration*outOfHomeDuration)

for (id in ids) {
 for (lag in lags){
        resultsList[[id]][[lag]] <- list()
        if (id == "PolynomRegTmax2") {
            formula.lm <- paste0(lag, "~ poly(outOfHomeDuration,2) + poly(tmax,2)")
            print("hi")
        } else if (id == "PolynomRegTmax2MIXED") {
            formula.lm <- paste0(lag, "~ poly(outOfHomeDuration,2) * poly(tmax,2)")
        } else if (id == "PolynomRegTmaxMIXEDstep2") {
            formula.lm <- paste0(lag, "~ outOfHomeDurationSquared * poly(tmax,2)")
        } else if (id == "PolynomRegTmaxMIXEDstep3") {
            formula.lm <- paste0(lag, "~ outOfHomeDurationSquared * tmax + tmaxSquared")
        } else if (id == "PolynomRegTmaxMIXEDstep4") {
            formula.lm <- paste0(lag, "~ outOfHomeDurationSquared * tmax")
        } else if (id == "PolynomRegTmaxMIXEDstep5") {
            formula.lm <- paste0(lag, "~ poly(outOfHomeDuration,2) + tmax + outOfHomeDurationSquared:tmax")
        } else {
            formula.lm <- paste0(lag, "~ poly(outOfHomeDuration,2) + tmax + outOfHomeDurationSquared:tmax")
        }

        model.lm <- lm(formula = formula.lm, data = joinedDataFrame)
        resultsList[[id]][[lag]][["Model"]] <- model.lm

        plot22 <- ggplot(data = joinedDataFrame) +
        geom_point(aes(x = outOfHomeDuration, y = .data[[lag]])) +
        geom_smooth(aes(x = outOfHomeDuration, y = .data[[lag]]), method = "lm") +
        ggtitle(title) +
        theme_minimal()

        formulax <- predict(model.lm, joinedDataFrame)
        plot23 <- ggplot(data = joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
        geom_smooth(aes(x=formulax, y = .data[[lag]], color="lightgrey"), method="lm", se=FALSE, size = 2) +
        ggtitle(title) +
        xlab(paste0("predicted values")) +
        ylab("changeOfIncidence") +
        geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
        scale_color_identity(labels = c("x=y", "Regression line"), guide = "legend") +
        theme_minimal() +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        geom_point(data = joinedDataFrame, aes(x=formulax, y = .data[[lag]], fill=Welle), size=3, shape=21)


        resultsList[[id]][[lag]][["PlotDataAndFit"]] <- plot22
        resultsList[[id]][[lag]][["PlotActualFitted"]] <- plot23
    }
}


summary(resultsList[["PolynomRegTmax2MIXED"]][["cOI_2weeksbefore"]][["Model"]])
summary(resultsList[["PolynomRegTmaxMIXEDstep2"]][["cOI_2weeksbefore"]][["Model"]])
summary(resultsList[["PolynomRegTmaxMIXEDstep3"]][["cOI_2weeksbefore"]][["Model"]])
summary(resultsList[["PolynomRegTmaxMIXEDstep4"]][["cOI_2weeksbefore"]][["Model"]])

#TODO: CONTINUE HERE ON 3/30
# Let's try a GAM
#natural spline and smoothingspline
splines <- c("ns", "s")
for (spline in splines){
    for (lag in lags){
        if (lag == "Sun_2week_lag") {
        weekdayString <- "changeOfIncidencelaggedMon"
        formula.lm <- paste0("changeOfIncidencelaggedMon ~", spline, "(outOfHomeDuration,2) + ", spline, "(tmax,2)")
        } else {
        weekdayString <- paste0("changeOfIncidencelagged", lag, "2")
        formula.lm <- paste0("changeOfIncidencelagged", lag, "2 ~", spline, "(outOfHomeDuration,2) + ", spline, "(tmax,2)")
        }
        model.lm <- lm(formula = formula.lm, data = joinedDataFrame, family = binomial) #Regression
        if (lag == "Mon") {
            title <- "14 Day lag"
            } else if (lag == "Tue") {
            title <- "13 Day lag"
            } else if (lag == "Wed") {
            title <- "12 Day lag"
            } else if (lag == "Thu") {
            title <- "11 Day lag"
            } else if (lag == "Fri") {
            title <- "10 Day lag"
            } else if (lag == "Sat") {
            title <- "9 Day lag"
            } else if (lag == "Sun") {
            title <- "8 Day lag"
            } else if (lag == "Sun_2week_lag") {
            title <- "7 Day lag"
            }
        plot22 <- ggplot(data = joinedDataFrame) + #First plot; x = outOfHomeDuration, y = changeOfIncidence
        geom_point(aes(x = outOfHomeDuration, y = .data[[weekdayString]])) +
        geom_smooth(aes(x= outOfHomeDuration, y = .data[[weekdayString]]), method = "lm") +
        ggtitle(title) +
        theme_minimal()

        plot23 <- ggplot(data = joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
        geom_smooth(aes(x=coefficients(model.lm)["(Intercept)"] + coefficients(model.lm)["outOfHomeDuration"] * outOfHomeDuration, y = .data[[weekdayString]], color="lightgrey"), method="lm", se=FALSE, size = 2) +
        ggtitle(title) +
        xlab("Intercept + a * outOfHomeDuration ") +
        ylab("changeOfIncidence") +
        geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
        scale_color_identity(labels = c("x=y", "Regression line"), guide="legend") +
        theme_minimal() +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        geom_point(data = joinedDataFrame, aes(x=coefficients(model.lm)["(Intercept)"] + coefficients(model.lm)["outOfHomeDuration"] * outOfHomeDuration, y = .data[[weekdayString]], fill=tmax), size=3, shape=21)


        resultsList[[paste0("Regression_DvsI", lag, spline, "log")]] <- model.lm
        resultsList[[paste0("Plot_DvsI_", lag, spline, "log")]] <- plot22
        resultsList[[paste0("ActualvsEstimation_DvsI_", lag, spline, "log")]] <- plot23
    }
}
