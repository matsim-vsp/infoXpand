library(tidyverse)
library(ISLR2)
library(lubridate)
library(splines)
library(gam)

joinedDataFrame2 <- joinedDataFrame

#Polynomial regression, but only using outOfHomeDuration
#Result -> Not worth it -> Multiple linear(!) regression model integrating temperature performs much better
weekdays <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Mon_1week_lag")
degrees <- c(2,3,4)

for(degree in degrees){
    for (weekday in weekdays){
        if (weekday == "Mon_1week_lag") {
        weekdayString <- "changeOfIncidencelaggedMon"
        formula.lm <- paste0("changeOfIncidencelaggedMon ~ poly(outOfHomeDuration, ", as.character(degree), ")")
        } else {
        weekdayString <- paste0("changeOfIncidencelagged", weekday, "2")
        formula.lm <- paste0("changeOfIncidencelagged", weekday, "2", " ~ poly(outOfHomeDuration, ", as.character(degree), ")")
        }
    DvsI.lm <- lm(formula = formula.lm, data = joinedDataFrame) #Regression
    if (weekday == "Mon") {
        title <- "14 Day lag"
        } else if (weekday == "Tue") {
        title <- "13 Day lag"
        } else if (weekday == "Wed") {
        title <- "12 Day lag"
        } else if (weekday == "Thu") {
        title <- "11 Day lag"
        } else if (weekday == "Fri") {
        title <- "10 Day lag"
        } else if (weekday == "Sat") {
        title <- "9 Day lag"
        } else if (weekday == "Sun") {
        title <- "8 Day lag"
        } else if (weekday == "Mon_1week_lag") {
        title <- "7 Day lag"
    }
    plot22 <- ggplot(data = joinedDataFrame) + #First plot; x = outOfHomeDuration, y = changeOfIncidence
    geom_point(aes(x = outOfHomeDuration, y = .data[[weekdayString]])) +
    geom_smooth(aes(x= outOfHomeDuration, y = .data[[weekdayString]]), method = "lm") +
    ggtitle(title) +
    theme_minimal()

    plot23 <- ggplot(data = joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
    geom_smooth(aes(x=coefficients(DvsI.lm)["(Intercept)"] + coefficients(DvsI.lm)["outOfHomeDuration"] * outOfHomeDuration, y = .data[[weekdayString]], color="lightgrey"), method="lm", se=FALSE, size = 2) +
    ggtitle(title) +
    xlab("Intercept + a * outOfHomeDuration ") +
    ylab("changeOfIncidence") +
    geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
    scale_color_identity(labels = c("x=y", "Regression line"), guide="legend") +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    geom_point(data = joinedDataFrame, aes(x=coefficients(DvsI.lm)["(Intercept)"] + coefficients(DvsI.lm)["outOfHomeDuration"] * outOfHomeDuration, y = .data[[weekdayString]], fill=tmax), size=3, shape=21)


    nestedplotlist[[paste0("Regression_DvsI", weekday, as.character(degree))]] <- DvsI.lm
    nestedplotlist[[paste0("Plot_DvsI_", weekday, as.character(degree))]] <- plot22
    nestedplotlist[[paste0("ActualvsEstimation_DvsI_", weekday, as.character(degree))]] <- plot23
    }
}

#Let's try tmax up to degree 2 and outOfHomeDuration up to degree 2
#i.e. changeOfIncidenceLagged ~ outOfHomeDuration + outOfHomeDuration^2 + tmax + tmax^2 -> NO MIX TERM
 for (weekday in weekdays){
        if (weekday == "Mon_1week_lag") {
        weekdayString <- "changeOfIncidencelaggedMon"
        formula.lm <- paste0("changeOfIncidencelaggedMon ~ poly(outOfHomeDuration,2) + poly(tmax,2)")
        } else {
        weekdayString <- paste0("changeOfIncidencelagged", weekday, "2")
        formula.lm <- paste0("changeOfIncidencelagged", weekday, "2", " ~ poly(outOfHomeDuration,2) + poly(tmax,2)")
        }
    DvsI.lm <- lm(formula = formula.lm, data = joinedDataFrame) #Regression
    if (weekday == "Mon") {
        title <- "14 Day lag"
        } else if (weekday == "Tue") {
        title <- "13 Day lag"
        } else if (weekday == "Wed") {
        title <- "12 Day lag"
        } else if (weekday == "Thu") {
        title <- "11 Day lag"
        } else if (weekday == "Fri") {
        title <- "10 Day lag"
        } else if (weekday == "Sat") {
        title <- "9 Day lag"
        } else if (weekday == "Sun") {
        title <- "8 Day lag"
        } else if (weekday == "Mon_1week_lag") {
        title <- "7 Day lag"
    }
    plot22 <- ggplot(data = joinedDataFrame) + #First plot; x = outOfHomeDuration, y = changeOfIncidence
    geom_point(aes(x = outOfHomeDuration, y = .data[[weekdayString]])) +
    geom_smooth(aes(x= outOfHomeDuration, y = .data[[weekdayString]]), method = "lm") +
    ggtitle(title) +
    theme_minimal()

    plot23 <- ggplot(data = joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
    geom_smooth(aes(x=coefficients(DvsI.lm)["(Intercept)"] + coefficients(DvsI.lm)["outOfHomeDuration"] * outOfHomeDuration, y = .data[[weekdayString]], color="lightgrey"), method="lm", se=FALSE, size = 2) +
    ggtitle(title) +
    xlab("Intercept + a * outOfHomeDuration ") +
    ylab("changeOfIncidence") +
    geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
    scale_color_identity(labels = c("x=y", "Regression line"), guide="legend") +
    theme_minimal() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    geom_point(data = joinedDataFrame, aes(x=coefficients(DvsI.lm)["(Intercept)"] + coefficients(DvsI.lm)["outOfHomeDuration"] * outOfHomeDuration, y = .data[[weekdayString]], fill=tmax), size=3, shape=21)


    nestedplotlist[[paste0("Regression_DvsI", weekday, "polytmax")]] <- DvsI.lm
    nestedplotlist[[paste0("Plot_DvsI_", weekday, "polytmax")]] <- plot22
    nestedplotlist[[paste0("ActualvsEstimation_DvsI_", weekday, "polytmax")]] <- plot23
    }

# Let's try a GAM
#natural spline and smoothingspline
splines <- c("ns", "s")
for (spline in splines){
    for (weekday in weekdays){
        if (weekday == "Mon_1week_lag") {
        weekdayString <- "changeOfIncidencelaggedMon"
        formula.lm <- paste0("changeOfIncidencelaggedMon ~", spline, "(outOfHomeDuration,2) + ", spline, "(tmax,2)")
        } else {
        weekdayString <- paste0("changeOfIncidencelagged", weekday, "2")
        formula.lm <- paste0("changeOfIncidencelagged", weekday, "2 ~", spline, "(outOfHomeDuration,2) + ", spline, "(tmax,2)")
        }
        DvsI.lm <- lm(formula = formula.lm, data = joinedDataFrame, family = binomial) #Regression
        if (weekday == "Mon") {
            title <- "14 Day lag"
            } else if (weekday == "Tue") {
            title <- "13 Day lag"
            } else if (weekday == "Wed") {
            title <- "12 Day lag"
            } else if (weekday == "Thu") {
            title <- "11 Day lag"
            } else if (weekday == "Fri") {
            title <- "10 Day lag"
            } else if (weekday == "Sat") {
            title <- "9 Day lag"
            } else if (weekday == "Sun") {
            title <- "8 Day lag"
            } else if (weekday == "Mon_1week_lag") {
            title <- "7 Day lag"
            }
        plot22 <- ggplot(data = joinedDataFrame) + #First plot; x = outOfHomeDuration, y = changeOfIncidence
        geom_point(aes(x = outOfHomeDuration, y = .data[[weekdayString]])) +
        geom_smooth(aes(x= outOfHomeDuration, y = .data[[weekdayString]]), method = "lm") +
        ggtitle(title) +
        theme_minimal()

        plot23 <- ggplot(data = joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
        geom_smooth(aes(x=coefficients(DvsI.lm)["(Intercept)"] + coefficients(DvsI.lm)["outOfHomeDuration"] * outOfHomeDuration, y = .data[[weekdayString]], color="lightgrey"), method="lm", se=FALSE, size = 2) +
        ggtitle(title) +
        xlab("Intercept + a * outOfHomeDuration ") +
        ylab("changeOfIncidence") +
        geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
        scale_color_identity(labels = c("x=y", "Regression line"), guide="legend") +
        theme_minimal() +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        geom_point(data = joinedDataFrame, aes(x=coefficients(DvsI.lm)["(Intercept)"] + coefficients(DvsI.lm)["outOfHomeDuration"] * outOfHomeDuration, y = .data[[weekdayString]], fill=tmax), size=3, shape=21)


        nestedplotlist[[paste0("Regression_DvsI", weekday, spline, "log")]] <- DvsI.lm
        nestedplotlist[[paste0("Plot_DvsI_", weekday, spline, "log")]] <- plot22
        nestedplotlist[[paste0("ActualvsEstimation_DvsI_", weekday, spline, "log")]] <- plot23
    }
}






