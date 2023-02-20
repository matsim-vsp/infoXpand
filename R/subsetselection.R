library(ISLR2)
library(MASS)
library(leaps)

#Can we improve our results by using automatic subset selection?
#In measuresOfAccuracy.R we have identified outOfHomeDuration*tmax with a 12 day lag to be the opimal model
#Can this be improved?

#Only keeping 12 day lag
twelve_day_lag <- joinedDataFrame[, c("Date", "changeOfIncidencelaggedWed2", "outOfHomeDuration", "tmax", "tavg", "prcp", "outdoorFraction", "outOfHomeDurationSquared", "tmaxSquared")]

twelvedayfit.full <- regsubsets(changeOfIncidencelaggedWed2 ~ . -Date, data = twelve_day_lag, nvmax= 29)
summary_best_subset <- summary(twelvedayfit.full)
#Conclusion from summary: Using a 3 variable model, one should use logOutOfHomeDuration, tmaxSquared, prcp
as.data.frame(summary_best_subset$outmat)

which.max(summary_best_subset$adjr2)
summary_best_subset$which[3,] #This identifies outOfHomeDuration, tmaxSquared and prcp as the most influential predictors!

#Building this 3 variable model
for (weekday in weekdays){
if (weekday == "Mon_1week_lag") {
formula.lm <- "changeOfIncidencelaggedMon ~ outOfHomeDuration + tmaxSquared + prcp" 
weekdayString <- "changeOfIncidencelaggedMon"
} else {
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ outOfHomeDuration + tmaxSquared + prcp")
weekdayString <- paste0("changeOfIncidencelagged",weekday,"2")
}
DtmaxSquaredprcp.lm <- lm(formula = formula.lm, data=joinedDataFrame) #Regression
if (weekday == "Mon") {
title <- "14 Day lag"
} else if (weekday == "Tue") {
title <- "13 Day lag"
} else if (weekday == "Wed") {
title <- "12 Day lag"
} else if (weekday == "Thu") {
title <- "11 Day lag"
} else if(weekday == "Fri") {
title <- "10 Day lag"
} else if (weekday == "Sat") {
title <- "9 Day lag"
} else if (weekday == "Sun") {
title <- "8 Day lag"
} else if (weekday == "Mon_1week_lag") {
title <- "7 Day lag"
}
#1st plot; x = outOfHomeDurationSquared, y = changeOfIncidence, color = outdoorFraction
plot22 <- ggplot(data=joinedDataFrame, aes(x=outOfHomeDurationSquared, color =outdoorFraction, y = .data[[weekdayString]])) +
geom_point() +
geom_smooth(method = "lm") +
ggtitle(title) +
theme_minimal() +
scale_color_identity(labels = c("Regression line", "x=y"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank())


plot23 <- ggplot(data=joinedDataFrame) + #2nd plot; x = model estimate, y = actual changeOfIncidence
geom_smooth(aes(x=coefficients(DtmaxSquaredprcp.lm)["(Intercept)"] + coefficients(DtmaxSquaredprcp.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DtmaxSquaredprcp.lm)["tmaxSquared"] * tmaxSquared + coefficients(DtmaxSquaredprcp.lm)["prcp"] * prcp, y = .data[[weekdayString]], color="lightgrey"), size=2, method = "lm", se=FALSE) +
geom_point(aes(x=coefficients(DtmaxSquaredprcp.lm)["(Intercept)"] + coefficients(DtmaxSquaredprcp.lm)["outOfHomeDuration"] * outOfHomeDuration + coefficients(DtmaxSquaredprcp.lm)["tmaxSquared"] * tmaxSquared + coefficients(DtmaxSquaredprcp.lm)["prcp"] * prcp, y = .data[[weekdayString]], fill = tavg), shape = 21, size = 3) +
ggtitle(title) +
xlab("Intercept + a * outOfHomeDuration^2 * outdoorFraction") +
ylab("changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color = "blue")) +
theme_minimal() +
scale_color_identity(labels = c("x=y", "Regression line"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank())

nestedplotlist[[paste0("Regression_DtmaxSquaredprcpvsI", weekday)]] <- DtmaxSquaredprcp.lm
nestedplotlist[[paste0("Plot_DtmaxSquaredprcpvsI", weekday)]] <- plot22
nestedplotlist[[paste0("ActualvsEstimate_DtmaxSquaredprcpvsI", weekday)]] <- plot23
}

grid.arrange(nestedplotlist[["Plot_DtmaxSquaredprcpvsIMon_1week_lag"]], nestedplotlist[["Plot_DtmaxSquaredprcpvsISun"]],nestedplotlist[["Plot_DtmaxSquaredprcpvsISat"]],nestedplotlist[["Plot_DtmaxSquaredprcpvsIFri"]], nestedplotlist[["Plot_DtmaxSquaredprcpvsIThu"]], nestedplotlist[["Plot_DtmaxSquaredprcpvsIWed"]], nestedplotlist[["Plot_DtmaxSquaredprcpvsITue"]], nestedplotlist[["Plot_DtmaxSquaredprcpvsIMon"]], nrow=3)
grid.arrange(nestedplotlist[["ActualvsEstimate_DtmaxSquaredprcpvsIMon_1week_lag"]], nestedplotlist[["ActualvsEstimate_DtmaxSquaredprcpvsISun"]],nestedplotlist[["ActualvsEstimate_DtmaxSquaredprcpvsISat"]],nestedplotlist[["ActualvsEstimate_DtmaxSquaredprcpvsIFri"]], nestedplotlist[["ActualvsEstimate_DtmaxSquaredprcpvsIThu"]], nestedplotlist[["ActualvsEstimate_DtmaxSquaredprcpvsIWed"]], nestedplotlist[["ActualvsEstimate_DtmaxSquaredprcpvsITue"]], nestedplotlist[["ActualvsEstimate_DtmaxSquaredprcpvsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimate_DtmaxSquaredprcpvsIMon_1week_lag"]], nestedplotlist[["ActualvsEstimate_DtmaxSquaredprcpvsISun"]],nestedplotlist[["ActualvsEstimate_DtmaxSquaredprcpvsISat"]],nestedplotlist[["ActualvsEstimate_DtmaxSquaredprcpvsIFri"]], nestedplotlist[["ActualvsEstimate_DtmaxSquaredprcpvsIThu"]], nestedplotlist[["ActualvsEstimate_DtmaxSquaredprcpvsIWed"]], nestedplotlist[["ActualvsEstimate_DtmaxSquaredprcpvsITue"]], nestedplotlist[["ActualvsEstimate_DtmaxSquaredprcpvsIMon"]], nrow=3)
#g <- arrangeGrob(nestedplotlist[["Plot_DtmaxSquaredprcpvsIMon_1week_lag"]], nestedplotlist[["Plot_DtmaxSquaredprcpvsISun"]],nestedplotlist[["Plot_DtmaxSquaredprcpvsISat"]],nestedplotlist[["Plot_DtmaxSquaredprcpvsIFri"]], nestedplotlist[["Plot_DtmaxSquaredprcpvsIThu"]], nestedplotlist[["Plot_DtmaxSquaredprcpvsIWed"]], nestedplotlist[["Plot_DtmaxSquaredprcpvsITue"]], nestedplotlist[["Plot_DtmaxSquaredprcpvsIMon"]], nrow=3)

summary(nestedplotlist[["Regression_DtmaxSquaredprcpvsIWed"]])
#Conclusion: This does not provide a better model than when using outOfHomeDuration*tmax as identified by the measuresOfAccuracy script!