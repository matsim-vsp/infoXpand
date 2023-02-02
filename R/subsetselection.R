library(ISLR2)
library(MASS)
library(leaps)

#Can we improve our results by using automatic subset selection?
#In measuresOfAccuracy.R we have identified outOfHomeDuration*tmax with a 12 day lag to be the opimal model
#Can this be improved?

#Only keeping 12 day lag
twelve_day_lag <- joinedDataFrame %>%
select(c("Date", "changeOfIncidencelaggedWed2", "outOfHomeDuration", "tmax", "tavg", "prcp", "outdoorFraction", "outOfHomeDurationSquared", "tmaxSquared", "logOutOfHomeDuration", "logtmax")) 

twelvedayfit.full <- regsubsets(changeOfIncidencelaggedWed2 ~ . , data = twelve_day_lag, nvmax= 29)
summary(twelvedayfit.full)
#Conclusion from summary: Using a 3 variable model, one should use logOutOfHomeDuration, tmaxSquared, prcp

#Building this 3 variable model
for (weekday in weekdays){
if (weekday == "Mon_1week_lag") {
formula.lm <- "changeOfIncidencelaggedMon ~ logOutOfHomeDuration * tmaxSquared * prcp" 
weekdayString <- "changeOfIncidencelaggedMon"
} else {
formula.lm <- paste0("changeOfIncidencelagged",weekday,"2", " ~ logOutOfHomeDuration * tmaxSquared * prcp")
weekdayString <- paste0("changeOfIncidencelagged",weekday,"2")
}
logDtmaxSquaredprcp.lm <- lm(formula = formula.lm, data=joinedDataFrame) #Regression
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
geom_smooth(aes(x=coefficients(logDtmaxSquaredprcp.lm)["(Intercept)"] + coefficients(logDtmaxSquaredprcp.lm)["logOutOfHomeDuration"] * logOutOfHomeDuration + coefficients(logDtmaxSquaredprcp.lm)["tmaxSquared"] * tmaxSquared + coefficients(logDtmaxSquaredprcp.lm)["prcp"] * prcp + coefficients(logDtmaxSquaredprcp.lm)["logOutOfHomeDuration:prcp"] * logOutOfHomeDuration * prcp + coefficients(logDtmaxSquaredprcp.lm)["logOutOfHomeDuration:tmaxSquared"] * logOutOfHomeDuration * tmaxSquared + coefficients(logDtmaxSquaredprcp.lm)["tmaxSquared:prcp"] * tmaxSquared * prcp + coefficients(logDtmaxSquaredprcp.lm)["logOutOfHomeDuration:tmaxSquared:prcp"] * logOutOfHomeDuration * tmaxSquared * prcp, y = .data[[weekdayString]], color="lightgrey"), size=2, method = "lm", se=FALSE) +
geom_point(aes(x=coefficients(logDtmaxSquaredprcp.lm)["(Intercept)"] + coefficients(logDtmaxSquaredprcp.lm)["logOutOfHomeDuration"] * logOutOfHomeDuration + coefficients(logDtmaxSquaredprcp.lm)["tmaxSquared"] * tmaxSquared + coefficients(logDtmaxSquaredprcp.lm)["prcp"] * prcp + coefficients(logDtmaxSquaredprcp.lm)["logOutOfHomeDuration:prcp"] * logOutOfHomeDuration * prcp + coefficients(logDtmaxSquaredprcp.lm)["logOutOfHomeDuration:tmaxSquared"] * logOutOfHomeDuration * tmaxSquared + coefficients(logDtmaxSquaredprcp.lm)["tmaxSquared:prcp"] * tmaxSquared * prcp + coefficients(logDtmaxSquaredprcp.lm)["logOutOfHomeDuration:tmaxSquared:prcp"] * logOutOfHomeDuration * tmaxSquared * prcp, y = .data[[weekdayString]], fill = tavg), shape = 21, size = 3) +
ggtitle(title) +
xlab("Intercept + a * outOfHomeDuration^2 * outdoorFraction") +
ylab("changeOfIncidence") +
geom_abline(aes(intercept = 0, slope = 1, color = "blue")) +
theme_minimal() +
scale_color_identity(labels = c("x=y", "Regression line"), guide = "legend") +
theme(legend.position = "bottom", legend.title = element_blank())

nestedplotlist[[paste0("Regression_logDtmaxSquaredprcpvsI", weekday)]] <- logDtmaxSquaredprcp.lm
nestedplotlist[[paste0("Plot_logDtmaxSquaredprcpvsI", weekday)]] <- plot22
nestedplotlist[[paste0("ActualvsEstimate_logDtmaxSquaredprcpvsI", weekday)]] <- plot23
}

grid.arrange(nestedplotlist[["Plot_logDtmaxSquaredprcpvsIMon_1week_lag"]], nestedplotlist[["Plot_logDtmaxSquaredprcpvsISun"]],nestedplotlist[["Plot_logDtmaxSquaredprcpvsISat"]],nestedplotlist[["Plot_logDtmaxSquaredprcpvsIFri"]], nestedplotlist[["Plot_logDtmaxSquaredprcpvsIThu"]], nestedplotlist[["Plot_logDtmaxSquaredprcpvsIWed"]], nestedplotlist[["Plot_logDtmaxSquaredprcpvsITue"]], nestedplotlist[["Plot_logDtmaxSquaredprcpvsIMon"]], nrow=3)
grid.arrange(nestedplotlist[["ActualvsEstimate_logDtmaxSquaredprcpvsIMon_1week_lag"]], nestedplotlist[["ActualvsEstimate_logDtmaxSquaredprcpvsISun"]],nestedplotlist[["ActualvsEstimate_logDtmaxSquaredprcpvsISat"]],nestedplotlist[["ActualvsEstimate_logDtmaxSquaredprcpvsIFri"]], nestedplotlist[["ActualvsEstimate_logDtmaxSquaredprcpvsIThu"]], nestedplotlist[["ActualvsEstimate_logDtmaxSquaredprcpvsIWed"]], nestedplotlist[["ActualvsEstimate_logDtmaxSquaredprcpvsITue"]], nestedplotlist[["ActualvsEstimate_logDtmaxSquaredprcpvsIMon"]], nrow=3)
g <- arrangeGrob(nestedplotlist[["ActualvsEstimate_logDtmaxSquaredprcpvsIMon_1week_lag"]], nestedplotlist[["ActualvsEstimate_logDtmaxSquaredprcpvsISun"]],nestedplotlist[["ActualvsEstimate_logDtmaxSquaredprcpvsISat"]],nestedplotlist[["ActualvsEstimate_logDtmaxSquaredprcpvsIFri"]], nestedplotlist[["ActualvsEstimate_logDtmaxSquaredprcpvsIThu"]], nestedplotlist[["ActualvsEstimate_logDtmaxSquaredprcpvsIWed"]], nestedplotlist[["ActualvsEstimate_logDtmaxSquaredprcpvsITue"]], nestedplotlist[["ActualvsEstimate_logDtmaxSquaredprcpvsIMon"]], nrow=3)
#g <- arrangeGrob(nestedplotlist[["Plot_logDtmaxSquaredprcpvsIMon_1week_lag"]], nestedplotlist[["Plot_logDtmaxSquaredprcpvsISun"]],nestedplotlist[["Plot_logDtmaxSquaredprcpvsISat"]],nestedplotlist[["Plot_logDtmaxSquaredprcpvsIFri"]], nestedplotlist[["Plot_logDtmaxSquaredprcpvsIThu"]], nestedplotlist[["Plot_logDtmaxSquaredprcpvsIWed"]], nestedplotlist[["Plot_logDtmaxSquaredprcpvsITue"]], nestedplotlist[["Plot_logDtmaxSquaredprcpvsIMon"]], nrow=3)

#Conclusion: This does not provide a better model than when using outOfHomeDuration*tmax as identified by the measuresOfAccuracy script!