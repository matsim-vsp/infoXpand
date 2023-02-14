library(lares)
library(automl)
library(h2o)

joinedDataFrameWed <- joinedDataFrame[, c("Date", "changeOfIncidencelaggedWed2", "tmax", "tavg", "outOfHomeDuration", "outdoorFraction", "prcp", "percentageChangeComparedToBeforeCorona")]

#Using automl package

amlmodel <- automl_train_manual(Xref = subset(joinedDataFrameWed , select = -c(changeOfIncidencelaggedWed2, Date)),
                               Yref = subset(joinedDataFrameWed , select = c(changeOfIncidencelaggedWed2))$changeOfIncidencelaggedWed2 %>% 
                               as.numeric(),
                               hpar = list(learningrate = 0.01,
                               minibatchsize = 2^2,
                               numiterations = 60))

prediction = automl_predict(model = amlmodel, X = subset(joinedDataFrameWed , select = -c(changeOfIncidencelaggedWed2, Date)))

res <- cbind(subset(joinedDataFrameWed , select = c(changeOfIncidencelaggedWed2))$changeOfIncidencelaggedWed2, prediction)
colnames(res) <- c('actual', 'predict')

ggplot(as.data.frame(res)) + geom_point(aes(x=predict, y = actual)) +
geom_abline(aes(intercept = 0, slope = 1, color="blue")) +
scale_color_identity(labels = c("x=y"), guide="legend") +
theme_minimal()

#Using lares packages

r <- h2o_automl(joinedDataFrameWed, y = "changeOfIncidencelaggedWed2", exclude_algos = NULL, quiet = TRUE)
r