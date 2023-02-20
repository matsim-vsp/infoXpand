library(automl)
library(h2o)
library(RemixAutoML)

joinedDataFrameWed <- joinedDataFrame[, c("Date", "changeOfIncidencelaggedWed2", "tmax", "tavg", "outOfHomeDuration", "outdoorFraction", "prcp", "percentageChangeComparedToBeforeCorona")]
joinedDataFrameWed <- joinedDataFrame[-c(31,32),]

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

#Using h2o
#start h2o cluster
invisible(h2o.init())

# convert data as h2o type #NOTE: THIS IS A FIRST TEST RUN, YOU OBVIOUSLY NEED TO PROPERLY CHOOOSE TRAIN AND TEST
joinedDataFrameWed_h <- as.h2o(joinedDataFrameWed)
splits <- h2o.splitFrame(joinedDataFrameWed_h, ratios=0.8, seed=1)
train_h <- splits[[1]]
test_h <- splits[[2]]

# set label type
y = 'changeOfIncidencelaggedWed2'
pred = setdiff(names(train), c(y, 'Date')) 

#convert variables to factors
train$changeOfIncidencelaggedWed2 <- as.factor(train$changeOfIncidencelaggedWed2)
test$changeOfIncidencelaggedWed2 <- as.factor(test$changeOfIncidencelaggedWed2)

# Run AutoML for 20 base models
aml <- h2o.automl(x = pred, y = y,
                training_frame = train_h,
                max_models = 20,
                seed = 1,
                max_runtime_secs = 20
                )

# AutoML Leaderboard
lb <- aml@leaderboard

#Prediction results on test data
prediction <- h2o.predict(aml@leader, test_h[,-2]) %>%
                            as.data.frame()


#Explain a mnodel
exm <- h2o.explain(aml, test_h)

# close h2o connection
h2o.shutdown(prompt = F)