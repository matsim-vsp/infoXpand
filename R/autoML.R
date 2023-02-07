library(automl)

xmat <- as.matrix(cbind(joinedDataFrame[, c("outOfHomeDuration", "tmax", "prcp")]))
ymat <- joinedDataFrame$changeOfIncidencelaggedSun2

start.time <- Sys.time()
amlmodel <- automl_train(Xref = xmat, Yref = ymat,
autopar = list(psopartpopsize = 15,
numiterations = 20,
auto_layers_max = 1,
nbcores = 4))
end.time <- Sys.time()
cat(paste('time ellapsed:', end.time - start.time, '\n'))

res <- cbind(ymat, automl_predict(model = amlmodel, X = xmat))
colnames(res) <- c('actual', 'predict')
head(res)

res <- as.data.frame(res)

ggplot(data = res) +
geom_point(aes(x = actual, y = predict)) +
geom_abline(intercept = 0, slope = 1) +
theme_minimal()
