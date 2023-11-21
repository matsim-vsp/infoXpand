

### MODEL SELECTION ###
#Model selection -> using INDOOR fraction
joinedDataFrame_indoor <- joinedDataFrame %>% filter(cOI_2weeksbefore < 2) %>%
                            filter(Date + 14 < "2021-01-01") %>% select(cOI_2weeksbefore, outOfHomeDuration, outOfHomeDurationSquared, outOfHomeDurationCubed, indoorFraction, indoorFractionSquared, indoorFractionCubed) %>% 
                            mutate(oOHindoorFraction = outOfHomeDuration * indoorFraction) %>%
                            mutate(oOH2indoorFraction = outOfHomeDurationSquared * indoorFraction) %>%
                            mutate(oOH3indoorFraction = outOfHomeDurationCubed * indoorFraction) %>% 
                            mutate(oOHindoorFraction2 = outOfHomeDuration * indoorFractionSquared) %>%
                            mutate(oOH2indoorFraction2 = outOfHomeDurationSquared * indoorFractionSquared) %>%
                            mutate(oOH3indoorFraction2 = outOfHomeDurationCubed * indoorFractionSquared) %>% 
                            mutate(oOHindoorFraction3 = outOfHomeDuration * indoorFractionCubed) %>%
                            mutate(oOH2indoorFraction3 = outOfHomeDurationSquared * indoorFractionCubed) %>%
                            mutate(oOH3indoorFraction3 = outOfHomeDurationCubed * indoorFractionCubed) 
y_train <- joinedDataFrame_indoor$cOI_2weeksbefore
joinedDataFrame_indoor <- joinedDataFrame_indoor %>% select(-cOI_2weeksbefore)
joinedDataFrame_indoor <- as.matrix(joinedDataFrame_indoor)

joinedDataFrame <- joinedDataFrame %>% filter(cOI_2weeksbefore<2) %>% filter(Date + 14 < "2021-01-01") %>%
mutate(Prediction = predict(resultsList[["oOH2+oOH2:out2"]][["cOI_2weeksbefore"]][["Model"]]), cOI_2weeksbefore)

#using LASSO to perform model selection
# 10-fold CV to find the optimal lambda
iteratons_lasso <- c()
for(i in 1 :100) {
enet.cv=cv.glmnet(x=joinedDataFrame_indoor, y=y_train,alpha=1, type="deviance", family="gaussian", standardize=TRUE, nfolds=10)
## Fit lasso model with 100 values for lambda
enet_mdl = glmnet(x=joinedDataFrame_indoor, y=y_train,alpha=1,standardize=TRUE,nlambda=100)
## Extract coefficients at optimal lambda
coef(enet_mdl,s = enet.cv$lambda.min)
summary(enet_mdl,s = enet.cv$lambda.min)
iteratons_lasso <- append(iteratons_lasso, enet.cv$lambda.min)
}
hist(iterations_lasso)
coef(enet_mdl,s = mean(iterations_lasso))
coef(enet_mdl,s = median(iterations_lasso))


#using ELASTIC NET to perform model selection
# 10-fold CV to find the optimal lambda
iterations_elasticnet <- c()
for(i in 1 : 100) {
enet.cv=cv.glmnet(x=joinedDataFrame_indoor, y=y_train,alpha=0.5, type="deviance", family="gaussian", standardize=TRUE, nfolds=10)
## Fit lasso model with 100 values for lambda
enet_mdl = glmnet(x=joinedDataFrame_indoor, y=y_train,alpha=0.5,standardize=TRUE,nlambda=100)
## Extract coefficients at optimal lambda
coef(enet_mdl,s=enet.cv$lambda.min)
summary(enet_mdl,s=enet.cv$lambda.min)
iterations_elasticnet <- append(iterations_elasticnet, enet.cv$lambda.min)
}
hist(iterations_elasticnet)
coef(enet_mdl, s = mean(iterations_elasticnet))
coef(enet_mdl, s = median(iterations_elasticnet))

#using REG SUBSET selection to perform model selection
mod.full <-  regsubsets(cOI_2weeksbefore ~ ., method = "exhaustive", data = as.data.frame(joinedDataFrame_indoor), nvmax = 10)
mod.summary <-  summary(mod.full)
which.min(mod.summary$bic)
which.max(mod.summary$adjr2)
coefficients(mod.full, id = 2)

plot(mod.summary$cp, xlab = "Subset Size", ylab="Cp", pch=20, type="l")
points(which.min(mod.summary$cp), min(mod.summary$cp), pch=4, col="red", lwd=7)

plot(mod.summary$bic, xlab="Subset Size", ylab="BIC", pch=20, type="l")
points(which.min(mod.summary$bic), min(mod.summary$bic), pch=4, col="red", lwd=7)

plot(mod.summary$adjr2, xlab="Subset Size", ylab="AdjrR^2", pch=20, type="l")
points(which.max(mod.summary$adjr2), max(mod.summary$adjr2), pch=4, col="red", lwd=7)

#Model selection --> using INDOOR fraction and enforcing that the MAXIMAL DEGREE is 3
joinedDataFrame_indoor <- joinedDataFrame %>% filter(cOI_2weeksbefore < 2) %>%
                            filter(Date + 14 < "2021-01-01") %>% select(cOI_2weeksbefore, outOfHomeDuration, outOfHomeDurationSquared, outOfHomeDurationCubed, indoorFraction, indoorFractionSquared, indoorFractionCubed) %>% 
                            mutate(oOHindoorFraction = outOfHomeDuration * indoorFraction) %>%
                            mutate(oOH2indoorFraction = outOfHomeDurationSquared * indoorFraction) %>%
                            #mutate(oOH3indoorFraction = outOfHomeDurationCubed * indoorFraction) %>% 
                            mutate(oOHindoorFraction2 = outOfHomeDuration * indoorFractionSquared) 
                           # mutate(oOH2indoorFraction2 = outOfHomeDurationSquared * indoorFractionSquared) %>%
                            #mutate(oOH3indoorFraction2 = outOfHomeDurationCubed * indoorFractionSquared) %>% 
                            #mutate(oOHindoorFraction3 = outOfHomeDuration * indoorFractionCubed) %>%
                            #mutate(oOH2indoorFraction3 = outOfHomeDurationSquared * indoorFractionCubed) %>%
                            #mutate(oOH3indoorFraction3 = outOfHomeDurationCubed * indoorFractionCubed) 
y_train <- joinedDataFrame_indoor$cOI_2weeksbefore
joinedDataFrame_indoor <- joinedDataFrame_indoor %>% select(-cOI_2weeksbefore)
joinedDataFrame_indoor <- as.matrix(joinedDataFrame_indoor)

# Using LASSO to perform model selection
# 10-fold CV to find the optimal lambda
iteratons_lasso <- c()
for(i in 1 :100) {
enet.cv=cv.glmnet(x=joinedDataFrame_indoor, y=y_train,alpha=1, type="deviance", family="gaussian", standardize=TRUE, nfolds=10, , intercept = FALSE)
## Fit lasso model with 100 values for lambda
enet_mdl = glmnet(x=joinedDataFrame_indoor, y=y_train,alpha=1,standardize=TRUE,nlambda=100, intercept = FALSE)
## Extract coefficients at optimal lambda
coef(enet_mdl,s = enet.cv$lambda.min)
summary(enet_mdl,s = enet.cv$lambda.min)
iteratons_lasso <- append(iteratons_lasso, enet.cv$lambda.min)
}
hist(iterations_lasso)
coef(enet_mdl,s = mean(iterations_lasso))
coef(enet_mdl,s = median(iterations_lasso))


# Using ELASTIC NET to perform model selection
# 10-fold CV to find the optimal lambda
iterations_elasticnet <- c()
for(i in 1 : 100) {
enet.cv=cv.glmnet(x=joinedDataFrame_indoor, y=y_train,alpha=0.5, type="deviance", family="gaussian", standardize=TRUE, nfolds=10, intercept = FALSE)
## Fit lasso model with 100 values for lambda
enet_mdl = glmnet(x=joinedDataFrame_indoor, y=y_train,alpha=0.5,standardize=TRUE,nlambda=100, intercept = FALSE)
## Extract coefficients at optimal lambda
coef(enet_mdl,s=enet.cv$lambda.min)
summary(enet_mdl,s=enet.cv$lambda.min)
iterations_elasticnet <- append(iterations_elasticnet, enet.cv$lambda.min)
}
hist(iterations_elasticnet)
coef(enet_mdl, s = mean(iterations_elasticnet))
coef(enet_mdl, s = median(iterations_elasticnet))

# Using REG SUBSET to perform model selection
mod.full <-  regsubsets(cOI_2weeksbefore ~ ., intercept = FALSE, method = "exhaustive", data = as.data.frame(joinedDataFrame_indoor), nvmax = 3)
mod.summary <-  summary(mod.full)
which.min(mod.summary$cp)
which.max(mod.summary$adjr2)
coefficients(mod.full, id = 2)

plot(mod.summary$cp, xlab = "Subset Size", ylab="Cp", pch=20, type="l")
points(which.min(mod.summary$cp), min(mod.summary$cp), pch=4, col="red", lwd=7)

plot(mod.summary$bic, xlab="Subset Size", ylab="BIC", pch=20, type="l")
points(which.min(mod.summary$bic), min(mod.summary$bic), pch=4, col="red", lwd=7)

plot(mod.summary$adjr2, xlab="Subset Size", ylab="AdjrR^2", pch=20, type="l")
points(which.max(mod.summary$adjr2), max(mod.summary$adjr2), pch=4, col="red", lwd=7)

#Model selection -> using OUTDOOR fraction
joinedDataFrame_outdoor <- joinedDataFrame %>% filter(cOI_2weeksbefore < 2) %>%
                            filter(Date + 14 < "2021-01-01") %>% select(cOI_2weeksbefore, outOfHomeDuration, outOfHomeDurationSquared, outOfHomeDurationCubed, outdoorFraction2, outdoorFraction2Squared, outdoorFraction2Cubed) %>% 
                            mutate(oOHoutdoorFraction = outOfHomeDuration * outdoorFraction2) %>%
                            mutate(oOH2outdoorFraction = outOfHomeDurationSquared * outdoorFraction2) %>%
                            mutate(oOH3outdoorFraction = outOfHomeDurationCubed * outdoorFraction2) %>% 
                            mutate(oOHoutdoorFraction2 = outOfHomeDuration * outdoorFraction2Squared) %>%
                            mutate(oOH2outdoorFraction2 = outOfHomeDurationSquared * outdoorFraction2Squared) %>%
                            mutate(oOH3outdoorFraction2 = outOfHomeDurationCubed * outdoorFraction2Squared) %>% 
                            mutate(oOHoutdoorFraction3 = outOfHomeDuration * outdoorFraction2Cubed) %>%
                            mutate(oOH2outdoorFraction3 = outOfHomeDurationSquared * outdoorFraction2Cubed) %>%
                            mutate(oOH3outdoorFraction3 = outOfHomeDurationCubed * outdoorFraction2Cubed) 
y_train <- joinedDataFrame_outdoor$cOI_2weeksbefore
joinedDataFrame_outdoor <- joinedDataFrame_outdoor %>% select(-cOI_2weeksbefore)
joinedDataFrame_outdoor <- as.matrix(joinedDataFrame_outdoor)

#Using LASSO to perform model selection
# 10-fold CV to find the optimal lambda
iteratopms_lasso <- c()
for(i in 1 :100) {
enet.cv=cv.glmnet(x=joinedDataFrame_outdoor, y=y_train,alpha=1, type="deviance", family="gaussian", standardize=TRUE, nfolds=10, nzero=3)
## Fit lasso model with 100 values for lambda
enet_mdl = glmnet(x=joinedDataFrame_outdoor, y=y_train,alpha=1,standardize=TRUE,nlambda=100, nzero=3)
## Extract coefficients at optimal lambda
coef(enet_mdl,s=enet.cv$lambda.min)
iterations_lasso <- append(iterations_lasso, enet.cv$lambda.min)
}
coef(enet_mdl,s=mean(iterations_lasso))
coef(enet_mdl,s=median(iterations_lasso))

#Using ELASTIC NET to perform model selection
# 10-fold CV to find the optimal lambda
iteration_elasticnet <- c()
for(i in 1:100){
enet.cv=cv.glmnet(x=joinedDataFrame_outdoor, y=y_train,alpha=0.5, type="deviance", family="gaussian", standardize=TRUE, nfolds=10, nzero=3)
## Fit lasso model with 100 values for lambda
enet_mdl = glmnet(x=joinedDataFrame_outdoor, y=y_train,alpha=0.5,standardize=TRUE,nlambda=100, nzero=3)
## Extract coefficients at optimal lambda
coef(enet_mdl,s=enet.cv$lambda.min)
iteration_elasticnet <- append(iteration_elasticnet, enet.cv$lambda.min)
}
coef(enet_mdl,s=mean(iteration_elasticnet))
coef(enet_mdl,s=median(iteration_elasticnet))

#Using REG SUBSET to perform model selection
mod.full <-  regsubsets(cOI_2weeksbefore ~ ., data = as.data.frame(joinedDataFrame_outdoor), nvmax = 10)
mod.summary <-  summary(mod.full)
which.min(mod.summary$bic)
which.max(mod.summary$adjr2)
coefficients(mod.full, id=8)

plot(mod.summary$cp, xlab="Subset Size", ylab="Cp", pch=20, type="l")
points(which.min(mod.summary$cp), min(mod.summary$cp), pch=4, col="red", lwd=7)

plot(mod.summary$bic, xlab="Subset Size", ylab="BIC", pch=20, type="l")
points(which.min(mod.summary$bic), min(mod.summary$bic), pch=4, col="red", lwd=7)

plot(mod.summary$adjr2, xlab="Subset Size", ylab="AdjrR^2", pch=20, type="l")
points(which.max(mod.summary$adjr2), max(mod.summary$adjr2), pch=4, col="red", lwd=7)


#Model selection -> using OUTDOOR fraction enforcing that the MAXIMAL DEGREE is equal to 3 and a NO INTERCEPT model
joinedDataFrame_outdoor <- joinedDataFrame %>% filter(cOI_2weeksbefore < 2) %>%
                            filter(Date + 14 < "2021-01-01") %>% select(cOI_2weeksbefore, outOfHomeDuration, outOfHomeDurationSquared, outOfHomeDurationCubed, outdoorFraction2, outdoorFraction2Squared, outdoorFraction2Cubed) %>% 
                            mutate(oOHoutdoorFraction = outOfHomeDuration * outdoorFraction2) %>%
                            mutate(oOH2outdoorFraction = outOfHomeDurationSquared * outdoorFraction2) %>%
                            #mutate(oOH3outdoorFraction = outOfHomeDurationCubed * outdoorFraction2) %>% 
                            mutate(oOHoutdoorFraction2 = outOfHomeDuration * outdoorFraction2Squared) 
                            #mutate(oOH2outdoorFraction2 = outOfHomeDurationSquared * outdoorFraction2Squared) %>%
                            #mutate(oOH3outdoorFraction2 = outOfHomeDurationCubed * outdoorFraction2Squared) %>% 
                            #mutate(oOHoutdoorFraction3 = outOfHomeDuration * outdoorFraction2Cubed) %>%
                            #mutate(oOH2outdoorFraction3 = outOfHomeDurationSquared * outdoorFraction2Cubed) %>%
                            #mutate(oOH3outdoorFraction3 = outOfHomeDurationCubed * outdoorFraction2Cubed) 
y_train <- joinedDataFrame_outdoor$cOI_2weeksbefore
joinedDataFrame_outdoor <- joinedDataFrame_outdoor %>% select(-cOI_2weeksbefore)
joinedDataFrame_outdoor <- as.matrix(joinedDataFrame_outdoor)

# Using LASSO to perform model selection
# 10-fold CV to find the optimal lambda
iteratopms_lasso <- c()
for(i in 1 :100) {
enet.cv=cv.glmnet(x=joinedDataFrame_outdoor, y=y_train,alpha=1, type="deviance", family="gaussian", standardize=TRUE, nfolds=10, intercept = FALSE)
## Fit lasso model with 100 values for lambda
enet_mdl = glmnet(x=joinedDataFrame_outdoor, y=y_train,alpha=1,standardize=TRUE,nlambda=100, intercept = FALSE)
## Extract coefficients at optimal lambda
coef(enet_mdl,s=enet.cv$lambda.min)
iterations_lasso <- append(iterations_lasso, enet.cv$lambda.min)
}
coef(enet_mdl,s=mean(iterations_lasso))
coef(enet_mdl,s=median(iterations_lasso))

# Using ELASTIC NET to perform model selection
# 10-fold CV to find the optimal lambda
iteration_elasticnet <- c()
for(i in 1:100){
enet.cv=cv.glmnet(x=joinedDataFrame_outdoor, y=y_train,alpha=0.5, type="deviance", family="gaussian", standardize=TRUE, nfolds=10, intercept = FALSE)
## Fit lasso model with 100 values for lambda
enet_mdl = glmnet(x=joinedDataFrame_outdoor, y=y_train,alpha=0.5,standardize=TRUE,nlambda=100, intercept = FALSE)
## Extract coefficients at optimal lambda
coef(enet_mdl,s=enet.cv$lambda.min)
iteration_elasticnet <- append(iteration_elasticnet, enet.cv$lambda.min)
}
coef(enet_mdl,s=mean(iteration_elasticnet))
coef(enet_mdl,s=median(iteration_elasticnet))

# Using REG SUBSET to perform model selection
mod.full <-  regsubsets(cOI_2weeksbefore ~ ., intercept = FALSE, data = as.data.frame(joinedDataFrame_outdoor), nvmax = 3)
mod.summary <-  summary(mod.full)
which.min(mod.summary$cp)
which.max(mod.summary$adjr2)
coefficients(mod.full, id=2)

plot(mod.summary$cp, xlab="Subset Size", ylab="Cp", pch=20, type="l")
points(which.min(mod.summary$cp), min(mod.summary$cp), pch=4, col="red", lwd=7)

plot(mod.summary$bic, xlab="Subset Size", ylab="BIC", pch=20, type="l")
points(which.min(mod.summary$bic), min(mod.summary$bic), pch=4, col="red", lwd=7)

plot(mod.summary$adjr2, xlab="Subset Size", ylab="AdjrR^2", pch=20, type="l")
points(which.max(mod.summary$adjr2), max(mod.summary$adjr2), pch=4, col="red", lwd=7)

#Further model comparison, now considering tmax and tavg
summary(resultsList[["oOH2+oOH2:tmax"]][["cOI_2weeksbefore"]][["Model"]])
AIC(resultsList[["oOH2+oOH2:tmax"]][["cOI_2weeksbefore"]][["Model"]])
BIC(resultsList[["oOH2+oOH2:tmax"]][["cOI_2weeksbefore"]][["Model"]])
summary(resultsList[["oOH2+oOH2:tavg"]][["cOI_2weeksbefore"]][["Model"]])
AIC(resultsList[["oOH2+oOH2:tavg"]][["cOI_2weeksbefore"]][["Model"]])
BIC(resultsList[["oOH2+oOH2:tavg"]][["cOI_2weeksbefore"]][["Model"]])

# Creation of scatter plots of dependent vs. independent variable
# Further: Regression line for linear/quadratic/cubic model is created and added to the scatter plot
oOH_x <- seq(min(joinedDataFrame$outOfHomeDuration), max(joinedDataFrame$outOfHomeDuration), length.out = 40)
oOH_y <- resultsList[["oOH"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[1]] + resultsList[["oOH"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[2]]*oOH_x

oOH2_x <- seq(min(joinedDataFrame$outOfHomeDuration), max(joinedDataFrame$outOfHomeDuration), length.out = 40)
oOH2_y <- resultsList[["oOH2"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[1]] + resultsList[["oOH2"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[2]]*oOH_x^2

oOH3_x <- seq(min(joinedDataFrame$outOfHomeDuration), max(joinedDataFrame$outOfHomeDuration), length.out = 40)
oOH3_y <- resultsList[["oOH3"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[1]] + resultsList[["oOH3"]][["cOI_2weeksbefore"]][["Model"]]$coefficients[[2]]*oOH_x^3


p1 <- joinedDataFrame %>% filter(cOI_2weeksbefore < 2) %>% filter(Date + 14 < "2021-01-01") %>%
ggplot(aes(x = outOfHomeDuration, y = cOI_2weeksbefore)) + 
geom_point() +
#geom_line(aes(oOH_x, oOH_y, color = "linear")) + #Adds regression line to scatter plot
#geom_line(aes(oOH2_x, oOH2_y, color = "quadratic")) +
#geom_line(aes(oOH3_x, oOH3_y, , color = "cubic")) +
theme_minimal() +
theme(legend.position = "bottom", text = element_text(size = 12), legend.title=element_blank()) +
xlab("Daily Out Of Home Duration \n per Person") +
ylab("Growth Multiplier")

p2 <- joinedDataFrame %>% filter(cOI_2weeksbefore < 2) %>% filter(Date + 14 < "2021-01-01") %>%
ggplot(aes(x = outdoorFraction2, y = cOI_2weeksbefore)) + 
geom_point() +
theme_minimal() +
theme(legend.position = "bottom", text = element_text(size = 12), legend.title=element_blank()) +
xlab("Outdoor \n Fraction") +
ylab("")

p3 <- joinedDataFrame %>% filter(cOI_2weeksbefore < 2) %>% filter(Date + 14 < "2021-01-01") %>%
ggplot(aes(x = outdoorFraction2*outOfHomeDuration, y = cOI_2weeksbefore)) + 
geom_point() +
theme_minimal() +
theme(legend.position = "bottom", text = element_text(size = 12), legend.title=element_blank()) +
xlab("Daily Out Of Home Duration/Person \n times Outdoor Fraction") +
ylab("")

p <- arrangeGrob(p1,p2, nrow=1)

ggsave("Independentvsdependent.pdf", p, dpi = 500, w = 9, h = 3)

# Extracting cross validation results, we are only looking at the cross validation results for the chosen model. 
# Model name in following line needs to be changed if one's interested in another model
cross_validation_results <- data.frame(resultsList[["oOH2+oOH2:out2_noInt"]][["cOI_2weeksbefore"]][["Model_10foldCV"]]$resample$MAE, resultsList[["oOH2+indoor"]][["cOI_2weeksbefore"]][["Model_10foldCV"]]$resample$RMSE, resultsList[["oOH2+indoor"]][["cOI_2weeksbefore"]][["Model_10foldCV"]]$resample$Resample)
colnames(cross_validation_results) <- c("MAE", "RMSE", "Fold")

cross_validation_results[1,3] <- "01"
cross_validation_results[2,3] <- "02"
cross_validation_results[3,3] <- "03"
cross_validation_results[4,3] <- "04"
cross_validation_results[5,3] <- "05"
cross_validation_results[6,3] <- "06"
cross_validation_results[7,3] <- "07"
cross_validation_results[8,3] <- "08"
cross_validation_results[9,3] <- "09"
cross_validation_results[10,3] <- "10"

p1 <- ggplot(data = cross_validation_results, aes(x = Fold, y = RMSE, group = 1)) +
geom_boxplot(color= "#7570B3", size = 1.2) +
geom_point(color = "#666666", size = 1.5) +
theme_minimal() +
theme(text = element_text(size = 13)) +
theme(axis.ticks.x = element_line(), 
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))

p2 <- ggplot(data = cross_validation_results, aes(x = Fold, y = MAE, group = 1)) +
geom_boxplot(color= "#7570B3", size = 1.2) +
geom_point(color = "#666666", size = 1.5) +
theme_minimal() +
theme(text = element_text(size = 13)) +
theme(axis.ticks.x = element_line(), 
                   axis.ticks.y = element_line(),
                   axis.ticks.length = unit(5, "pt"))

p <- arrangeGrob(p2,p1, nrow=1)

#PLOTTING REGRESSION RESULTS IN 3D
# Model output using 2(!) variables 
summary(resultsList[["oOH+oOH2:out2_noInt"]][["cOI_2weeksbefore"]][["Model"]])
coefficients(mod.full, id=2)

reduced_joinedDataFrame <- joinedDataFrame %>% filter(cOI_2weeksbefore < 2) %>% filter(Date +14 < "2021-01-01") %>% 
  select("outOfHomeDurationCubed", "outOfHomeDurationSquared","outOfHomeDuration", "outdoorFraction2", "cOI_2weeksbefore", "Welle") %>%
  mutate(Welle_color = case_when(Welle == "First Wave" ~ "#1B9E77",
                      Welle == "Summer period" ~ "#7570B3", Welle == "Second Wave" ~ "#D95F02"))

my_surface <- function(f, n=10, ...) {
  x <- seq(min(reduced_joinedDataFrame$outOfHomeDuration), max(reduced_joinedDataFrame$outOfHomeDuration), length = 100)
  y <- seq(min(reduced_joinedDataFrame$outdoorFraction2), max(reduced_joinedDataFrame$outdoorFraction2), length = 100)
  z <- outer(x,y,f)
  surface3d(x, y, z, ...)
}

# Coefficients have been manually extracted
f <- function(outOfHomeDuration, outdoorFraction2){
  0.024224 * outOfHomeDuration * outOfHomeDuration  -0.005036  * outOfHomeDuration * outOfHomeDuration * outdoorFraction2
}

x1 <- reduced_joinedDataFrame$outOfHomeDurationSquared
x2 <- reduced_joinedDataFrame$outdoorFraction2
y <- f(x1, x2)

plot3d(reduced_joinedDataFrame$outOfHomeDuration,reduced_joinedDataFrame$outdoorFraction2,reduced_joinedDataFrame$cOI_2weeksbefore, type="p", col = reduced_joinedDataFrame$Welle_color, xlab="out Of Home Duration", ylab="outdoor Fraction", zlab="change of Incidence", size = 7)
my_surface(f, alpha = 0.2)


#Model output using 3(!) variables 
summary(resultsList[["oOH3+oOHout+oOH2out"]][["cOI_2weeksbefore"]][["Model"]])

coefficients(mod.full, id=3)

my_surface <- function(f, n=10, ...) {
  x <- seq(min(reduced_joinedDataFrame$outOfHomeDuration), max(reduced_joinedDataFrame$outOfHomeDuration), length = 100)
  y <- seq(min(reduced_joinedDataFrame$outdoorFraction2), max(reduced_joinedDataFrame$outdoorFraction2), length = 100)
  z <- outer(x,y,f)
  surface3d(x, y, z, ...)
}


f <- function(outOfHomeDuration = reduced_joinedDataFrame$outOfHomeDuration, outdoorFraction2 =  reduced_joinedDataFrame$outdoorFraction2){
  0.003426 * outOfHomeDuration * outOfHomeDuration * outOfHomeDuration + 0.309101 * outOfHomeDuration * outdoorFraction2  -0.048321 * outOfHomeDuration * outOfHomeDuration * outdoorFraction2
}
x1 <- reduced_joinedDataFrame$outOfHomeDuration
x2 <- reduced_joinedDataFrame$outdoorFraction2
y <- f(x1, x2, x3, x4)

plot3d(x1,x2,reduced_joinedDataFrame$cOI_2weeksbefore, type="p", col = reduced_joinedDataFrame$Welle_color, xlab="out Of Home Duration", ylab="outdoor Fraction", zlab="change of Incidence", size = 7)
my_surface(f, alpha = 0.2)
