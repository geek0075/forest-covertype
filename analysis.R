## Load required libraries
library(tidyverse)
library(caret)
## Load training and test data
load("rda/dat.rda")

## Set up 2-fold cross-validation
control <- trainControl(method = "cv", number = 2)

## Train, predict, and evaluate k-Nearest Neighbor model
## NOTE: This takes close to 7 hours
train_knn <- train(y ~ ., method = "knn", data = dat$train, trControl = control)
## NOTE: This takes close to 2 hours
yhat_knn <- predict(train_knn, dat$test[-55])
cm_knn <- confusionMatrix(data = yhat_knn, reference = dat$test$y)
print(cm_knn)

## Train, predict, and evaluate Random Forest model
## NOTE: This takes close to 6 hours
train_rf <- train(y ~ ., method = "rf", data = dat$train, trControl = control)
## NOTE: This is quick - about 1 minute
yhat_rf <- predict(train_rf, dat$test[-55])
cm_rf <- confusionMatrix(data = yhat_rf, reference = dat$test$y)
print(cm_rf)

## Train, predict, and evaluate Linear Discriminant Analysis model
## NOTE: This is quick - about 6 minutes
train_lda <- train(y ~ ., method = "lda", data = dat$train, trControl = control)
## NOTE: This is quick - about 1 minute
yhat_lda <- predict(train_lda, dat$test[-55])
cm_lda <- confusionMatrix(data = yhat_lda, reference = dat$test$y)
print(cm_lda)

## Train, predict, and evaluate Penalized Multinomial Regression model
## NOTE: This is relatively quick - about 50 minutes
train_mn <- train(y ~ ., method = "multinom", data = dat$train, trControl = control)
## NOTE: This is quick - about 1 minute
yhat_mn <- predict(train_mn, dat$test[-55])
cm_mn <- confusionMatrix(data = yhat_mn, reference = dat$test$y)
print(cm_mn)

## Train, predict, and evaluate Support Vector Machines with Linear Kernel model
## NOTE: This takes close to 27 hours
train_svm <- train(y ~ ., method = "svmLinear", data = dat$train, trControl = control)
## NOTE: This is quick - about 1 minute
yhat_svm <- predict(train_svm, dat$test[-55])
cm_svm <- confusionMatrix(data = yhat_svm, reference = dat$test$y)
print(cm_svm)

## Build an Ensemble from several of the models already trained
## knn, rf, lda, multinom, svmLinear
## create data frame with individual model predictions
pred <- data.frame(knn = yhat_knn, rf = yhat_rf, lda = yhat_lda, multinom = yhat_mn, svmLinear = yhat_svm)
## create list with individual model accuracies
acc <- sapply(pred, function(y_hat) {
  mean(y_hat == dat$test$y)
})
## print individual model accuracies
print(acc)
## > acc
##       knn        rf       lda  multinom svmLinear 
## 0.9622214 0.9648849 0.6787790 0.6981717 0.7269359
## create vector of classes
classes <- c("Spruce", "Lodgepole", "Panderosa", "Cottonwood", "Aspen", "Douglas", "Krummholz")
## compute ensemble predictions using majority vote. needs tie breaker
yhat_ensemble <- sapply(1:nrow(pred), function(i) {
  votes <- pred[i,]
  probs <- c(mean(votes == "Spruce"), mean(votes == "Lodgepole"), mean(votes == "Panderosa"), mean(votes == "Cottonwood"), mean(votes == "Aspen"), mean(votes == "Douglas"), mean(votes == "Krummholz"))
  classes[which.max(probs)]
})
## convert ensemble predictions to factor
yhat_ensemble <- factor(yhat_ensemble, levels = levels(dat$test$y))
## compute ensemble accuracy
acc_ensemble <- mean(yhat_ensemble == dat$test$y)
## print ensemble accuracy
print(acc_ensemble)
# [1] 0.790385



