###########################################
# Check required libraries, download and 
# Install if needed
###########################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

###########################################
# Load required libraries
###########################################

library(tidyverse)
library(caret)

###########################################
# Load training and test data
###########################################

load("rda/dat.rda")

###########################################
# Set up 2-fold cross-validation
###########################################

control <- trainControl(method = "cv", number = 2)

###########################################
# Train k-Nearest Neighbor model
# NOTE: This takes close to 7 hours
###########################################

train_knn <- train(y ~ ., method = "knn", data = dat$train, trControl = control)

###########################################
# Predict k-Nearest Neighbor model
# NOTE: This takes close to 2 hours
###########################################

yhat_knn <- predict(train_knn, dat$test[-55])

###########################################
# Evaluate k-Nearest Neighbor model
###########################################

cm_knn <- confusionMatrix(data = yhat_knn, reference = dat$test$y)
print(cm_knn)

###########################################
# Train Random Forest model
# NOTE: This takes close to 6 hours
###########################################

train_rf <- train(y ~ ., method = "rf", data = dat$train, trControl = control)

###########################################
# Predict Random Forest model
# NOTE: This is quick - about 1 minute
###########################################

yhat_rf <- predict(train_rf, dat$test[-55])

###########################################
# Evaluate Random Forest model
###########################################

cm_rf <- confusionMatrix(data = yhat_rf, reference = dat$test$y)
print(cm_rf)

###########################################
# Train Linear Discriminant Analysis model
# NOTE: This is quick - about 6 minutes
###########################################

train_lda <- train(y ~ ., method = "lda", data = dat$train, trControl = control)

###########################################
# Predict Linear Discriminant Analysis model
# NOTE: This is quick - about 1 minute
###########################################

yhat_lda <- predict(train_lda, dat$test[-55])

###########################################
# Evaluate Linear Discriminant Analysis model
###########################################

cm_lda <- confusionMatrix(data = yhat_lda, reference = dat$test$y)
print(cm_lda)

###########################################
# Train Penalized Multinomial Regression model
# NOTE: This is relatively quick - about 50 minutes
###########################################

train_mn <- train(y ~ ., method = "multinom", data = dat$train, trControl = control)

###########################################
# Predict Penalized Multinomial Regression model
# NOTE: This is quick - about 1 minute
###########################################

yhat_mn <- predict(train_mn, dat$test[-55])

###########################################
# Evaluate Penalized Multinomial Regression model
###########################################

cm_mn <- confusionMatrix(data = yhat_mn, reference = dat$test$y)
print(cm_mn)

###########################################
# Train Support Vector Machines with Linear Kernel model
# NOTE: This takes close to 27 hours
###########################################

train_svm <- train(y ~ ., method = "svmLinear", data = dat$train, trControl = control)

###########################################
# Predict Support Vector Machines with Linear Kernel model
# NOTE: This is quick - about 1 minute
###########################################

yhat_svm <- predict(train_svm, dat$test[-55])
cm_svm <- confusionMatrix(data = yhat_svm, reference = dat$test$y)
print(cm_svm)

###########################################
# Build an Ensemble from models already trained
# knn, rf, lda, multinom, svmLinear
###########################################

###########################################
# Create data frame with individual model predictions
###########################################

pred <- data.frame(knn = yhat_knn, rf = yhat_rf, lda = yhat_lda, multinom = yhat_mn, svmLinear = yhat_svm)

###########################################
# Create list with individual model accuracies
###########################################

acc <- sapply(pred, function(y_hat) {
   mean(y_hat == dat$test$y)
})

###########################################
# Print individual model accuracies
###########################################

print(acc)

###########################################
# Create vector of classes for use in majority vote
###########################################

classes <- c("Spruce", "Lodgepole", "Panderosa", "Cottonwood", "Aspen", "Douglas", "Krummholz")

###########################################
# Compute ensemble predictions using majority vote.
# NOTE: Needs tie breaker
###########################################

yhat_ensemble <- sapply(1:nrow(pred), function(i) {
   votes <- pred[i,]
   probs <- c(mean(votes == "Spruce"), mean(votes == "Lodgepole"), mean(votes == "Panderosa"), mean(votes == "Cottonwood"), mean(votes == "Aspen"), mean(votes == "Douglas"), mean(votes == "Krummholz"))
   classes[which.max(probs)]
})

###########################################
# Convert ensemble predictions to factor
###########################################

yhat_ensemble <- factor(yhat_ensemble, levels = levels(dat$test$y))

###########################################
# Compute ensemble accuracy
###########################################

acc_ensemble <- mean(yhat_ensemble == dat$test$y)

###########################################
# Print ensemble accuracy
###########################################

print(acc_ensemble)