# rm(list = ls())

# To import a library, run this command first:
#    install.packages(x), x is a string with the name of the library

library(tidyverse)
library(rpart)
library(rpart.plot)
library(AppliedPredictiveModeling)
library(caret)
library(MASS)
library(e1071)
library(ROCR) # ROC and AUC

source("functions.R")
source("datasets.R")

train.dataset <- get.train.dataset()
predictors <- train.dataset$predictors
classes <- train.dataset$classes
test <- get.test.dataset()

### K-Fold Cross Validation

set.seed(1)
cvSplits <- createFolds(classes, k = 10, returnTrain = TRUE)

fulllda <- c(); fullknn <- c(); fullrpart <- c(); fullsvm <- c()

for (i in 1:10) {
  trainFold <- cvSplits[[i]]
  trainPredictors <- predictors[trainFold,]
  trainClasses <- classes[trainFold]
  testPredictors <- predictors[-trainFold,]
  testClasses <- classes[-trainFold]

  # lda (MASS)
  # ldaFit <- lda(x = trainPredictors, grouping = trainClasses)
  # ldaPredictions <- predict(ldaFit, newdata = testPredictors, type = "class")
  # table_mat <- table(testClasses, ldaPredictions$class)
  # fulllda[[i]] <- sum(diag(table_mat)) / sum(table_mat)

  # knn3 (caret)
  knnFit <- knn3(x = trainPredictors, y = trainClasses, k = 5)
  knnPredictions <- predict(knnFit, newdata = testPredictors, type = "class")
  table_mat <- table(testClasses, knnPredictions)
  fullknn[[i]] <- sum(diag(table_mat)) / sum(table_mat)

  # Get the full data set
  aux <- add_column(trainPredictors, status = trainClasses)

  # rpart (rpart)
  rpartFit <- rpart(status ~ ., data = aux)
  rpartPredictions <- predict(rpartFit, newdata = testPredictors, type = "class")
  table_mat <- table(testClasses, rpartPredictions)
  fullrpart[[i]] <- sum(diag(table_mat)) / sum(table_mat)

  # svm (e1071)
  svmFit <- svm(status ~ ., data = aux)
  svmPredictions <- predict(svmFit, newdata = testPredictors, type = "class")
  table_mat <- table(testClasses, svmPredictions)
  fullsvm[[i]] <- sum(diag(table_mat)) / sum(table_mat)
}

fulllda
mean(fulllda)

fullknn
mean(fullknn)

fullrpart
mean(fullrpart)

fullsvm
mean(fullsvm)


### Build the model with the full dataset

full_dataset <- add_column(predictors, status = classes)

# K-Nearest Neighbors
#model <- knn3(x = predictors, y = classes, k = 5)

# CART
model <- rpart(status ~ ., data = full_dataset)
rpart.plot(model, extra = 106)


### Check ROC, AUC and accuracy
predictions <- predict(model, newdata = predictors, type = "prob")

pred_ROCR <- prediction(predictions[,"-1"], classes)
roc_ROCR <- performance(pred_ROCR, "tpr", "fpr")
plot(roc_ROCR, main = "ROC curve")
abline(0, 1, lty = 2)

auc_ROCR <- performance(pred_ROCR, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]

plot(performance(pred_ROCR, "acc"))

table_mat <- table(classes, predictions)
sum(diag(table_mat)) / sum(table_mat)


### Predict values
prediction <- predict(model, test, type = "prob")
prediction

generate.prediction(test, prediction[,"-1"], toCSV = TRUE)
