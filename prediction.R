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


source("functions.R")
source("datasets.R")

train.dataset <- get.train.dataset()
predictors <- train.dataset$predictors
classes <- train.dataset$classes
test <- get.test.dataset()

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
  ldaFit <- lda(x = trainPredictors, grouping = trainClasses)
  ldaPredictions <- predict(ldaFit, newdata = testPredictors, type = "class")
  table_mat <- table(testClasses, ldaPredictions$class)
  accuracy <- sum(diag(table_mat)) / sum(table_mat)
  # fulllda[[i]] <- list("model" = ldaFit, "table" = table_mat, "accuracy" = accuracy)
  fulllda[[i]] <- accuracy

  # knn3 (caret)
  knnFit <- knn3(x = trainPredictors, y = trainClasses, k = 5)
  knnPredictions <- predict(knnFit, newdata = testPredictors, type = "class")
  table_mat <- table(testClasses, knnPredictions)
  accuracy <- sum(diag(table_mat)) / sum(table_mat)
  # fullknn[[i]] <- list("model" = knnFit, "table" = table_mat, "accuracy" = accuracy)
  fullknn[[i]] <- accuracy

  # rpart (rpart)
  aux <- add_column(trainPredictors, status = trainClasses)
  rpartFit <- rpart(status ~ ., data = aux)
  rpartPredictions <- predict(rpartFit, newdata = testPredictors, type = "class")
  table_mat <- table(testClasses, rpartPredictions)
  accuracy <- sum(diag(table_mat)) / sum(table_mat)
  # fullrpart[[i]] <- list("model" = rpartFit, "table" = table_mat, "accuracy" = accuracy)
  fullrpart[[i]] <- accuracy

  # svm (e1071)
  svmFit <- svm(status ~ ., data = aux)
  svmPredictions <- predict(svmFit, newdata = testPredictors, type = "class")
  table_mat <- table(testClasses, svmPredictions)
  accuracy <- sum(diag(table_mat)) / sum(table_mat)
  # fullsvm[[i]] <- list("model" = svmFit, "table" = table_mat, "accuracy" = accuracy)
  fullsvm[[i]] <- accuracy
}

fulllda
mean(fulllda)

fullknn
mean(fullknn)

fullrpart
mean(fullrpart)

fullsvm
mean(fullsvm)


# Build the model with the full dataset
full_dataset <- add_column(predictors, status = classes)
model <- rpart(status ~ ., data = full_dataset)
rpart.plot(model, extra = 106)

# Check accuracy
predictions <- predict(model, newdata = predictors, type = "class")
table_mat <- table(classes, predictions)
sum(diag(table_mat)) / sum(table_mat)

# Predict values
prediction <- predict(model, test, type = "prob")
table(prediction)

generate.prediction(loan.test, prediction[,"-1"], toCSV = TRUE)
