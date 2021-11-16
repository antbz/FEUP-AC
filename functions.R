# Perform 10-fold cross validation with lda, knn, rpart and svm
k.fold.cross.validation <- function(predictors, classes) {
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
    fulllda[[i]] <- sum(diag(table_mat)) / sum(table_mat)

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

  return (list("lda" = mean(fulllda), "knn" = mean(fullknn), "rpart" = mean(fullrpart), "svm" = mean(fullsvm)))
}

# Deals with imbalanced datasets
imbalanced.classification <- function(dataset) {
  print(table(dataset$status))

  # Over Sampling
  # dataset <- ovun.sample(status ~ ., data = dataset, method = "over", N = 564, seed = 1)$data

  # Under Sampling
  # dataset <- ovun.sample(status ~ ., data = dataset, method = "under", N = 92, seed = 1)$data

  # Under and Over Sampling
  dataset <- ovun.sample(status ~ ., data = dataset, method = "both", N = 500, seed = 1)$data

  # Over Sampling
  # dataset <- ROSE(status ~ ., data = dataset, seed = 1)$data

  print(table(dataset$status))
  return (dataset)
}

# Generates the prediction.csv file
generate.prediction <- function(data, prediction, toCSV = FALSE) {
  for (row in 1:nrow(data)) {
    data[row, "status"] <- prediction[[row]]
  }

  data <- dplyr::select(data, c('loan_id', 'status'))
  names(data) <- c('Id', 'Predicted')

  if (toCSV) write.csv(data, "prediction.csv", row.names = FALSE)
  else print(data)
}
