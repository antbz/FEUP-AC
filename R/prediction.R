# rm(list = ls())

# To import a library, run this command first:
#    install.packages(x), x is a string with the name of the library

library(tidyverse)
library(skimr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(AppliedPredictiveModeling)
library(caret)
library(MASS)
library(e1071)
library(ROCR) # ROC and AUC
library(ROSE)
library(Hmisc) # Correlation
library(corrplot) # Correlation
library(PerformanceAnalytics) # Correlation
library(EnvStats)

source("functions.R")
source("datasets.R")

train.dataset <- get.train.dataset()
test.dataset <- dplyr::select(get.test.dataset(), -status)

### Correlation

correlation.matrix <- rcorr(as.matrix(train.dataset))

corrplot(correlation.matrix$r, type="upper", order="hclust",
         p.mat = correlation.matrix$P, sig.level = 0.01, insig = "blank")

chart.Correlation(train.dataset, histogram=TRUE, pch=19)


### Imbalanced Classification

# balanced.dataset <- imbalanced.classification(train.dataset)


### Extract Predictors and Classes

classes <- factor(train.dataset$status)
predictors <- dplyr::select(train.dataset, -status)


### K-Fold Cross Validation

# k.fold.cross.validation(predictors, classes)


### Build the model with the full dataset

model <- rpart(status ~ ., data = train.dataset, method = "class")
rpart.plot(model, extra = 106)


### Check ROC, AUC and accuracy

predictions <- predict(model, newdata = predictors, type = "prob")

pred_ROCR <- prediction(predictions, classes)
roc_ROCR <- performance(pred_ROCR, "tpr", "fpr")
plot(roc_ROCR, main = "ROC curve")
abline(0, 1, lty = 2)

auc_ROCR <- performance(pred_ROCR, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR

table_mat <- table(classes, predictions[,"-1"])
sum(diag(table_mat)) / sum(table_mat)


### Predict values

prediction <- predict(model, test.dataset, type = "prob")
prediction

generate.prediction(test.dataset, prediction[,"-1"], toCSV = TRUE)

## Clean prediction csv

predictions <- read.csv("../prediction_xgboost.csv",sep = ";")
predictions <- dplyr::select(predictions, -c("status","prediction","positive"))
predictions <- rename(predictions, "Id" = "loan_id") %>% rename("Predicted" = "negative") %>%
  mutate(Predicted = ifelse(Predicted >= 0.95,1,Predicted)) %>%
  mutate(Predicted = ifelse(Predicted <= 0.05, 0, Predicted))
write.csv(predictions,"../prediction_clean.csv",sep = ",", row.names = FALSE)
