#rm(list = ls())

library(rpart)
library(rpart.plot)
library(dplyr)
library(ROSE)

source("functions.r")

# Data sets
account <- read.csv("data/account.csv", sep = ";")
card.test <- read.csv("data/card_test.csv", sep = ";")
card.train <- read.csv("data/card_train.csv", sep = ";")
client <- read.csv("data/client.csv", sep = ";")
disposition <- read.csv("data/disp.csv", sep = ";")
demograph <- read.csv("data/district.csv", sep = ";")
loan.test <- read.csv("data/loan_test.csv", sep = ";")
loan.train <- read.csv("data/loan_train.csv", sep = ";")
transactions.test <- read.csv("data/trans_test.csv", sep = ";")
transactions.train <- read.csv("data/trans_train.csv", sep = ";")

# Build train data set
data <- loan.train
data <- merge(data, account, by = "account_id", all.x = TRUE, suffixes = c('_loan', '_account'))

disp.owners <- filter(disposition, type == "OWNER")
data <- merge(data, disp.owners, by = "account_id",  suffixes = c('', '_disp'))
data <- merge(data, card.train, by = "disp_id", all.x = TRUE,  suffixes = c('', '_card'))
#data <- select(data, c('date_loan', 'amount', 'duration', 'payments', 'status', 'frequency', 'date_account', 'type', 'type_card', 'issued'))

# Build test data set
test <- loan.test
test <- merge(test, account, by = "account_id", all.x = TRUE, suffixes = c('_loan', '_account'))
test <- merge(test, disp.owners, by = "account_id",  suffixes = c('', '_disp'))
test <- merge(test, card.test, by = "disp_id", all.x = TRUE,  suffixes = c('', '_card'))

data_train <- create_train_test(data, 0.8, train = TRUE)
data_test <- create_train_test(data, 0.8, train = FALSE)

fit <- rpart(status~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)

predict_unseen <-predict(fit, data_test, type = 'class')

table_mat <- table(data_test$status, predict_unseen)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)

print(paste('Accuracy for test', accuracy_Test))

prediction <- predict(fit, test, type = 'prob')

generate.prediction(loan.test, prediction[,"-1"], toCSV = FALSE)

