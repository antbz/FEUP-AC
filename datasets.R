# Data sets
# account <- read.csv("data/account.csv", sep = ";")
# card.test <- read.csv("data/card_test.csv", sep = ";")
# card.train <- read.csv("data/card_train.csv", sep = ";")
# client <- read.csv("data/client.csv", sep = ";")
# disposition <- read.csv("data/disp.csv", sep = ";")
# demograph <- read.csv("data/district.csv", sep = ";")
# loan.test <- read.csv("data/loan_test.csv", sep = ";")
# loan.train <- read.csv("data/loan_train.csv", sep = ";")
# transactions.test <- read.csv("data/trans_test.csv", sep = ";")
# transactions.train <- read.csv("data/trans_train.csv", sep = ";")

get.train.dataset <- function() {
  loan.train <- read.csv("data/loan_train.csv", sep = ";")
  account <- read.csv("data/account.csv", sep = ";")
  disposition <- read.csv("data/disp.csv", sep = ";")

  data <- loan.train %>%
    left_join(account, by = "account_id", suffix = c('_loan', '_account')) %>%
    left_join(disposition, by = "account_id", suffix = c('', '_disp')) %>%
    group_by(account_id) %>% mutate(n_accounts = n()) %>%
    distinct(loan_id, account_id, .keep_all = TRUE) %>%
    mutate(frequency = as.numeric(frequency)) %>%
    dplyr::select(-type)

  classes <- factor(data$status)
  predictors <- dplyr::select(data, -status)

  return (list("predictors" = predictors, "classes" = classes))
}

get.test.dataset <- function() {
  loan.test <- read.csv("data/loan_test.csv", sep = ";")
  account <- read.csv("data/account.csv", sep = ";")
  disposition <- read.csv("data/disp.csv", sep = ";")

  data <- loan.test %>%
    left_join(account, by = "account_id", suffix = c('_loan', '_account')) %>%
    left_join(disposition, by = "account_id", suffix = c('', '_disp')) %>%
    group_by(account_id) %>% mutate(n_accounts = n()) %>%
    distinct(loan_id, account_id, .keep_all = TRUE) %>%
    mutate(frequency = as.numeric(frequency)) %>%
    dplyr::select(-type)

  return (dplyr::select(data, -status))
}
