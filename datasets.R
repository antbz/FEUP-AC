### Data Sets

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


### Prepare Data Sets

prepare.datasets <- function(train = TRUE) {
  account.dataset()
  client.dataset()
  disposition.dataset()
  demograph.dataset()

  if (train) {
    transacions.dataset(train = TRUE)
    loan.dataset(train = TRUE)
    card.dataset(train = TRUE)
  } else {
    transacions.dataset(train = FALSE)
    loan.dataset(train = FALSE)
    card.dataset(train = FALSE)
  }
}

account.dataset <- function() {
  account <- read.csv("data/account.csv", sep = ";")
  account <<- account %>%
    mutate(account_year = date %/% 10000 + 1900) %>%
    mutate(account_month = date %% 10000 %/% 100) %>%
    mutate(account_day = date %% 100) %>%
    dplyr::select(-date)
}

client.dataset <- function() {
  client <- read.csv("data/client.csv", sep = ";")
  client <<- client %>%
    mutate(birth_year = birth_number %/% 10000 + 1900) %>%
    mutate(birth_month = birth_number %% 10000 %/% 100) %>%
    mutate(birth_day = birth_number %% 100) %>%
    mutate(gender = as.factor(ifelse(birth_month > 12, "women", "men"))) %>%
    mutate(birth_month = ifelse(birth_month > 12, birth_month - 50, birth_month)) %>%
    dplyr::select(-birth_number)
}

disposition.dataset <- function() {
  disposition <- read.csv("data/disp.csv", sep = ";")
  disposition <<- disposition %>%
    group_by(account_id) %>% mutate(n_clients = n()) %>% ungroup() %>%
    filter(type == "OWNER") %>%
    dplyr::select(-type)
}

transacions.dataset <- function(train = TRUE) {
  if (train) transactions <- read.csv("data/trans_train.csv", sep = ";")
  else transactions <- read.csv("data/trans_test.csv", sep = ";")
  # transactions <- read.csv("data/trans_train.csv", sep = ";")

  transactions <<- transactions %>%
    mutate(trans_year = date %/% 10000 + 1900) %>%
    mutate(trans_month = date %% 10000 %/% 100) %>%
    mutate(trans_day = date %% 100)
}

loan.dataset <- function(train = TRUE) {
  if (train) loan <- read.csv("data/loan_train.csv", sep = ";")
  else loan <- read.csv("data/loan_test.csv", sep = ";")
  # loan <- read.csv("data/loan_train.csv", sep = ";")

  loan <<- loan %>%
    mutate(loan_year = date %/% 10000 + 1900) %>%
    mutate(loan_month = date %% 10000 %/% 100) %>%
    mutate(loan_day = date %% 100) %>%
    dplyr::select(-date)
}

card.dataset <- function(train = TRUE) {
  if (train) card <- read.csv("data/card_train.csv", sep = ";")
  else card <- read.csv("data/card_test.csv", sep = ";")
  # card <- read.csv("data/card_train.csv", sep = ";")

  card <<- card %>%
    mutate(card_year = issued %/% 10000 + 1900) %>%
    mutate(card_month = issued %% 10000 %/% 100) %>%
    mutate(card_day = issued %% 100) %>%
    dplyr::select(-issued)
}

demograph.dataset <- function() {
  demograph <- read.csv("data/district.csv", sep = ";")
  demograph <<- demograph %>%
    mutate(unemploymant.rate.95 = as.numeric(as.character(unemploymant.rate.95))) %>%
    mutate(no..of.commited.crimes.95 = as.numeric(as.character(no..of.commited.crimes.95))) %>%
    dplyr::select(-name)
}


### Merge Data Sets

get.train.dataset <- function() {
  prepare.datasets(train = TRUE)

  data <- loan %>%
    left_join(account, by = "account_id", suffix = c('_loan', '_account')) %>%
    left_join(disposition, by = "account_id", suffix = c('', '_disp')) %>%
    left_join(client, by = "client_id", suffix = c('', '_client')) %>%
    mutate_if(is.factor, as.numeric) %>%
    dplyr::select(-c(loan_id, account_id, district_id, disp_id, client_id))

  classes <- factor(data$status)
  predictors <- dplyr::select(data, -status)

  return (list("predictors" = predictors, "classes" = classes))
}

get.test.dataset <- function() {
  prepare.datasets(train = FALSE)

  data <- loan %>%
    left_join(account, by = "account_id", suffix = c('_loan', '_account')) %>%
    left_join(disposition, by = "account_id", suffix = c('', '_disp')) %>%
    left_join(client, by = "client_id", suffix = c('', '_client')) %>%
    dplyr::select(-c(account_id, district_id, disp_id, client_id))

  return (dplyr::select(data, -status))
}
