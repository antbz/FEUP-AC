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
  account <- dplyr::distinct(account)
  account <<- account %>%
    mutate(account_year = date %/% 10000 + 1900) %>%
    mutate(account_month = date %% 10000 %/% 100) %>%
    mutate(account_day = date %% 100) %>%
    dplyr::select(-c(date, account_day))
  account$frequency[account$frequency == "monthly issuance"] <- "monthly"
  account$frequency[account$frequency == "weekly issuance"] <- "weekly"
  account$frequency[account$frequency == "issuance after transaction"] <- "immediate"
}
age = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)

  age = to_lt$year - from_lt$year

  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

client.dataset <- function() {
  client <- read.csv("data/client.csv", sep = ";")
  client <- dplyr::distinct(client)
  client <<- client %>%
    mutate(birth_year = birth_number %/% 10000 + 1900) %>%
    mutate(birth_month = birth_number %% 10000 %/% 100) %>%
    mutate(birth_day = birth_number %% 100) %>%
    mutate(gender = as.factor(ifelse(birth_month > 12, "women", "men"))) %>%
    mutate(birth_month = ifelse(birth_month > 12, birth_month - 50, birth_month)) %>%
    dplyr::select(-c(birth_number))
}

disposition.dataset <- function() {
  disposition <- read.csv("data/disp.csv", sep = ";")
  disposition <- dplyr::distinct(disposition)
  disposition <<- disposition %>%
    group_by(account_id) %>% mutate(n_clients = n()) %>% ungroup() %>%
    filter(type == "OWNER") %>%
    dplyr::select(-type)
}

transacions.dataset <- function(train = TRUE) {
  if (train) transactions <- read.csv("data/trans_train.csv", sep = ";")
  else transactions <- read.csv("data/trans_test.csv", sep = ";")
  # transactions <- read.csv("data/trans_train.csv", sep = ";")
  transactions <- dplyr::distinct(transactions)
  transactions <<- transactions %>%
    mutate(trans_year = date %/% 10000 + 1900) %>%
    mutate(trans_month = date %% 10000 %/% 100) %>%
    mutate(trans_day = date %% 100)
}

loan.dataset <- function(train = TRUE) {
  if (train) loan <- read.csv("data/loan_train.csv", sep = ";")
  else loan <- read.csv("data/loan_test.csv", sep = ";")
  # loan <- read.csv("data/loan_train.csv", sep = ";")
  loan <- dplyr::distinct(loan)
  loan <<- loan %>%
    mutate(loan_year = date %/% 10000 + 1900) %>%
    mutate(loan_month = date %% 10000 %/% 100) %>%
    mutate(loan_day = date %% 100) %>%
    dplyr::select(-c(date))
}

card.dataset <- function(train = TRUE) {
  if (train) card <- read.csv("data/card_train.csv", sep = ";")
  else card <- read.csv("data/card_test.csv", sep = ";")
  # card <- read.csv("data/card_train.csv", sep = ";")
  card <- dplyr::distinct(card)
  card <<- card %>%
    mutate(card_year = issued %/% 10000 + 1900) %>%
    mutate(card_month = issued %% 10000 %/% 100) %>%
    mutate(card_day = issued %% 100) %>%
    dplyr::select(-c(issued, card_day))
}

demograph.dataset <- function() {
  demograph <- read.csv("data/district.csv", sep = ";")
  demograph <- dplyr::distinct(demograph)
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
    # left_join(card, by = "disp_id", suffic =c('', '_card')) %>%
    mutate_if(is.factor, as.numeric) %>%
    mutate(age = age(as.Date(paste(as.character(birth_year),as.character(birth_month),as.character(birth_day), sep="-")),as.Date(paste(as.character(loan_year),as.character(loan_month),as.character(loan_day), sep="-")))) %>%
    dplyr::select(-c(loan_id, account_id, district_id, disp_id, client_id, birth_year, birth_month, birth_day, loan_day))

  return (data)
}

get.test.dataset <- function() {
  prepare.datasets(train = FALSE)

  data <- loan %>%
    left_join(account, by = "account_id", suffix = c('_loan', '_account')) %>%
    left_join(disposition, by = "account_id", suffix = c('', '_disp')) %>%
    left_join(client, by = "client_id", suffix = c('', '_client')) %>%
    mutate_if(is.factor, as.numeric) %>%
    mutate(age = age(as.Date(paste(as.character(birth_year),as.character(birth_month),as.character(birth_day), sep="-")),as.Date(paste(as.character(loan_year),as.character(loan_month),as.character(loan_day), sep="-")))) %>%
    dplyr::select(-c(loan_id, account_id, district_id, disp_id, client_id, birth_year, birth_month, birth_day, loan_day))


  return (data)
}
