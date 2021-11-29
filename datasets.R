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


### Helper functions

age = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)

  age = to_lt$year - from_lt$year

  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

days <- function(from, to) {
  return (as.numeric(to - from))
}


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

  # missing.values <- sapply(account, function(x) sum(is.na(x)))
  # mv.account <- data.frame(missing.values)
  # summary(account)
  # account %>% skim()

  account <<- account %>%
    mutate(account_year = date %/% 10000 + 1900) %>%
    mutate(account_month = date %% 10000 %/% 100) %>%
    mutate(account_day = date %% 100) %>%
    mutate(frequency = recode(frequency,
        "monthly issuance" = "monthly",
        "weekly issuance" = "weekly",
        "issuance after transaction" = "immediate"
    )) %>%
    dplyr::select(-c(date, district_id))
}

client.dataset <- function() {
  client <- read.csv("data/client.csv", sep = ";")
  client <- dplyr::distinct(client)

  # missing.values <- sapply(client, function(x) sum(is.na(x)))
  # mv.client <- data.frame(missing.values)
  # summary(client)
  # client  %>% skim()

  client <<- client %>%
    mutate(birth_year = birth_number %/% 10000 + 1900) %>%
    mutate(birth_month = birth_number %% 10000 %/% 100) %>%
    mutate(birth_day = birth_number %% 100) %>%
    mutate(gender = as.factor(ifelse(birth_month > 12, "F", "M"))) %>%
    mutate(birth_month = ifelse(birth_month > 12, birth_month - 50, birth_month)) %>%
    dplyr::select(-c(birth_number))
}

disposition.dataset <- function() {
  disposition <- read.csv("data/disp.csv", sep = ";")
  disposition <- dplyr::distinct(disposition)

  # missing.values <- sapply(disposition, function(x) sum(is.na(x)))
  # mv.disposition <- data.frame(missing.values)
  # summary(disposition)
  # disposition %>% skim()

  disposition <<- disposition %>%
    group_by(account_id) %>% mutate(n_clients = n()) %>% ungroup() %>%
    filter(type == "OWNER") %>%
    dplyr::select(-type)
}

transacions.dataset <- function(train = TRUE) {
  if (train) transactions <- read.csv("data/trans_train.csv", sep = ";")
  else transactions <- read.csv("data/trans_test.csv", sep = ";")

  transactions <- dplyr::distinct(transactions)

  # missing.values <- sapply(transactions, function(x) sum(is.na(x)))
  # mv.transactions <- data.frame(missing.values)
  # summary(transactions)
  # transactions %>% skim()

  transactions <<- transactions %>%
    mutate(trans_year = date %/% 10000 + 1900) %>%
    mutate(trans_month = date %% 10000 %/% 100) %>%
    mutate(trans_day = date %% 100) %>%
    mutate(type = as.character(type)) %>%
    mutate(type = ifelse(type == "credit", type, "withdrawal")) %>%
    mutate(amount = ifelse(type == "credit", amount, -1 * amount)) %>%
    mutate(type = as.factor(type)) %>%
    mutate(operation = as.character(operation)) %>%
    mutate(operation = ifelse(operation == "", as.character(k_symbol), operation)) %>%
    mutate(operation = as.factor(operation)) %>%
    mutate(operation = recode(operation,
        "credit in cash" = "credit_cash",
        "withdrawal in cash" = "withdraw_cash",
        "collection from another bank" = "collect_bank",
        "remittance to another bank" = "remittance_bank",
        "credit card withdrawal" = "withdraw_card",
        "interest credited" = "interest"
    )) %>%
    mutate(is_sanction = (as.character(k_symbol) == "sanction interest if negative balance")) %>%
    dplyr::select(-c(bank, account, trans_day, k_symbol, date))
}

loan.dataset <- function(train = TRUE) {
  if (train) loan <- read.csv("data/loan_train.csv", sep = ";")
  else loan <- read.csv("data/loan_test.csv", sep = ";")
  # loan <- read.csv("data/loan_train.csv", sep = ";")
  loan <- dplyr::distinct(loan)

  # missing.values <- sapply(loan, function(x) sum(is.na(x)))
  # mv.loan <- data.frame(missing.values)
  # summary(loan)
  # loan %>% skim()

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

  # missing.values <- sapply(card, function(x) sum(is.na(x)))
  # mv.card <- data.frame(missing.values)
  # summary(card)
  # card %>% skim()

  card <<- card %>%
    mutate(card_year = issued %/% 10000 + 1900) %>%
    mutate(card_month = issued %% 10000 %/% 100) %>%
    mutate(card_day = issued %% 100) %>%
    dplyr::select(-c(issued, card_day))
}

demograph.dataset <- function() {
  demograph <- read.csv("data/district.csv", sep = ";")
  demograph <- dplyr::distinct(demograph)

  # missing.values <- sapply(demograph, function(x) sum(is.na(x)))
  # mv.demograph <- data.frame(missing.values)
  # summary(demograph)
  # demograph %>% skim()

  demograph <<- demograph %>%
    rename("inhabitants" = "no..of.inhabitants") %>%
    rename("municipalities_inhabitants_under_499" = "no..of.municipalities.with.inhabitants...499") %>%
    rename("municipalities_inhabitants_500_1999" = "no..of.municipalities.with.inhabitants.500.1999") %>%
    rename("municipalities_inhabitants_2000_9999" = "no..of.municipalities.with.inhabitants.2000.9999") %>%
    rename("municipalities_inhabitants_over_10000" = "no..of.municipalities.with.inhabitants..10000") %>%
    rename("cities" = "no..of.cities") %>%
    rename("ratio_urban_inhabitants" = "ratio.of.urban.inhabitants") %>%
    rename("average_salary" = "average.salary") %>%
    rename("unemploymant_rate_95" = "unemploymant.rate.95") %>%
    rename("unemploymant_rate_96" = "unemploymant.rate.96") %>%
    rename("enterpreneurs_per_1000" = "no..of.enterpreneurs.per.1000.inhabitants") %>%
    rename("commited_crimes_95" = "no..of.commited.crimes.95") %>%
    rename("commited_crimes_96" = "no..of.commited.crimes.96") %>%
    dplyr::select(-name)

  demograph <<- demograph %>%
    mutate(unemploymant_rate_95 = ifelse(is.na(unemploymant_rate_95), mean(unemploymant_rate_95, na.rm=TRUE), unemploymant_rate_95)) %>%
    mutate(commited_crimes_95 = ifelse(is.na(commited_crimes_95), mean(commited_crimes_95, na.rm=TRUE), commited_crimes_95))


}


### Aggregate Function

aggregate.transactions <- function() {
  agg.transactions <<- transactions %>%
    group_by(account_id) %>%
    # Number of operations
    mutate(n_operation = n()) %>%
    # Count and Ratio for Credit or Withdrawal operations
    mutate(n_credit = sum(as.character(type) == "credit")) %>%
    mutate(n_withdrawal = sum(as.character(type) == "withdrawal")) %>%
    mutate(ratio_credit = mean(as.character(type) == "credit")) %>%
    mutate(ratio_withdrawal = mean(as.character(type) == "withdrawal")) %>%
    # Number of operations of each type
    mutate(n_collect_bank = sum(as.character(operation) == "collect_bank")) %>%
    mutate(n_withdraw_card = sum(as.character(operation) == "withdraw_card")) %>%
    mutate(n_credit_cash = sum(as.character(operation) == "credit_cash")) %>%
    mutate(n_interest = sum(as.character(operation) == "interest")) %>%
    mutate(n_remittance_bank = sum(as.character(operation) == "remittance_bank")) %>%
    mutate(n_withdraw_cash = sum(as.character(operation) == "withdraw_cash")) %>%
    # Ratio of operations of each type
    mutate(ratio_collect_bank = mean(as.character(operation) == "collect_bank")) %>%
    mutate(ratio_withdraw_card = mean(as.character(operation) == "withdraw_card")) %>%
    mutate(ratio_credit_cash = mean(as.character(operation) == "credit_cash")) %>%
    mutate(ratio_interest = mean(as.character(operation) == "interest")) %>%
    mutate(ratio_remittance_bank = mean(as.character(operation) == "remittance_bank")) %>%
    mutate(ratio_withdraw_cash = mean(as.character(operation) == "withdraw_cash")) %>%
    # Min, max, mean, sd for amount
    mutate(amount_min = min(amount)) %>%
    mutate(amount_max = max(amount)) %>%
    mutate(amount_mean = mean(amount)) %>%
    mutate(amount_std = sd(amount)) %>%
    # Min, max, mean, sd for balance
    mutate(balance_min = min(balance)) %>%
    mutate(balance_max = max(balance)) %>%
    mutate(balance_mean = mean(balance)) %>%
    mutate(balance_std = sd(balance)) %>%
    # Number of sanctions
    mutate(n_sanctions = sum(is_sanction)) %>%
    # Remove unwanted columns
    dplyr::select(-c(trans_id, type, operation, amount, balance, trans_year, trans_month, is_sanction)) %>%
    # Unique
    distinct
}


### Merge Data Sets

get.train.dataset <- function() {
  prepare.datasets(train = TRUE)

  aggregate.transactions()

  data <- loan %>%
    # Join with account
    left_join(account, by = "account_id") %>%
    mutate(account_days = days(
      as.Date(paste(as.character(account_year),as.character(account_month),as.character(account_day), sep="-"), format = "%Y-%m-%d"),
      as.Date(paste(as.character(loan_year),as.character(loan_month),as.character(loan_day), sep="-"), format = "%Y-%m-%d")
    )) %>%

    # Join with disposition
    left_join(disposition, by = "account_id") %>%

    # Join with client
    left_join(client, by = "client_id") %>%
    mutate(age = age(
      as.Date(paste(as.character(birth_year),as.character(birth_month),as.character(birth_day), sep="-")),
      as.Date(paste(as.character(loan_year),as.character(loan_month),as.character(loan_day), sep="-"))
    )) %>%

    # Join with card
    left_join(card, by = "disp_id") %>%
    mutate(has_card = ifelse(is.na(card_id), 0, 1)) %>%
    dplyr::select(-c(card_id, type, card_month, card_year)) %>%

    # Join with demograpgh
    left_join(demograph, by = "district_id") %>%

    # Join with transactions
    left_join(agg.transactions, by = "account_id") %>%
    mutate(n_operation_per_day = n_operation / account_days) %>%
    mutate(n_credit_per_day = n_credit / account_days) %>%
    mutate(n_withdrawal_per_day = n_withdrawal / account_days) %>%
    mutate(n_collect_bank_per_day = n_collect_bank / account_days) %>%
    mutate(n_withdraw_card_per_day = n_withdraw_card / account_days) %>%
    mutate(n_credit_cash_per_day = n_credit_cash / account_days) %>%
    mutate(n_interest_per_day = n_interest / account_days) %>%
    mutate(n_remittance_bank_per_day = n_remittance_bank / account_days) %>%
    mutate(n_withdraw_cash_per_day = n_withdraw_cash / account_days) %>%
    mutate(n_sanctions_per_day = n_sanctions / account_days) %>%

    # Drop unwanted columns
    dplyr::select(-c(
      loan_day,
      account_id, account_year, account_month, account_day,
      disp_id,
      client_id, birth_year, birth_month, birth_day,
      district_id
    ))

  # Verify NA
  # missing.values <- sapply(data, function(x) sum(is.na(x)))
  # mv.data <- data.frame(missing.values)
  # mv.data$total <- nrow(data)
  # mv.data$pct <- round((mv.data$missing.values / mv.data$total)*100)
  # summary(data)
  #
  write.csv(data, file = "train_data.csv")

  return (data)
}

get.test.dataset <- function() {
  prepare.datasets(train = FALSE)

  aggregate.transactions()

  data <- loan %>%
    # Join with account
    left_join(account, by = "account_id") %>%
    mutate(account_days = days(
      as.Date(paste(as.character(account_year),as.character(account_month),as.character(account_day), sep="-"), format = "%Y-%m-%d"),
      as.Date(paste(as.character(loan_year),as.character(loan_month),as.character(loan_day), sep="-"), format = "%Y-%m-%d")
    )) %>%

    # Join with disposition
    left_join(disposition, by = "account_id") %>%

    # Join with client
    left_join(client, by = "client_id") %>%
    mutate(age = age(
      as.Date(paste(as.character(birth_year),as.character(birth_month),as.character(birth_day), sep="-")),
      as.Date(paste(as.character(loan_year),as.character(loan_month),as.character(loan_day), sep="-"))
    )) %>%

    # Join with card
    left_join(card, by = "disp_id") %>%
    mutate(has_card = ifelse(is.na(card_id), 0, 1)) %>%
    dplyr::select(-c(card_id, type, card_month, card_year)) %>%

    # Join with demograpgh
    left_join(demograph, by = "district_id") %>%

    # Join with transactions
    left_join(agg.transactions, by = "account_id") %>%
    mutate(n_operation_per_day = n_operation / account_days) %>%
    mutate(n_credit_per_day = n_credit / account_days) %>%
    mutate(n_withdrawal_per_day = n_withdrawal / account_days) %>%
    mutate(n_collect_bank_per_day = n_collect_bank / account_days) %>%
    mutate(n_withdraw_card_per_day = n_withdraw_card / account_days) %>%
    mutate(n_credit_cash_per_day = n_credit_cash / account_days) %>%
    mutate(n_interest_per_day = n_interest / account_days) %>%
    mutate(n_remittance_bank_per_day = n_remittance_bank / account_days) %>%
    mutate(n_withdraw_cash_per_day = n_withdraw_cash / account_days) %>%
    mutate(n_sanctions_per_day = n_sanctions / account_days) %>%

    # Drop unwanted columns
    dplyr::select(-c(
      loan_day,
      account_id, account_year, account_month, account_day,
      disp_id,
      client_id, birth_year, birth_month, birth_day,
      district_id
    ))

  # Verify NA
  # missing.values <- sapply(data, function(x) sum(is.na(x)))
  # mv.data <- data.frame(missing.values)
  # mv.data$total <- nrow(data)
  # mv.data$pct <- round((mv.data$missing.values / mv.data$total)*100)
  # summary(data)
  #

  write.csv(data,file = "test_data.csv")

  return (data)
}

train.data <- get.train.dataset()
test.data <- get.test.dataset()

