create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

generate.prediction <- function(data, prediction, toCSV = FALSE) {
  for (row in 1:nrow(data)) {
    data[row, "status"] <- prediction[[row]]
  }

  data <- select(data, c('loan_id', 'status'))
  names(data) <- c('Id', 'Predicted')

  if (toCSV) write.csv(data, "prediction.csv", row.names = FALSE)
  else print(data)
}
