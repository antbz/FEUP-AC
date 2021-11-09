generate.prediction <- function(data, prediction, toCSV = FALSE) {
  for (row in 1:nrow(data)) {
    data[row, "status"] <- prediction[[row]]
  }

  data <- dplyr::select(data, c('loan_id', 'status'))
  names(data) <- c('Id', 'Predicted')

  if (toCSV) write.csv(data, "prediction.csv", row.names = FALSE)
  else print(data)
}
