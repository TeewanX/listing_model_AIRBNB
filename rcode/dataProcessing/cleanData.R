cleanData <- function(data, parameters) {
  data <- data[price > 0]
  return(data)
}