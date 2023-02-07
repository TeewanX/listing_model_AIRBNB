selectLongListData <- function(data, configuration) {
  data <- data[, configuration$parameters$long_list, with = FALSE]
  return(data)
}

selectRegressionListData <- function(data, configuration) {
  data <- data[, configuration$parameters$regression_list, with = FALSE]
  return(data)
}

selectXGBListData <- function(data, configuration) {
  data <- data[, configuration$parameters$xgb_list, with = FALSE]
  return(data)
}