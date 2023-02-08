#function for selecting the chosen long list
selectLongListData <- function(data, configuration) {
  data <- data[, configuration$parameters$long_list, with = FALSE]
  return(data)
}

#function for selecting the chosen regression variables
selectRegressionListData <- function(data, configuration) {
  data <- data[, configuration$parameters$regression_list, with = FALSE]
  return(data)
}

#function for selecting the chosen XGB variables
selectXGBListData <- function(data, configuration) {
  data <- data[, configuration$parameters$xgb_list, with = FALSE]
  return(data)
}