calculateRSquared <- function(predicted_data, actual_data) {
  rsquared <- 1 - (sum((predicted_data - actual_data)^2)/sum((actual_data - mean(actual_data))^2))
  return(rsquared)
}

calculateAdjustedRsquared <- function(rsquared, predicted_data, configuration) {
  adj_rsquared <- 1 - (1-rsquared)*(length(predicted_data) - 1)/(length(predicted_data) - configuration$parameters$model$no_of_variables - 1)
  return(adj_rsquared)
}

calculateMeanSquaredError <- function(predicted_data, actual_data) {
  mse <- mean((actual_data - predicted_data)^2)
  return(mse)
}

calculateMeanAbsoluteError <- function(predicted_data, actual_data) {
  mae <- mean(abs(actual_data - predicted_data))
  return(mae)
}

calculateRootMeanSquaredError <- function(predicted_data, actual_data) {
  rmse <- sqrt(mean((actual_data - predicted_data)^2))
  return(rmse)
}
