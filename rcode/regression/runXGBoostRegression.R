runXGBoostRegression <- function(data_list, configuration, params = list(max_depth = 6, eta = 0.05, subsample = 0.8, lambda = 0.5, alpha = 0, gamma = 0.75, min_child_weight = 5), hypertune = F) {
  price_stats <- readRDS('output/price_stats.RDS')
  model_list <- list()
  set.seed(20)
  for (k in 1:configuration$parameters$data$training$kfolds) {
    model_list[[k]] <- list()
    test_data     <- data_list[['test_data']][[k]]
    training_data <- xgb.DMatrix(data = as.matrix(data_list[['train_data']][[k]])[,-c(1,2)], label = as.matrix(data_list[['train_data']][[k]])[,2])
    testing_data  <- xgb.DMatrix(data = as.matrix(data_list[['test_data']][[k]])[,-c(1,2)], label = as.matrix(data_list[['test_data']][[k]])[,2])
    watchlist     <- list(train = training_data, test = testing_data)
    xgb_model     <- xgb.train(params = params,
                               data = training_data, 
                               watchlist = watchlist,
                               booster = 'gbtree', 
                               nrounds = 500,
                               early_stopping_rounds = 20,
                               verbose = 0)
  
    predicted_data <- predict(xgb_model, newdata = testing_data) 
    model_list[[k]][['model']] <- xgb_model
    model_list[[k]][['rsquared']] <- calculateRSquared(exp(predicted_data * price_stats[2] + price_stats[1]), test_data$price)
    model_list[[k]][['adj_rsquared']] <- calculateAdjustedRsquared(model_list[[k]][['rsquared']], predicted_data , configuration)
    model_list[[k]][['mse']] <- calculateMeanSquaredError(exp(predicted_data*price_stats[2] + price_stats[1]), test_data$price)
    model_list[[k]][['mae']] <- calculateMeanAbsoluteError(exp(predicted_data * price_stats[2] + price_stats[1]), test_data$price)
    model_list[[k]][['rmse']] <- calculateRootMeanSquaredError(exp(predicted_data * price_stats[2] + price_stats[1]), test_data$price)
    model_list[[k]][['rsquared_log']] <- calculateRSquared(predicted_data, test_data$log_price)
    model_list[[k]][['adj_rsquared_log']] <- calculateAdjustedRsquared(model_list[[k]][['rsquared_log']], predicted_data , configuration)
    model_list[[k]][['residuals']] <- test_data$price - exp(predicted_data * price_stats[2] + price_stats[1])
    model_list[[k]][['mse_log']] <- calculateMeanSquaredError(predicted_data, test_data$log_price)
    model_list[[k]][['mae_log']] <- calculateMeanAbsoluteError(predicted_data, test_data$log_price)
    model_list[[k]][['rmse_log']] <- calculateRootMeanSquaredError(predicted_data, test_data$log_price)
    model_list[[k]][['residuals_log']] <- test_data$log_price - predicted_data
    model_list[[k]][['test_data']] <- test_data
    model_list[[k]][['test_data']][['log_predictions']] <- predicted_data
    model_list[[k]][['test_data']][['predictions']] <- exp(predicted_data * price_stats[2] + price_stats[1])
  }
  mean_adj_rsquared <- scales::percent(mean(sapply(model_list, '[[','adj_rsquared')), accuracy = 0.1)
  message("Performance of the models:")
  message(paste0("The average R^2 for the models on the actual price was:", mean_adj_rsquared))
  
  suggested_model <- model_list[[which(sapply(model_list, '[[','adj_rsquared') == max(sapply(model_list, '[[', 'adj_rsquared')))]]
  
  if (hypertune) {
    return(mean_adj_rsquared)
  } else {
    return(suggested_model)
  }
}

hypertuneParameters <- function(data_list, configuration) {
  tuning_list <- list(max_depth = seq(5,10), eta = seq(0.05, 0.5, 0.05), subsample = seq(0.6, 1, 0.1), lambda = seq(0, 2, 0.25), alpha = seq(0, 2, 0.25), gamma = seq(0, 2, 0.25), min_child_weight = seq(4, 8))
  params = list(max_depth = 5, eta = 0.05, subsample = 0.75, lambda = 0, alpha = 0, gamma = 0, min_child_weight = 0)
  for (parameter in names(tuning_list)) {
    print(parameter)
    parameter_performance <- c()
    for (parameter_value in tuning_list[[parameter]]) {
      print(parameter_value)
      params[[parameter]] <- parameter_value
      parameter_performance <- c(parameter_performance, runXGBoostRegression(data_list, configuration, params = params, hypertune = T))
    }
    params[[parameter]] <- tuning_list[[parameter]][which(parameter_performance == max(parameter_performance))]
  }
  return(params)
}