runXGBoostRegression <- function(data_list, configuration, params = list(max_depth = 6, eta = 0.05, subsample = 0.8, lambda = 0.5, alpha = 0, gamma = 0.75, min_child_weight = 5), hypertune = F) {
  #loading back in the unstandardized parameters to evaluate the performance on the actual price
  price_stats <- readRDS('output/price_stats.RDS')
  model_list <- list()
  
  #fixing the randomness to ensure reproducibility
  set.seed(20)
  
  #looping through the K-folds to produce K models with different train/test sets
  for (k in 1:configuration$parameters$data$training$kfolds) {
    model_list[[k]] <- list()
    #storing test_data for later performance checks
    test_data     <- data_list[['test_data']][[k]]
    
    #creating the xgb data structures used for the algorithm
    training_data <- xgb.DMatrix(data = as.matrix(data_list[['train_data']][[k]])[,-c(1,2)], label = as.matrix(data_list[['train_data']][[k]])[,2])
    testing_data  <- xgb.DMatrix(data = as.matrix(data_list[['test_data']][[k]])[,-c(1,2)], label = as.matrix(data_list[['test_data']][[k]])[,2])
    watchlist     <- list(train = training_data, test = testing_data)
    #running the XGB algorithm.
    xgb_model     <- xgb.train(params = params,
                               data = training_data, 
                               watchlist = watchlist,
                               booster = 'gbtree', 
                               nrounds = 500,
                               early_stopping_rounds = 20,
                               verbose = 0)
    #predicting values for the test_data to save as details
    predicted_data <- predict(xgb_model, newdata = testing_data) 
    
    #storing model details for comparison and evaluation
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
  
  #using the average R squared as an indicator of performance and choosing the
  #highest R squared as the final model
  mean_adj_rsquared <- scales::percent(mean(sapply(model_list, '[[','adj_rsquared')), accuracy = 0.1)
  message("Performance of the models:")
  message(paste0("The average R^2 for the models on the actual price was:", mean_adj_rsquared))
  
  suggested_model <- model_list[[which(sapply(model_list, '[[','adj_rsquared') == max(sapply(model_list, '[[', 'adj_rsquared')))]]
  
  #special option if only running the regression to find the best XGB parameters
  if (hypertune) {
    return(mean_adj_rsquared)
  } else {
    return(suggested_model)
  }
}

#function for finding the best XGB parameters
hypertuneParameters <- function(data_list, configuration) {
  #potential values for the XGB parameters
  tuning_list <- list(max_depth = seq(5,10), eta = seq(0.05, 0.5, 0.05), subsample = seq(0.6, 1, 0.1), lambda = seq(0, 2, 0.25), alpha = seq(0, 2, 0.25), gamma = seq(0, 2, 0.25), min_child_weight = seq(4, 8))
  #starting values for the XGB parameters
  params = list(max_depth = 5, eta = 0.05, subsample = 0.75, lambda = 0, alpha = 0, gamma = 0, min_child_weight = 0)
  
  #cycling through the potential values per parameter and choosing the best.
  #Only then moving onto the next parameter
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
