runLinearRegression <- function(data_list, configuration) {
  price_stats <- readRDS('output/price_stats.RDS')
  model_list <- list()
  for (k in 1:configuration$parameters$data$training$kfolds) {
    model_list[[k]] <- list()
    training_data <- data_list[['train_data']][[k]]
    test_data     <- data_list[['test_data']][[k]]
    model <- lm(data = training_data, formula = log_price ~.-price)
    summary(model)
    while (length(model$coefficients) > configuration$parameters$model$no_of_variables+1) {
      eliminated_variable <- varImp(model) %>% arrange(Overall) %>% rownames() %>% first() %>% gsub(pattern = "TRUE", replacement = "", fixed = TRUE)
      training_data <- training_data[,-eliminated_variable, with = FALSE]
      model <- lm(data = training_data, formula = log_price ~.-price)
    }
    
    predicted_data <- predict(model, newdata = test_data)
    model_list[[k]][['model']] <- model
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
  message("Summary of the final model:")
  print(summary(suggested_model[['model']]))
  return(suggested_model)
}