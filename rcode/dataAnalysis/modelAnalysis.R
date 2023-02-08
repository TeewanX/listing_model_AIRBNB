performModelAssessment <- function(model_details, configuration) {
  #tracking the model type for slightly different approaches and tagging
  if(class(model_details$model) == 'xgb.Booster') {
    model_type <- 'xgb'
  } else {
    model_type <- 'mlr'
  }
  
  #providing information on the Rsquared and RMSE
  performance_statement <- paste0('The final RSquared on the log of the price was ', 
                                  scales::percent(model_details$adj_rsquared_log, accuracy = 0.1), 
                                  ". If we only consider the actual price, this drops down to ", 
                                  scales::percent(model_details$adj_rsquared, accuracy = 0.1), 
                                  ". The average error is ",
                                  round(model_details$mae, digits = 1), 
                                  "EUR. However, the RMSE is ",
                                  round(model_details$rmse, digits = 1),"EUR.")
  message(performance_statement)
  
  
  ###Plots###
  
  #creating additional variables used for analysis
  plot_data <- model_details$test_data %>% arrange(price)
  plot_data$residuals <- plot_data$price - plot_data$predictions
  plot_data$residuals_log <- plot_data$log_price - plot_data$log_predictions
  plot_data$smooth_price <- (plot_data$price + lag(plot_data$price) + lag(plot_data$price, 2) + lead(plot_data$price) + lead(plot_data$price, 2))/5
  
  
  #plotting logarithmic residuals vs logarithmic price
  p <- ggplot(plot_data) +  
    stat_density2d(aes(x = log_price, y = residuals_log, colour = after_stat(level)), linewidth = 1) +
    theme_minimal() +
    theme(text = element_text(size = 20)) +
    labs(title = "Residuals of the logarithmic prediction",
         x = 'Log(Price)', 
         y = 'Log(Price) - Log(Prediction)', 
         colour = "Density") +
    scale_colour_continuous(labels = scales::percent)
  plot(p)
  file_name <- paste0('output/',model_type,"/log_residuals.png")
  ggsave(filename = file_name, plot = p, height = 10, width = 15, bg = 'transparent')
  
  
  p <- ggplot(plot_data) +
    geom_line(aes(x = smooth_price, y = smooth_price, col = 'Actual price')) +
    geom_point(aes(x = price, y = predictions, col = 'Predicted price', shape = 'Predicted price'), size = 0.5, show.legend = F) +
    theme_minimal() +
    theme(text = element_text(size = 20)) +
    labs(title = "Actual prices vs predicted prices",
         x = 'Price',
         y = 'Price',
         colour = 'Legend',
         shape = 'Legend')
  plot(p)
  file_name <- paste0('output/',model_type,"/actual_vs_prediction.png")
  ggsave(filename = file_name, plot = p, height = 10, width = 15, bg = 'transparent')
  
  
  #calculating variable importance
  if (model_type == 'xgb') {
    importance <- xgb.importance(feature_names = model_details$model$feature_names, model = model_details$model)[1:10,1:2] %>%
      rename('metric' = 'Gain')
  } else {
    importance <- varImp(model_details$model) %>% 
      arrange(-Overall) %>% 
      tibble::rownames_to_column(var = 'Feature') %>%
      summarise(Feature, metric = Overall/max(2*Overall))
  }
  
  #plotting variable importance
  plot_data <- importance
  p <- ggplot(plot_data) +
    theme_minimal() +
    geom_col(aes(x = reorder(Feature,metric, sum), y = metric), fill = 'salmon') +
    coord_flip() +
    labs(x = 'Feature',
         y = 'Importance',
         title = paste0('Variable importance in ', model_type, ' model')) +
    scale_y_continuous(labels = scales::percent) +
    theme(text = element_text(size = 20))
  plot(p)
  file_name <- paste0('output/',model_type,"/variable_importance.png")
  ggsave(filename = file_name, plot = p, height = 10, width = 15, bg = 'transparent')
}
