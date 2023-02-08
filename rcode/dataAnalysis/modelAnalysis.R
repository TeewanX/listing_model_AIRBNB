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
  
  
  #plotting based on the accommodates field
  plot_data_accommodates <- plot_data %>% 
    group_by(accommodates) %>%
    summarise(mean_error = mean(price - predictions),
              mean_price = mean(price),
              mean_prediction = mean(predictions),
              records = n()
              )
  accommodates_stats <- readRDS('output/normalization/accommodates.RDS')
  plot_data_accommodates$accommodates <- plot_data_accommodates$accommodates * accommodates_stats[2] + accommodates_stats[1]
  p <- ggplot(plot_data_accommodates) +
    geom_line(aes(x = accommodates, y = mean_price, col = 'Actual price'), linewidth = 1) +
    geom_line(aes(x = accommodates, y = mean_prediction, col = 'Predicted price'), linewidth = 1) +
    theme_minimal() +
    theme(text = element_text(size = 20)) +
    labs(title = 'Actual prices vs predicted prices over accommodates',
         x = 'Accommodates',
         y = 'Price in EUR',
         colour = 'Legend') +
    ylim(c(0, NA))
  print(p)
  file_name <- paste0('output/',model_type,"/actual_vs_prediction_accommodates.png")
  ggsave(filename = file_name, plot = p, height = 10, width = 15, bg = 'transparent')
  
  #plotting based on distance to city center
  plot_data_distance <- plot_data %>%
    mutate(distance_to_city_center = cut(log_distance_to_center, breaks = 50)) %>%
    group_by(distance_to_city_center) %>%
    summarise(mean_error = mean(price-predictions),
              mean_price = mean(price),
              mean_prediction = mean(predictions),
              records = n(),
              distance_to_city_center = max(as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", distance_to_city_center))))
  log_distance_stats <- readRDS('output/normalization/log_distance_to_center.RDS')
  plot_data_distance$distance_to_city_center <- round(exp(plot_data_distance$distance_to_city_center * log_distance_stats[2] + log_distance_stats[1])/1000,2)
  p <- ggplot(plot_data_distance) +
    geom_line(aes(x = distance_to_city_center, y = mean_price, col = 'Actual price'), linewidth = 1) +
    geom_line(aes(x = distance_to_city_center, y = mean_prediction, col = 'Predicted price'), linewidth = 1) +
    theme_minimal() +
    theme(text = element_text(size = 20)) +
    labs(title = 'Actual prices vs predicted prices per distance group',
         x = 'Distance to city center in km',
         y = 'Price in EUR',
         colour = 'Legend') +
    ylim(c(0, NA))
  print(p)
  file_name <- paste0('output/',model_type,"/actual_vs_prediction_distance.png")
  ggsave(filename = file_name, plot = p, height = 10, width = 15, bg = 'transparent')
  
  
  #plotting based on price bucket
  plot_data_price_bucket <- plot_data %>%
    mutate(price_bucket = cut(price, seq(0, 500, 10))) %>%
    group_by(price_bucket) %>%
    summarise(mean_error = mean(price-predictions),
              mean_price = mean(price),
              mean_prediction = mean(predictions),
              records = n(),
              price_bucket = max(as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", price_bucket))))
  
  p <- ggplot(plot_data_price_bucket) +
    geom_line(aes(x = price_bucket, y = mean_price, col = 'Actual price'), linewidth = 1) +
    geom_line(aes(x = price_bucket, y = mean_prediction, col = 'Prediction price'), linewidth = 1) +
    theme_minimal() +
    theme(text = element_text(size = 20)) +
    labs(title = 'Actual prices vs predicted prices per price bucket',
         x = 'Price bucket',
         y = 'Price in EUR',
         colour = 'Legend')
  print(p)
  file_name <- paste0('output/',model_type,"/actual_vs_prediction_price.png")
  ggsave(filename = file_name, plot = p, height = 10, width = 15, bg = 'transparent')
  
  p <- ggplot(plot_data_price_bucket) +
    geom_point(aes(x = price_bucket, y = mean_error), col = 'salmon') +
    theme_minimal() +
    theme(text = element_text(size = 20)) +
    labs(title = 'Error in prediction per price bucket',
         x = 'Price bucket',
         y = 'Delta in EUR')
  print(p)
  file_name <- paste0('output/',model_type,"/prediction_error.png")
  ggsave(filename = file_name, plot = p, height = 10, width = 15, bg = 'transparent')
  
  p <- ggplot(plot_data_price_bucket) +
    geom_point(aes(x = price_bucket, y = records), col = 'salmon') +
    theme_minimal() +
    theme(text = element_text(size = 20)) +
    labs(title = 'Number of test records per price bucket',
         x = 'Price bucket',
         y = 'Number of records')    
  print(p)
  file_name <- paste0('output/',model_type,"/no_of_records_per_price.png")
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



compareModels <- function(suggested_xlr_model, suggested_mlr_model, configuration) {
  price_stats <- readRDS('output/normalization/log_price.RDS')
  testing_data_xgb <- suggested_xgb_model$test_data
  testing_data_xgb$predictions_xgb <- testing_data$predictions
  testing_data_xgb$predictions <- NULL
  mlr_predicted_data <- predict(suggested_mlr_model$model, newdata = testing_data_xgb)
  testing_data_xgb$predictions_mlr <- exp(mlr_predicted_data * price_stats[2] + price_stats[1])
  
  #creating additional variables used for analysis
  plot_data <- testing_data_xgb %>% arrange(price)
  plot_data$smooth_price <- (plot_data$price + lag(plot_data$price) + lag(plot_data$price, 2) + lead(plot_data$price) + lead(plot_data$price, 2))/5
  
  #plotting based on the accommodates field
  plot_data_accommodates <- plot_data %>% 
    group_by(accommodates) %>%
    summarise(mean_price = mean(price),
              mean_prediction_xgb = mean(predictions_xgb),
              mean_prediction_mlr = mean(predictions_mlr),
              records = n()
    )
  accommodates_stats <- readRDS('output/normalization/accommodates.RDS')
  plot_data_accommodates$accommodates <- plot_data_accommodates$accommodates * accommodates_stats[2] + accommodates_stats[1]
  p <- ggplot(plot_data_accommodates) +
    geom_line(aes(x = accommodates, y = mean_price, col = 'Actual price'), linewidth = 1) +
    geom_line(aes(x = accommodates, y = mean_prediction_xgb, col = 'Predicted price - XGB'), linewidth = 1) +
    geom_line(aes(x = accommodates, y = mean_prediction_mlr, col = 'Predicted price - MLR'), linewidth = 1) +
    theme_minimal() +
    theme(text = element_text(size = 20)) +
    labs(title = 'Actual prices vs predicted prices over accommodates',
         x = 'Accommodates',
         y = 'Price in EUR',
         colour = 'Legend') +
    ylim(c(0, NA)) +
    scale_color_manual(values = c('Actual price' = 'salmon','Predicted price - MLR' = 'cyan','Predicted price - XGB' = 'seagreen'))
  print(p)
  file_name <- paste0("output/comparison/actual_vs_prediction_accommodates.png")
  ggsave(filename = file_name, plot = p, height = 10, width = 15, bg = 'transparent')
  
  #plotting based on distance to city center
  plot_data_distance <- plot_data %>%
    mutate(distance_to_city_center = cut(log_distance_to_center, breaks = 50)) %>%
    group_by(distance_to_city_center) %>%
    summarise(mean_price = mean(price),
              mean_prediction_xgb = mean(predictions_xgb),
              mean_prediction_mlr = mean(predictions_mlr),
              records = n(),
              distance_to_city_center = max(as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", distance_to_city_center))))
  log_distance_stats <- readRDS('output/normalization/log_distance_to_center.RDS')
  plot_data_distance$distance_to_city_center <- round(exp(plot_data_distance$distance_to_city_center * log_distance_stats[2] + log_distance_stats[1])/1000,2)
  p <- ggplot(plot_data_distance) +
    geom_line(aes(x = distance_to_city_center, y = mean_price, col = 'Actual price'), linewidth = 1) +
    geom_line(aes(x = distance_to_city_center, y = mean_prediction_xgb, col = 'Predicted price - XGB'), linewidth = 1) +
    geom_line(aes(x = distance_to_city_center, y = mean_prediction_mlr, col = 'Predicted price - MLR'), linewidth = 1) +
    theme_minimal() +
    theme(text = element_text(size = 20)) +
    labs(title = 'Actual prices vs predicted prices per distance group',
         x = 'Distance to city center in km',
         y = 'Price in EUR',
         colour = 'Legend') +
    ylim(c(0, NA)) +
    scale_color_manual(values = c('Actual price' = 'salmon','Predicted price - MLR' = 'cyan','Predicted price - XGB' = 'seagreen'))
  print(p)
  file_name <- paste0("output/comparison/actual_vs_prediction_distance.png")
  ggsave(filename = file_name, plot = p, height = 10, width = 15, bg = 'transparent')
  
  
  #plotting based on price bucket
  plot_data_price_bucket <- plot_data %>%
    mutate(price_bucket = cut(price, seq(0, 500, 10))) %>%
    group_by(price_bucket) %>%
    summarise(mean_price = mean(price),
              mean_prediction_xgb = mean(predictions_xgb),
              mean_prediction_mlr = mean(predictions_mlr),
              records = n(),
              price_bucket = max(as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", price_bucket))))
  
  p <- ggplot(plot_data_price_bucket) +
    geom_line(aes(x = price_bucket, y = mean_price, col = 'Actual price'), linewidth = 1) +
    geom_line(aes(x = price_bucket, y = mean_prediction_xgb, col = 'Predicted price - XGB'), linewidth = 1) +
    geom_line(aes(x = price_bucket, y = mean_prediction_mlr, col = 'Predicted price - MLR'), linewidth = 1) +
    theme_minimal() +
    theme(text = element_text(size = 20)) +
    labs(title = 'Actual prices vs predicted prices per price bucket',
         x = 'Price bucket',
         y = 'Price in EUR',
         colour = 'Legend') +
    ylim(c(0, NA)) +
    scale_color_manual(values = c('Actual price' = 'salmon','Predicted price - MLR' = 'cyan','Predicted price - XGB' = 'seagreen'))
  print(p)
  file_name <- paste0("output/comparison/actual_vs_prediction_price.png")
  ggsave(filename = file_name, plot = p, height = 10, width = 15, bg = 'transparent')
  
}