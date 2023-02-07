createTrainTestData <- function(data, configuration) {
  
  set.seed(20)
  train_indices <- createDataPartition(data$log_price, 
                                       p = configuration$parameters$data$training$data_split, 
                                       times = configuration$parameters$data$training$kfolds,
                                       list = TRUE)
  train_list <- list()
  test_list <- list()
  for (fold in 1:configuration$parameters$data$training$kfolds) {
    train_list[[fold]] <- data[train_indices[[fold]]]
    test_list[[fold]]  <- data[-train_indices[[fold]]]
  }
  
  return(list(train_data = train_list, test_data = test_list))
}