createTrainTestData <- function(data, configuration) {
  #fix the seed to ensure reproducibility
  set.seed(20)
  
  #partition K training/testing splits with a split of P. Stratified for log_price
  train_indices <- createDataPartition(data$log_price, 
                                       p = configuration$parameters$data$training$data_split, 
                                       times = configuration$parameters$data$training$kfolds,
                                       list = TRUE)
  
  #creating lists of training and testing sets
  train_list <- list()
  test_list <- list()
  for (fold in 1:configuration$parameters$data$training$kfolds) {
    train_list[[fold]] <- data[train_indices[[fold]]]
    test_list[[fold]]  <- data[-train_indices[[fold]]]
  }
  
  return(list(train_data = train_list, test_data = test_list))
}