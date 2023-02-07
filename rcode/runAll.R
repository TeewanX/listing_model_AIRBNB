runAll <- function() {
  
  configuration <- loadConfiguration('configuration/configuration.yaml')
  
  data <- loadData(configuration)
  message('Data loaded in and fields dropped - Round 1')
  data <- enhanceData(data, configuration)
  data <- selectLongListData(data, configuration)
  data <- createDummyFields(data, configuration)
  message('Data processed and fields dropped - Round 2')
  data <- findSignificantFields(data, configuration)
  message('Correlations calculated and fields dropped - Round 3')
  determineCorrelations(data, configuration)
  
  print("============== MLR MODDEL ================")
  
  mlr_data <- selectRegressionListData(data, configuration)
  mlr_data_list <- createTrainTestData(mlr_data, configuration)
  message('Multivariate linear regression training/testing data prepared and fields dropped - Round 4')
  suggested_mlr_model <- runLinearRegression(mlr_data_list, configuration)
  message('MLR model calibration complete!')
  performModelAssessment(suggested_mlr_model, configuration)
  
  print("============== XGB MODEL ==================")
  xgb_data <- selectXGBListData(data, configuration)
  xgb_data_list <- createTrainTestData(xgb_data, configuration)
  message('eXtreme gradient boosting training/testing data prepared and fields dropped - Round 5')
  suggested_xgb_model <- runXGBoostRegression(xgb_data_list, configuration)
  message('XGB model calibration complete')
  
  
  performModelAssessment(suggested_xgb_model, configuration)
}