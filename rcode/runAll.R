runAll <- function() {
  
  #loading the configuration along with the parameters and any required packages
  configuration <- loadConfiguration('configuration/configuration.yaml')
  
  #loading the data
  data <- loadData(configuration)
  message('Data loaded in and fields dropped - Round 1')
  
  #filtering data for extreme outliers, reformatting fields and creating 
  #derived variables
  data <- enhanceData(data, configuration)
  
  #dropping columns based on expert judgement data quality
  data <- selectLongListData(data, configuration)
  
  #creating dummy variables for all candidate fields
  data <- createDummyFields(data, configuration)
  message('Data processed and fields dropped - Round 2')
  
  #removing fields with too low correlation
  data <- findSignificantFields(data, configuration)
  datmessage('Correlations calculated and fields dropped - Round 3')
  
  #exporting a correlation matrix for all remaining fields
  determineCorrelations(data, configuration)
  
  #starting the calibration of the multivariate linear regression model
  print("============== MLR MODDEL ================")
  
  #selecting candidate fields for the MLR model based on expert judgement
  mlr_data <- selectRegressionListData(data, configuration)
  
  #creating a train test split to train multiple models with their own test test
  mlr_data_list <- createTrainTestData(mlr_data, configuration)
  message('Multivariate linear regression training/testing data prepared and fields dropped - Round 4')
  
  #running a backward regression algorith to find the best MLR model
  suggested_mlr_model <- runLinearRegression(mlr_data_list, configuration)
  message('MLR model calibration complete!')
  
  #showing some general plots and stats on the models performance
  performModelAssessment(suggested_mlr_model, configuration)
  
  
  
  #starting the calibration of the eXtreme Gradient Boosting model
  print("============== XGB MODEL ==================")
  
  #selecting candidate fields for the XGB model based on expert judgement
  xgb_data <- selectXGBListData(data, configuration)
  
  #creating a train test split to train multiple models with their own test set
  xgb_data_list <- createTrainTestData(xgb_data, configuration)
  message('eXtreme gradient boosting training/testing data prepared and fields dropped - Round 5')
  
  #running the XGBoost algorithm to find the best XGB model
  suggested_xgb_model <- runXGBoostRegression(xgb_data_list, configuration)
  message('XGB model calibration complete')
  
  #showing some general plots and stats on the models performance
  performModelAssessment(suggested_xgb_model, configuration)
}