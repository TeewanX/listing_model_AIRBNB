runAll <- function() {
  
  parameters <- loadParameters('parameters/parameters.yaml')
  
  data <- loadData(parameters)
  
  data <- enhanceData(data, parameters)
  
  
}