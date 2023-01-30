loadData <- function(parameters) {
  data <- read.csv(parameters$data$location) %>% as.data.table()
  data <- dropColumns(data, parameters)
  data <- assertColumnClasses(data, parameters)
  data <- cleanData(data, parameters)
  return(data)
}