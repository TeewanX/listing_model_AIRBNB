loadData <- function(configuration) {
  data <- read.csv(configuration$input_data$location) %>% as.data.table()
  data <- dropColumns(data, configuration)
  data <- assertColumnClasses(data, configuration)
  return(data)
}