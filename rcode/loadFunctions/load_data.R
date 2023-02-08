loadData <- function(configuration) {
  #read the raw csv
  data <- read.csv(configuration$input_data$location) %>% as.data.table()
  #drop columns that are not specified for a certain class
  data <- dropColumns(data, configuration)
  #ensure the columns have the right format and cast them into the correct form if not
  data <- assertColumnClasses(data, configuration)
  return(data)
}