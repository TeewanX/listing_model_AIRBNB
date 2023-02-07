exportDataFrame <- function(data, location, filename) {
  if (!any(class(data) %in% c('data.table', 'data.frame'))){
    stop(paste0('Data is not a dataframe or datatable, but ', class(data)))
  }
  write.xlsx(data, file = paste0(location,"/",filename))
}