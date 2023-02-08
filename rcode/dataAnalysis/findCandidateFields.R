findSignificantFields <- function(data, configuration) {
  #taking all the fields that are numeric and calculating their correlation with
  # the dependent variable
  numeric_data <- data[,sapply(data, class) %in% c('numeric'), with = FALSE]
  numeric_correlations <- data.frame(cor(numeric_data, use = 'pairwise.complete.obs', method = 'pearson'))
  numeric_exclusions <- colnames(numeric_data)[abs(numeric_correlations$log_price) < configuration$parameters$short_list$correlation_lower_threshold]
  numeric_data[,numeric_exclusions] <- NULL
  
  
  #taking all the logical variables and calculating their biserial correlation
  # with the dependent variable
  logical_data <- data[,sapply(data, class) %in% c('logical', 'integer'), with = FALSE]
  for (binary in colnames(logical_data)) {
    if (abs(biserial.cor(data$log_price, as.logical(logical_data[[binary]]))) < configuration$parameters$short_list$correlation_lower_threshold) {
      logical_data[[binary]] <- NULL
    }
  }

  short_list_data <- cbind(numeric_data, logical_data)
  
  return(short_list_data)  
}

