dropColumns <- function(data, configuration) {
  #Dropping all the columns which were deemed unnecessary
  keep_columns    <- unlist(configuration$parameters$columns, use.names = F)
  columns_to_keep <- intersect(colnames(data), keep_columns)
  new_data        <- data[, ..columns_to_keep]
  return(new_data)
}


assertColumnClasses <- function(data, configuration) {
  #for all the classes it ensures they are of the correct type. It also removes
  #unnecessary symbols to cast them in to the correct data type.
  for (class in names(configuration$parameters$columns)) {
    columns_to_transform <- configuration$parameters$columns[[class]]
    if (class == 'dates') {
      data[, columns_to_transform] <- lapply(data[, ..columns_to_transform], function(x) ifelse(x == '', NA, x))
      data[, columns_to_transform] <- lapply(data[, ..columns_to_transform], as.Date)
    } else if (class == 'characters') {
      data[, columns_to_transform] <- lapply(data[, ..columns_to_transform], function(x) ifelse(x == '', NA, x))
      data[, columns_to_transform] <- lapply(data[, ..columns_to_transform], as.character)
    } else if (class == 'integers') {
      data[, columns_to_transform] <- lapply(data[, ..columns_to_transform], function(x) ifelse(x == '', NA, x))
      data[, columns_to_transform] <- lapply(data[, ..columns_to_transform], as.integer)
    } else if (class == 'numericals') {
      data[, columns_to_transform] <- lapply(data[, ..columns_to_transform], function(x) ifelse(x == '', NA, x))
      data[, columns_to_transform] <- lapply(data[, ..columns_to_transform], gsub, pattern = "[^0-9.]",replacement = "")
      data[, columns_to_transform] <- lapply(data[, ..columns_to_transform], as.numeric)
    } else if (class == 'logicals') {
      data[, columns_to_transform] <- lapply(data[, ..columns_to_transform], function(x) ifelse(x == 't', TRUE, FALSE))
    } else if (class == 'categoricals') {
      data[, columns_to_transform] <- lapply(data[, ..columns_to_transform], function(x) ifelse(x == '', NA, x))
      data[, columns_to_transform] <- lapply(data[, ..columns_to_transform], as.factor)
    }
  }
  return(data)
}


