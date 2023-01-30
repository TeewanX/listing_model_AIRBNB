dropColumns <- function(data, parameters) {
  #Dropping all the columns which were deemed unnecessary
  keep_columns    <- unlist(parameters$configuration$columns, use.names = F)
  columns_to_keep <- intersect(colnames(data), keep_columns)
  new_data        <- data[, ..columns_to_keep]
  return(new_data)
}


assertColumnClasses <- function(data, parameters) {
  for (class in names(parameters$configuration$columns)) {
    columns_to_transform <- parameters$configuration$columns[[class]]
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


