enhanceData <- function(data, parameters) {
  data <- enhanceCalendarUpdated(data, parameters)
  data <- enhanceReviewPeriod(data, parameters)
  data <- enhanceAmenities(data, parameters)
  data <- normalizeNumericals(data, parameters)
  
  return(data)
}


enhanceCalendarUpdated <- function(data, parameters) {
  
  calendar_updated_cleansed <- vector(mode = 'numeric', length = nrow(data))
  
  calendar_updated_cleansed[sapply(data$calendar_updated, grepl, pattern = 'day')] <- 1
  calendar_updated_cleansed[sapply(data$calendar_updated, grepl, pattern = 'week')] <- 7
  calendar_updated_cleansed[sapply(data$calendar_updated, grepl, pattern = 'month')] <- 30
  calendar_updated_cleansed <- calendar_updated_cleansed * as.numeric(sapply(data$calendar_updated, gsub, pattern = '[^0-9]', replacement = ''))
  calendar_updated_cleansed[data$calendar_updated == 'today'] <- 0
  calendar_updated_cleansed[data$calendar_updated == 'yesterday'] <- 1
  calendar_updated_cleansed[data$calendar_updated == 'a week ago'] <- 7
  
  data$calendar_updated_cleansed <- calendar_updated_cleansed
  data$calendar_updated <- NULL
  
  return(data)
}

enhanceReviewPeriod <- function(data, parameters) {
  data$review_period <- as.integer(data$last_review - data$first_review)
  return(data)
}

enhanceAmenities <- function(data, parameters) {
  data$amenities <- tolower(data$amenities)
  data$has_tv <- lapply(data$amenities, grepl, pattern = 'tv', fixed = TRUE)
  data$has_internet <- lapply(data$amenities, grepl, pattern = 'internet', fixed = TRUE)
  data$has_wifi <- lapply(data$amenities, grepl, pattern = 'wifi', fixed = TRUE)
  data$has_parking <- lapply(data$amenities, grepl, pattern = 'parking', fixed = TRUE)
  data$amenities <- NULL
  return(data)
}

normalizeNumericals <- function(data, parameters) {
  save_stats <- c(mean(data$price), sd(data$price))
  saveRDS(save_stats, 'output/price_stats.RDS')
  for (col in colnames(data)) {
    if (is.numeric(data[[col]])) {
      data[[col]] <- (data[[col]] - mean(data[[col]], na.rm = T)) / sd(data[[col]], na.rm = T)
    }
  }
  return(data)
}