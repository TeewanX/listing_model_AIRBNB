enhanceData <- function(data, configuration) {
  data <- filterOutliers(data, configuration)
  data <- enhancePrice(data, configuration)
  data <- enhanceLuxury(data, configuration)
  data <- enhanceTransit(data, configuration)
  data <- enhanceDistanceToCenter(data, configuration)
  data <- enhancePropertyType(data, configuration)
  data <- enhanceHostResponseTime(data, configuration)
  data <- enhanceSecurityDeposit(data, configuration)
  data <- enhanceCleaningFee(data, configuration)
  data <- enhanceCalendarUpdated(data, configuration)
  data <- enhanceReviewPeriod(data, configuration)
  data <- enhanceAmenities(data, configuration)
  data <- normalizeNumericals(data, configuration)
  data <- imputeValues(data, configuration)

  return(data)
}

filterOutliers <- function(data, configuration) {
  data <- data[price < configuration$parameters$data$price_cutoff]
  data <- data[price > 1]
  return(data)
}

enhancePrice <- function(data, configuration) {
  data$log_price <- log(data$price)
  return(data)
}

enhanceLuxury <- function(data, configuration) {
  data$luxurious <-   (grepl(pattern = 'luxur', tolower(data$description)) | 
                       grepl(pattern = 'penthouse', tolower(data$description)) | 
                       grepl(pattern = 'luxur', tolower(data$summary)) | 
                       grepl(pattern = 'penthouse', tolower(data$description)))
  return(data)
}

enhanceDistanceToCenter <- function(data, configuration) {
  city_center_coordinates <- c(configuration$parameters$extra_variables$distance_to_center)
  suppressWarnings(
    distance_to_center <- c(distm(city_center_coordinates, data[,c('latitude', 'longitude'), with = F]))
  )
  data$distance_to_center <- distance_to_center
  data$log_distance_to_center <- log(distance_to_center)
  return(data)
  }

enhanceTransit <- function(data, configuration) {
  data$mentions_transit <- !is.na(data$transit)
  return(data)
}

enhancePropertyType <- function(data, configuration) {
  data$property_very_high <- ifelse(data$property_type %in% configuration$parameters$extra_variables$propertyType$very_high, T, F)
  data$property_high      <- ifelse(data$property_type %in% configuration$parameters$extra_variables$propertyType$high, T, F)
  data$property_medium    <- ifelse(data$property_type %in% configuration$parameters$extra_variables$propertyType$medium, T, F)
  data$property_low       <- ifelse(data$property_type %in% configuration$parameters$extra_variables$propertyType$low, T, F)
  data$property_very_low  <- ifelse(data$property_type %in% configuration$parameters$extra_variables$propertyType$very_low, T, F)
  
  return(data)
}

enhanceHostResponseTime <- function(data, configuration) {
  data[is.na(host_response_time),]$host_response_time <- "N/A"
  return(data)
}

enhanceSecurityDeposit <- function(data, configuration) {
  data[is.na(security_deposit),]$security_deposit <- 0.0
  data$high_deposit <- ifelse(data$security_deposit > configuration$parameters$extra_variables$high_deposit$amount, T, F)
  return(data)
}

enhanceCleaningFee <- function(data, configuration) {
  data[is.na(cleaning_fee),]$cleaning_fee <- 0.0
  return(data)
}


enhanceCalendarUpdated <- function(data, configuration) {
  
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

enhanceReviewPeriod <- function(data, configuration) {
  data$review_period <- as.integer(data$last_review - data$first_review)
  return(data)
}

enhanceAmenities <- function(data, configuration) {
  data$amenities <- tolower(data$amenities)
  data$no_of_amenities <- sapply(strsplit(data$amenities, split = ','), length)
  data$has_tv <- sapply(data$amenities, grepl, pattern = 'tv', fixed = TRUE)
  data$has_internet <- sapply(data$amenities, grepl, pattern = 'internet', fixed = TRUE)
  data$has_wifi <- sapply(data$amenities, grepl, pattern = 'wifi', fixed = TRUE)
  data$has_parking <- sapply(data$amenities, grepl, pattern = 'parking', fixed = TRUE)
  data$amenities <- NULL
  return(data)
}

normalizeNumericals <- function(data, configuration) {
  save_stats <- c(mean(data$log_price), sd(data$log_price))
  saveRDS(save_stats, file = 'output/price_stats.RDS')
  for (col in colnames(data)) {
    if (col == 'price') {next}
    else if (is.numeric(data[[col]])) {
      data[[col]] <- (data[[col]] - mean(data[[col]], na.rm = T)) / sd(data[[col]], na.rm = T)
    }
  }
  return(data)
}

imputeValues <- function(data, configuration) {
  data[is.na(beds)]$beds <- median(data$beds, na.rm = T)
  data[is.na(bedrooms)]$bedrooms <- median(data$bedrooms, na.rm = T)
  data[is.na(bathrooms)]$bathrooms <- median(data$bathrooms, na.rm = T)
  
  return(data)
}

createDummyFields <- function(data, configuration) {
  data <- dummy_cols(data, remove_first_dummy = FALSE, remove_selected_columns = TRUE)
  colnames(data) <- gsub(pattern = "[`/.() -]", replacement = "_", colnames(data))
  colnames(data) <- gsub(pattern = "__", replacement = "_", colnames(data))
  colnames(data) <- gsub(pattern = "__", replacement = "_", colnames(data))
  
  
  return(data)
}