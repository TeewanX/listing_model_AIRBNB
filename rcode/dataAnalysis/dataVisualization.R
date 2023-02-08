plotHistogram <- function(data, variable_name, configuration) {
  
  #creating histogram for the data set
  hist_data <- suppressWarnings(hist(data, plot = F, bins = 25))
  hist_data <- data.frame(hist_data[c('mids', 'counts')])
  
  #plotting the histogram
  p <- ggplot(hist_data) +
    theme_minimal() +
    theme(text = element_text(size = 20)) +
    geom_col(aes(x = mids, y = counts), fill = 'salmon') +
    labs(x = variable_name,
         y = 'Count',
         title = paste0('Histogram of ', variable_name))
  plot(p)
  file_name <- paste0('output/data_analysis/histogram_', variable_name,".png")
  ggsave(filename = file_name, plot = p, height = 10, width = 15, bg = 'transparent')
}