determineCorrelations <- function(data, configuration) {
  
  #determining the correlation based on the Pearson correlation and 
  # pairwise complete observations
  corr.matrix <- cor(data, use = 'pairwise.complete.obs', method = 'pearson')
  corr.df     <- data.frame(corr.matrix)
  
  #Saving the correlation matrix
  exportDataFrame(corr.df, configuration$output_data$correlations, 'correlation_matrix_short_list.xlsx')
}