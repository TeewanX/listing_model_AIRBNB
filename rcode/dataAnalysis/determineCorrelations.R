determineCorrelations <- function(data, configuration) {
  corr.matrix <- cor(data, use = 'pairwise.complete.obs', method = 'pearson')
  corr.df     <- data.frame(corr.matrix)
  exportDataFrame(corr.df, configuration$output_data$correlations, 'correlation_matrix_short_list.xlsx')
}