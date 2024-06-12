# function for predicting error rates using the trained NNs
predict_error_rates_indels <- function(read_positions_df,
                                       model) {
  # Predict error rates for read positions from trained DREAM model
  if (nrow(read_positions_df) == 0) {
    predictions_df <-
      data.frame(
        ATCG = numeric(),
        D = numeric(),
        I = numeric()
      )
  } else {
    predictions_df <- model %>%
      predict(read_positions_df) %>%
      data.frame() %>%
      rename(
        ATCG = X1,  # Assuming the first column represents the combined probabilities for A, T, C, G
        D = X2,
        I = X3
      )
  }
  
  
  return(predictions_df)
}