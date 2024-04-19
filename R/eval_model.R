# Function to evaluate the performance of an indel model
eval_model <- function(prediction, testData){
  logsum = 0
  
  n = c("ATCG", "D", "I")
  N = c("A", "T", "C", "G")
  
  prediction = prediction[n]
  
  # Rows represent predicted values and cols the observed values
  confusion_matrix = data.frame(c(0,0,0),c(0,0,0),c(0,0,0))
  rownames(confusion_matrix) = n
  colnames(confusion_matrix) = n
  
  obs = testData$obs
  col_max <- apply(prediction, 1, which.max)
  
  for (i in 1:length(obs)){
  
    if (col_max[i] == 1){
      logsum = logsum + log(prediction[i,1])
      if (obs[i] %in% N){confusion_matrix[1,1] = confusion_matrix[1,1] +1}
      if (obs[i] == "D"){confusion_matrix[1,2] = confusion_matrix[1,2] +1}
      if (obs[i] == "I"){confusion_matrix[1,3] = confusion_matrix[1,3] +1}
      next
    }
  
    if (col_max[i] == 2){
      logsum = logsum + log(prediction[i,2])
      if (obs[i] %in% N){confusion_matrix[2,1] = confusion_matrix[2,1] +1}
      if (obs[i] == "D"){confusion_matrix[2,2] = confusion_matrix[2,2] +1}
      if (obs[i] == "I") {confusion_matrix[2,3] = confusion_matrix[2,3] +1}
      next
    }
  
    else {
      logsum = logsum + log(prediction[i,3])
      if (obs[i] %in% N){confusion_matrix[3,1] = confusion_matrix[3,1] +1}
      if (obs[i] == "D"){confusion_matrix[3,2] = confusion_matrix[3,2] +1}
      if (obs[i] == "I"){confusion_matrix[3,3] = confusion_matrix[3,3] +1}
    }
  }
  r = sum(confusion_matrix[1,1]+confusion_matrix[2,2]+confusion_matrix[3,3]) / sum(confusion_matrix)
  
  return(c("Confusion matrix" = confusion_matrix, "Loglikelihood" = logsum, "accuracy" = r))
}
