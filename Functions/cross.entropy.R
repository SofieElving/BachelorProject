#Calculates cross entropy for true vector, p (1-hot), and predicted values, phat
cross.entropy <- function(p, phat){
  phat <- pmax(pmin(phat, 1 - 1e-15), 1e-15) # ensures no zero values!
  x <- sum(p * log(phat))
  return(-x)
}

# Applies cross.entropy to the truth list of vectors and the dataframe of preds
cross.entropy.vector <- function(p_, phat_){
  n <-  length(phat_[,1])
  x <- vector(length = n)
  for (i in 1:n){
    x[i] = cross.entropy(p_[[i]], phat_[i,])
  }
  return(x)
}