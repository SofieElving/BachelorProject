# embeds observation vector (ie data$obs = "D","D","I","A",...,"G") to list of one hot encoded vectors
# object used in various scipts to avoid redundency 
embed_obs <- function(obs){
  n <- length(obs)
  x <- vector("list", length = n)
  for (i in 1:n){
    v <- c(0,0,0)
    if (obs[i] == "D"){v[2]=1;x[[i]]=v;next}
    if (obs[i] == "I"){v[3]=1;x[[i]]=v;next}
    else{v[1]=1;x[[i]]=v;next}
  }
  return(x)
}

#OLD function
# embeds observation vector (ie data$obs = "D","D","I","A",...,"G") to one hot encoded vectors as rows in df 
#embed_obs <- function(obs){
#  n <-  length(obs)
#  df <- as.data.frame(matrix(0, nrow = n, ncol = 3))
#  names(df) <- c("ATCG", "D", "I")
#  for (i in 1:n){
#    if (obs[i] %in% c("A","T","C","G")){df[i,1]=1; next}
#    if (obs[i] == "D"){df[i,2]=1; next}
#    else{df[i,1]=1}
#  }
#  return(df)
#}