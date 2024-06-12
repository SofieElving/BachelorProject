# Function that evalutes the different models from the recursive backwards selection (used in other scripts) 
# Takes model number and returns model selection dataframe
model_selection <- function(model_nr){
  
  stopifnot(model_nr %in% c(1:13)) # Not a valid model, Try again

  source("/faststorage/project/ctdna_nn_F2024/Code/Helper functions/cross.entropy.R") # Cross entropy
  library(tidyverse)

# Following is read in script document so we don't have to load every time we run the function:
# indexes <- readRDS("~/ctdna_nn_F2024/data/indexes2.RData")
# one_hot_obs_all <- readRDS("/faststorage/project/ctdna_nn_F2024/data/one_hot_obs_all.RData")
#  CE_all_model <- readRDS("/faststorage/project/ctdna_nn_F2024/data/CE_all_model.RData") # Cross Entropy (CE) for the "all" model
  
  #create output dataframe:
  df <- data.frame(
    model = c(rep(paste0("model_",model_nr),5)), 
    fold_idx = 1:5, 
    n = sapply(1:5, function(x) length(indexes[[x]])),
    median_indiv_loss = rep(0,5),
    mean_indiv_loss = rep(0,5),
    sd_indiv_loss = rep(0,5),
    median_diff_loss = rep(0,5),
    mean_diff_loss = rep(0,5),
    sd_diff_loss = rep(0,5),
    lower_q_diff_loss = rep(0,5),
    upper_q_diff_loss = rep(0,5)
  )
  
  # Read all predictions:
  preds <- vector("list", length = 5)
  for (i in 1:5){
    preds[[i]] <- read.csv(paste0("/faststorage/project/ctdna_nn_F2024/ModelSelection/model_",model_nr, "/output/pred", i, "1.csv"))[c("ATCG","D","I")]
  }
  
  # Calculate CE for feature:
  CE_feature <- vector("list", length = 5)
  for (i in 1:5){
    CE_feature[[i]] <- cross.entropy.vector(one_hot_obs_all[[i]], preds[[i]])
    df[i,"median_indiv_loss"] = median(CE_feature[[i]])
    df[i, "mean_indiv_loss"] = mean(CE_feature[[i]])
    df[i, "sd_indiv_loss"] = sd(CE_feature[[i]])
  }
  
  if (model_nr == 13){
  saveRDS(CE_feature, "/faststorage/project/ctdna_nn_F2024/data/CE_all_model1.RData")
  CE_all_model <- readRDS("/faststorage/project/ctdna_nn_F2024/data/CE_all_model1.RData") # Cross Entropy (CE) for the "all" model
  }
  
  # Calculate values
  for (i in 1:5){
    diff <- CE_feature[[i]] - CE_all_model[[i]]
    summ <- summary(diff)
    df[i,"median_diff_loss"] = summ["Median"]
    df[i, "mean_diff_loss"] = summ["Mean"]
    df[i, "sd_diff_loss"] = sd(diff)
    df[i, "lower_q_diff_loss"] = summ["1st Qu."]
    df[i, "upper_q_diff_loss"] = summ["3rd Qu."]
  }
  
  return(df)
}