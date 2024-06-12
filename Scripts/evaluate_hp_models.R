# Evaluating the models where different hyperparameters have been applied

source("/faststorage/project/ctdna_nn_F2024/Code/eval_model.R")
source("/faststorage/project/ctdna_nn_F2024/Code/Helper functions/cross.entropy.R")
indexes <- readRDS("~/ctdna_nn_F2024/data/indexes2.RData")
one_hot_obs_all <- readRDS("/faststorage/project/ctdna_nn_F2024/data/one_hot_obs_all.RData")
obs_all <- readRDS("/faststorage/project/ctdna_nn_F2024/data/all_obs_indels.RData")
k = 5
df = FALSE

for (lr_i in 4:4){
  for (batch_i in 1:3){
    c = 0 
    l = 0
    ce = c() 
    for (i in 1:k){
      obs <- obs_all[indexes[[i]]]
      pred <- read.csv(paste0("/faststorage/project/ctdna_nn_F2024/ParameterTuning/",lr_i, "_", batch_i, "/output/pred", i, ".csv"))[c("ATCG","D","I")]
      one_hot <- one_hot_obs_all[[i]]
      out <- eval_model_fast(pred, obs, one_hot)
      l = l + out$Loglikelihood
      ce = c(ce, out$Cross_Entropy)
      c = c + length(obs)
    }
    
    if (!is.data.frame(df)){
      df = data.frame(model = paste0(lr_i, "_", batch_i), mean_loglikelihood = l/c, mean_cross_entropy = mean(ce))
    }
    else{
      df = rbind(df, data.frame(model = paste0(lr_i, "_", batch_i), mean_loglikelihood = l/c, mean_cross_entropy = mean(ce)))
    }
  }
}

prev = read.csv("/faststorage/project/ctdna_nn_F2024/data/hyper_param_eval.csv")[c("model","mean_loglikelihood","mean_cross_entropy")]
df = rbind(prev, df)
write.csv(df, "/faststorage/project/ctdna_nn_F2024/data/hyper_param_eval.csv")