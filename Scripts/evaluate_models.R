# script to evaluate the performance of models (using different architectures)

# Setup:
source("/faststorage/project/ctdna_nn_F2024/Code/eval_model.R")
source("/faststorage/project/ctdna_nn_F2024/Code/Helper functions/cross.entropy.R")
indexes <- readRDS("/faststorage/project/ctdna_nn_F2024/data/indexes2.RData")
one_hot_obs_all <- readRDS("/faststorage/project/ctdna_nn_F2024/data/one_hot_obs_all.RData")
obs_all <- readRDS("/faststorage/project/ctdna_nn_F2024/data/all_obs_indels.RData")

df = read.csv("/faststorage/project/ctdna_nn_F2024/ALT/data/models_eval.csv")[c("mean_cross_entropy", "model")]  # read old

# models aready tested:
# "initial", "initx2", "initx4", "initx8", "4layer_cone", "3layer_cylinder", "4layer_cylinder", "6layer_dimond", "5layer_point"
# "4layer_cone", "3layer_cylinder", "6layer_cone"

models = c("5layer_cylinder")

for (model in models) {
    k = 5
    c = 0 
    l = 0
    ce = c() 
    for (i in 1:k){
      obs <- obs_all[indexes[[i]]]
      pred <- read.csv(paste0("/faststorage/project/ctdna_nn_F2024/ALT/gwf/models/", model, "/output/pred", i, ".csv"))[c("ATCG","D","I")]
      one_hot <- one_hot_obs_all[[i]]
      ce = c(ce, cross.entropy.vector(one_hot, pred))
    }
    
    if (!is.data.frame(df)){
      df = data.frame(mean_cross_entropy = mean(ce), model = paste0(model))
      write.csv(df, "/faststorage/project/ctdna_nn_F2024/ALT/data/models_eval.csv")
    } else {
      df = read.csv("/faststorage/project/ctdna_nn_F2024/ALT/data/models_eval.csv")[c("mean_cross_entropy", "model")]  # read old
      df = rbind(df, data.frame(mean_cross_entropy = mean(ce), model = paste0(model))) # append new row
      write.csv(df, "/faststorage/project/ctdna_nn_F2024/ALT/data/models_eval.csv") # save data frame
    }
}
write.csv(df, "/faststorage/project/ctdna_nn_F2024/ALT/data/models_eval.csv")