# sript for creating dataframe to use in feature selection plot

# setup
##########
source("/faststorage/project/ctdna_nn_F2024/Code/Helper functions/feature_importance.R") # feature importance function
indexes <- readRDS("~/ctdna_nn_F2024/data/indexes2.RData")
one_hot_obs_all <- readRDS("/faststorage/project/ctdna_nn_F2024/data/one_hot_obs_all.RData")
#CE_all <- readRDS("/faststorage/project/ctdna_nn_F2024/data/CE_all.RData") # Cross Entropy (CE) for the "all" model
##########

# create CE_all1
#########
source("/faststorage/project/ctdna_nn_F2024/Code/Helper functions/cross.entropy.R") # Cross entropy
  # Read all predictions:
preds <- vector("list", length = 5)
for (i in 1:5){
    preds[[i]] <- read.csv(paste0("/faststorage/project/ctdna_nn_F2024/ModelsFeatures/all/pred", i, "1.csv"))[c("ATCG","D","I")]
  }
  
  # Calculate CE for feature:
  CE_all <- vector("list", length = 5)
  for (i in 1:5){
    CE_all[[i]] <- cross.entropy.vector(one_hot_obs_all[[i]], preds[[i]])
  }

saveRDS(CE_all, "/faststorage/project/ctdna_nn_F2024/data/CE_all1.RData")
#########

# Begin feature importance calculation:
##########
features <- c(
  "umiErrors", "umiCount", "triCtx2", "strand", "seqLength", "readIndex", "localGC", 
  "localComp2", "localComp1", "insertionsRead", "fragmentSize", "firstInPair", "deletionsRead"
)

feature_importance_df = FALSE
for (feature in features){
  
  if (!is.data.frame(feature_importance_df)){
    feature_importance_df <- feature_importance(feature)
    
  }
  
  else{
  
  df_new <- feature_importance(feature)
  feature_importance_df <- rbind(feature_importance_df, df_new)
  }
  
}

write.csv(feature_importance_df, "/faststorage/project/ctdna_nn_F2024/data/feature_importance_1.csv")
##########