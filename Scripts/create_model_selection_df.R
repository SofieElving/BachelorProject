# sript for creating dataframe to use in model selection plot

# setup
##########
source("/faststorage/project/ctdna_nn_F2024/Code/Helper functions/model_selection.R") # model selection function
indexes <- readRDS("~/ctdna_nn_F2024/data/indexes2.RData")
one_hot_obs_all <- readRDS("/faststorage/project/ctdna_nn_F2024/data/one_hot_obs_all.RData")
#CE_all_model <- readRDS("/faststorage/project/ctdna_nn_F2024/data/CE_all_model.RData") # Cross Entropy (CE) for the "all" model
##########


# Begin feature importance calculation:
##########
models <- c(13:1)

model_selection_df = FALSE
for (model_nr in models){
  
  if (!is.data.frame(model_selection_df)){
    model_selection_df <- model_selection(model_nr)
    CE_all_model <- readRDS("/faststorage/project/ctdna_nn_F2024/data/CE_all_model.RData") # Cross Entropy (CE) for the "all" model
  }
  
  else{
  
  df_new <- model_selection(model_nr)
  model_selection_df <- rbind(model_selection_df, df_new)
  }
  
}

write.csv(model_selection_df, "/faststorage/project/ctdna_nn_F2024/data/model_selection.csv")
##########