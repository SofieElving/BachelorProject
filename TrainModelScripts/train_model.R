# Example of a script for training a model
###############################
#         v CHANGE v          # ########################################################
###############################

model_id = 1

lr_id = 1; batch_id = 1; min_delta_id = 1; patience_id = 1 

user = 1 # set user:  1 = Marcus, 0 = Sofie
seed_value = 123

###############################
#       v DON'T TOUCH v       # ########################################################
###############################

# User:
if (user){
    reticulate::use_condaenv("Dreams_env", required = TRUE)
  } else {
    reticulate::use_condaenv("/home/sofieelving/miniforge3/envs/dreams_env",required=TRUE)
  }

# load libraries:
library(tidyverse); library(dreams); library(tensorflow)

k = 5 # number of k-fold cross validation

# set seeds:
set.seed(seed_value)
tensorflow::set_random_seed(seed_value)

# READ DATA AND INDEXES:
data <- read_csv("~/ctdna_nn_F2024/data/training_data_indels2.csv",
                 name_repair = function(x) gsub("data\\.", "", x))
indexes <- readRDS("~/ctdna_nn_F2024/data/indexes2.RData")

model_architecures = readRDS("/faststorage/project/ctdna_nn_F2024/ALT/data/RData/model_architecures.RData")
model_name = model_architecures[[model_id]][1]
model_layers = model_architecures[[model_id]][-1]

# Parameters for search:
parameters <-  readRDS("/faststorage/project/ctdna_nn_F2024/ALT/data/RData/parameters.RData") 

# Features chosen through LOCO and greedy model selection:
# features <- readRDS("~/ctdna_nn_F2024/data/final_feature_list.RData") 
features <- c("read_index", "strand", "trinucleotide_ctx", "first_in_pair", 
              "umi_count", "seq_length", "fragment_size", "local_GC")


# Initialize output paths
model_paths <- c()
pred_paths  <- c()
log_paths   <- c()

for (i in 1:k){
  model_paths[i] = paste0("/faststorage/project/ctdna_nn_F2024/ALT/gwf/models/", model_name,"/output/model", i, ".hdf5")
  pred_paths[i]  = paste0("/faststorage/project/ctdna_nn_F2024/ALT/gwf/models/", model_name,"/output/pred", i, ".csv")
  log_paths[i]   = paste0("/faststorage/project/ctdna_nn_F2024/ALT/gwf/models/", model_name,"/output/log", i, ".csv")
}

# PREDICTION FUNCTION:
source("/faststorage/project/ctdna_nn_F2024/Code/Helper functions/predict_error_rates_indels.R")
########################################################################################


###############################
#  TRAINING FOR EACH K-FOLD   # ########################################################
###############################

for (i in 1:k) {
  # Get training and test data
  data_train <- data[-indexes[[i]],]
  data_test  <- data[indexes[[i]],]
  
  df_train <- c(); df_test <- c()
  df_train$data <- data_train[1:21]; df_test$data <- data_test[1:21]
  df_train$info <- data_train[22:25]; df_test$info <- data_test[22:25]
  df_train$data$first_in_pair <- as.integer(df_train$data$first_in_pair)
  df_test$data$first_in_pair <- as.integer(df_test$data$first_in_pair)
  

################# EALY STOPPING #############################
#############################################################
# Split training data into training and validation data used for early stopping SNV
  split_ratio = 0.8

  df_train$data <- df_train$data %>%
    mutate(derived_patient_id = sub("_.*", "", !!sym('qname')))

# Extract unique patient identifiers
  patient_ids_indel <- unique(df_train$data$derived_patient_id)

# Sample a subset of patient ids for training
  train_patient_ids_indel <- sample(patient_ids_indel, size = length(patient_ids_indel) * split_ratio)

# Split the data into training and validation sets
  train_data_indel <- list(
    data = df_train$data[df_train$data$derived_patient_id %in% train_patient_ids_indel, ],
    info = df_train$info
  )

  validation_data_indel <- list(
    data = df_train$data[!df_train$data$derived_patient_id %in% train_patient_ids_indel, ],
    info = df_train$info
  )
################# EALY STOPPING #############################
#############################################################
  
  
  # Run model on training data  
  model <- train_dreams_model_indels(
    train_data_indel,
    validation_data_indel,
    layers = model_layers,
    model_features = features,       
    lr = parameters[[1]][lr_id],
    batch_size = parameters[[2]][batch_id], 
    epochs = 750,                                                                             
    model_file_path = model_paths[i],
    log_file_path = log_paths[i],
    min_delta = parameters[[3]][min_delta_id],
    patience = parameters[[4]][patience_id])
  
  
  # Run prediction on test data
  pred_error <- predict_error_rates_indels(df_test$data, model)
  
  write.csv(pred_error, file = pred_paths[i])
}
########################################################################################