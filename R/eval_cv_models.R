library(tidyverse)
source("/faststorage/project/ctdna_nn_F2024/Code/eval_model.R")

indexes <- readRDS("~/ctdna_nn_F2024/data/indexes.RData")
data <- read_csv("~/ctdna_nn_F2024/data/training_data_indels.csv",
                 name_repair = function(x) gsub("data\\.", "", x))
         
#####
pred_paths <-
  c(
    "~/ctdna_nn_F2024/ModelsCV/5layer2xOpt/pred1.csv",
    "~/ctdna_nn_F2024/ModelsCV/5layer2xOpt/pred2.csv",
    "~/ctdna_nn_F2024/ModelsCV/5layer2xOpt/pred3.csv",
    "~/ctdna_nn_F2024/ModelsCV/5layer2xOpt/pred4.csv",
    "~/ctdna_nn_F2024/ModelsCV/5layer2xOpt/pred5.csv"
  )
  
#####
k = length(pred_paths)
c = 0 
l = 0       
for (i in 1:k) {
  data_test <- data[indexes[[i]],]
  print("evaluating new model")
  pred <- read.csv(pred_paths[i])
  out <- eval_model(pred, data_test)
  print("evaluation complete:")
  print(out)
  l = l + out$Loglikelihood
  c = c + length(data_test$obs)
}


print("all models evaluated")
print("total loglikelihood:")
print(l)
print("relative loglikelihood:")
print(l/c)