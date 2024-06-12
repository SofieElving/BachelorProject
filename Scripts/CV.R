# Script for making indexes for CV-folds separated on Donor.

library(tidyverse)
library(caret)

# Read data
data <- read_csv("~/ctdna_nn_F2024/data/training_data_indels2.csv", name_repair = function(x) gsub("data\\.", "", x))

df <- c()
df$data <- data[1:21]
df$info <- data[22:25]


# Regular expression pattern to match everything before the first underscore to make Donors list
pattern <- '([^_]*)_.+'
Donors <- sapply(df$data$qname, function(x) sub(pattern, '\\1', x))

# Find unique patients
unique_patients <- unique(Donors)

# Shuffle the order of unique patients
set.seed(123) # for reproducibility
unique_patients <- sample(unique_patients)

# Split the unique patients into k folds
k <- 5
folds <- cut(seq(1, length(unique_patients)), breaks = k, labels = FALSE)

# Create a list to store fold assignments
fold_assignments <- vector("list", length = k)

# Assign patients to folds
for (i in 1:length(unique_patients)) {
  fold_assignments[[folds[i]]] <- c(fold_assignments[[folds[i]]], unique_patients[i])
}

# Create a list to store fold indices
fold_indices <- vector("list", length = k)

# Assign observations to folds based on patient assignments
for (i in 1:k) {
  patients_in_fold <- fold_assignments[[i]]
  fold_indices[[i]] <- which(Donors %in% patients_in_fold)
}

# Save as RData
saveRDS(fold_indices, file = "~/ctdna_nn_F2024/data/indexes2.RData")