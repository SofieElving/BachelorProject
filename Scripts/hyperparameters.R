# Original parameters fron DREAMS:

learning_rate_list = c(0.01)
batch_size_list = c(32000)
min_delta_list = c(0.001)
patience_list = c(10)

parameter_grid = list(learning_rate_list, batch_size_list, min_delta_list, patience_list)

saveRDS(parameter_grid, "/faststorage/project/ctdna_nn_F2024/ALT/data/RData/parameters.RData")

# Create parameter grid for hyperparameter tuning: (OBS! only add in the end of list even if values are smaller)
learning_rates = c(0.005, 0.01, 0.02, 0.001)
batch_sizes = c(16000, 32000, 64000)

parameter_grid = list(learning_rates, batch_sizes)

saveRDS(parameter_grid, "~/ctdna_nn_F2024/data/hyper_parameter_grid.RData"
)