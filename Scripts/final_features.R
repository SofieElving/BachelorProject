# Save the final feature list from LOCO and greedy selection:

features = c("read_index", "strand", "trinucleotide_ctx", "local_complexity_2",
             "seq_length", "fragment_size", "local_GC")

saveRDS(features, "~/ctdna_nn_F2024/data/final_feature_list.RData")