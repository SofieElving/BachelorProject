# list of names and layers of different models

# Initial search:
model_architecures = list(
  c("initial",          c(128, 64, 32)),             # 1,  original model architecture
  c("initx2",           c(256, 128, 64)),            # 2
  c("initx4",           c(512, 256, 128)),           # 3
  c("initx8",           c(1024, 512, 256)),          # 4
  c("4layer_cone",      c(256, 128, 64, 32)),        # 5
  c("3layer_cylinder",  c(128, 128, 128)),           # 6
  c("4layer_cylinder",  c(128, 128, 128, 128)),      # 7
  c("6layer_dimond",    c(32, 64, 128, 64, 32)),     # 8
  c("5layer_point",     c(128, 64, 64, 64, 32)),     # 9
  c("5layer_cone",      c(512, 256, 128, 64, 32)),   # 10
  c("6layer_cone",      c(1024, 512, 256, 128, 64, 32)), # 11
  c("5layer_cylinder",  c(128, 128, 128, 128, 128))  # 12
)

saveRDS(model_architecures, "/faststorage/project/ctdna_nn_F2024/ALT/data/RData/model_architecures.RData")