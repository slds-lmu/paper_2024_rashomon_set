# more detail: https://arxiv.org/pdf/2209.08040

# "car evaluation" dataset from UCI
data.car <- fread("https://github.com/ubc-systopia/treeFarms/raw/refs/heads/main/experiments/datasets/car_evaluation/data.csv")
saveRDS(data.car, "data/data_car.rds")

# Monk 2
data.monk2 <- fread("https://github.com/ubc-systopia/treeFarms/raw/refs/heads/main/experiments/datasets/monk_2/data.csv")
saveRDS(data.monk2, "data/data_monk2.rds")

# Penguin
## not available

# Breast cancer
## not available

# Compas (binarized)
data.compas.binarized <- fread("https://github.com/ubc-systopia/treeFarms/raw/refs/heads/main/experiments/datasets/compas/binned.csv")
saveRDS(data.compas.binarized, "data/data_compas-binarized.rds")

# FICO
data.fico <- fread("https://github.com/ubc-systopia/treeFarms/raw/refs/heads/main/experiments/datasets/fico/original.csv")
saveRDS(data.fico, "data/data_fico.rds")

# FICO (binarized)
data.fico.binarized <- fread("https://github.com/ubc-systopia/treeFarms/raw/refs/heads/main/experiments/datasets/fico/fico-binary.csv")
saveRDS(data.fico.binarized, "data/data_fico-binarized.rds")
