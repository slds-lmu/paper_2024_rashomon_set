


stop("This file should be run manually")

## If renv is already present:
renv::restore()
# and restart R

## The following is only necessary if no renv.lock is present
renv::init(bare = TRUE)

## restart R session

renv::settings$snapshot.type("all")
renv::install(c(
  "data.table", "RhpcBLASctl", "mlr3", "ggplot2", "DiceKriging", "batchtools",
  "mlr3pipelines", "mlr3learners", "mlr3mbo", "xgboost", "rpart", "R6", "checkmate", "testthat", "rgenoud",
  "RhpcBLASctl", "ranger",
  "paradox",
  "devtools",
  "mlr3fairness",
  "languageserver",
  "mlr3data",
  "glmnet",
  "snow",  # batchtools socket parallelization
  "ggplot2",
  "iml",
  "tidyr",
  "GGally",
  "patchwork"
))

renv::snapshot()
