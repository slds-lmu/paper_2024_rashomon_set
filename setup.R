


stop("This file should be run manually")

## If renv is already present:
renv::restore()

# restart R session
reticulate::virtualenv_create("rashomon")

renv::use_python(reticulate::virtualenv_python("rashomon"))
# restart R

renv::restore()  # should install python packages

# and restart R

#################

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
  "reticulate",
  "jsonlite"
))

reticulate::virtualenv_create("rashomon")

renv::use_python(reticulate::virtualenv_python("rashomon"))

reticulate::py_install("scikit-learn<1.6")
reticulate::py_install("treefarms")
reticulate::py_install("gosdt")

renv::snapshot()
