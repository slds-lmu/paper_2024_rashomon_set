
task.bh <- tsk("boston_housing")
task.bh$select(selector_invert(selector_cardinality_greater_than(2))(task.bh))

task.gc <- tsk("german_credit")

task.cs <- tsk("compas")
task.cs$select(selector_invert(selector_name("is_recid"))(task.cs))

task.bs <- tsk("bike_sharing")
task.bs$select(selector_invert(selector_type("character"))(task.bs))

sampleSyntheticTask <- function(n, seed = 1) {
  assertInt(n)
  assertInt(seed)
  old.seed <- get0(".Random.seed", .GlobalEnv, ifnotfound = NULL)
  on.exit({
    if (is.null(old.seed)) {
      rm(".Random.seed", envir = .GlobalEnv)
    } else {
      assign(".Random.seed", old.seed, envir = .GlobalEnv)
    }
  })
  if (utils::compareVersion(sprintf("%s.%s", R.version$major, R.version$minor), "4.3.0") < 0) {
    stop("R version 4.3.0 or higher is required.")
  }
  RNGversion("4.3.0")

  set.seed(seed)

  # the following inits the columns X1, X3, and X5 directly, and inits
  # the noise variables e2, e4, and eY.
  # We create the matrix 'byrow', so if we get call sample_synthetic_task(n = 2), the first
  # row will have the same values as you get with sample_synthetic_task(n = 1).
  data <- as.data.table(matrix(rnorm(n * 6), ncol = 6, byrow = TRUE))
  colnames(data) <- c(paste0("X.", 1:5), "Y")

  data[, X.2 := X.1 + X.2 * 0.001]
  data[, X.4 := X.3 + X.4 * 0.1]
  data[, Y := X.4 + X.5 + X.4 * X.5 + Y * 0.1]

  as_task_regr(data, target = "Y", id = "synthetic_task")
}

task.st <- sampleSyntheticTask(10000)

data.car <- readRDS("data/data_car.rds")
task.car <- as_task_classif(data.car, target = "class_1", id = "car")

data.monk2 <- readRDS("data/data_monk2.rds")
task.monk2 <- as_task_classif(data.monk2, target = "class_1", id = "monk2")

data.breast.cancer <- tsk("breast_cancer")$data()[, .(
  cl_thickness_10 = as.integer(cl_thickness == "10"),
  cell_size_1 = as.integer(cell_size == "1"),
  cell_size_10 = as.integer(cell_size == "10"),
  cell_shape_1 = as.integer(cell_shape == "1"),
  marg_adhesion_1 = as.integer(marg_adhesion == "1"),
  epith_c_size_2 = as.integer(epith_c_size == "2"),
  bare_nuclei_10 = as.integer(bare_nuclei == "10"),
  normal_nucleoli_1 = as.integer(normal_nucleoli == "1"),
  normal_nucleoli_10 = as.integer(normal_nucleoli == "10"),
  class
  )
]
task.breast.cancer <- as_task_classif(data.breast.cancer, target = "class", id = "breast_cancer")

data.compas.binarized <- readRDS("data/data_compas-binarized.rds")
setnames(data.compas.binarized, make.names(colnames(data.compas.binarized)))
task.compas.binarized <- as_task_classif(data.compas.binarized,
  target = "recidivate.within.two.years.1", id = "compas_binarized"
)

data.fico <- readRDS("data/data_fico.rds")
setnames(data.fico, make.names(colnames(data.fico)))
task.fico <- as_task_classif(data.fico, target = "PoorRiskPerformance", id = "fico")

data.fico.binarized <- readRDS("data/data_fico-binarized.rds")
setnames(data.fico.binarized, make.names(colnames(data.fico.binarized)))
task.fico.binarized <- as_task_classif(data.fico.binarized,
  target = "RiskPerform", id = "fico_binarized"
)

