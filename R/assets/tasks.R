
task.bh <- tsk("boston_housing")
task.bh$select(selector_invert(selector_cardinality_greater_than(2))(task.bh))

task.gc <- tsk("german_credit")

task.cs <- tsk("compas")
task.cs$select(selector_invert(selector_name("is_recid"))(task.cs))

task.bs <- tsk("bike_sharing")
task.bs$select(selector_invert(selector_type("character"))(task.bs))


sample_synthetic_task <- function(n, seed = 1) {
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

  data[, X.2 := X.1 + X.2 * .001]
  data[, X.4 := X.3 + X.4 * .1]
  data[, Y := X.4 + X.5 + X.4 * X.5 + Y * .1]

  as_task_regr(data, target = "Y", id = "synthetic_task")
}

task.st <- sample_synthetic_task(10000)


