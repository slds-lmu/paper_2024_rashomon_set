

# generate splits to be used as input by batchtools

# split data batch tools problem.
# should be inited with 'seed'
# splits data into train and validation data using the `resamplingiter` iteration of `resampling`
# - data: mlr3 `Task`
# - job: batchtools job (ignored)
# - resampling: mlr3 `Resampling`, not instantiated
# - resamplingiter: scalar integer
# return: named list
# - `training`: training data Task
# - `validation`: validation data Task
splitDataBTP <- function(data, job, resampling, resamplingiter) {
  assertClass(data, "Task")
  assertChoice(resampling, names(list.resampling.tuning.outer))
  resampling <- list.resampling.tuning.outer[[resampling]]
  assertInt(resamplingiter, lower = 1, upper = resampling$iters)
  resampling <- resampling$clone(deep = TRUE)$instantiate(data)
  list(
    training = data$clone(deep = TRUE)$filter(rows = resampling$train_set(resamplingiter)),
    validation = data$clone(deep = TRUE)$filter(rows = resampling$test_set(resamplingiter))
  )
}

getResamplingIterTable <- function(resampling) {
  data.table(resampling = resampling, resamplingiter = seq_len(list.resampling.tuning.outer[[resampling]]$iters))
}


generateCanonicalDataSplits <- function(task, ratio = 2 / 3, seed = 1) {
  # not using mlr3 resampling here, because we don't trust it to make the same splits in every situation
  assertClass(task, "Task")
  assertNumber(ratio, lower = 0, upper = 1)
  seed <- assertInt(seed, coerce = TRUE)
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
  n <- task$nrow
  train.set <- sample.int(n, size = min(n - 1, max(1, round(n * ratio))))
  test.set <- setdiff(seq_len(n), train.set)
  list(
    training = task$clone(deep = TRUE)$filter(rows = train.set),
    validation = task$clone(deep = TRUE)$filter(rows = test.set)
  )
}
