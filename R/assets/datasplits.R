

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
