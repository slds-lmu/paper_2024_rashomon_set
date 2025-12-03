### Predictive performance of multiple models

## Batchtools structure:
## - Problem: provides, for each (task, learner, model), the true target values,
##   the corresponding predictions, and the task type.
## - Algorithm: takes this object and computes RMSE (regression) or
##   Brier score (binary classification).

source("init.R")

library(batchtools)
library(data.table)
library(mlr3measures)


## Load predictions ------------------------------------------------------------

load("data/results_preds_all_but_TreeFARMS.RData")  # object: `preds`


## Build design: one row per (task, learner, model) ----------------------------

design_perf = rbindlist(
  lapply(names(preds), function(taskname) {
    rbindlist(
      lapply(names(preds[[taskname]]), function(learnername) {
        n_models = length(preds[[taskname]][[learnername]])
        data.table(
          taskname = taskname,
          learnername = learnername,
          model.no = seq_len(n_models)
        )
      })
    )
  })
)


## Experiment registry ---------------------------------------------------------

# writeable = TRUE only once !!!!
# regr = makeExperimentRegistry(
#   file.dir = "/media/external/ewaldf/all_but_TreeFARMS_perf",
#   source   = "init.R"
# )
regr = loadRegistry("/media/external/ewaldf/all_but_TreeFARMS_perf", writeable = TRUE)


## Problem: provide truth, predictions and task info ---------------------------

addProblem("perfdata", fun = function(data, job, taskname, learnername, model.no) {
  # Task and validation split
  task = list.tasks[[taskname]]
  
  if (taskname == "bs") {
    task_data = as.data.frame(task$data())
    task_id = task$id
    task_target = task$target_names
    for (i in seq_along(task_data)) {
      if (is.logical(task_data[[i]])) task_data[[i]] = as.factor(task_data[[i]])
    }
    task = as_task_regr(task_data, target = task_target, id = task_id)
  }
  
  task_valid = generateCanonicalDataSplits(task, ratio = 2 / 3, seed = 1)$validation
  truth = task_valid$data(cols = task_valid$target_names)[[1]]
  
  # Predictions for this (task, learner, model)
  pred_obj = preds[[taskname]][[learnername]][[model.no]]
  
  list(
    task_type = class(task_valid)[1L],
    properties = task_valid$properties,
    positive = if (inherits(task_valid, "TaskClassif")) task_valid$positive else NA_character_,
    truth = truth,
    pred = pred_obj
  )
})


## Algorithm: compute performance metric ---------------------------------------

addAlgorithm("eval_perf", fun = function(data, instance, job) {
  if (identical(instance$task_type, "TaskClassif") && identical(instance$properties, "twoclass")) {
    score.val = bbrier(
      truth = instance$truth,
      prob = instance$pred$prob[, 1],
      positive = instance$positive
    )
    score.type = "brier"
  } else if (identical(instance$task_type, "TaskRegr")) {
    score.val = rmse(
      truth = instance$truth,
      response = instance$pred$response
    )
    score.type = "rmse"
  } else {
    stop("Unsupported task type for performance evaluation.")
  }
  
  list(test.score = score.val, score = score.type)
})


## Add experiments -------------------------------------------------------------

addExperiments(
  prob.designs = list(perfdata = design_perf),
  algo.designs = list(eval_perf = data.table()),
  repls = 1,
  reg = regr
)

load("data/results_preds_all_but_TreeFARMS.RData")
batchExport(list(preds = preds), reg = regr)

## Run batchtools --------------------------------------------------------------

regr$cluster.functions = makeClusterFunctionsSocket(ncpus = 25)

testJob(1)

submitJobs(findErrors())
submitJobs()
waitForJobs()


## Extract and save results ----------------------------------------------------

job_tab = getJobTable(reg = regr)
res_list = reduceResultsList(reg = regr)

# Build a flat data.table with the desired columns
res_dt = rbindlist(lapply(seq_along(res_list), function(i) {
  data.table(
    task = job_tab$prob.pars[[i]]$taskname,
    learner = job_tab$prob.pars[[i]]$learnername,
    model.no = job_tab$prob.pars[[i]]$model.no,
    test.score = res_list[[i]]$test.score,
    score = res_list[[i]]$score
  )
}))

print(res_dt)

save(res_dt, file = "data/results_modelperformances.RData")


