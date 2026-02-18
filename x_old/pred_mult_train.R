### Predictive performance of multiple models

## Batchtools structure:
## - Problem: provides, for each (task, learner, model), the true target values,
##   the corresponding predictions, and the task type.
## - Algorithm: takes this object and computes RMSE (regression) or
##   Brier score (binary classification).

source("init.R")

library(batchtools)
library(data.table)


## Experiment registry ---------------------------------------------------------

# writeable = TRUE only once !!!!
# regr = makeExperimentRegistry(
#   file.dir = "/media/external/ewaldf/all_train_preds",
#   source   = "init.R"
# )
regr = loadRegistry("/media/external/ewaldf/all_train_preds", writeable = TRUE)


## Problem: provide truth, predictions and task info ---------------------------

addProblem("fromlist", fun = function(data, job, taskname) {
  task = list.tasks[[taskname]]
  
  # Fix logical features (for FeatureImp)
  if(taskname == "bs"){
    task_data = as.data.frame(task$data())
    task_id = task$id
    task_target = task$target_names
    for(i in seq_along(task_data)) {
      if (is.logical(task_data[[i]])) task_data[[i]] = as.factor(task_data[[i]])
    }
    task = as_task_regr(task_data, target = task_target, id = task_id)
  }
  
  # Return of the training split
  generateCanonicalDataSplits(task, ratio = 2 / 3, seed = 1)$training
})


## Algorithm: compute performance metric ---------------------------------------

addAlgorithm("predict", fun = function(data, instance, job, learnername, model.no, rds, model.id, RS) {
  # TreeFARMS Models
  if(learnername == "TreeFARMS"){
    model <- readRDS(sprintf("/media/external/rashomon/datafiles/treefarms/treefarms_%s.rds", RS))
    try(model$modelcontainer)
    model$param_set$values$selected_tree <- as.character(model.id)
  } else {
    name = sprintf("/media/external/rashomon/rashomon_models/%s/%s/%s",
                   learnername, job$pars$prob.pars$taskname, rds)
    model = readRDS(name)
  }
  
  # Fix models in case of task bs (logical features)
  if(job$pars$prob.pars$taskname == "bs"){
    # fix model
    holiday.special = ppl("convert_types", "factor", "logical", selector_name(c("holiday", "working_day")), id = "holiday.special")
    invisible(holiday.special$train(instance)) # list.tasks$bs))
    xstate = model$state
    gr = holiday.special$clone(deep = TRUE) %>>% model$clone(deep = TRUE)
    lr = as_learner(gr$clone(deep = TRUE))
    lr$state = xstate
    lr$state$train_task = instance$clone(deep = TRUE)$filter(0)
    lr$model = gr$state
    lr$model[[gr$ids()[[2]]]] = xstate
    model = lr
    rm(gr, lr, xstate, holiday.special)
  }
  
  # Predict
  X = instance$data(cols = instance$feature_names)
  model$predict_newdata(X)
})

## Create design ---------------------------------------------------------------
load("data/design_TreeFARMS.RData")
design_TreeFARMS = design

load("data/design_all_but_TreeFARMS.RData")
design_new = merge(design, design_TreeFARMS, all = TRUE)

## Add experiments -------------------------------------------------------------

addExperiments(
  prob.designs = list(fromlist = data.table(taskname = design_new$rn)),
  algo.designs = list(predict = design_new[,-"rn"]),
  repls = 1,
  reg = regr,
  combine = "bind"
)

## Run batchtools --------------------------------------------------------------

regr$cluster.functions = makeClusterFunctionsSocket(ncpus = 10)

testJob(1)

submitJobs(findErrors())
submitJobs()
waitForJobs()

findErrors() |> getJobTable() -> jt
submitJobs(ids = jt$job.id)



## Extract and save results ----------------------------------------------------

job_table = getJobTable()
job_table = job_table[is.na(error)]

preds_train = list()


for(i in job_table$job.id){
  taskname = job_table[job.id == i]$prob.pars[[1]]$taskname
  learnername = job_table[job.id == i]$algo.pars[[1]]$learnername
  if(learnername =="TreeFARMS"){
    model.id = job_table[job.id == i]$algo.pars[[1]]$model.id
  } else {
    model.no = job_table[job.id == i]$algo.pars[[1]]$model.no
  }
  
  
  result = reduceResultsList(ids = i)
  
  if(!(taskname %in% names(preds_train))){
    preds_train[[taskname]] = list()
    preds_train[[taskname]][[learnername]] = list()
    if(learnername =="TreeFARMS"){
      preds_train[[taskname]][[learnername]][[model.id]] = result[[1]]
    } else {
      preds_train[[taskname]][[learnername]][[model.no]] = result[[1]]
    }
  } else if(!(learnername %in% names(preds_train[[taskname]]))) {
    preds_train[[taskname]][[learnername]] = list()
    if(learnername =="TreeFARMS"){
      preds_train[[taskname]][[learnername]][[model.id]] = result[[1]]
    } else {
      preds_train[[taskname]][[learnername]][[model.no]] = result[[1]]
    }
  } else {
    if(learnername =="TreeFARMS"){
      preds_train[[taskname]][[learnername]][[model.id]] = result[[1]]
    } else {
      preds_train[[taskname]][[learnername]][[model.no]] = result[[1]]
    }
  }
  
  rm(result)
  if (i %% 1000 == 0) print(paste("id", i, "done"))
}

save(preds_train, file = "data/results_train_preds.RData")


