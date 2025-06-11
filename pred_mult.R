### Predictive multiplicity
source("init.R")

library(batchtools)

## Generate Predictions -------------------------------------------------------

# writeable = TRUE only once !!!!
# regr = makeExperimentRegistry(file.dir = "/media/external/ewaldf/all_but_TreeFARMS_preds",
#                               source = "init.R"
# )
regr = loadRegistry("/media/external/ewaldf/all_but_TreeFARMS_preds", writeable = TRUE)

# Define Cluster-Configurations
regr$cluster.functions = makeClusterFunctionsSocket(ncpus = 25)

# Define Problem
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
  
  # Return of the validation split
  generateCanonicalDataSplits(task, ratio = 2 / 3, seed = 1)$validation
})


# Define Algorithm
addAlgorithm("predict", fun = function(data, instance, job, learnername, model.no) {
  # TreeFARMS Models
  name = sprintf("/media/external/rashomon/datafiles/%s/%s/samplemodel_%s_%s_%04d.rds",
                 job$pars$prob.pars$taskname, learnername, learnername, job$pars$prob.pars$taskname, model.no)
  model = readRDS(name)
  
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

# Create benchmark design
load("data/design_all_but_TreeFARMS.RData")

# Experiments
addExperiments(
  prob.designs = list(fromlist = data.table(taskname = design$rn)),
  algo.designs = list(predict = design[,-"rn"]),
  repls = 1,
  combine = "bind"
)


# Run batchtools
testJob(1)

submitJobs(findErrors())
submitJobs()
waitForJobs()


## Extract Results ----------------------------------------------------------

# save results per data set and learner
job_table = getJobTable()
preds = list()


for(i in 1:length(job_table$prob.pars)){
  taskname = job_table$prob.pars[[i]]$taskname
  learnername = job_table$algo.pars[[i]]$learnername
  model.no = job_table$algo.pars[[i]]$model.no
  
  result = reduceResultsList(ids = i)
  
  if(!(taskname %in% names(preds))){
    preds[[taskname]] = list()
    preds[[taskname]][[learnername]] = list()
    preds[[taskname]][[learnername]][[model.no]] = result[[1]]
  } else if(!(learnername %in% names(preds[[taskname]]))) {
    preds[[taskname]][[learnername]] = list()
    preds[[taskname]][[learnername]][[model.no]] = result[[1]]
  } else {
    preds[[taskname]][[learnername]][[model.no]] = result[[1]]
  }
  
  rm(result)
  if (i %% 1000 == 0) print(paste(i, "done"))
}

save(preds, file = paste0("data/results_preds_all_but_TreeFARMS.RData"))

