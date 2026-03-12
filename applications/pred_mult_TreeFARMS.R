### Measure predictive multiplicity
source("../init.R")

library(CVXR)
library(batchtools)

## Generate Predictions -------------------------------------------------------

# writeable = TRUE only once !!!!
# regr = makeExperimentRegistry(file.dir = "/media/external/ewaldf/TreeFARMS_preds",
#                               source = "init.R", packages = "iml"
# )
regr = loadRegistry("/media/external/ewaldf/TreeFARMS_preds", writeable = TRUE)

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
addAlgorithm("predict", fun = function(data, instance, job, learnername, model.id, RS) {
  # TreeFARMS Models
  model <- readRDS(sprintf("/media/external/rashomon/datafiles/treefarms/treefarms_%s.rds", RS))
  try(model$modelcontainer)
  model$param_set$values$selected_tree <- as.character(model.id)

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

# Create benchmark design by sampling models from TreeFARMS
set.seed(12)
treefarms.info <- fread("/media/external/rashomon/datafiles/treefarms/treefarms_info.csv")
model.ids <- treefarms.info[offset == 0.05 & balance == FALSE & successful == TRUE & use.adder == FALSE, job.id]
design <- data.table(rn=character(), learnername=character(),
                     model.id=character(), RS = integer())
for(model.id in model.ids){
  model <- readRDS(sprintf("/media/external/rashomon/datafiles/treefarms/treefarms_%s.rds", model.id))
  try(model$modelcontainer)
  model.nos <- replicate(1200, model$sampleTreeIndex())
  design_tmp <- data.table(rn = treefarms.info[job.id == model.id, taskname],
                           learnername = "TreeFARMS",
                           model.id = model.nos,
                           RS = model.id)
  design <- rbind(design, design_tmp)
}


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
preds_TreeFARMS = list()


for(i in unique(design$rn)){
  learnername = "TreeFARMS"
  id = which(unique(design$rn) == i)
  ids = job_table$job.id[((id-1)*1200+1):(id*1200)]
  ids = ids[is.na(job_table$error[job_table$job.id %in% ids])]
  if(length(ids) >= 1000){
    ids = ids[1:1000]
  } else {
    cat(paste("Only", length(ids), "models for data set", i))
  }
  preds_TreeFARMS[[i]] = reduceResultsList(ids = ids)
}

save(preds_TreeFARMS, file = paste0("../data/results_preds_TreeFARMS.RData"))

