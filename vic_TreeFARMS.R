# source("prepare_tasks.R")
source("init.R")

library(batchtools)
library(ggplot2)
library(tidyr)
library(data.table)
library(iml)


# writeable = TRUE only once !!!!
# regr = makeExperimentRegistry(file.dir = "/media/external/ewaldf/TreeFARMS",
#                               source = "init.R", packages = "iml"
# )
regr = loadRegistry("/media/external/ewaldf/TreeFARMS", writeable = TRUE)

# Define Cluster-Configurations
regr$cluster.functions = makeClusterFunctionsSocket(ncpus = 12)

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

# Define Algorithm for VIC calculation based on PFI
addAlgorithm("calculate_vic_pfi", fun = function(data, instance, job, learnername, model.id, RS) {
  # browser()
  model <- readRDS(sprintf("/media/external/rashomon/datafiles/treefarms/treefarms_%s.rds", RS))
  try(model$modelcontainer)
  model$param_set$values$selected_tree <- model.id
  
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
  
  # Function calculating PFI
  calculate_pfi = function(task, model, seed, perm.reps){
    set.seed(seed)
    X = task$data(cols = task$feature_names)
    if(model$task_type == "regr"){
      y = task$data(cols = task$target_names)
      predictor = Predictor$new(model = model, data = X, y = y)
      # PFI via ratio (default). Alternative: compare = "difference"
      FeatureImp$new(predictor, loss = "rmse", compare = "difference",
                     n.repetitions = perm.reps)
    } else if (model$task_type == "classif") {  # only binary targets
      y = task$data(cols = task$target_names)[[1]]
      y = ifelse(y == task$positive, 1, 0)
      # Define a custom prediction function that returns class labels (not probabilities)
      predict_function_class = function(model, newdata) {
        model$predict_newdata(newdata)$prob[,1]
      }
      predictor = Predictor$new(model = model, data = X, y = y, predict.function = predict_function_class)
      FeatureImp$new(predictor, loss = "mse", compare = "difference",
                     n.repetitions = perm.reps)
    } else {
      stop("Unsupported task type")
    }
  }
  
  # Calculate VIC
  calculate_pfi(task = instance, model = model, seed = 1, perm.reps = 10)
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



addExperiments(
  prob.designs = list(fromlist = data.table(taskname = design$rn)),
  algo.designs = list(calculate_vic_pfi = design[,-"rn"]),
  repls = 1,
  combine = "bind"
)



testJob(1)

submitJobs(findErrors())
submitJobs()
waitForJobs()



#### Extract results ###########################################################
# writeable = TRUE only once !!!!
regr = batchtools::loadRegistry("/media/external/XXX/bs_nnet_svm", writeable = TRUE)
# setDefaultRegistry(regr)

## save results per data set and learner
save_results = function(job_table, ids, learnername){
  list.pfi_tmp = list()
  # save median importance and feature from batchtools results
  list.pfi_tmp[[learnername]] = reduceResultsList(ids = ids, reg = regr, fun = function(x) {
    tab = x$results
    subset(tab, select = c(feature, importance))
  })
  # merge in one data.frame per data set > VIC
  vic_tmp = data.frame(feature = list.pfi_tmp[[learnername]][[1]]$feature)
  for(j in 1:length(ids)){
    vic_tmp = merge(vic_tmp,
                    list.pfi_tmp[[learnername]][[j]][,c("feature","importance")],
                    by = "feature")
    colnames(vic_tmp)[j+1] = paste0("pfi_", learnername, "_m", ids[j])
  }
  
  res.list = list()
  res.list$list.pfi = list.pfi_tmp
  res.list$vic = vic_tmp
  return(res.list)
}

job_table = getJobTable()
list.pfi = list()
vic = list()

# perform for each data set:
unique(design$rn)
for(i in unique(design$rn)){
  learnername = "TreeFARMS"
  id = which(unique(design$rn) == i)
  ids = job_table$job.id[((id-1)*1200+1):(id*1200)]
  ids = ids[is.na(job_table$error[job_table$job.id %in% ids])]
  if(length(ids) >= 1000){
    ids = ids[1:1000]
  } else {
    cat(paste("Only", length(ids), "Models for data set", i))
  }
  res = save_results(job_table, ids, learnername)
  if(!(i %in% names(list.pfi))) list.pfi[[i]] = list()
  list.pfi[[i]][[learnername]] = res$list.pfi[[learnername]]
  if(!(i %in% names(vic))){
    vic[[i]] = res$vic
  } else {
    vic[[i]] = merge(vic[[i]],
                                    res$vic,
                                    by = "feature")
  }
}

rn = unique(design$rn)
# normalize VIC: max importance = 1
vic_normalized = list()
for(i in 1:length(rn)){
  vic_normalized[[rn[i]]] = vic[[rn[i]]]
  names = names(vic[[rn[i]]])
  max_per_model = apply(vic_normalized[[rn[i]]][,-1], 2, max)
  for(j in 1:length(max_per_model)){
    vic_normalized[[rn[i]]][,j+1] = vic_normalized[[rn[i]]][,j+1]/max_per_model[j]
  }
}

save(vic, vic_normalized, file = paste0("data/results_vic_TreeFARMS.RData"))

