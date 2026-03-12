source("../init.R")

library(batchtools)
library(ggplot2)
library(tidyr)
library(data.table)


# writeable = TRUE only once !!!!
# regr = makeExperimentRegistry(file.dir = "/media/external/ewaldf/rashomon/PFI_values",
#                               source = "init.R", packages = "iml"
# )
regr = loadRegistry("/media/external/ewaldf/rashomon/PFI_values", writeable = TRUE)

# Define Cluster-Configurations
regr$cluster.functions = makeClusterFunctionsSocket(ncpus = 5)

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
addAlgorithm("calculate_vic_pfi", fun = function(data, instance, job, learnername, model.no, rds) {
  # browser()
  options(future.globals.maxSize= 20e8)
  name = sprintf("/media/external/rashomon/rashomon_models/%s/%s/%s",
                 learnername, job$pars$prob.pars$taskname, rds)
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

  # Function calculating PFI
  calculate_pfi = function(task, model, seed, perm.reps){
    set.seed(seed)
    X = task$data(cols = task$feature_names)
    pfi = list()
    if(model$task_type == "regr"){
      y = task$data(cols = task$target_names)
      predictor = Predictor$new(model = model, data = X, y = y)
      # PFI via ratio (default). Alternative: compare = "difference"
      pfi$diff = FeatureImp$new(predictor, loss = "rmse", compare = "difference",
                                n.repetitions = perm.reps)

      pfi$ratio = FeatureImp$new(predictor, loss = "rmse", n.repetitions = perm.reps)
    } else if (model$task_type == "classif") {  # only binary targets
      y = task$data(cols = task$target_names)[[1]]
      y = ifelse(y == task$positive, 1, 0)
      # Define a custom prediction function that returns class labels (not probabilities)
      predict_function_class = function(model, newdata) {
        model$predict_newdata(newdata)$prob[,1]
      }
      predictor = Predictor$new(model = model, data = X, y = y, predict.function = predict_function_class)
      pfi$diff = FeatureImp$new(predictor, loss = "mse", compare = "difference",
                                n.repetitions = perm.reps)
      pfi$ratio = FeatureImp$new(predictor, loss = "mse", n.repetitions = perm.reps)
    } else {
      stop("Unsupported task type")
    }
    return(pfi)
  }

  # Calculate VIC
  calculate_pfi(task = instance, model = model, seed = 1, perm.reps = 10)
})

load("../data/design_all_but_TreeFARMS.RData")

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
regr = batchtools::loadRegistry("/media/external/ewaldf/rashomon/PFI_values", writeable = TRUE)
# setDefaultRegistry(regr)

pre_design = pre_design[no.models != 0,,]
job_table = getJobTable()
# list.pfi = list()
vic = list()
# names(list.pfi)

# for(i in 1:length(pre_design$count)){
for(i in 1:length(job_table$prob.pars)){
  taskname = job_table$prob.pars[[i]]$taskname
  learnername = job_table$algo.pars[[i]]$learnername
  model.no = job_table$algo.pars[[i]]$model.no

  result = reduceResultsList(ids = i, reg = regr, fun = function(x) {
    tab1 = x[[1]]$results
    subset(tab1, select = c(feature, importance))
  })

  if(!(taskname %in% names(vic)) || dim(vic[[taskname]])[1] == 0){
    vic[[taskname]] = result[[1]]
  } else {
    vic[[taskname]] = merge(vic[[taskname]],
                            result[[1]],
                            by = "feature")
  }
  colnames(vic[[taskname]])[length(vic[[taskname]])] = paste0("pfi_", learnername, "_m", model.no)
  rm(result)
  if (i %% 1000 == 0) print(paste(i, "done"))
}

# normalize VIC: max importance = 1
vic_normalized = list()
for(i in 1:length(pre_design$no.models)){
  vic_normalized[[pre_design$rn[i]]] = vic[[pre_design$rn[i]]]
  names = names(vic[[pre_design$rn[i]]])
  max_per_model = apply(vic_normalized[[pre_design$rn[i]]][,-1], 2, max)
  for(j in 1:length(max_per_model)){
    vic_normalized[[pre_design$rn[i]]][,j+1] = vic_normalized[[pre_design$rn[i]]][,j+1]/max_per_model[j]
  }
}

if(length(unique(design$learnername)) == 1){
  tmp_vic = vic
  tmp_vic_norm = vic_normalized
  load("data/results_vic_bs.RData")
  learnername = unique(design$learnername)
  vic$bs = cbind(vic$bs, tmp_vic$bs[-1])
  vic_normalized$bs = cbind(vic_normalized$bs, tmp_vic_norm$bs[-1])
}

save(vic, vic_normalized, file = paste0("../data/results_vic_all_but_TreeFARMS.RData"))
