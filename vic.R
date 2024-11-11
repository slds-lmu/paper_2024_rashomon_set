### Only works if "run_models_dims.RData" is up to date. !!!!!!!!!!!!!!!!!!!!!!


source("init.R")

# future::plan(list("sequential", future::tweak("multisession", workers = future::availableCores())))
# options("mlr3.exec_chunk_bins" = future::nbrOfWorkers() * 5)

library(batchtools)
library(iml)
library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(ggplot2)
library(tidyr)
library(GGally)
library(patchwork)


# Registry
regr = makeExperimentRegistry(file.dir = NA,
                              source = "init.R"
)

# Define Cluster-Configurations
regr$cluster.functions = makeClusterFunctionsSocket(ncpus = 30)

# Define Problem
addProblem("fromlist", fun = function(data, job, taskname) {
  task = list.tasks[[taskname]]

  # Check logical variables and convert to factor
  data = as.data.frame(task$data())
  for(i in seq_along(data)) {
    if (is.logical(data[[i]])) data[[i]] = as.factor(data[[i]])
  }

  # Return of the validation split
  generateCanonicalDataSplits(task, ratio = 2 / 3, seed = 1)$validation
})

# Define Algorithm for VIC calculation based on PFI
addAlgorithm("calculate_vic_pfi", fun = function(data, instance, job, learnername) {
  # Load models
  load("run_models_dims.RData")
  no.models = run_models_dims[[learnername]][job$taskname]
  models = list()
  for(i in 1:no.models) {
    name = sprintf("/media/external/rashomon/datafiles/%s/%s/samplemodel_%s_%s_%04d.rds",
                   job$taskname, learnername, learnername, job$taskname, i)
    models[[i]] = readRDS(name)
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

  # Fix models in case of task bs (logical features)
  if(job$taskname == "bs"){
    holiday.special <- ppl("convert_types", "factor", "logical", selector_name(c("holiday", "working_day")), id = "holiday.special")
    invisible(holiday.special$train(list.tasks$bs))
    models$bs <- lapply(models$bs, function(x) {
      xstate <- x$state
      gr <- holiday.special$clone(deep = TRUE) %>>% x$clone(deep = TRUE)
      lr <- as_learner(gr$clone(deep = TRUE))
      lr$state <- xstate
      lr$state$train_task <- list.tasks$bs$clone(deep = TRUE)$filter(0)
      lr$model <- gr$state
      lr$model[[gr$ids()[[2]]]] <- xstate
      lr
    })
  }

  # Calculate VIC
  list.pfi = lapply(models, calculate_pfi, task = instance, seed = 1, perm.reps = 10)
  vic = data.frame(feature = list.pfi[[1]]$results$feature)
  for(i in 1:no.models) {
    vic <- merge(vic,
                 list.pfi[[i]]$results[,c("feature", "importance")],
                 by = "feature")
  }
  colnames(vic) <- c("feature",
                     paste0("pfi_", rep(learnername, no.models),
                            "_m", 1:no.models))

  # Normalize VIC
  vic_normalized = vic
  max_per_model = apply(vic_normalized[,-1], 2, max)
  for(i in 1:length(max_per_model)){
    vic_normalized[,i+1] = vic_normalized[,i+1]/max_per_model[i]
  }

  output = list(vic, vic_normalized)
  names(output) = c(paste0("vic_", job$taskname, "_", learnername),
                    paste0("vic_scaled_", job$taskname, "_", learnername))
  output
})

addExperiments(
  prob.designs = list(fromlist = data.table(taskname = names(list.tasks))),
  algo.designs = list(calculate_vic_pfi = data.table(learnername = names(list.learners.regr)))
)

testJob(1)

submitJobs(findErrors())
