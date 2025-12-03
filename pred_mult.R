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
regr$cluster.functions = makeClusterFunctionsSocket(ncpus = 20)

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
addAlgorithm("predict", fun = function(data, instance, job, learnername, model.no, rds) {
  
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
findErrors() |> getJobTable() -> jt
submitJobs(ids = jt$job.id)

getJobTable() -> jt
job_table = jt[design$learnername != "global"]

job_table = getJobTable()

preds = list()


for(i in job_table$job.id){
  taskname = job_table[job.id == i]$prob.pars[[1]]$taskname
  learnername = job_table[job.id == i]$algo.pars[[1]]$learnername
  model.no = job_table[job.id == i]$algo.pars[[1]]$model.no
  
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
  if (i %% 1000 == 0) print(paste("id", i, "done"))
}

save(preds, file = paste0("data/results_preds_all_but_TreeFARMS.RData"))



## Predictive Performance #####################################################

library(mlr3measures)

### Functions ###
generateTest = function(taskname){
  task = list.tasks[[taskname]]
  
  ## Fix logical features (for FeatureImp)
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
}
# test
# test_task = generateTest(names(list.tasks.binarized)[1])

calculatePerfMetric = function(preds, taskname, task_test, isTreeFARMS = FALSE,
                               perf_m = data.table(task = character(), 
                                                   learner = character(), 
                                                   test.score = numeric(), 
                                                   score = character())){
  if(isTreeFARMS == FALSE){
    learnernames = names(preds[[taskname]])
    if(is.null(learnernames) == TRUE) stop("Learner is not TreeFARMS but preds also does not contain a learner information in the right format.")
  }else{
    learnernames = "TreeFARMS"
  }
  for(learnername in learnernames){
    if(isTreeFARMS == FALSE){
      preds_tmp = preds[[taskname]][[learnername]]
    }else{
      preds_tmp = preds[[taskname]]
    }
    
    if(class(task_test)[1] == "TaskClassif"){
      if(task_test$properties == "twoclass"){
        for(model.no in 1:length(preds_tmp)){
          perf_m = rbind(
            perf_m, 
            data.table(task = taskname, learner = learnername,
                       test.score = bbrier(truth = task_test$data(cols = task_test$target_names)[[1]],
                                           prob = preds_tmp[[model.no]]$prob[,1],
                                           positive = task_test$positive
                       ),
                       score = "brier"))
        }}
    }else if(class(task_test)[1] == "TaskRegr"){
      for(model.no in 1:length(preds_tmp)){
        perf_m = rbind(
          perf_m, 
          data.table(task = taskname, learner = learnername,
                     test.score = rmse(truth = task_test$data(cols = task_test$target_names)[[1]],
                                       response = preds_tmp[[model.no]]$response
                     ),
                     score = "rmse"))
      }
    }}
  perf_m
}
# test 
# test_perfM = calculatePerfMetric(preds, names(list.tasks.binarized)[1], test_task)

### Scores #####

# save here: 
perf_m = data.table(task = character(), learner = character(), 
                    test.score = numeric(), score = character())


#### All but TreeFARMS ####
load("data/results_preds_all_but_TreeFARMS.RData")

for(taskname in names(list.tasks)){
  # Create test task
  task_test = generateTest(taskname)
  
  # Calculate performance metric
  perf_m = calculatePerfMetric(preds, taskname, task_test, FALSE, perf_m)
}
perf_m 

#### TreeFARMS ####
load("data/results_preds_TreeFARMS.RData")

for(taskname in names(list.tasks.binarized)){
  # Create test task
  task_test = generateTest(taskname)
  
  # Calculate performance metric
  perf_m = calculatePerfMetric(preds, taskname, task_test, TRUE, perf_m)
}
perf_m 

# save
save(perf_m, file = paste0("data/results_modelperformances.RData"))


### Plot #####

library(ggplot2)

ggplot(perf_m[!(task %in% c("bs", "st")),]) +
  # geom_boxplot(aes(x = feature, y = Value), fill = "gray") +
  geom_boxplot(aes(x = task, y = test.score, fill = learner)) +
  coord_flip() +
  # labs(y = "Importance", x = "Feature", fill = "Model Class",
       # title = paste("PFI values (max importance = 1):", task.key)) +
  theme_minimal(base_size = 15)  +
  theme(legend.text = element_text(size=13)) +
  guides(alpha = FALSE, color = guide_legend(override.aes = list(size=8)))
