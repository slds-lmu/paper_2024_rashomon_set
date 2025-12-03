## Rashomon Capacity ----------------------------------------------------------
source("init.R")

library(batchtools)

# regr = makeExperimentRegistry(file.dir = "/media/external/ewaldf/all_but_TreeFARMS_RC",
#                               source = "init.R", packages = "CVXR"
# )
regr = loadRegistry("/media/external/ewaldf/all_but_TreeFARMS_RC", writeable = TRUE)
regr$cluster.functions = makeClusterFunctionsSocket(ncpus = 5)
saveRegistry()

# Define Problem
addProblem("fromlist", fun = function(data, job, taskname, learnername = NULL) {
  if(!is.null(learnername)){
    if(learnername == "TreeFARMS"){
      output = preds[[taskname]]
    }else{
      output = preds[[taskname]][[learnername]]
    }
  }else{
    preds = preds[[taskname]]
    output = do.call(c, preds)
  }
  
  # Return
  output
})

# instance: predictions

# Define Algorithm
addAlgorithm("RashomonCapacity", fun = function(data, instance, job) {
  
  if(is.null(instance[[1]]$prob)){
    num.datapoints <- length(instance[[1]]$row_ids)    # observations
    num_models <- length(instance)        # models
    
    # CVXR variable for weights
    w <- Variable(num_models, name = "w")
    
    # Extract the probability matrices
    predictions_list <- lapply(instance, function(x) x$response)
    # Combine into a matrix: n_observations x num_models
    # Each column is the predictions from one model for all observations.
    H_matrix <- simplify2array(predictions_list)
    
    objective <- sum_entries(H_matrix^2 %*% w) - sum_entries(power(H_matrix %*% w, 2))
    
  }else{
    num.classes <- 2
    num.models <- length(instance)
    num.datapoints <- nrow(instance[[1]]$prob)
    
    
    # predmats: num.models times num.datapoints x num.classes
    predmats <- lapply(instance, function(p) {
      p$prob
    })
    
    predarray <- simplify2array(predmats)  # array [num.datapoints, num.classes, num.models]
    
    # vector of length num.models which gets then taken %*% w
    pred.entropy.vector <- apply(
      ifelse(predarray < 1e-12, 0, -predarray * log(predarray)),
      3, sum
    )
    
    pred.entropy.matrix <- matrix(predarray, nrow = num.datapoints * num.classes, ncol = num.models)
    
    # stopifnot(setequal(pred.entropy.matrix[, 1], predarray[, , 1]))
    # stopifnot(setequal(pred.entropy.matrix[, 2], predarray[, , 2]))
    # stopifnot(!setequal(pred.entropy.matrix[, 2], predarray[, 2, ]))
    # stopifnot(!setequal(pred.entropy.matrix[, 2], predarray[2, , ]))
    
    w <- Variable(num.models, name = "w")
    
    objective <- sum_entries(entr(pred.entropy.matrix %*% w)) - t(pred.entropy.vector) %*% w
  }
  
  constraints <- list(w >= 0, sum_entries(w) == 1)
  
  problem <- Problem(Maximize(objective), constraints)
  
  result <- solve(problem, solver = "SCS", verbose = TRUE, num_iter = 100000, 
                  feastol = 1e-7, abstol = 1e-7, reltol = 1e-7,
                  acceleration_lookback = 10
  )
  
  oldresult <- NULL
  
  if (result$status != "optimal") {
    oldresult <- result
    result <- solve(problem, solver = "ECOS", verbose = TRUE, num_iter = 1000, 
                    feastol = 1e-7, abstol = 1e-7, reltol = 1e-7)
  }
  
  list(
    value = result$value / num.datapoints,
    status = result$status,
    solver = result$solver,
    iters = result$num_iters,
    var = result$getValue(w),
    status.old = oldresult$status,
    solver.old = oldresult$solver,
    iters.old = oldresult$num_iters,
    value.old = oldresult$value / num.datapoints,
    time.old = oldresult$solve_time
  )
})


# Experiments
# Create benchmark design for all but TreeFARMS
load("data/results_preds_all_but_TreeFARMS.RData")
batchExport(list(preds = preds), reg = regr)
combinations_df <- do.call(rbind, lapply(names(preds), function(task) {
  learners <- names(preds[[task]])
  data.frame(task = task, learner = learners, stringsAsFactors = FALSE)
}))
# Rashomon Capacity per task per learner
addExperiments(
  prob.designs = list(fromlist = data.table(taskname = combinations_df$task,
                                             learnername = combinations_df$learner)),
  algo.designs = list(RashomonCapacity = data.table()),
  repls = 1,
  combine = "bind"
)
# Rashomon Capacity per task ## Sinn?
# addExperiments(
#   prob.designs = list(fromlist = data.table(taskname = names(preds),
#                                              learnername = NULL)),
#   algo.designs = list(RashomonCapacity = data.table()),
#   repls = 1,
#   combine = "bind"
# )




# Run batchtools
testJob(1)

submitJobs(findErrors())
submitJobs()
waitForJobs()

# findExpired()
findErrors() |> getJobTable() -> jt
submitJobs(ids = jt$job.id)
jt_full = getJobTable()
jt_RC = jt_full[algorithm == "RashomonCapacity"]
submitJobs(ids = jt_RC$job.id)

## Extract Results ----------------------------------------------------------

# save results 
job_table = getJobTable()

# result_pred.mult = data.table(taskname = character(), learnername = character(), RS.algo = character(), pred.mult = numeric())
# load("data/results_pred.mult_all_but_TreeFARMS.RData")
results <- vector("list", length(job_table$job.id))
count <- 1

for(i in job_table$job.id){
  taskn = job_table[job.id == i]$prob.pars[[1]]$taskname
  if(!is.null(job_table[job.id == i]$prob.pars[[1]]$learnername)){
    learnern = job_table[job.id == i]$prob.pars[[1]]$learnername
  }else{
    learnern = NA
  } 
  RS.algo = "CASHomon"
  if(!(length(reduceResultsList(ids = i)[[1]]) == 0)){
    pred.mult = reduceResultsList(ids = i)[[1]]$value
  }else{
    pred.mult = NA
  } 
  
  results[[count]] <- data.table(taskname = taskn, learnername = learnern, RS.algo = RS.algo, pred.mult = pred.mult)
  count <- count + 1
}
result_pred.mult = do.call(rbind, results)

save(result_pred.mult, file = "data/results_pred.mult_all_but_TreeFARMS.RData")

load("data/results_pred.mult_all_but_TreeFARMS.RData")
i = 48831+23
i = 48831+36

ids = c(48831+23, 48831+36)