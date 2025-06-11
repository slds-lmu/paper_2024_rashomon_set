### Measure predictive multiplicity
source("init.R")

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

save(preds_TreeFARMS, file = paste0("data/results_preds_TreeFARMS.RData"))

## Rashomon Capacity ----------------------------------------------------------

load("data/results_preds_TreeFARMS.RData")
library(CVXR)

preds_data = preds_TreeFARMS$cr

# Problem dimensions
n <- length(preds_data[[1]]$row_ids)    # observations
k <- length(preds_data)                 # models
m <- ncol(preds_data[[1]]$prob)         # labels

# Extract the probability matrices from each predictor
prob_list <- lapply(preds_data, function(x) x$prob)
# Stack into an array of shape n x k x m
# Use `simplify2array()` to stack and then aperm to get correct dimensions
P_array <- simplify2array(prob_list)  # This gives an array of dim n x m x k
# Rearrange dimensions to n x k x m
P_array <- aperm(P_array, c(1, 3, 2))

# CVXR variable for weights
w <- Variable(k)

# CVXR-compatible entropy: H(p) = sum(entr(p_d)), where entr(x) = -x*log(x)
entropy_cvxr <- function(p_vector_expr) {
  sum_entries(entr(p_vector_expr))
}

# Pre-calculate H(P^{i,j}) constants
H_P_ij_constants <- array(0, dim = c(n, k))
for (i_idx in 1:n) {
  for (j_idx in 1:k) {
    p_vec <- P_array[i_idx, j_idx, ]
    H_P_ij_constants[i_idx, j_idx] <- -sum(p_vec[p_vec > 1e-12] * log(p_vec[p_vec > 1e-12]))
  }
}

# Build objective terms
obj_terms_list <- list()
for (i_idx in 1:n) {
  P_i_group_matrix <- P_array[i_idx, , ] # k x m matrix for group i
  
  # Q_i = sum_j w_j P^{i,j} (CVXR expression, 1 x m)
  Q_i_expr <- t(w) %*% P_i_group_matrix
  
  # H(Q_i) (concave CVXR expression)
  H_Q_i_expr <- entropy_cvxr(Q_i_expr)
  
  # sum_j w_j H(P^{i,j}) (linear CVXR expression)
  sum_w_H_Pij_expr <- t(w) %*% H_P_ij_constants[i_idx, ]
  
  # Term for group i: H(Q_i) - sum_j w_j H(P^{i,j}) (concave CVXR expression)
  term_i_expr <- H_Q_i_expr - sum_w_H_Pij_expr
  obj_terms_list[[i_idx]] <- term_i_expr
}

# Overall objective: sum of concave terms = concave
objective_expr <- Reduce(`+`, obj_terms_list)

# Constraints for w (simplex)
constraints_list <- list(w >= 0, sum(w) == 1)

# The objective is concave, CVXR can maximize a concave objective.
problem <- Problem(Maximize(objective_expr), constraints_list)
result <- solve(problem) 

RashomonCapacity_TreeFARMS = 2^result$value


save(RashomonCapacity_TreeFARMS, file = paste0("data/result_RashomonCapacity_TreeFARMS.RData"))
