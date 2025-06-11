## Rashomon Capacity ----------------------------------------------------------
source("init.R")

library(batchtools)

regr = loadRegistry("/media/external/ewaldf/all_but_TreeFARMS_preds", writeable = TRUE)
regr$cluster.functions = makeClusterFunctionsSocket(ncpus = 5)

# Define Problem
addProblem("fromlist2", fun = function(data, job, taskname, learnername) {
  if(!is.null(learnername)){
    output = preds[[taskname]][[learnername]]
  }else if(learnername == "TreeFARMS"){
    output = preds[[taskname]]
  }else{
    preds = preds[[taskname]]
    output = do.call(c, preds)
  }
  
  # Return
  output
})

# Define Algorithm
addAlgorithm("RashomonCapacity", fun = function(data, instance, job) {
  n <- length(instance[[1]]$row_ids)    # observations
  num_models <- length(instance)        # models
  
  # CVXR variable for weights
  w <- Variable(num_models)
  
  # Extract the probability matrices from each predictor
  if(is.null(instance[[1]]$prob)){
    predictions_list <- lapply(instance, function(x) x$response)
    # Combine into a matrix: n_observations x num_models
    # Each column is the predictions from one model for all observations.
    H_matrix <- do.call(cbind, predictions_list)
    # Precompute squared predictions (element-wise)
    H_matrix_sq <- H_matrix^2
    
    # Build objective terms
    obj_terms_list <- list()
    for (i_idx in 1:n) {
      # Predictions of all models for instance i
      h_i_vec <- H_matrix[i_idx, , drop = TRUE] # Ensures it's a vector
      # Squared predictions of all models for instance i
      h_i_sq_vec <- H_matrix_sq[i_idx, , drop = TRUE]
      
      # E_w[h(x_i)] as a CVXR expression
      expected_h_i <- t(h_i_vec) %*% w 
      # E_w[h(x_i)^2] as a CVXR expression
      expected_h_sq_i <- t(h_i_sq_vec) %*% w
      
      # Var(h(x_i) | w) = E_w[h(x_i)^2] - (E_w[h(x_i)])^2
      # This is (linear term) - (convex term), which is concave.
      var_i_expr <- expected_h_sq_i - CVXR::power(expected_h_i, 2)
      
      obj_terms_list[[i_idx]] <- var_i_expr
    }
  }else{
    prob_list <- lapply(instance, function(x) x$prob)
    # Stack into an array of shape n x k x m
    # Use `simplify2array()` to stack and then aperm to get correct dimensions
    P_array <- simplify2array(prob_list)  # This gives an array of dim n x m x k
    # Rearrange dimensions to n x k x m
    P_array <- aperm(P_array, c(1, 3, 2))
    # CVXR-compatible entropy: H(p) = sum(entr(p_d)), where entr(x) = -x*log(x)
    entropy_cvxr <- function(p_vector_expr) {
      sum_entries(entr(p_vector_expr))
    }
    # Pre-calculate H(P^{i,j}) constants
    H_P_ij_constants <- array(0, dim = c(n, num_models))
    for (i_idx in 1:n) {
      for (j_idx in 1:num_models) {
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
  }
  
  # Overall objective: sum of concave terms = concave
  objective_expr <- Reduce(`+`, obj_terms_list)
  
  # Constraints for w (simplex)
  constraints_list <- list(w >= 0, sum(w) == 1)
  
  # The objective is concave, CVXR can maximize a concave objective.
  problem <- Problem(Maximize(objective_expr), constraints_list)
  result <- solve(problem)
  
  # result$value is the maximized sum of variances: sup_w { sum_i [Var(h(x_i)|w)] }
  # We want the average of this maximized variance.
  maximized_average_variance <- result$value / n
  
  # The final metric is often reported as a standard deviation for interpretability.
  # Ensure variance is non-negative before taking sqrt due to potential numerical precision issues.
  # regression_spread_metric <- sqrt(max(0, maximized_average_variance))
  
  maximized_average_variance
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
  prob.designs = list(fromlist2 = data.table(taskname = combinations_df$task, 
                                             learnername = combinations_df$learner)),
  algo.designs = list(RashomonCapacity = data.table()),
  repls = 1,
  combine = "bind"
)
# Rashomon Capacity per task
addExperiments(
  prob.designs = list(fromlist2 = data.table(taskname = names(preds), 
                                             learnername = NULL)),
  algo.designs = list(RashomonCapacity = data.table()),
  repls = 1,
  combine = "bind"
)

# Create benchmark design for TreeFARMS
load("data/results_preds_TreeFARMS.RData")
batchExport(list(preds = preds), reg = regr)
# Rashomon Capacity per task
addExperiments(
  prob.designs = list(fromlist2 = data.table(taskname = names(preds), 
                                             learnername = "TreeFARMS")),
  algo.designs = list(RashomonCapacity = data.table()),
  repls = 1,
  combine = "bind"
)




# Run batchtools
testJob(48831)

submitJobs(findErrors())
submitJobs()
waitForJobs()

findErrors() |> getJobTable() -> jt

## Extract Results ----------------------------------------------------------

# save results per data set and learner
job_table = getJobTable()
preds = list()







### OLD ###################

preds_data = preds$cr

# Problem dimensions
n <- length(preds_data[[1]]$row_ids)    # observations
k <- length(preds_data)                 # models (num_models)
# m <- ncol(preds_data[[1]]$prob)         # labels

rashomon_capacity <- function(preds_data, num_models, n){
  # Extract the probability matrices from each predictor
  prob_list <- lapply(preds_data, function(x) x$prob)
  # Stack into an array of shape n x k x m
  # Use `simplify2array()` to stack and then aperm to get correct dimensions
  P_array <- simplify2array(prob_list)  # This gives an array of dim n x m x k
  # Rearrange dimensions to n x k x m
  P_array <- aperm(P_array, c(1, 3, 2))
  
  # CVXR variable for weights
  w <- Variable(num_models)
  
  # CVXR-compatible entropy: H(p) = sum(entr(p_d)), where entr(x) = -x*log(x)
  entropy_cvxr <- function(p_vector_expr) {
    sum_entries(entr(p_vector_expr))
  }
  
  # Pre-calculate H(P^{i,j}) constants
  H_P_ij_constants <- array(0, dim = c(n, num_models))
  for (i_idx in 1:n) {
    for (j_idx in 1:num_models) {
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
  
  
  2 ^ result$value
}

rashomon_capacity(preds_data, k, n)

save(XXX, file = paste0("data/result_RashomonCapacity_all_but_TreeFARMS.RData"))
