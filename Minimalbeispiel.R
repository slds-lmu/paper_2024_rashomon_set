library(CVXR)

source("init.R")

load("data/results_preds_all_but_TreeFARMS.RData")

combinations_df <- do.call(rbind, lapply(names(preds), function(task) {
  learners <- names(preds[[task]])
  data.frame(task = task, learner = learners, stringsAsFactors = FALSE)
}))

# Algorithm 
calculateRashomonCapacity <- function(instance) {
  
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
}


# binary classification
taskname = combinations_df$task[1]
learnername = combinations_df$learner[1]
instance = preds[[taskname]][[learnername]]

result = calculateRashomonCapacity(instance)

# Regression  
taskname = combinations_df$task[8]
learnername = combinations_df$learner[8]
instance = preds[[taskname]][[learnername]]

result_reg = calculateRashomonCapacity(instance)


### OLD ###########
n <- length(instance[[1]]$row_ids)    # observations
num_models <- length(instance)        # models


# Extract the probability matrices from each predictor
  prob_list <- lapply(instance, function(x) x$prob)
  # Stack into an array of shape n x k x m
  # Use `simplify2array()` to stack and then aperm to get correct dimensions
  P_array <- simplify2array(prob_list)  # This gives an array of dim n x k x m
  # Rearrange dimensions to n x m x k
  P_array <- aperm(P_array, c(1, 3, 2))
  
  # smaller example
  # num_models = 5
  # n = 10
  # P_array = P_array[1:n, 1:num_models, ]

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
  obj_terms_list <- vector("list", n)
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
  # objective_expr <- Reduce(`+`, obj_terms_list)
  objective_expr <- sum_entries(do.call(vstack, obj_terms_list))  # linear-time construction
  
  
# Constraints for w (simplex)
constraints_list <- list(w >= 0, sum(w) == 1)

# stop(123)

# The objective is concave, CVXR can maximize a concave objective.
problem <- Problem(Maximize(objective_expr), constraints_list)
result <- solve(problem)
# result$value is the maximized sum of variances: sup_w { sum_i [Var(h(x_i)|w)] }
# We want the average of this maximized variance.
base_value <- result$value / n

# Final metric based on task type
# if(is.null(instance[[1]]$prob)){ # REGRESSION
#   final_metric <- sqrt(max(0, base_value))
# } else { # CLASSIFICATION
#   final_metric <- 2^base_value
# }

base_value


# Regression Example ###########################################################
 
taskname = combinations_df$task[8]
learnername = combinations_df$learner[8]
instance = preds[[taskname]][[learnername]]

# Algorithm start 
n <- length(instance[[1]]$row_ids)    # observations
num_models <- length(instance)        # models

# CVXR variable for weights
w <- Variable(num_models)

# Extract the probability matrices from each predictor
 
  predictions_list <- lapply(instance, function(x) x$response)
  # Combine into a matrix: n_observations x num_models
  # Each column is the predictions from one model for all observations.
  H_matrix <- do.call(cbind, predictions_list)
  # Precompute squared predictions (element-wise)
  H_matrix_sq <- H_matrix^2
  
  # Vectorized objective for regression
  E_h_X_sq_per_instance <- H_matrix_sq %*% w  # CVXR expr: n x 1 vector
  E_h_X_per_instance <- H_matrix %*% w      # CVXR expr: n x 1 vector
  
  Var_per_instance <- E_h_X_sq_per_instance - power(E_h_X_per_instance, 2) # CVXR expr: n x 1 vector
  
  objective_expr <- sum_entries(Var_per_instance) # CVXR expr: scalar
  

# Constraints for w (simplex)
constraints_list <- list(w >= 0, sum(w) == 1)

# The objective is concave, CVXR can maximize a concave objective.
problem <- Problem(Maximize(objective_expr), constraints_list)
result <- solve(problem)

# result$value is the maximized sum of variances: sup_w { sum_i [Var(h(x_i)|w)] }
# We want the average of this maximized variance.
base_value <- result$value / n

# Final metric based on task type
# if(is.null(instance[[1]]$prob)){ # REGRESSION
#   final_metric <- sqrt(max(0, base_value))
# } else { # CLASSIFICATION
#   final_metric <- 2^base_value
# }

base_value


# Problems ####################################################################

# classification id 24
taskname = combinations_df$task[24]
learnername = combinations_df$learner[24]
instance = preds[[taskname]][[learnername]]

# Algorithm start 
n <- length(instance[[24]]$row_ids)    # observations
num_models <- length(instance)        # models

# CVXR variable for weights
w <- Variable(num_models)

# Extract the probability matrices from each predictor
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
obj_terms_list <- vector("list", n)
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
# objective_expr <- Reduce(`+`, obj_terms_list)

# summe = obj_terms_list[[1]]+obj_terms_list[[2]]
# for(i in 3:length(obj_terms_list)){
#   summe = summe + obj_terms_list[[i]]
#   print(i)
# }
# 
# idx = c(seq(1, length(obj_terms_list), 100), length(obj_terms_list))
# idx
# 
# 
# objective_expr1 <- Reduce(`+`, obj_terms_list[1:100])
# 
# objective_expr2 <- Reduce(`+`, obj_terms_list[101:200])
# 
# objective_expr1 + objective_expr2

# obj_terms_list[[1]] + obj_terms_list[[2]]
# 
objective_expr <- sum_entries(do.call(vstack, obj_terms_list))  # linear-time construction

# Constraints for w (simplex)
constraints_list <- list(w >= 0, sum(w) == 1)

# The objective is concave, CVXR can maximize a concave objective.
problem <- Problem(Maximize(objective_expr), constraints_list)
result <- solve(problem)

# result$value is the maximized sum of variances: sup_w { sum_i [Var(h(x_i)|w)] }
# We want the average of this maximized variance.
base_value <- result$value / n

# Final metric based on task type
# if(is.null(instance[[1]]$prob)){ # REGRESSION
#   final_metric <- sqrt(max(0, base_value))
# } else { # CLASSIFICATION
#   final_metric <- 2^base_value
# }

base_value
