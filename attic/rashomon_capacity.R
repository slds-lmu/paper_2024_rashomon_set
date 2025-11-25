source("init.R")

learner <- list.learners.classif$glmnet
tasks <- generateCanonicalDataSplits(list.tasks$bc)
set.seed(1)
configs <- generate_design_random(learner$param_set$search_space(), 100)$transpose()

learner.used <- learner$clone(deep = TRUE)
predictions <- lapply(configs, function(cfg) {
  learner.used$param_set$set_values(.values = cfg)
  learner.used$train(tasks$training)
  learner.used$predict(tasks$validation)
})



# we have `N` datapoints, for which `K` models predict a probability distribution over `M` classes
# want to find
#   sup_{ P_over_models } E_{ (X, y) ~ P_over_data } [
#       inf_{ q \in P_over_classes } E_{ f ~ P_over_models } [ d_{KL}( f(X) || q ) ]
#   ]
#
# expand term for d_{KL}
#
#   sup_{ w \in P_over_models } E_{ (X, y) ~ P_over_data } [
#       inf_{ q \in P_over_classes } sum_{k=1}^K w_k ( H(f_k(X), q) - H(f_k(X)) )
#   ]
#
#   inf_q sum_{k=1}^K w_k H(f_k(X), q) = inf_q sum_{k=1}^K w_k sum_{m=1}^M -f_k(X)_m log(q_m)
#       = inf_q sum_{m=1}^M -sum_{k=1}^K w_k f_k(X)_m log(q_m)
# which takes on its minimum value at q_m = sum_{k=1}^K w_k f_k(X)_m and is just H(sum_{k=1}^K w_k f_k(X))
#
#   sup_w E_{ (X, y) ~ P_over_data } [
#       H( sum_{i=1}^k w_i f_i(X) ) - sum_{i=1}^k w_i H(f_i(X))
#   ]
#
# Expectation "over data" is just all test points
#
#   sup_w (1/N) sum_{n=1}^N { H( sum_{k=1}^K w_k f_k(X_n) ) - sum_{k=1}^K w_k H(f_k(X_n)) }
#
# Column vector w, matrix F(X) = [f_k(X)_m]_{m,k}
#
#   sup_w (1/N) sum_{n=1}^N { H( F(X_n) %*% w ) - H(F(X_n)_k,.) %*% w }
#
# The matrix on the right is a constant, we can precompute the sum over N terms
#
#   sup_w (1/N) sum_{n=1}^N H( F(X_n) %*% w ) - (1/N) sum_{n=1}^N H(F(X_n)_k,.) %*% w
#
# The H() on the left is already a sum, so we have
#
#   sup_w (1/N) sum_{n=1}^N sum_{m=1}^M entr( F(X_n)_m %*% w ) - (1/N) sum_{n=1}^N H(F(X_n)_k,.) %*% w
#   = ( sup_w sum_{n=1}^N sum_{m=1}^M entr( F(X_n)_m %*% w ) - sum_{n=1}^N H(F(X_n)_k,.) %*% w ) / N

library("CVXR")


calculateRashomonCapacity <- function(predictions) {
  num.classes <- 2
  num.models <- length(predictions)
  num.datapoints <- nrow(predictions[[1]]$prob)


  # predmats: num.models times num.datapoints x num.classes
  predmats <- lapply(predictions, function(p) {
    p$prob
  })

  predarray <- simplify2array(predmats)  # array [num.datapoints, num.classes, num.models]

  # vector of length num.models which gets then taken %*% w
  pred.entropy.vector <- apply(
    ifelse(predarray < 1e-12, 0, -predarray * log(predarray)),
    3, sum
  )

  # [ num.datapoints * num.classes, num.models ] %*% [ num.models ]

  pred.entropy.matrix <- matrix(predarray, nrow = num.datapoints * num.classes, ncol = num.models)

  # stopifnot(setequal(pred.entropy.matrix[, 1], predarray[, , 1]))
  # stopifnot(setequal(pred.entropy.matrix[, 2], predarray[, , 2]))
  # stopifnot(!setequal(pred.entropy.matrix[, 2], predarray[, 2, ]))
  # stopifnot(!setequal(pred.entropy.matrix[, 2], predarray[2, , ]))

  w <- Variable(num.models, name = "w")

  objective <- sum_entries(entr(pred.entropy.matrix %*% w)) - t(pred.entropy.vector) %*% w

  constraints <- list(w >= 0, sum_entries(w) == 1)

  prob <- Problem(Maximize(objective), constraints)

  result <- solve(prob, solver = "SCS", verbose = TRUE, num_iter = 100000, feastol = 1e-7, abstol = 1e-7, reltol = 1e-7,
    acceleration_lookback = 10
  )
  oldresult <- NULL

  if (result$status != "optimal") {
    oldresult <- result
    result <- solve(prob, solver = "ECOS", verbose = TRUE, num_iter = 1000, feastol = 1e-7, abstol = 1e-7, reltol = 1e-7)
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


result <- calculateRashomonCapacity(predictions)

library("torch")

num.classes <- 2
num.models <- length(predictions)
num.datapoints <- nrow(predictions[[1]]$prob)


# predmats: num.models times num.datapoints x num.classes
predmats <- lapply(predictions, function(p) {
  p$prob
})

predarray <- simplify2array(predmats)  # array [num.datapoints, num.classes, num.models]

# vector of length num.models which gets then taken %*% w
pred.entropy.vector <- apply(
  ifelse(predarray < 1e-12, 0, -predarray * log(predarray)),
  3, sum
)

pred.entropy.matrix <- matrix(predarray, nrow = num.datapoints * num.classes, ncol = num.models)

pem.torch <- torch_tensor(pred.entropy.matrix, dtype = torch_float64())
pev.torch <- torch_tensor(pred.entropy.vector, dtype = torch_float64())

w.vect <- torch_zeros(num.models, requires_grad = TRUE)
opt <- optim_adamw(params = list(w.vect), lr = 1e-2, weight_decay = 0)

iters <- 100000

progress <- data.table(iter = seq_len(iters), loss = numeric(iters))
lastloss <- Inf
for (i in seq_len(iters)) {
  opt$zero_grad()
  w.projected <- nnf_softmax(w.vect, dim = 1, dtype = torch_float64())
  pemw <- torch_matmul(pem.torch, w.projected)  # log-softmax
  objective <- torch_sum(-pemw * torch_log(torch_clamp(pemw, min = 1e-12))) - torch_dot(pev.torch, w.projected)
  loss <- -objective
  loss$backward()
  opt$step()
  lossnum <- as.numeric(loss)
  progress[iter == i, loss := lossnum]
  if (i %% 10 == 0) {
    cat(sprintf("iter %s: loss = %s\n", i, lossnum))
  }
  if (lastloss - lossnum < 1e-8) {
    break
  }
  lastloss <- lossnum
}

progress <- progress[seq_len(i)]

wresult <- as.numeric(nnf_softmax(w.vect, dim = 1)$detach())

plot(result$var, wresult)

progress[, rc := -loss / num.datapoints]



plot(progress$iter, progress$rc)
abline(h = result$value, col = "red")

plot(progress$iter, log10(abs(progress$rc - last(progress$rc))))

plot(progress$iter, log10(abs(progress$rc - result$value)))
