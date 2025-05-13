
test_that("LearnerClassifGosdt basic train and predict", {
  set.seed(123)
  data.train <- data.table(
    f1 = as.numeric(runif(100) > 0.5),
    f2 = as.numeric(runif(100) > 0.75)
  )
  data.train[, target := factor(rbinom(100, 1, prob = 0.5 * f1 + 0.25 * f2 + 0.1), labels = c("neg", "pos"))]
  task <- as_task_classif(data.train, target = "target", id = "gosdt_basic")

  learner <- LearnerClassifGosdt$new()
  learner$predict_type <- "prob"
  learner$param_set$set_values(
    regularization = 0.01,
    allow_small_reg = TRUE # For small dataset and low regularization
  )

  expect_silent(learner$train(task))

  # Check model state
  expect_true(is.list(learner$model), info = "Learner model is not a list after training.")
  expect_s3_class(learner$model$party.list$tree, "partynode")
  expect_identical(learner$model$target.levels, task$class_names)

  # Predict (should give both response and prob)
  prediction <- learner$predict(task)
  expect_s3_class(prediction, "PredictionClassif")

  # Check response
  expect_true("response" %in% prediction$predict_types, info = "Response not in predict_types.")
  expect_factor(prediction$response, levels = task$class_names, len = task$nrow,
    info = "Prediction response has incorrect levels, length, or type.")

  # Check probabilities
  expect_true("prob" %in% prediction$predict_types, info = "Probabilities not in predict_types.")
  expect_numeric(prediction$prob, lower = 0, upper = 1, any.missing = FALSE,
    info = "Prediction probabilities are not numeric or not in [0,1] range.")
  expect_equal(colnames(prediction$prob), task$class_names,
    info = "Colnames of probability matrix do not match task class_names.")
  expect_equal(rowSums(prediction$prob), rep(1, task$nrow), tolerance = 1e-6,
    info = "Row sums of probability matrix do not equal 1.")

  # response prediction is the class with the highest probability
  expect_identical(as.integer(prediction$response), apply(prediction$prob, 1, which.max))
})

test_that("LearnerClassifGosdt predict_type 'response' works", {
  set.seed(456)
  data.train <- data.table(
    fA = c(rep(0L, 10), rep(1L, 10)),
    fB = sample(0L:1L, 20, replace = TRUE)
  )
  data.train[, target_val := factor(ifelse(fA == 0L, "type_X", "type_Y"))]
  task <- TaskClassif$new(id = "gosdt_resp_only", backend = data.train, target = "target_val")

  learner <- LearnerClassifGosdt$new()
  learner$predict_type <- "response"
  learner$param_set$set_values(
    regularization = 0.01,
    allow_small_reg = TRUE
  )

  expect_silent(learner$train(task))
  expect_list(learner$model)

  prediction <- learner$predict(task)
  expect_s3_class(prediction, "PredictionClassif")

  # Check response
  expect_equal(prediction$predict_types, "response", info = "Predict_types should only contain 'response'.")
  expect_factor(prediction$response, levels = task$class_names, len = task$nrow,
    info = "Prediction response has incorrect levels, length, or type.")

  # Check that probabilities are not present
  expect_null(prediction$prob, info = "Prediction probabilities should be NULL when predict_type is 'response'.")
})

test_that("LearnerClassifGosdt 'allow_small_reg' parameter functionality", {
  set.seed(789)
  n.samples <- 20
  data.small <- data.table(
    feat = sample(0L:1L, n.samples, replace = TRUE)
  )
  data.small[, target_cls := factor(rbinom(n.samples, 1, prob = 0.3 + 0.4 * feat), labels = c("c1", "c2"))]
  task.small <- TaskClassif$new(id = "gosdt_small_reg", backend = data.small, target = "target_cls")

  # Regularization value that would trigger the check
  # GOSDT error if regularization * n_samples < 1
  reg.val <- 0.5 / n.samples # this makes reg.val * n.samples = 0.5 < 1
  reg.val.allowed <- 1 / n.samples

  learner.reg.restricted <- LearnerClassifGosdt$new()
  learner.reg.restricted$param_set$set_values(
    regularization = reg.val
    # allow_small_reg = FALSE (default)
  )

  learner.reg.nonrestricted <- LearnerClassifGosdt$new()
  learner.reg.nonrestricted$param_set$set_values(
    regularization = reg.val,
    allow_small_reg = TRUE
  )

  learner.reg.allowed <- LearnerClassifGosdt$new()
  learner.reg.allowed$param_set$set_values(
    regularization = reg.val.allowed,
    allow_small_reg = TRUE
  )

  learner.reg.restricted$train(task.small)
  learner.reg.nonrestricted$train(task.small)
  learner.reg.allowed$train(task.small)

  # having 'allow_small_reg = FALSE' internally resets the regularization to the 'allowed' value,
  # so the resulting model should be identical to the model with the higher regularization.
  expect_identical(
    learner.reg.restricted$model$models_string,
    learner.reg.allowed$model$models_string,
  )

  expect_false(identical(
    learner.reg.nonrestricted$model$models_string,
    learner.reg.restricted$model$models_string
  ))

})

test_that("LearnerClassifGosdt 'balance = TRUE' effect on imbalanced task", {
  set.seed(101)
  n <- 1000
  # f1 predicts A, but in all cases A is in the minority, so when going for accuracy, constant 'B' prediction is best.
  data.imbalanced <- data.table(
    f1 = sample(0L:1L, n, replace = TRUE, prob = c(0.95, 0.05)),
    f2 = sample(0L:1L, n, replace = TRUE)
  )
  prob.A <- ifelse(data.imbalanced$f1 == 1, 0.3, 0.05) # If f1=1, higher chance of A
  data.imbalanced[, target := factor(rbinom(n, 1, prob.A), labels = c("B", "A"))] # A is rare-ish
  task.imbalanced <- as_task_classif(data.imbalanced, target = "target", id = "gosdt_imbalance")

  learner.unbalanced <- LearnerClassifGosdt$new()
  learner.unbalanced$param_set$set_values(
    regularization = 0.01,
    allow_small_reg = TRUE,
    balance = FALSE # Default
  )
  learner.unbalanced$train(task.imbalanced)
  pred.unbalanced <- learner.unbalanced$predict(task.imbalanced)
  acc.unbalanced <- pred.unbalanced$score(msr("classif.acc"))
  bacc.unbalanced <- pred.unbalanced$score(msr("classif.bacc"))

  learner.balanced <- LearnerClassifGosdt$new()
  learner.balanced$param_set$set_values(
    regularization = 0.01,
    allow_small_reg = TRUE,
    balance = TRUE
  )
  learner.balanced$train(task.imbalanced)
  pred.balanced <- learner.balanced$predict(task.imbalanced)
  acc.balanced <- pred.balanced$score(msr("classif.acc"))
  bacc.balanced <- pred.balanced$score(msr("classif.bacc"))

  # Expect balanced model to have better BACC, potentially at the cost of ACC
  expect_gt(bacc.balanced, bacc.unbalanced)
  expect_lt(acc.balanced, acc.unbalanced)
})


test_that("LearnerClassifGosdt 'depth_budget' adherence", {
  set.seed(112)
  # XOR task, requires depth 2 for perfect classification
  # get better than 0.5 accuracy with depth 1 by imbalancing samples
  data.xor <- data.table(f1 = c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1), f2 = c(0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1))
  data.xor[, target := factor(f1 != f2, labels = c("false", "true"))] # XOR
  task.xor <- as_task_classif(data.xor, target = "target", id = "gosdt_xor_depth")

  common.params <- list(regularization = 0, allow_small_reg = TRUE)

  # Test depth_budget = 0 (root node only)
  learner.d0 <- LearnerClassifGosdt$new()
  learner.d0$param_set$set_values(.values = common.params)
  learner.d0$param_set$values$depth_budget <- 0L
  learner.d0$train(task.xor)
  tree.d0 <- learner.d0$model$party.list$tree
  expect_s3_class(tree.d0, "partynode")
  expect_true(partykit::is.terminal(tree.d0))

  # Test depth_budget = 1
  learner.d1 <- LearnerClassifGosdt$new()
  learner.d1$param_set$set_values(.values = common.params)
  learner.d1$param_set$values$depth_budget <- 1L
  learner.d1$train(task.xor)
  tree.d1 <- learner.d1$model$party.list$tree
  expect_s3_class(tree.d1, "partynode")
  expect_false(partykit::is.terminal(tree.d1))
  expect_length(partykit::nodeids(tree.d1), 3L)  # 1 split node, 2 terminal nodes

  # Test depth_budget = 2 (optimal for XOR)
  learner.d2 <- LearnerClassifGosdt$new()
  learner.d2$param_set$set_values(.values = common.params)
  learner.d2$param_set$values$depth_budget <- 2L
  learner.d2$train(task.xor)
  tree.d2 <- learner.d2$model$party.list$tree
  expect_s3_class(tree.d2, "partynode")
  expect_false(partykit::is.terminal(tree.d2))
  expect_length(partykit::nodeids(tree.d2), 7L)  # 3 split nodes, 4 terminal nodes
  pred.d2 <- learner.d2$predict(task.xor)
  expect_identical(unname(pred.d2$score(msr("classif.ce"))), 0)


  # Test depth_budget = 3 (more than needed for XOR)
  learner.d3 <- LearnerClassifGosdt$new()
  learner.d3$param_set$set_values(.values = common.params)
  learner.d3$param_set$values$depth_budget <- 3L
  learner.d3$train(task.xor)
  tree.d3 <- learner.d3$model$party.list$tree
  expect_s3_class(tree.d3, "partynode")
  # Should still find the optimal depth 2 tree
  expect_false(partykit::is.terminal(tree.d3))
  expect_length(partykit::nodeids(tree.d3), 7L)  # 3 split nodes, 4 terminal nodes

  # Test depth_budget not set (NULL -> unlimited in GOSDT Python)
  learner.dn <- LearnerClassifGosdt$new()
  learner.dn$param_set$set_values(.values = common.params) # depth_budget is not set
  learner.dn$train(task.xor)
  tree.dn <- learner.dn$model$party.list$tree
  expect_s3_class(tree.dn, "partynode")
  # Should find the optimal depth 2 tree for XOR
  expect_false(partykit::is.terminal(tree.dn))
  expect_length(partykit::nodeids(tree.dn), 7L)  # 3 split nodes, 4 terminal nodes

  # Test regularization
  learner.reg <- LearnerClassifGosdt$new()
  learner.reg$param_set$set_values(.values = common.params) # depth_budget is not set
  learner.reg$param_set$values$regularization <- 0.2
  learner.reg$train(task.xor)
  tree.reg <- learner.reg$model$party.list$tree
  expect_s3_class(tree.reg, "partynode")
  # Should find the optimal depth 2 tree for XOR
  expect_false(partykit::is.terminal(tree.reg))
  expect_length(partykit::nodeids(tree.reg), 3L)  # depth 1
  learner.reg$param_set$values$regularization <- 0.49
  learner.reg$train(task.xor)
  tree.reg2 <- learner.reg$model$party.list$tree
  expect_s3_class(tree.reg2, "partynode")
  expect_true(partykit::is.terminal(tree.reg2))

})

test_that("LearnerClassifGosdt handles target factor level variations", {
  set.seed(2) # Same seed as TreeFarms test for this data
  data.base <- data.table(
    feature1 = as.numeric(runif(100) > 0.5), # Reduced N for GOSDT speed
    feature2 = as.numeric(runif(100) > 0.75),
    feature3 = as.numeric(runif(100) > 0.25)
  )

  data.task1 <- copy(data.base)
  data.task1[, target := rnorm(100, 0.5 * feature1 + feature2 * (1 - feature3)) > 0.5]
  levelnames1 <- c("positive", "negative") # GOSDT gets 0 for first, 1 for second level
  data.task1[, target := factor(levelnames1[target + 1], levels = levelnames1)]
  task1 <- as_task_classif(data.task1, target = "target", id = "task_orig_levels_gosdt")

  # Task 2: Levels permuted ("negative", "positive")
  data.task2 <- copy(data.base)
  data.task2[, target_permuted := factor(data.task1$target, levels = rev(levels(data.task1$target)))]
  task2 <- as_task_classif(data.task2, target = "target_permuted", id = "task_perm_levels_gosdt")

  # Task 3: Values permuted (actual class assignments swapped, levels kept same as task1)
  data.task3 <- copy(data.base)
  original.levels.t1 <- levels(data.task1$target) # "positive", "negative"
  data.task3[, target_swapped_values := factor(
    ifelse(data.task1$target == original.levels.t1[[1]], original.levels.t1[[2]], original.levels.t1[[1]]),
    levels = original.levels.t1
  )]
  task3 <- as_task_classif(data.task3, target = "target_swapped_values", id = "task_swap_val_gosdt")

  # Common learner setup
  getLearner <- function() {
    learner <- LearnerClassifGosdt$new()
    learner$predict_type <- "prob"
    learner$param_set$set_values(
      regularization = 0.02, # Small enough reg
      allow_small_reg = TRUE  # For N=100 this reg might be < 1/N
    )
    return(learner)
  }

  learner1 <- getLearner()
  learner2 <- getLearner()
  learner3 <- getLearner()

  expect_silent(learner1$train(task1))
  expect_silent(learner2$train(task2))
  expect_silent(learner3$train(task3))

  expect_list(learner1$model)
  expect_list(learner2$model)
  expect_list(learner3$model)

  pred1 <- learner1$predict(task1)
  pred2 <- learner2$predict(task2)
  pred3 <- learner3$predict(task3)

  # Task1 levels: positive (0), negative (1)
  # Task2 levels: negative (0), positive (1)
  # The character representation of response should be the same.
  expect_identical(as.character(pred1$response), as.character(pred2$response))
  expect_identical(as.integer(pred1$response), 3L - as.integer(pred2$response))

  # Probabilities:
  # colnames(pred1$prob) = c("positive", "negative")
  # colnames(pred2$prob) = c("negative", "positive")
  # If tree is same, P(positive|x) from learner1 should be P(positive|x) from learner2.
  # So, pred1$prob[, "positive"] == pred2$prob[, "positive"]
  expect_equal(pred1$prob[, task1$class_names[1]], pred2$prob[, task2$class_names[2]], tolerance = 1e-6)
  expect_equal(pred1$prob[, task1$class_names[2]], pred2$prob[, task2$class_names[1]], tolerance = 1e-6)
  expect_identical(colnames(pred1$prob), rev(colnames(pred2$prob)))

  # Task3: values swapped, levels same as task1 ("positive", "negative")
  # So target for task3 is opposite of task1. Tree structure should be "opposite".
  # If task1 predicts "positive" for an instance, task3 should predict "negative".
  expect_true(all(as.character(pred1$response) != as.character(pred3$response)))
  expect_identical(as.integer(pred1$response), 3L - as.integer(pred3$response))

  # Probabilities for task3:
  # If task1 P("positive") = p, P("negative") = 1-p
  # Task3 should give P("positive") = 1-p, P("negative") = p
  expect_equal(pred1$prob[, task1$class_names[1]], pred3$prob[, task1$class_names[2]], tolerance = 1e-6)
  expect_equal(pred1$prob[, task1$class_names[2]], pred3$prob[, task1$class_names[1]], tolerance = 1e-6)
  expect_identical(colnames(pred1$prob), colnames(pred3$prob))
})

