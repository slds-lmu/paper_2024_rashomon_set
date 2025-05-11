test_that("LearnerClassifTreeFarms basic train and predict", {
  # Create a simple, solvable task with binary features
  set.seed(2)
  data.testing <- data.table(
    feature1 = as.numeric(runif(1000) > 0.5),
    feature2 = as.numeric(runif(1000) > 0.75),
    feature3 = as.numeric(runif(1000) > 0.25)
  )

  data.testing[, target := rnorm(1000, 0.5 * feature1 + feature2 * (1 - feature3)) > 0.5]
  levelnames <- c("0", "1")
  data.testing[, target := factor(levelnames[target + 1], levels = levelnames)]
  task <- as_task_classif(data.testing, target = "target", id = "testing")

  learner <- LearnerClassifTreeFarms$new()
  # Set to prob to ensure model$data is populated for prob prediction during predict()
  learner$predict_type <- "prob"

  # Set hyperparameters for a quick, successful train
  # Low regularization and a generous rashomon bound to find at least one tree.
  # Short time limit for CI, and suppress verbose output.
  learner$param_set$set_values(
    regularization = 0.001,
    rashomon_bound_adder = 1
  )

  # Train
  learner$train(task)

  # Check model state
  expect_false(is.null(learner$model), info = "Learner model is NULL after training.")
  expect_s3_class(learner$modelcontainer, "python.builtin.object")

  num.trees <- tryCatch({
    as.numeric(learner$tree.count)
  }, warning = function(w) {
    message(sprintf("Warning while getting tree count: %s", conditionMessage(w)))
    -1 # Indicate an issue without stopping the test
  }, error = function(e) {
    message(sprintf("Error while getting tree count: %s", conditionMessage(e)))
    -1 # Indicate an issue
  })
  expect_gte(num.trees, 0)

  if (num.trees > 0) {
    learner$param_set$values$selected_tree <- "1" # Select the first tree

    # Predict (should give both response and prob since predict_type was "prob" during train)
    prediction <- learner$predict(task)
    expect_s3_class(prediction, "PredictionClassif")

    # Check response
    expect_true("response" %in% prediction$predict_types, info = "Response not in predict_types.")
    expect_factor(prediction$response, levels = task$class_names, len = task$nrow,
      info = "Prediction response has incorrect levels, length, or type.")

    # Check probabilities
    expect_true("prob" %in% prediction$predict_types, info = "Probabilities not in predict_types.")
    expect_numeric(prediction$prob, lower = 0, upper = 1,
      info = "Prediction probabilities are not numeric or not in [0,1] range.")
    expect_equal(colnames(prediction$prob), task$class_names,
      info = "Colnames of probability matrix do not match task class_names.")
    expect_equal(rowSums(prediction$prob), rep(1, task$nrow), tolerance = 1e-6,
      info = "Row sums of probability matrix do not equal 1.")

    # response prediction is the class with the highest probability
    expect_identical(as.integer(prediction$response), apply(prediction$prob, 1, which.max))

  } else {
    warning("LearnerClassifTreeFarms did not find any trees in the basic train test. Prediction tests skipped.")
  }
})

test_that("LearnerClassifTreeFarms predict_type 'response' works", {
  # Create a simple, solvable task with binary features
  set.seed(2) # for reproducibility, different seed from previous
  data.train <- data.table::data.table(
    featureA = c(rep(0L, 10), rep(1L, 10)), # Ensure integer 0/1
    featureB = sample(0L:1L, 20, replace = TRUE) # Ensure integer 0/1
  )
  data.train[, target_col := factor(ifelse(featureA == 0L, "type_X", "type_Y"))]
  task <- mlr3::TaskClassif$new(id = "simple_treefarms_task_resp", backend = data.train, target = "target_col")

  learner <- LearnerClassifTreeFarms$new()
  learner$predict_type <- "response" # Explicitly set to response

  # Set hyperparameters for a quick, successful train
  learner$param_set$set_values(
    regularization = 0.001,
    rashomon_bound_adder = 1 # Ensure some non-zero bound if regularization is tiny
  )

  # Train
  learner$train(task)

  # Check model state
  expect_false(is.null(learner$model), info = "Learner model is NULL after training.")
  expect_s3_class(learner$modelcontainer, "python.builtin.object")

  num.trees <- tryCatch({
    as.numeric(learner$tree.count)
  }, warning = function(w) {
    message(sprintf("Warning while getting tree count: %s", conditionMessage(w)))
    -1 # Indicate an issue without stopping the test
  }, error = function(e) {
    message(sprintf("Error while getting tree count: %s", conditionMessage(e)))
    -1 # Indicate an issue
  })
  expect_gte(num.trees, 0)

  if (num.trees > 0) {
    # selected_tree defaults to "1", so no need to set it explicitly if we want the first tree.
    # learner$param_set$values$selected_tree <- "1"

    # Predict
    prediction <- learner$predict(task)
    expect_s3_class(prediction, "PredictionClassif")

    # Check response
    expect_equal(prediction$predict_types, "response", info = "Predict_types should only contain 'response'.")
    expect_factor(prediction$response, levels = task$class_names, len = task$nrow,
      info = "Prediction response has incorrect levels, length, or type.")

    # Check that probabilities are not present
    expect_null(prediction$prob, info = "Prediction probabilities should be NULL when predict_type is 'response'.")

  } else {
    warning("LearnerClassifTreeFarms did not find any trees in the 'response' predict_type test. Prediction tests skipped.")
  }
})

test_that("LearnerClassifTreeFarms handles target factor level variations", {

  set.seed(2)
  data.base <- data.table(
    feature1 = as.numeric(runif(1000) > 0.5),
    feature2 = as.numeric(runif(1000) > 0.75),
    feature3 = as.numeric(runif(1000) > 0.25)
  )

  data.task1 <- copy(data.base)
  data.task1[, target := rnorm(1000, 0.5 * feature1 + feature2 * (1 - feature3)) > 0.5]
  levelnames <- c("positive", "negative")
  data.task1[, target := factor(levelnames[target + 1], levels = levelnames)]
  task1 <- as_task_classif(data.task1, target = "target", id = "task_original_levels")

  # Task 2: Levels permuted (e.g., "positive" becomes level 1, "negative" level 2)
  data.task2 <- copy(data.base)
  data.task2[, target_permuted := factor(data.task1$target, levels = rev(levels(data.task1$target)))]
  task2 <- as_task_classif(data.task2, target = "target_permuted", id = "task_permuted_levels")

  # Task 3: Values permuted (actual class assignments swapped, levels kept)
  data.task3 <- copy(data.base)
  original.levels <- levels(data.task1$target)
  data.task3[, target_swapped_values := factor(
    ifelse(data.task1$target == original.levels[[1]], original.levels[[2]], original.levels[[1]]),
    levels = original.levels
  )]
  task3 <- as_task_classif(data.task3, target = "target_swapped_values", id = "task_swapped_values")

  # Common learner setup
  getLearner <- function() {
    learner <- LearnerClassifTreeFarms$new()
    learner$predict_type <- "prob"
    learner$param_set$set_values(
      regularization = 0.01,
      rashomon_bound_multiplier = 1
    )
    return(learner)
  }

  learner1 <- getLearner()
  learner2 <- getLearner()
  learner3 <- getLearner()

  learner1$train(task1)
  learner2$train(task2)
  learner3$train(task3)

  # Check if all learners found trees
  num.trees1 <- as.numeric(learner1$tree.count)
  num.trees2 <- as.numeric(learner2$tree.count)
  num.trees3 <- as.numeric(learner3$tree.count)

  expect_gt(num.trees1, 0)
  expect_gt(num.trees2, 0)
  expect_gt(num.trees3, 0)

  if (num.trees1 == 0 || num.trees2 == 0 || num.trees3 == 0) {
    skip("One of the learners did not find trees, skipping detailed prediction checks.")
  }

  expect_identical(num.trees1, num.trees2)
  expect_identical(num.trees1, num.trees3)

  pred1 <- learner1$predict(task1)
  pred2 <- learner2$predict(task2)
  pred3 <- learner3$predict(task3)

  # Predictions for task2 should be the 'same' as task1 due to level permutation
  # e.g. if task1 predicts "positive", task2 should predict "positive" for the same row,
  # but "positive" for task2 is level 1, while "negative" for task1 is level 1.
  # So the integer representation of the factor should be the opposite
  expect_identical(as.integer(pred1$response), 3L - as.integer(pred2$response))
  expect_identical(as.character(pred1$response), as.character(pred2$response))
  # Columns of probability matrix for task2 should be reversed compared to task1
  expect_equal(pred1$prob[, 1], pred2$prob[, 2], tolerance = 1e-6,
    info = "Probabilities for first class of task1 do not match second class of task2.")
  expect_equal(pred1$prob[, 2], pred2$prob[, 1], tolerance = 1e-6,
    info = "Probabilities for second class of task1 do not match first class of task2.")
  expect_identical(colnames(pred1$prob), rev(colnames(pred2$prob)))

  # task3 has swapped codes with same lavels as task1, so prediction codes are the same as in pred2, but labels
  # are swapped
  expect_identical(as.integer(pred2$response), as.integer(pred3$response))
  expect_true(all(pred3$response != pred1$response))

  # Probabilities for task3 should also be 'swapped' relative to task1
  # If task1 P(class1=neg)=0.8, P(class2=pos)=0.2
  # task3 for same instance would be P(class1=neg)=0.2, P(class2=pos)=0.8
  expect_equal(pred1$prob[, 1], pred3$prob[, 2], tolerance = 1e-6,
    info = "Probabilities for first class of task1 do not match second class of task3.")
  expect_equal(pred1$prob[, 2], pred3$prob[, 1], tolerance = 1e-6,
    info = "Probabilities for second class of task1 do not match first class of task3.")
  expect_identical(colnames(pred1$prob), colnames(pred3$prob))

})

test_that("LearnerClassifTreeFarms classify() method matches predict() response", {

  set.seed(2)
  data.testing <- data.table(
    feature1 = as.numeric(runif(1000) > 0.5),
    feature2 = as.numeric(runif(1000) > 0.75),
    feature3 = as.numeric(runif(1000) > 0.25)
  )

  data.testing[, target := rnorm(1000, 0.5 * feature1 + feature2 * (1 - feature3)) > 0.5]
  levelnames <- c("0", "1")
  data.testing[, target := factor(levelnames[target + 1], levels = levelnames)]
  task <- as_task_classif(data.testing, target = "target", id = "testing")

  learner <- LearnerClassifTreeFarms$new()
  # predict_type = "response" is sufficient as we only compare response
  learner$predict_type <- "response"
  learner$param_set$set_values(
    regularization = 0.001,
    rashomon_bound_adder = 1
  )

  learner$train(task)

  num.trees <- as.numeric(learner$tree.count)
  expect_gt(num.trees, 0)

  if (num.trees > 0) {
    # Predict on a subset of the task (e.g., first 10 rows)
    prediction.subset.indices <- 1:10
    task.subset <- task$clone()$filter(prediction.subset.indices)

    standard.prediction <- learner$predict(task.subset)
    expect_s3_class(standard.prediction, "PredictionClassif")

    # Get the first tree from the model container (Python is 0-indexed)
    first.tree.model <- learner$modelcontainer$get_tree_at_idx(0)

    target.levels <- learner$model$target.levels

    for (i in seq_along(prediction.subset.indices)) {
      row.idx <- prediction.subset.indices[i]
      # Extract features for the current row as a numeric vector
      # The python side expects a list/vector of numerics
      current.features.dt <- task$data(rows = row.idx, cols = task$feature_names)
      current.features.vec <- as.numeric(current.features.dt[1, ])


      # Call classify() on the tree model
      # classify() returns a Python object (e.g., py_int) which needs conversion
      r.classify.pred.value <- first.tree.model$classify(current.features.vec)[[1]] # Should be 0 or 1

      # Convert 0/1 to factor with task's levels
      # TreeFarms maps first level to 0, second to 1 internally
      manual.factor.pred <- factor(r.classify.pred.value, levels = c(0L, 1L), labels = target.levels)

      # Compare with the standard prediction's response
      expect_equal(
        as.character(manual.factor.pred), # Compare as character to avoid factor level mismatch issues if any
        as.character(standard.prediction$response[i]),
        info = sprintf("Mismatch between classify() and predict() for row %d (task row_id %s)", i, task$row_ids[row.idx])
      )
    }
  } else {
    warning("LearnerClassifTreeFarms did not find any trees in classify() method test. Detailed checks skipped.")
  }
})

test_that("LearnerClassifTreeFarms get_tree_metric_at_idx()$loss matches classif.ce", {

  set.seed(2)
  data.testing <- data.table(
    feature1 = as.numeric(runif(1000) > 0.5),
    feature2 = as.numeric(runif(1000) > 0.75),
    feature3 = as.numeric(runif(1000) > 0.25)
  )

  data.testing[, target := rnorm(1000, 0.5 * feature1 + feature2 * (1 - feature3)) > 0.5]
  levelnames <- c("0", "1")
  data.testing[, target := factor(levelnames[target + 1], levels = levelnames)]
  task <- as_task_classif(data.testing, target = "target", id = "testing")

  learner <- LearnerClassifTreeFarms$new()
  learner$predict_type <- "response" # CE is based on response
  learner$param_set$set_values(
    regularization = 0, # Allow a few trees to be found
    rashomon_bound_adder = 1 # get basically all trees
  )

  learner$train(task)

  num.trees <- as.numeric(learner$tree.count)
  expect_gt(num.trees, 0)

  if (num.trees > 0) {
    # Iterate through each tree found
    for (tree.idx.r in seq_len(num.trees)) {
      # Get loss from the model container (Python is 0-indexed for get_tree_metric_at_idx)
      # The loss reported by treefarms is equivalent to misclassification error
      tree.metric <- learner$modelcontainer$get_tree_metric_at_idx(tree.idx.r - 1)
      reported.loss <- tree.metric$loss

      # Set the learner to predict with the current tree
      learner$param_set$values$selected_tree <- as.character(tree.idx.r)

      # Predict on the training set
      prediction.train <- learner$predict(task)

      # Calculate classification error (ce)
      calculated.ce <- prediction.train$score(msr("classif.ce"))

      # TODO: the following does sometimes not hold (e.g. regularization = 0.005, tree.idx.r = 2); I wonder whether
      # this is a bug in the package.
      expect_equal(
        unname(reported.loss),
        unname(calculated.ce),
        tolerance = 1e-6, # Add a small tolerance for potential floating point inaccuracies
        info = sprintf("Reported loss for tree %d (%f) does not match calculated CE (%f) on training data.",
                       tree.idx.r, reported.loss, calculated.ce)
      )
    }
  } else {
    warning("LearnerClassifTreeFarms did not find any trees in get_tree_metric_at_idx test. Checks skipped.")
  }
})

test_that("LearnerClassifTreeFarms learns a simple logical rule perfectly", {

  data.train <- CJ(
    f1 = 0:1,
    f2 = 0:1,
    f3 = 0:1
  )
  # Target: f1 OR (f2 AND f3)
  data.train[, target_logic := factor(
    fifelse(f1 == 1L | (f2 == 1L & f3 == 1L), "TRUE_CLASS", "FALSE_CLASS")
  )]
  task <- mlr3::TaskClassif$new(id = "task_logical_rule", backend = data.train, target = "target_logic")

  learner <- LearnerClassifTreeFarms$new()
  learner$predict_type <- "prob" # Need probs for checking 0/1
  learner$param_set$set_values(
    regularization = 0,
    rashomon_bound_adder = 0.00001  # value of 0 gives 0 trees because of the way TreeFarms handles passing of config
  )
  learner$train(task)
  learner$tree.count

  num.trees <- as.numeric(learner$tree.count)

  expect_gt(num.trees, 0)

  if (num.trees > 0) {
    # Default selected_tree is "1", which should be the optimal one when rashomon = FALSE
    for (tree.idx in seq_len(num.trees)) {
      learner$param_set$values$selected_tree <- as.character(tree.idx)
      prediction <- learner$predict(task)

      # Check for perfect accuracy on the training set
      expect_equal(
        unname(prediction$score(msr("classif.acc"))), 1,
        info = "Learner did not achieve 100% accuracy on the perfectly learnable logical task."
      )

      # Check that probabilities are deterministic (0 or 1) and correct
      expected.prob.matrix <- matrix(0, ncol = 2, nrow = task$nrow)
      expected.prob.matrix[cbind(seq_len(task$nrow), ifelse(data.train$target_logic == task$class_names[1], 1, 2))] <- 1
      expect_equal(unname(prediction$prob), expected.prob.matrix, tolerance = 1e-6)
    }
  } else {
    warning("LearnerClassifTreeFarms did not find any trees in the logical rule test. Checks skipped.")
  }
})

expectTreeSubset <- function(learner1, learner2) {
  num.trees1 <- as.numeric(learner1$tree.count)
  expect_gt(num.trees1, 0)

  num.trees2 <- as.numeric(learner2$tree.count)
  expect_gt(num.trees2, 0)

  if (num.trees1 > 0 && num.trees2 > 0) {
    # Expect more trees with a larger bound
    # (if this fails, the bounds need to be adjusted to being further apart)
    expect_gt(num.trees2, num.trees1)

    # Collect JSON representations of trees for learner1
    json.trees1 <- vapply(seq_len(num.trees1), function(i) {
      # Using R integer for indexing
      learner1$modelcontainer$get_tree_at_idx(i - 1)$json()
    }, character(1))

    # Collect JSON representations of trees for learner2
    json.trees2 <- vapply(seq_len(num.trees2), function(i) {
      learner2$modelcontainer$get_tree_at_idx(i - 1)$json()
    }, character(1))

    expect_identical(anyDuplicated(json.trees1), 0L)
    expect_identical(anyDuplicated(json.trees2), 0L)

    expect_true(all(json.trees1 %in% json.trees2))

  } else {
    warning("One of the learners in rashomon_bound_adder test did not find trees. Superset check skipped.")
  }
}

test_that("LearnerClassifTreeFarms rashomon_bound_adder increases tree count and forms superset", {

  set.seed(2)
  data.testing <- data.table(
    feature1 = as.numeric(runif(1000) > 0.5),
    feature2 = as.numeric(runif(1000) > 0.75),
    feature3 = as.numeric(runif(1000) > 0.25)
  )

  data.testing[, target := rnorm(1000, 0.5 * feature1 + feature2 * (1 - feature3)) > 0.5]
  levelnames <- c("0", "1")
  data.testing[, target := factor(levelnames[target + 1], levels = levelnames)]
  task <- as_task_classif(data.testing, target = "target", id = "testing")

  learner.base <- LearnerClassifTreeFarms$new()
  learner.base$param_set$set_values(
    regularization = 0.01 # Small regularization
  )

  # Train with a small rashomon_bound_adder
  learner1 <- learner.base$clone(deep = TRUE)
  learner1$param_set$set_values(rashomon_bound_adder = 0.02)
  learner1$train(task)

  # Train with a larger rashomon_bound_adder
  learner2 <- learner.base$clone(deep = TRUE)
  learner2$param_set$set_values(rashomon_bound_adder = 0.2) # Significantly larger
  learner2$train(task)

  expectTreeSubset(learner1, learner2)
})


test_that("LearnerClassifTreeFarms rashomon_bound_multiplier increases tree count and forms superset", {

  set.seed(2)
  data.testing <- data.table(
    feature1 = as.numeric(runif(1000) > 0.5),
    feature2 = as.numeric(runif(1000) > 0.75),
    feature3 = as.numeric(runif(1000) > 0.25)
  )

  data.testing[, target := rnorm(1000, 0.5 * feature1 + feature2 * (1 - feature3)) > 0.5]
  levelnames <- c("0", "1")
  data.testing[, target := factor(levelnames[target + 1], levels = levelnames)]
  task <- as_task_classif(data.testing, target = "target", id = "testing")

  learner.base <- LearnerClassifTreeFarms$new()
  learner.base$param_set$set_values(
    regularization = 0.01 # Small regularization
  )

  # Train with a small rashomon_bound_multiplier
  learner1 <- learner.base$clone(deep = TRUE)
  learner1$param_set$set_values(rashomon_bound_multiplier = 0.03)
  learner1$train(task)

  # Train with a larger rashomon_bound_multiplier
  learner2 <- learner.base$clone(deep = TRUE)
  learner2$param_set$set_values(rashomon_bound_multiplier = 0.2) # Significantly larger
  learner2$train(task)

  expectTreeSubset(learner1, learner2)
})

test_that("LearnerClassifTreeFarms rashomon_bound increases tree count and forms superset", {

  set.seed(2)
  data.testing <- data.table(
    feature1 = as.numeric(runif(1000) > 0.5),
    feature2 = as.numeric(runif(1000) > 0.75),
    feature3 = as.numeric(runif(1000) > 0.25)
  )

  data.testing[, target := rnorm(1000, 0.5 * feature1 + feature2 * (1 - feature3)) > 0.5]
  levelnames <- c("0", "1")
  data.testing[, target := factor(levelnames[target + 1], levels = levelnames)]
  task <- as_task_classif(data.testing, target = "target", id = "testing")

  learner.base <- LearnerClassifTreeFarms$new()
  learner.base$param_set$set_values(
    regularization = 0.01 # Small regularization
  )

  # Train with a small rashomon_bound
  learner1 <- learner.base$clone(deep = TRUE)
  learner1$param_set$set_values(rashomon_bound = 0.45)
  learner1$train(task)

  # Train with a larger rashomon_bound
  learner2 <- learner.base$clone(deep = TRUE)
  learner2$param_set$set_values(rashomon_bound = 0.5) # Significantly larger
  learner2$train(task)

  expectTreeSubset(learner1, learner2)
})

test_that("LearnerClassifTreeFarms tree.count active binding works correctly", {
  learner <- LearnerClassifTreeFarms$new()

  # Before training
  expect_null(learner$modelcontainer, info = "Model container should be NULL before training.")
  expect_identical(learner$tree.count, "0", info = "tree.count should be '0' before training.")

  # Setup task and train
  set.seed(8)
  data.train <- data.table::data.table(
    f1 = sample(0L:1L, 20, replace = TRUE),
    f2 = sample(0L:1L, 20, replace = TRUE)
  )
  data.train[, target_col := factor(ifelse(f1 == 1L, "A", "B"))]
  task <- mlr3::TaskClassif$new(id = "task_tree_count", backend = data.train, target = "target_col")

  learner$param_set$set_values(
    regularization = 0.01,
    rashomon_bound_adder = 0.1 # Encourage a few trees
  )
  learner$train(task)

  # After training
  expect_false(is.null(learner$modelcontainer), info = "Model container should not be NULL after training.")

  tree.count.string <- learner$tree.count
  expect_character(tree.count.string, len = 1, info = "tree.count should return a character string.")
  expect_true(grepl("^[0-9]+$", tree.count.string), info = "tree.count string should represent a non-negative integer.")

  num.trees.from.binding <- as.integer(tree.count.string)
  expect_gte(num.trees.from.binding, 0)

  # Compare with direct call to modelcontainer's method
  num.trees.from.container <- learner$modelcontainer$get_tree_count()
  expect_identical(num.trees.from.binding, num.trees.from.container,
                info = "tree.count from binding does not match get_tree_count from modelcontainer.")

})

test_that("LearnerClassifTreeFarms sampleTreeIndex method works", {
  learner <- LearnerClassifTreeFarms$new()
  expect_error(learner$sampleTreeIndex(), "Model was not fitted.")

  # Use a task that should generate a small, manageable number of trees
  set.seed(9)
  data.train <- data.table::data.table(
    f1 = c(1, 1, 0, 0, 1, 0),
    f2 = c(1, 0, 0, 1, 0, 1),
    f3 = c(0, 1, 0, 0, 1, 1)
  )
  data.train[, target_col := factor(ifelse(f1 == 1L | (f2 == 1L & f3 == 1L), "CLASS_X", "CLASS_Y"))]
  task <- mlr3::TaskClassif$new(id = "task_sample_index", backend = data.train, target = "target_col")

  learner$param_set$set_values(
    regularization = 0.001,
    rashomon_bound_adder = 1
  )
  learner$train(task)

  actual.tree.count <- as.integer(learner$tree.count)

  # Single sample check
  sampled.char <- learner$sampleTreeIndex()
  expect_character(sampled.char, len = 1, info = "sampleTreeIndex should return a character string.")

  sampled.int <- as.integer(sampled.char)
  # This tests the prompt's expectation: "samples in [1, tree.count]"
  # If sampleTreeIndex can return "0", this expect_gte will fail.
  expect_gte(sampled.int, 1)
  expect_lte(sampled.int, actual.tree.count)

  # Approximate uniformity / coverage check
  n.samples <- actual.tree.count * 50 # Sample many times
  sampled.indices.int <- integer(n.samples)
  all.sampled.chars.valid <- TRUE
  tree.samples <- replicate(n.samples, learner$sampleTreeIndex())
  expect_character(tree.samples, len = n.samples, pattern = "^[1-9][0-9]*$",
    info = "sampleTreeIndex should return a character string.")
  tree.samples.int <- as.integer(tree.samples)
  expect_integer(tree.samples.int, len = n.samples, lower = 1, upper = actual.tree.count,
    info = "sampleTreeIndex should return an integer between 1 and tree.count.")

  # Check if all trees from 1 to actual.tree.count were sampled at least once
  unique.sampled <- unique(tree.samples.int)
  expected.indices <- seq_len(actual.tree.count)
  missing.indices <- setdiff(expected.indices, unique.sampled)

  expect_true(length(missing.indices) == 0,
    info = sprintf(
      "Not all tree indices were sampled after %d attempts. Missing: %s. Tree count: %d. Unique sampled: %s",
      n.samples, toString(missing.indices), actual.tree.count, toString(sort(unique.sampled))
  ))

})

