test_that("LearnerRegrKMExtraConjoined initialization works", {
  # Create a conjoined parameter set
  ps.a <- ps(xa1 = p_dbl(0, 1))
  ps.b <- ps(xb1 = p_dbl(0, 1), xb2 = p_dbl(0, 1))
  ps.conjoined <- conjoinSpaces(list(a = ps.a, b = ps.b))

  learner.conjoined <- LearnerRegrKMExtraConjoined$new(ps.conjoined)

  expect_class(learner.conjoined, "LearnerRegrKMExtraConjoined")
  expect_class(learner.conjoined$baselearner, "LearnerRegrKMExtra")
  expect_identical(learner.conjoined$id, "regr.km.extra.conjoined")
  expect_set_equal(learner.conjoined$packages, c("DiceKriging", "mlr3"))
})

test_that("LearnerRegrKMExtraConjoined predictions match individual models", {
  # Create parameter spaces
  ps.a <- ps(xa1 = p_dbl(0, 1))
  ps.b <- ps(xb1 = p_dbl(0, 1), xb2 = p_dbl(0, 1))
  ps.conjoined <- conjoinSpaces(list(a = ps.a, b = ps.b))

  # Create training data
  set.seed(2)
  n.per.space <- 10

  # Space A data: 1D sine wave
  data.a <- data.table(
    subspace = "a",
    a.xa1 = runif(n.per.space),
    b.xb1 = NA_real_,
    b.xb2 = NA_real_,
    y = NA_real_
  )
  data.a[, y := sin(2 * pi * a.xa1)]

  # Space B data: 2D sine waves
  data.b <- data.table(
    subspace = "b",
    a.xa1 = NA_real_,
    b.xb1 = runif(n.per.space),
    b.xb2 = runif(n.per.space),
    y = NA_real_
  )
  data.b[, y := sin(2 * pi * b.xb1) * cos(2 * pi * b.xb2)]

  data.train <- rbind(data.a, data.b)[sample.int(nrow(data.a) + nrow(data.b))]
  data.train[, subspace := factor(subspace, levels = c("a", "b"))]
  task.train <- TaskRegr$new("conjoined", backend = data.train, target = "y")

  # Create test data
  test.grid.1d <- seq(0, 1, length.out = 20)
  test.grid.2d <- expand.grid(b.xb1 = test.grid.1d, b.xb2 = test.grid.1d)

  data.test.a <- data.table(
    subspace = "a",
    a.xa1 = test.grid.1d,
    b.xb1 = NA_real_,
    b.xb2 = NA_real_,
    y = sin(2 * pi * test.grid.1d)
  )

  data.test.b <- data.table(
    subspace = "b",
    a.xa1 = NA_real_,
    b.xb1 = test.grid.2d$b.xb1,
    b.xb2 = test.grid.2d$b.xb2,
    y = sin(2 * pi * test.grid.2d$b.xb1) * cos(2 * pi * test.grid.2d$b.xb2)
  )

  data.test <- rbind(data.test.a, data.test.b)
  data.test[, subspace := factor(subspace, levels = c("a", "b"))]
  arows <- sample.int(nrow(data.test)) <= nrow(data.test.a)
  reorder <- numeric(nrow(data.test))
  reorder[arows] <- seq_len(nrow(data.test.a))
  reorder[!arows] <- seq_len(nrow(data.test.b)) + nrow(data.test.a)
  data.test <- data.test[reorder]
  task.test <- TaskRegr$new("conjoined_test", backend = data.test, target = "y")

  # Train individual models for comparison
  learner.a <- LearnerRegrKMExtra$new()
  learner.a$param_set$values$nugget.estim <- TRUE
  learner.a$param_set$values$optim.method <- "BFGS"
  learner.a$param_set$values$multistart <- 20
  learner.a$predict_type <- "se"
  learner.a$encapsulate("evaluate", lrn("regr.featureless", predict_type = "se"))
  task.train.a <- TaskRegr$new(
    "space_a",
    backend = data.train[subspace == "a", .(x = a.xa1, y)],
    target = "y"
  )
  task.test.a <- TaskRegr$new(
    "space_a_test",
    backend = data.test[subspace == "a", .(x = a.xa1, y)],
    target = "y"
  )
  learner.a$train(task.train.a)

  learner.b <- LearnerRegrKMExtra$new()
  learner.b$param_set$values$nugget.estim <- TRUE
  learner.b$param_set$values$optim.method <- "BFGS"
  learner.b$param_set$values$multistart <- 20
  learner.b$predict_type <- "se"
  learner.b$encapsulate("evaluate", lrn("regr.featureless", predict_type = "se"))
  task.train.b <- TaskRegr$new(
    "space_b",
    backend = data.train[subspace == "b", .(x1 = b.xb1, x2 = b.xb2, y)],
    target = "y"
  )
  task.test.b <- TaskRegr$new(
    "space_b_test",
    backend = data.test[subspace == "b", .(x1 = b.xb1, x2 = b.xb2, y)],
    target = "y"
  )
  learner.b$train(task.train.b)

  # Train conjoined model
  learner.conjoined <- LearnerRegrKMExtraConjoined$new(ps.conjoined)
  learner.conjoined$param_set$values$nugget.estim <- TRUE
  learner.conjoined$param_set$values$optim.method <- "BFGS"
  learner.conjoined$param_set$values$multistart <- 20
  learner.conjoined$predict_type <- "se"
  learner.conjoined$encapsulate("evaluate", lrn("regr.featureless", predict_type = "se"))
  learner.conjoined$train(task.train)

  # Get predictions
  pred.conjoined <- learner.conjoined$predict(task.test)
  pred.a <- learner.a$predict(task.test.a)
  pred.b <- learner.b$predict(task.test.b)

  # Check that predictions match for space A
  expect_equal(
    pred.conjoined$response[data.test$subspace == "a"],
    pred.a$response,
    tolerance = 1e-6
  )

  # Check that predictions match for space B
  expect_equal(
    pred.conjoined$response[data.test$subspace == "b"],
    pred.b$response,
    tolerance = 1e-6
  )

  # Check that SE predictions match for space A
  expect_equal(
    pred.conjoined$se[data.test$subspace == "a"],
    pred.a$se,
    tolerance = 1e-6
  )

  # Check that SE predictions match for space B
  expect_equal(
    pred.conjoined$se[data.test$subspace == "b"],
    pred.b$se,
    tolerance = 1e-6
  )
})

test_that("LearnerRegrKMExtraConjoined conditional SE predictions match individual models", {
  # Create parameter spaces
  ps.a <- ps(xa1 = p_dbl(0, 1))
  ps.b <- ps(xb1 = p_dbl(0, 1), xb2 = p_dbl(0, 1))
  ps.conjoined <- conjoinSpaces(list(a = ps.a, b = ps.b))

  # Create training data (similar to previous test)
  set.seed(2)
  n.per.space <- 10

  data.a <- data.table(
    subspace = "a",
    a.xa1 = runif(n.per.space),
    b.xb1 = NA_real_,
    b.xb2 = NA_real_,
    y = NA_real_
  )
  data.a[, y := sin(2 * pi * a.xa1)]

  data.b <- data.table(
    subspace = "b",
    a.xa1 = NA_real_,
    b.xb1 = runif(n.per.space),
    b.xb2 = runif(n.per.space),
    y = NA_real_
  )
  data.b[, y := sin(2 * pi * b.xb1) * cos(2 * pi * b.xb2)]

  data.train <- rbind(data.a, data.b)[sample.int(nrow(data.a) + nrow(data.b))]
  data.train[, subspace := factor(subspace, levels = c("a", "b"))]
  task.train <- TaskRegr$new("conjoined", backend = data.train, target = "y")

  # Create test points and new points for conditional SE
  test.grid.1d <- seq(0, 1, length.out = 20)
  test.grid.2d <- expand.grid(b.xb1 = test.grid.1d[1:5], b.xb2 = test.grid.1d[1:5])

  data.test.a <- data.table(
    subspace = "a",
    a.xa1 = test.grid.1d[1:5],
    b.xb1 = NA_real_,
    b.xb2 = NA_real_,
    y = sin(2 * pi * test.grid.1d[1:5])
  )

  data.test.b <- data.table(
    subspace = "b",
    a.xa1 = NA_real_,
    b.xb1 = test.grid.2d$b.xb1,
    b.xb2 = test.grid.2d$b.xb2,
    y = sin(2 * pi * test.grid.2d$b.xb1) * cos(2 * pi * test.grid.2d$b.xb2)
  )

  data.test <- rbind(data.test.a, data.test.b)
  data.test[, subspace := factor(subspace, levels = c("a", "b"))]
  arows <- sample.int(nrow(data.test)) <= nrow(data.test.a)
  reorder <- numeric(nrow(data.test))
  reorder[arows] <- seq_len(nrow(data.test.a))
  reorder[!arows] <- seq_len(nrow(data.test.b)) + nrow(data.test.a)
  data.test <- data.test[reorder]
  task.test <- TaskRegr$new("conjoined_test", backend = data.test, target = "y")

  # Create new points for conditional SE
  new.points.a <- data.table(
    subspace = "a",
    a.xa1 = c(0.3, 0.7),
    b.xb1 = NA_real_,
    b.xb2 = NA_real_,
    y = sin(2 * pi * c(0.3, 0.7))
  )

  new.points.b <- data.table(
    subspace = "b",
    a.xa1 = NA_real_,
    b.xb1 = c(0.2, 0.8),
    b.xb2 = c(0.4, 0.6),
    y = sin(2 * pi * c(0.2, 0.8)) * cos(2 * pi * c(0.4, 0.6))
  )

  new.points <- rbind(new.points.a, new.points.b)
  new.points[, subspace := factor(subspace, levels = c("a", "b"))]
  task.new <- TaskRegr$new("new_points", backend = new.points, target = "y")

  # Train individual models
  learner.a <- LearnerRegrKMExtra$new()
  learner.a$param_set$values$nugget.estim <- TRUE
  learner.a$param_set$values$optim.method <- "BFGS"
  learner.a$param_set$values$multistart <- 20
  learner.a$predict_type <- "se"
  learner.a$encapsulate("evaluate", lrn("regr.featureless", predict_type = "se"))
  task.train.a <- TaskRegr$new(
    "space_a",
    backend = data.train[subspace == "a", .(x = a.xa1, y)],
    target = "y"
  )
  task.test.a <- TaskRegr$new(
    "space_a_test",
    backend = data.test[subspace == "a", .(x = a.xa1, y)],
    target = "y"
  )
  task.new.a <- TaskRegr$new(
    "space_a_new",
    backend = new.points[subspace == "a", .(x = a.xa1, y)],
    target = "y"
  )
  learner.a$train(task.train.a)

  learner.b <- LearnerRegrKMExtra$new()
  learner.b$param_set$values$nugget.estim <- TRUE
  learner.b$param_set$values$optim.method <- "BFGS"
  learner.b$param_set$values$multistart <- 20
  learner.b$predict_type <- "se"
  learner.b$encapsulate("evaluate", lrn("regr.featureless", predict_type = "se"))
  task.train.b <- TaskRegr$new(
    "space_b",
    backend = data.train[subspace == "b", .(x1 = b.xb1, x2 = b.xb2, y)],
    target = "y"
  )
  task.test.b <- TaskRegr$new(
    "space_b_test",
    backend = data.test[subspace == "b", .(x1 = b.xb1, x2 = b.xb2, y)],
    target = "y"
  )
  task.new.b <- TaskRegr$new(
    "space_b_new",
    backend = new.points[subspace == "b", .(x1 = b.xb1, x2 = b.xb2, y)],
    target = "y"
  )
  learner.b$train(task.train.b)

  # Train conjoined model
  learner.conjoined <- LearnerRegrKMExtraConjoined$new(ps.conjoined)
  learner.conjoined$param_set$values$nugget.estim <- TRUE
  learner.conjoined$param_set$values$optim.method <- "BFGS"
  learner.conjoined$param_set$values$multistart <- 20
  learner.conjoined$predict_type <- "se"
  learner.conjoined$encapsulate("evaluate", lrn("regr.featureless", predict_type = "se"))
  learner.conjoined$train(task.train)

  # Get conditional SE predictions
  cond.se.conjoined <- learner.conjoined$predictConditionalSE(task.new, task.test)
  cond.se.a <- learner.a$predictConditionalSE(task.new.a, task.test.a)
  cond.se.b <- learner.b$predictConditionalSE(task.new.b, task.test.b)

  # Check that conditional SE predictions match for space A
  # We only compare the relevant parts of the matrix
  expect_equal(
    unname(cond.se.conjoined[data.test$subspace == "a", new.points$subspace == "a"]),
    unname(cond.se.a),
    tolerance = 1e-6
  )

  # Check that conditional SE predictions match for space B
  expect_equal(
    unname(cond.se.conjoined[data.test$subspace == "b", new.points$subspace == "b"]),
    unname(cond.se.b),
    tolerance = 1e-6
  )

  # Check that cross-space conditional SEs are unchanged
  # When conditioning on space A points, space B predictions should be unchanged
  expect_equal(
    unname(cond.se.conjoined[data.test$subspace == "b", new.points$subspace == "a"]),
    matrix(learner.conjoined$predict(task.test$clone(deep = TRUE)$filter(which(data.test$subspace == "b")))$se,
           ncol = sum(new.points$subspace == "a"),
           nrow = sum(data.test$subspace == "b")),
    tolerance = 1e-6
  )

  # When conditioning on space B points, space A predictions should be unchanged
  expect_equal(
    unname(cond.se.conjoined[data.test$subspace == "a", new.points$subspace == "b"]),
    matrix(learner.conjoined$predict(task.test$clone(deep = TRUE)$filter(which(data.test$subspace == "a")))$se,
           ncol = sum(new.points$subspace == "b"),
           nrow = sum(data.test$subspace == "a")),
    tolerance = 1e-6
  )

  expect_equal(
    learner.conjoined$predict(task.test$clone(deep = TRUE)$filter(which(data.test$subspace == "a")))$se,
    learner.a$predict(task.test.a)$se,
    tolerance = 1e-6
  )

  expect_equal(
    learner.conjoined$predict(task.test$clone(deep = TRUE)$filter(which(data.test$subspace == "b")))$se,
    learner.b$predict(task.test.b)$se,
    tolerance = 1e-6
  )

  cond.se.conjoined.aonly <- learner.conjoined$predictConditionalSE(
    task.new$clone(deep = TRUE)$filter(which(new.points$subspace == "a")[1]),
    task.test
  )

  expect_equal(
    unname(cond.se.conjoined.aonly[data.test$subspace == "a", , drop = FALSE]),
    unname(cond.se.a[, 1, drop = FALSE]),
    tolerance = 1e-6
  )

  expect_equal(
    unname(cond.se.conjoined.aonly[data.test$subspace == "b", , drop = FALSE]),
    matrix(learner.conjoined$predict(task.test$clone(deep = TRUE)$filter(which(data.test$subspace == "b")))$se,
           ncol = 1,
           nrow = sum(data.test$subspace == "b")),
    tolerance = 1e-6
  )

})

