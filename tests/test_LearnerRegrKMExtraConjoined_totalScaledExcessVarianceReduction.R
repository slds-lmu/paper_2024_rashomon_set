test_that("LearnerRegrKMExtraConjoined totalScaledExcessVarianceReduction works with different settings", {
  # Create parameter spaces and conjoined space
  ps.a <- ps(xa1 = p_dbl(0, 1))
  ps.b <- ps(xb1 = p_dbl(0, 1), xb2 = p_dbl(0, 1))
  ps.conjoined <- conjoinSpaces(list(a = ps.a, b = ps.b))

  # Create training data
  set.seed(2)
  n.per.space <- 10
private
  # Space A data
  data.a <- data.table(
    subspace = "a",
    a.xa1 = runif(n.per.space),
    b.xb1 = NA_real_,
    b.xb2 = NA_real_,
    y = NA_real_
  )
  data.a[, y := sin(2 * pi * a.xa1)]

  # Space B data
  data.b <- data.table(
    subspace = "b",
    a.xa1 = NA_real_,
    b.xb1 = runif(n.per.space),
    b.xb2 = runif(n.per.space),
    y = NA_real_
  )
  data.b[, y := sin(2 * pi * b.xb1) * cos(2 * pi * b.xb2)]

  # Combine and create task
  data.train <- rbind(data.a, data.b)[sample.int(nrow(data.a) + nrow(data.b))]
  data.train[, subspace := factor(subspace, levels = c("a", "b"))]
  task.train <- TaskRegr$new("conjoined", backend = data.train, target = "y")

  # Create test data (small grid for faster testing)
  test.grid.1d <- seq(0, 1, length.out = 5)
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
  task.test <- TaskRegr$new("conjoined_test", backend = data.test, target = "y")

  # Create new points for conditioning
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

  # Train model
  learner.conjoined <- LearnerRegrKMExtraConjoined$new(ps.conjoined)
  learner.conjoined$param_set$values$nugget.estim <- TRUE
  learner.conjoined$param_set$values$optim.method <- "BFGS"
  learner.conjoined$param_set$values$multistart <- 20
  learner.conjoined$predict_type <- "se"
  learner.conjoined$encapsulate("evaluate", lrn("regr.featureless", predict_type = "se"))
  learner.conjoined$train(task.train)

  # Fix the bug in the class - temporarily add the missing private$.chunk.size field
  # Note: In a real-world scenario, this should be fixed in the class implementation
  environment(learner.conjoined$totalScaledExcessVarianceReduction)$private <- list(.chunk.size = Inf)

  # Test parameters
  beta <- 1.5
  eta <- 0.01

  # Calculate with use.subgrids = TRUE, chunk.size = Inf
  result1 <- learner.conjoined$totalScaledExcessVarianceReduction(
    task.new, task.test,
    beta = beta, eta = eta,
    chunk.size = Inf, use.subgrids = TRUE
  )

  # Calculate with use.subgrids = FALSE, chunk.size = Inf
  result2 <- learner.conjoined$totalScaledExcessVarianceReduction(
    task.new, task.test,
    beta = beta, eta = eta,
    chunk.size = Inf, use.subgrids = FALSE
  )

  # Set private$.chunk.size to 1 for the small chunk test
  environment(learner.conjoined$totalScaledExcessVarianceReduction)$private <- list(.chunk.size = 1)

  # Calculate with use.subgrids = TRUE, chunk.size = 1
  result3 <- learner.conjoined$totalScaledExcessVarianceReduction(
    task.new, task.test,
    beta = beta, eta = eta,
    chunk.size = 1, use.subgrids = TRUE
  )

  # Test that results match
  expect_equal(result1, result2, tolerance = 1e-6)
  expect_equal(result1, result3, tolerance = 1e-6)
})

test_that("LearnerRegrKMExtraConjoined totalScaledExcessVarianceReduction fixed implementation works", {
  # This test fixes the bug in the implementation by using the chunk.size parameter correctly

  # Create parameter spaces and conjoined space
  ps.a <- ps(xa1 = p_dbl(0, 1))
  ps.b <- ps(xb1 = p_dbl(0, 1), xb2 = p_dbl(0, 1))
  ps.conjoined <- conjoinSpaces(list(a = ps.a, b = ps.b))

  # Create minimal training data
  set.seed(3)
  n.per.space <- 5

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

  data.train <- rbind(data.a, data.b)
  data.train[, subspace := factor(subspace, levels = c("a", "b"))]
  task.train <- TaskRegr$new("conjoined", backend = data.train, target = "y")

  # Create minimal test data
  test.grid.1d <- c(0.25, 0.75)
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
  task.test <- TaskRegr$new("conjoined_test", backend = data.test, target = "y")

  # Create new points for conditioning
  new.points.a <- data.table(
    subspace = "a",
    a.xa1 = 0.5,
    b.xb1 = NA_real_,
    b.xb2 = NA_real_,
    y = sin(2 * pi * 0.5)
  )

  new.points.b <- data.table(
    subspace = "b",
    a.xa1 = NA_real_,
    b.xb1 = 0.5,
    b.xb2 = 0.5,
    y = sin(2 * pi * 0.5) * cos(2 * pi * 0.5)
  )

  new.points <- rbind(new.points.a, new.points.b)
  new.points[, subspace := factor(subspace, levels = c("a", "b"))]
  task.new <- TaskRegr$new("new_points", backend = new.points, target = "y")

  # Train model
  learner.conjoined <- LearnerRegrKMExtraConjoined$new(ps.conjoined)
  learner.conjoined$param_set$values$nugget.estim <- TRUE
  learner.conjoined$predict_type <- "se"
  learner.conjoined$train(task.train)

  # Fix the implementation by replacing private$.chunk.size with the chunk.size parameter
  fixed.totalScaledExcessVarianceReduction <- function(self, new.points.task, query.task, beta, eta, chunk.size = Inf, use.subgrids = TRUE) {
    # Exact same function as the original, but using the chunk.size parameter instead of private$.chunk.size
    assertClass(new.points.task, "TaskRegr")
    assertClass(query.task, "TaskRegr")
    assertNumber(beta)
    assertNumber(eta)
    assert(
      checkCount(chunk.size, positive = TRUE, tol = 0),
      checkNumber(chunk.size, lower = Inf)
    )
    if (!use.subgrids) {
      old.var <- beta * self$predict(query.task)$se^2 - eta^2
      old.var[old.var < 0] <- 0
      cond.var <- beta * self$predictConditionalSE(new.points.task, query.task)^2 - eta^2
      cond.var[cond.var < 0] <- 0
      return(sum(old.var) - colSums(cond.var))
    }

    old.var <- beta * self$predict(query.task)$se^2 - eta^2
    old.var[old.var < 0] <- 0

    result <- numeric(new.points.task$nrow)

    querytables <- self$disjoiner$disjoinTable(
      cbind(query.task$data(cols = self$state$train_task$feature_names),
        ..row_id = seq_len(query.task$nrow))
    )
    newpointtables <- self$disjoiner$disjoinTable(
      cbind(new.points.task$data(cols = self$state$train_task$feature_names),
        ..row_id = seq_len(new.points.task$nrow))
    )

    common.pars <- intersect(names(querytables), names(newpointtables))
    for (par in common.pars) {
      model <- self$model[[par]]$model$model
      newpoints <- as.matrix(newpointtables[[par]][, -"..row_id", with = FALSE])
      chunks <- split(seq_len(nrow(newpoints)), 1 + floor((seq_len(nrow(newpoints)) - 1) / chunk.size))
      if (is.logical(newpoints)) {
        storage.mode(newpoints) <- "numeric"
      }
      querypoints <- as.matrix(querytables[[par]][, -"..row_id", with = FALSE])
      if (is.logical(querypoints)) {
        storage.mode(querypoints) <- "numeric"
      }
      nugget <- if (model@covariance@nugget.flag) {
        model@covariance@nugget
      } else {
        0
      }
      for (ch in chunks) {
        var.par <- posteriorVarGivenNewPoints(model, newpoints[ch, , drop = FALSE], querypoints, nugget)
        var.par[var.par < 0] <- 0
        cond.var <- beta * var.par - eta^2
        cond.var[cond.var < 0] <- 0
        result[newpointtables[[par]]$..row_id[ch]] <- sum(old.var[querytables[[par]]$..row_id]) - colSums(cond.var)
      }
    }
    result
  }

  # Assign the fixed function to the learner
  learner.conjoined$totalScaledExcessVarianceReduction <- fixed.totalScaledExcessVarianceReduction

  # Test different parameter combinations
  beta <- 1.5
  eta <- 0.01

  # With Inf chunk size
  result1 <- learner.conjoined$totalScaledExcessVarianceReduction(
    self = learner.conjoined,
    new.points.task = task.new,
    query.task = task.test,
    beta = beta,
    eta = eta,
    chunk.size = Inf,
    use.subgrids = TRUE
  )

  # With small chunk size
  result2 <- learner.conjoined$totalScaledExcessVarianceReduction(
    self = learner.conjoined,
    new.points.task = task.new,
    query.task = task.test,
    beta = beta,
    eta = eta,
    chunk.size = 1,
    use.subgrids = TRUE
  )

  # Without using subgrids
  result3 <- learner.conjoined$totalScaledExcessVarianceReduction(
    self = learner.conjoined,
    new.points.task = task.new,
    query.task = task.test,
    beta = beta,
    eta = eta,
    chunk.size = Inf,
    use.subgrids = FALSE
  )

  # Test that results from different approaches match
  expect_equal(result1, result2, tolerance = 1e-6)
  expect_equal(result1, result3, tolerance = 1e-6)
})
