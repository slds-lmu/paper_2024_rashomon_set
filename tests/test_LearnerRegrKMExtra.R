test_that("LearnerRegrKMExtra initialization works", {
  learner.extra <- LearnerRegrKMExtra$new()

  expect_class(learner.extra, "LearnerRegrKMExtra")
  expect_class(learner.extra$baselearner, "LearnerRegrKM")
  expect_identical(learner.extra$id, "regr.km.extra")
  expect_set_equal(learner.extra$packages, c("DiceKriging", "mlr3"))
  expect_identical(learner.extra$feature_types, learner.extra$baselearner$feature_types)
  expect_identical(learner.extra$predict_types, learner.extra$baselearner$predict_types)
})

test_that("LearnerRegrKMExtra predictions match LearnerRegrKM", {
  # Create simple 1D regression task
  in.x <- runif(40)
  data.train <- data.table(
    x = in.x,
    y = sin(2 * pi * in.x)
  )
  data.test <- data.table(
    x = seq(0.1, 0.9, length.out = 40),
    y = sin(2 * pi * seq(0.1, 0.9, length.out = 40))
  )
  task.train <- TaskRegr$new(
    "sinwave",
    backend = data.train,
    target = "y"
  )
  task.test <- TaskRegr$new(
    "sinwave_test",
    backend = data.test,
    target = "y"
  )

  # Train both learners with same parameters
  learner.extra <- LearnerRegrKMExtra$new()
  learner.extra$param_set$set_values(nugget = 1e-8)
  learner.extra$encapsulate("evaluate", lrn("regr.featureless"))
  learner.base <- LearnerRegrKM$new()
  learner.base$param_set$set_values(nugget = 1e-8)
  learner.base$encapsulate("evaluate", lrn("regr.featureless"))

  for (covtype in c("gauss", "matern5_2")) {
    for (predict_type in c("response", "se")) {
      learner.extra$param_set$values$covtype <- covtype
      learner.base$param_set$values$covtype <- covtype
      learner.extra$predict_type <- predict_type
      learner.base$predict_type <- predict_type

      learner.extra$train(task.train)
      learner.base$train(task.train)

      pred.extra <- learner.extra$predict(task.test)
      pred.base <- learner.base$predict(task.test)

      # Check that predictions match
      expect_equal(
        pred.extra$response,
        pred.base$response,
        tolerance = 1e-4,
        info = sprintf("Response mismatch with covtype = %s", covtype)
      )
      if (predict_type == "se") {
        expect_equal(
          pred.extra$se,
          pred.base$se,
          tolerance = 1e-4,
          info = sprintf("SE mismatch with covtype = %s", covtype)
        )
      }
    }
  }

})

test_that("posteriorVarGivenNewPoints matches sequential DiceKriging updates", {
  # Create simple 1D regression task similar to covexp.R
  in.x <- runif(5)
  data.train <- data.table(
    x = in.x,
    y = sin(2 * pi * in.x)
  )
  data.test <- data.table(
    x = seq(0, 1, length.out = 200),
    y = sin(2 * pi * seq(0, 1, length.out = 200))
  )
  task.train <- TaskRegr$new(
    "sinwave",
    backend = data.train,
    target = "y"
  )

  # Train base model
  learner.base <- LearnerRegrKM$new()
  learner.base$param_set$set_values(nugget = 1e-8)
  learner.base$encapsulate("evaluate", lrn("regr.featureless"))
  learner.base$train(task.train)
  model <- learner.base$model

  # Generate new points
  newpoint.x <- runif(3)
  newpoint <- data.table(
    x = newpoint.x,
    y = sin(2 * pi * newpoint.x)
  )

  # Get posterior variances using our function
  newmat <- posteriorVarGivenNewPoints(
    model = model,
    Xcand = as.matrix(newpoint[, .(x)]),
    M = as.matrix(data.test[, .(x)]),
    new.nugget = 0
  )

  # For each new point, update model and compare variances
  for (i in seq_len(nrow(newpoint))) {
    model.updated <- DiceKriging::update(
      model,
      newX = as.matrix(newpoint[i, .(x)]),
      newy = as.matrix(newpoint[i, .(y)]),
      cov.reestim = FALSE,
      trend.reestim = FALSE,
      nugget.reestim = FALSE
    )

    preds.updated <- DiceKriging::predict(
      model.updated,
      as.matrix(data.test[, .(x)]),
      type = "SK",
      se.compute = TRUE
    )

    # The squared standard errors from DiceKriging should match our computed variances
    expect_equal(
      unname(preds.updated$sd^2),
      unname(newmat[, i]),
      tolerance = 1e-6,
      info = sprintf("Variance mismatch for point %d", i)
    )
  }

})

test_that("posteriorVarGivenNewPoints matches sequential DiceKriging updates with nonzero nugget", {
  # Create simple 1D regression task similar to covexp.R
  in.x <- runif(5)
  data.train <- data.table(
    x = in.x,
    y = sin(2 * pi * in.x)
  )
  data.test <- data.table(
    x = seq(0, 1, length.out = 200),
    y = sin(2 * pi * seq(0, 1, length.out = 200))
  )
  task.train <- TaskRegr$new(
    "sinwave",
    backend = data.train,
    target = "y"
  )
  for (nugget.val in c(0, 0.1, 0.3, 0.7)) {
    # Train base model with significant nugget
    learner.base <- LearnerRegrKM$new()
    learner.base$param_set$set_values(nugget = nugget.val)
    learner.base$encapsulate("evaluate", lrn("regr.featureless"))
    learner.base$train(task.train)
    model <- learner.base$model

    # Generate new points
    newpoint.x <- runif(3)
    newpoint <- data.table(
      x = newpoint.x,
      y = sin(2 * pi * newpoint.x)
    )

    # Get posterior variances using our function
    newmat <- posteriorVarGivenNewPoints(
      model = model,
      Xcand = as.matrix(newpoint[, .(x)]),
      M = as.matrix(data.test[, .(x)]),
      new.nugget = nugget.val  # Use same nugget for new points
    )

    # For each new point, update model and compare variances
    for (i in seq_len(nrow(newpoint))) {
      model.updated <- DiceKriging::update(
        model,
        newX = as.matrix(newpoint[i, .(x)]),
        newy = as.matrix(newpoint[i, .(y)]),
        cov.reestim = FALSE,
        trend.reestim = FALSE,
        nugget.reestim = FALSE
      )

      preds.updated <- DiceKriging::predict(
        model.updated,
        as.matrix(data.test[, .(x)]),
        type = "SK",
        se.compute = TRUE
      )

      # The squared standard errors from DiceKriging should match our computed variances
      expect_equal(
        unname(preds.updated$sd^2) - nugget.val,  # remove aleatoric noise
        unname(newmat[, i]),
        tolerance = 1e-6,
        info = sprintf("Variance mismatch for point %d with nugget = %g", i, nugget.val)
      )
    }
  }

})

test_that("predictConditionalSE matches sequential updates with estimated nugget", {
  # Create simple 1D regression task with 10 points
  in.x <- runif(10)
  data.train <- data.table(
    x = c(in.x, in.x),
    y = sin(2 * pi * c(in.x, in.x)) + c(rep(0, length(in.x)), rep(0.5, length(in.x)))
  )
  data.test <- data.table(
    x = seq(0, 1, length.out = 200),
    y = sin(2 * pi * seq(0, 1, length.out = 200))
  )
  task.train <- TaskRegr$new(
    "sinwave",
    backend = data.train,
    target = "y"
  )
  task.test <- TaskRegr$new(
    "sinwave_test",
    backend = data.test,
    target = "y"
  )

  # Train both learners with nugget estimation
  learner.extra <- LearnerRegrKMExtra$new()
  learner.extra$param_set$set_values(
    nugget.estim = TRUE,
    covtype = "matern5_2"
  )
  learner.extra$predict_type <- "se"
  learner.extra$encapsulate("evaluate", lrn("regr.featureless", predict_type = "se"))
  learner.extra$train(task.train)

  learner.base <- LearnerRegrKM$new()
  learner.base$param_set$set_values(
    nugget.estim = TRUE,
    covtype = "matern5_2"
  )
  learner.base$predict_type <- "se"
  learner.base$encapsulate("evaluate", lrn("regr.featureless", predict_type = "se"))
  learner.base$train(task.train)

  # Generate new points to condition on
  newpoint.x <- runif(3)
  newpoint <- data.table(
    x = newpoint.x,
    y = sin(2 * pi * newpoint.x)
  )
  task.new <- TaskRegr$new(
    "sinwave_new",
    backend = newpoint,
    target = "y"
  )

  # Get conditional variances using our method
  newmat <- learner.extra$predictConditionalSE(task.new, task.test)

  # For each new point, update model and compare variances
  model <- learner.base$model  # get the km object
  nugget.val <- model@covariance@nugget  # get estimated nugget

  for (i in seq_len(nrow(newpoint))) {
    model.updated <- DiceKriging::update(
      model,
      newX = as.matrix(newpoint[i, .(x)]),
      newy = as.matrix(newpoint[i, .(y)]),
      cov.reestim = FALSE,
      trend.reestim = FALSE,
      nugget.reestim = FALSE
    )

    preds.updated <- DiceKriging::predict(
      model.updated,
      as.matrix(data.test[, .(x)]),
      type = "SK",
      se.compute = TRUE
    )

    # The squared standard errors from DiceKriging should match our computed variances
    expect_equal(
      unname(preds.updated$sd^2) - nugget.val,  # remove aleatoric noise
      unname(newmat[, i]^2),
      tolerance = 1e-6,
      info = sprintf("Variance mismatch for point %d with estimated nugget = %g", i, nugget.val)
    )
  }
})

