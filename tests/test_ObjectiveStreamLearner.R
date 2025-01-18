test_that("ObjectiveStreamLearner initialization works", {
  # Create basic holdout resampling
  rsmp <- rsmp("holdout", ratio = 0.5)
  msr <- msr("regr.mse")

  obj <- ObjectiveStreamLearner$new(
    learner = learner.tree.regr,
    task = task.bh,
    resampling = rsmp,
    measure = msr,
    seed = c(1L, 2L)
  )

  expect_identical(obj$id, "lrn_regr.rpart_task_boston_housing_rsmp_holdout_msr_regr.mse")
  expect_equal(obj$domain, learner.tree.regr$param_set$search_space())  # nolint
  expect_true(obj$minimize)
  expect_class(obj$learner, "Learner")
  expect_class(obj$task, "Task")
  expect_class(obj$resampling, "Resampling")
  expect_class(obj$measure, "Measure")

  # Test with custom ID
  obj.custom <- ObjectiveStreamLearner$new(
    learner = learner.tree.regr,
    task = task.bh,
    resampling = rsmp,
    measure = msr,
    id = "custom_id",
    seed = c(1L, 2L)
  )

  expect_identical(obj.custom$id, "custom_id")

  # Test with custom domain
  custom.domain <- ps(
    cp = p_dbl(0, 1),
    minsplit = p_int(5, 20)
  )
  obj.domain <- ObjectiveStreamLearner$new(
    learner = learner.tree.regr,
    task = task.bh,
    resampling = rsmp,
    measure = msr,
    domain = custom.domain,
    seed = c(1L, 2L)
  )

  # if we change to cloning the domain in the future, this would need to be expect_equal
  expect_identical(obj.domain$domain, custom.domain)

  # Test with measure that should be maximized
  msr.max <- msr("regr.ktau")
  obj.max <- ObjectiveStreamLearner$new(
    learner = learner.tree.regr,
    task = task.bh,
    resampling = rsmp,
    measure = msr.max,
    seed = c(1L, 2L)
  )

  expect_false(obj.max$minimize)
})

test_that("ObjectiveStreamLearner sampling works", {
  rsmp <- rsmp("holdout", ratio = 0.5)
  msr <- msr("regr.mse")

  obj <- ObjectiveStreamLearner$new(
    learner = learner.tree.regr,
    task = task.bh,
    resampling = rsmp,
    measure = msr,
    seed = c(1L, 2L)
  )

  samples <- obj$sample(3)
  expect_data_table(samples, nrows = 3)
  expect_named(samples, c(learner.tree.regr$param_set$search_space()$ids(), ".id"))
  expect_identical(samples$.id, 1:3)

  # Test empty sample
  empty.samples <- obj$sample(0)
  expect_data_table(empty.samples, nrows = 0)
  expect_named(empty.samples, c(learner.tree.regr$param_set$search_space()$ids(), ".id"))
})

test_that("ObjectiveStreamLearner evaluation works", {
  rsmp <- rsmp("holdout", ratio = 0.5)
  msr <- msr("regr.mse")

  obj <- ObjectiveStreamLearner$new(
    learner = learner.tree.regr,
    task = task.bh,
    resampling = rsmp,
    measure = msr,
    seed = c(1L, 2L)
  )

  samples <- obj$sample(3)
  values <- obj$eval(samples)
  expect_numeric(values, len = 3, finite = TRUE)
})

test_that("ObjectiveStreamLearner respects seeds", {
  rsmp <- rsmp("holdout", ratio = 0.5)
  msr <- msr("regr.mse")

  # Same seed gives same results
  obj1 <- ObjectiveStreamLearner$new(
    learner = learner.tree.regr,
    task = task.bh,
    resampling = rsmp,
    measure = msr,
    seed = c(1L, 2L)
  )

  obj2 <- ObjectiveStreamLearner$new(
    learner = learner.tree.regr,
    task = task.bh,
    resampling = rsmp,
    measure = msr,
    seed = c(1L, 2L)
  )

  expect_identical(obj1$sample(5), obj2$sample(5))

  samples1 <- obj1$sample(3)
  samples2 <- obj2$sample(3)
  expect_identical(obj1$eval(samples1), obj2$eval(samples2))

  # Different sampling seed gives different samples
  obj3 <- ObjectiveStreamLearner$new(
    learner = learner.tree.regr,
    task = task.bh,
    resampling = rsmp,
    measure = msr,
    seed = c(2L, 2L)
  )

  expect_false(identical(obj1$sample(5), obj3$sample(5)))

  # Different evaluation seed gives different results
  obj4 <- ObjectiveStreamLearner$new(
    learner = learner.tree.regr,
    task = task.bh,
    resampling = rsmp,
    measure = msr,
    seed = c(1L, 3L)
  )

  obj5 <- ObjectiveStreamLearner$new(
    learner = learner.tree.regr,
    task = task.bh,
    resampling = rsmp,
    measure = msr,
    seed = c(1L, 2L)
  )

  samples4 <- obj4$sample(3)
  samples5 <- obj5$sample(3)
  expect_identical(samples4, samples5)  # same sampling seed
  expect_false(identical(obj4$eval(samples4), obj5$eval(samples5)))  # different eval seed
})

test_that("ObjectiveStreamLearner handles instantiated resampling", {
  rsmp <- rsmp("holdout", ratio = 0.5)
  msr <- msr("regr.mse")

  # Instantiate resampling
  rsmp$instantiate(task.bh)

  obj1 <- ObjectiveStreamLearner$new(
    learner = learner.tree.regr,
    task = task.bh,
    resampling = rsmp,
    measure = msr,
    seed = c(1L, 2L)
  )

  obj2 <- ObjectiveStreamLearner$new(
    learner = learner.tree.regr,
    task = task.bh,
    resampling = rsmp,
    measure = msr,
    seed = c(1L, 3L)  # different sampling seed
  )

  samples1 <- obj1$sample(3)
  samples2 <- obj2$sample(3)
  expect_identical(samples1, samples2)  # same samples due to same sampling seed
  # same evaluation results in spite of different evaluation seed
  expect_identical(obj1$eval(samples1), obj2$eval(samples2))
})

test_that("ObjectiveStreamLearner properly clones input objects", {
  rsmp <- rsmp("holdout", ratio = 0.5)
  msr <- msr("regr.mse")

  original.learner <- learner.tree.regr$clone(deep = TRUE)
  original.task <- task.bh$clone(deep = TRUE)
  original.resampling <- rsmp$clone(deep = TRUE)
  original.measure <- msr$clone(deep = TRUE)

  obj <- ObjectiveStreamLearner$new(
    learner = original.learner,
    task = original.task,
    resampling = original.resampling,
    measure = original.measure,
    seed = c(1L, 2L)
  )

  # Modify original objects
  original.learner$param_set$values$cp <- 0.5
  original.task$row_roles$use <- 1:10
  original.resampling$param_set$values$ratio <- 0.7

  # Check that objective's objects are unchanged
  expect_identical(obj$learner$param_set$values$cp, learner.tree.regr$param_set$values$cp)
  expect_false(identical(obj$learner$param_set$values$cp, original.learner$param_set$values$cp))
  expect_identical(obj$task$row_roles$use, task.bh$row_roles$use)
  expect_false(identical(obj$task$row_roles$use, original.task$row_roles$use))
  expect_identical(obj$resampling$param_set$values$ratio, rsmp$param_set$values$ratio)
  expect_false(identical(obj$resampling$param_set$values$ratio, original.resampling$param_set$values$ratio))
})

test_that("ObjectiveStreamLearner handles measure properties correctly", {
  rsmp <- rsmp("holdout", ratio = 0.5)

  # Test measure that requires model
  msr.model <- msr("time_train")
  obj.model <- ObjectiveStreamLearner$new(
    learner = learner.tree.regr,
    task = task.bh,
    resampling = rsmp,
    measure = msr.model,
    seed = c(1L, 2L)
  )

  samples <- obj.model$sample(2)
  expect_numeric(expect_no_error(obj.model$eval(samples)), len = 2, finite = TRUE, any.missing = FALSE)

  # Test measure that requires task
  msr.task <- msr("selected_features")
  obj.task <- ObjectiveStreamLearner$new(
    learner = learner.tree.regr,
    task = task.bh,
    resampling = rsmp,
    measure = msr.task,
    seed = c(1L, 2L)
  )

  expect_integerish(expect_no_error(obj.task$eval(samples)),
    len = 2, lower = 0, upper = 13, any.missing = FALSE, tol = 0)
})

test_that("ObjectiveStreamLearner handles parameter transformations", {
  lrn <- learner.tree.regr$clone(deep = TRUE)
  lrn$param_set$values <- list()
  rsmp <- rsmp("holdout", ratio = 0.5)
  msr <- msr("regr.mse")

  # Create domain with transformation
  domain <- ps(
    cp = p_dbl(0, 1),
    .extra_trafo = function(x) {
      x$cp <- x$cp^2  # square the cp parameter
      x
    }
  )

  obj <- ObjectiveStreamLearner$new(
    learner = lrn,
    task = task.bh,
    resampling = rsmp,
    measure = msr,
    domain = domain,
    seed = c(1L, 2L)
  )

  samples <- obj$sample(3)
  expect_true(all(samples$cp >= 0 & samples$cp <= 1))
  expect_no_error(obj$eval(samples))
})

test_that("ObjectiveStreamLearner evaluation matches manual resampling", {
  rsmp <- rsmp("holdout", ratio = 0.5)
  msr <- msr("regr.mse")

  # Instantiate resampling
  rsmp$instantiate(task.bh)

  obj <- ObjectiveStreamLearner$new(
    learner = learner.tree.regr,
    task = task.bh,
    resampling = rsmp,
    measure = msr,
    seed = c(1L, 2L)
  )

  # Get some samples and their evaluations
  samples <- obj$sample(3)
  auto.results <- obj$eval(samples)

  # Manually perform the same evaluations
  manual.results <- vapply(seq_len(nrow(samples)), function(i) {
    # Clone learner to avoid side effects
    lrn <- learner.tree.regr$clone(deep = TRUE)
    trafo <- lrn$param_set$search_space()$trafo
    # Set parameters according to sample
    lrn$param_set$values <- trafo(as.list(samples[i, -".id", with = FALSE]))
    # Perform manual resampling
    rr <- resample(task.bh, lrn, rsmp, store_models = FALSE)
    # Get aggregated performance
    rr$aggregate(msr)
  }, numeric(1))

  expect_identical(auto.results, manual.results)
})
