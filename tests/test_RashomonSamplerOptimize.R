# Global test data
x.samples.1 <- data.table(
  .id = 1:4,
  x = c(-4, -2, 2, 4),
  .score = c(16, 4, 4, 16)
)

x.samples.2 <- data.table(
  .id = 5:20,
  x = seq(-5, 5, length.out = 16)
)

# Helper function for sampler initialization and setup
initializeSampler <- function(aqf, minimize, learner) {
  domain <- ps(x = p_dbl(-5, 5))
  sampler <- RashomonSamplerOptimize$new(
    id = "optimize",
    domain = domain,
    minimize = minimize,
    learner = learner,
    aqf = aqf,
    search.grid.size = 20,
    seed = 1
  )

  # Initial samples with known scores
  x.samples.1.scores <- if (minimize) x.samples.1$.score else -x.samples.1$.score
  x.samples.init <- copy(x.samples.1)
  x.samples.init$.score <- x.samples.1.scores
  sampler$tellXSamples(x.samples.init, scorecol = ".score")

  # search grid with unknown scores
  sampler$tellXSamples(x.samples.2)

  sampler
}

test_that("RashomonSamplerOptimize initialization works", {
  domain <- ps(
    x1 = p_dbl(0, 1),
    x2 = p_dbl(0, 1)
  )

  learner <- lrn("regr.km", predict_type = "se")

  sampler <- RashomonSamplerOptimize$new(
    id = "optimize",
    domain = domain,
    minimize = TRUE,
    learner = learner,
    aqf = AqfMean(),
    search.grid.size = 30,
    seed = 1
  )

  expect_identical(sampler$id, "optimize")
  expect_identical(sampler$domain, domain)
  expect_true(sampler$minimize)
  expect_equal(sampler$learner, learner)  # nolint
  expect_false(identical(sampler$learner, learner))
  expect_identical(sampler$search.grid.size, 30L)

  # Test with minimize = FALSE
  sampler.max <- RashomonSamplerOptimize$new(
    id = "optimize",
    domain = domain,
    minimize = FALSE,
    learner = learner,
    aqf = AqfMean(),
    search.grid.size = 30,
    seed = 1
  )

  expect_false(sampler.max$minimize)
})

test_that("RashomonSamplerOptimize works with mean acquisition function", {
  # Test with learner that doesn't support SE
  learner.no.se <- lrn("regr.rpart")
  sampler <- initializeSampler(AqfMean(), TRUE, learner.no.se)

  # Should work with mean acquisition function
  y.request <- sampler$askYValues()
  expect_data_table(y.request, nrows = 1)
  expect_named(y.request, c("x", ".id"))
  expect_identical(x.samples.2[J(y.request$.id), x, on = ".id"], y.request$x)

  # Should fail with other acquisition functions
  sampler.2 <- RashomonSamplerOptimize$new(
    id = "optimize", domain = domain, minimize = TRUE,
    learner = learner.no.se, aqf = AqfEi(),
    search.grid.size = 20, seed = 1
  )
  sampler.2$tellXSamples(x.samples.1, scorecol = ".score")
  sampler.2$tellXSamples(x.samples.2)

  expect_error(sampler.2$askYValues(), "acquisition function result.*Contains missing values")
})

test_that("RashomonSamplerOptimize sets predict_type automatically when possible", {
  # Test with learner that supports SE, but does not have predict_type set
  learner.se <- lrn("regr.km")$encapsulate("evaluate", lrn("regr.featureless"))
  expect_identical(learner.se$predict_type, "response")

  sampler <- initializeSampler(AqfSd(), TRUE, learner.se)
  expect_identical(sampler$learner$predict_type, "se")
  expect_identical(learner.se$predict_type, "response")  # was not changed by reference

  # Should work with sd acquisition function
  y.request <- sampler$askYValues()
  expect_data_table(y.request, nrows = 1)
  expect_named(y.request, c("x", ".id"))
  expect_identical(x.samples.2[J(y.request$.id), x, on = ".id"], y.request$x)
})

test_that("RashomonSamplerOptimize works for all acquisition functions", {
  # Test with learner that supports SE
  learner.se <- lrn("regr.km", predict_type = "se")$encapsulate("evaluate",
    lrn("regr.featureless", predict_type = "se"))
  aqfs <- list(AqfMean = AqfMean(), AqfSd = AqfSd(), AqfEi = AqfEi(), AqfLcb = AqfLcb(1))

  all.aqfs <- mget(ls(pattern = "^Aqf", envir = .GlobalEnv), envir = .GlobalEnv)
  all.aqfs <- names(all.aqfs)[vapply(all.aqfs, inherits, logical(1), "function")]
  expect_set_equal(names(aqfs), all.aqfs)

  for (aqf.name in names(aqfs)) {
    aqf <- aqfs[[aqf.name]]
    sampler <- initializeSampler(aqf, TRUE, learner.se)

    # Should work with sd acquisition function
    y.request <- sampler$askYValues()
    expect_data_table(y.request, nrows = 1, info = aqf.name)
    expect_named(y.request, c("x", ".id"), info = aqf.name)
    expect_identical(x.samples.2[J(y.request$.id), x, on = ".id"], y.request$x, info = aqf.name)
  }
})

test_that("RashomonSamplerOptimize handles optimization correctly", {
  func <- function(x) x$x^2  # simple quadratic with minimum at 0
  learner <- lrn("regr.km", predict_type = "se")$encapsulate("evaluate",
    lrn("regr.featureless", predict_type = "se"))

  aqfs <- rbindlist(list(
    list(name = character(0), aqf = list(), does.optimize = logical(0)),
    list(name = "AqfMean", aqf = list(AqfMean()), does.optimize = TRUE),
    list(name = "AqfSd", aqf = list(AqfSd()), does.optimize = FALSE),
    list(name = "AqfEi", aqf = list(AqfEi()), does.optimize = TRUE),
    list(name = "AqfLcb", aqf = list(AqfLcb(1)), does.optimize = TRUE)
  ))

  all.aqfs <- mget(ls(pattern = "^Aqf", envir = .GlobalEnv), envir = .GlobalEnv)
  all.aqfs <- names(all.aqfs)[vapply(all.aqfs, inherits, logical(1), "function")]
  expect_set_equal(aqfs$name, all.aqfs)

  doOptimize <- function(aqf, minimize) {
    sampler <- initializeSampler(aqf, minimize, learner)

    # Get next point to evaluate
    y.request <- sampler$askYValues()
    expect_data_table(y.request, nrows = 1)
    expect_named(y.request, c("x", ".id"))
    expect_identical(x.samples.2[J(y.request$.id), x, on = ".id"], y.request$x)

    # Evaluate new point
    y.request$.score <- func(list(x = y.request$x))
    sampler$tellYValues(y.request)

    # Get next point to evaluate
    y.request.2 <- sampler$askYValues()
    expect_data_table(y.request.2, nrows = 1)
    expect_named(y.request.2, c("x", ".id"))
    expect_identical(x.samples.2[J(y.request.2$.id), x, on = ".id"], y.request.2$x)
    expect_false(y.request.2$x == y.request$x)

    # Evaluate new point
    y.request.2$.score <- func(list(x = y.request.2$x))
    sampler$tellYValues(y.request.2)

    # Get next point to evaluate
    y.request.3 <- sampler$askYValues()
    expect_data_table(y.request.3, nrows = 1)
    expect_named(y.request.3, c("x", ".id"))
    expect_identical(x.samples.2[J(y.request.3$.id), x, on = ".id"], y.request.3$x)
    expect_false(y.request.3$x %in% c(y.request$x, y.request.2$x))

    # Evaluate new point
    y.request.3$.score <- func(list(x = y.request.3$x))

    values.known <- rbind(y.request, y.request.2, y.request.3)

    rbind(x.samples.1, values.known, use.names = TRUE, fill = TRUE)
  }

  for (do.minimize in c(TRUE, FALSE)) {
    for (i in seq_len(nrow(aqfs))) {
      aqf <- aqfs$aqf[[i]]
      results <- doOptimize(aqf, do.minimize)
      if (!aqfs$does.optimize[i]) {
        # if we don't optimize, we can't make any assumptions about the result,
        # but we still run the optimization to make sure no errors happen.
        next
      }
      if (do.minimize) {
        expect_lt(min(results$.score), 1, label = sprintf("%s (minimize = %s)", aqfs$name[i], do.minimize))
      } else {
        expect_gt(max(results$.score), -1, label = sprintf("%s (minimize = %s)", aqfs$name[i], do.minimize))
      }
    }
  }
})

test_that("RashomonSamplerOptimize handles first point selection correctly", {
  domain <- ps(x = p_dbl(-5, 5))
  learner <- lrn("regr.km", predict_type = "se")

  sampler <- RashomonSamplerOptimize$new(
    id = "optimize",
    domain = domain,
    minimize = TRUE,
    learner = learner,
    aqf = AqfEi(),
    search.grid.size = 20,
    seed = 1
  )

  # Initial grid
  x.samples <- data.table(
    .id = 1:20,
    x = seq(-5, 5, length.out = 20),
    .score = NA_real_
  )
  sampler$tellXSamples(x.samples, scorecol = ".score")

  # First point should be randomly selected
  y.request <- expect_warning(
    sampler$askYValues(),
    "No known points yet, picking first point randomly"
  )
  expect_data_table(y.request, nrows = 1)
  expect_identical(x.samples[J(y.request$.id), x, on = ".id"], y.request$x)
})
