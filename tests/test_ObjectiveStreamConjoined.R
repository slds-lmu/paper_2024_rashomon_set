# Common test objects
test.domain <- ps(
  x1 = p_dbl(0, 1),
  x2 = p_dbl(0, 1)
)

test.domain.cat <- ps(
  x1 = p_dbl(0, 1),
  cat = p_fct(c("a", "b", "c"))
)

# Dummy Objective Streams for testing
DummyObjectiveStream1 <- R6Class("DummyObjectiveStream1",
  inherit = ObjectiveStream,
  private = list(
    .eval = function(x) {
      rep(1, nrow(x)) # always return 1
    },
    .sample = function(n) {
      response <- matrix(runif(n * 2), nrow = n, byrow = TRUE)
      data.table(
        x1 = response[, 1],
        x2 = response[, 2]
      )
    }
  )
)

DummyObjectiveStream2 <- R6Class("DummyObjectiveStream2",
  inherit = ObjectiveStream,
  private = list(
    .eval = function(x) {
      rep(2, nrow(x)) # always return 2
    },
    .sample = function(n) {
      response <- matrix(rnorm(n * 2), nrow = n, byrow = TRUE)
      data.table(
        x1 = response[, 1],
        x2 = response[, 2]
      )
    }
  )
)

DummyObjectiveStreamCat <- R6Class("DummyObjectiveStreamCat",
  inherit = ObjectiveStream,
  private = list(
    .eval = function(x) rep(1, nrow(x)),
    .sample = function(n) {
      data.table(
        x1 = runif(n),
        cat = sample(c("a", "b", "c"), n, replace = TRUE)
      )
    }
  )
)

# New dummy stream classes for evaluation mapping and ordering tests
DummyEvalMapStream1 <- R6Class("DummyEvalMapStream1",
  inherit = ObjectiveStream,
  private = list(
    .eval = function(x) {
      # After column remapping, x will have columns "x1" and "x2"
      # Evaluation: x1 + 10 * x2
      x[["x1"]] + 10 * x[["x2"]]
    },
    .sample = function(n) {
      # Return a deterministic sample: n rows with fixed values.
      data.table(x1 = rep(0.1, n), x2 = rep(0.2, n))
    }
  )
)

DummyEvalMapStream2 <- R6Class("DummyEvalMapStream2",
  inherit = ObjectiveStream,
  private = list(
    .eval = function(x) {
      # Evaluation: x1 + 100 * x2
      x[["x1"]] + 100 * x[["x2"]]
    },
    .sample = function(n) {
      # Return a deterministic sample: n rows with fixed values.
      data.table(x1 = rep(0.3, n), x2 = rep(0.4, n))
    }
  )
)

test_that("ObjectiveStreamConjoined initialization works", {
  obj.stream1 <- DummyObjectiveStream1$new(id = "os1", domain = test.domain, minimize = TRUE, seed = c(1L, 2L))
  obj.stream2 <- DummyObjectiveStream2$new(id = "os2", domain = test.domain, minimize = TRUE, seed = c(3L, 4L))
  objective.streams <- list(os1 = obj.stream1, os2 = obj.stream2)

  obj <- ObjectiveStreamConjoined$new(
    id = "conjoined",
    objective.streams = objective.streams,
    sampling.strategy = "random",
    seed = 123L
  )

  expect_identical(obj$id, "conjoined")
  expect_identical(
    as.data.table(obj$domain),
    as.data.table(
      ps_union(list(os1 = test.domain, os2 = test.domain, ps(subspace = p_fct(c("os1", "os2")))), tag_sets = TRUE))
  )
  expect_true(obj$minimize)
  # equal but not identical, because objective.streams are cloned
  expect_equal(obj$objective.streams, objective.streams)  # nolint
  expect_identical(obj$sampling.strategy, "random")
  expect_identical(obj$weights, c(1, 1))

  # Test roundrobin strategy
  obj.rr <- ObjectiveStreamConjoined$new(
    id = "conjoined_rr",
    objective.streams = objective.streams,
    sampling.strategy = "roundrobin",
    seed = 123L
  )
  expect_identical(obj.rr$sampling.strategy, "roundrobin")
  expect_identical(obj.rr$weights, c(1, 1))

  # Test custom weights
  weights <- c(0.2, 0.8)
  obj.weighted <- ObjectiveStreamConjoined$new(
    id = "conjoined_weighted",
    objective.streams = objective.streams,
    sampling.strategy = "random",
    weights = weights,
    seed = 123L
  )
  expect_identical(obj.weighted$weights, weights)

  obj.stream1 <- DummyObjectiveStream1$new(id = "os1", domain = test.domain, minimize = FALSE, seed = c(1L, 2L))
  obj.stream2 <- DummyObjectiveStream2$new(id = "os2", domain = test.domain, minimize = FALSE, seed = c(3L, 4L))
  objective.streams <- list(os1 = obj.stream1, os2 = obj.stream2)

  # Test minimize = FALSE
  obj.max <- ObjectiveStreamConjoined$new(
    id = "conjoined_max",
    objective.streams = objective.streams,
    sampling.strategy = "random",
    seed = 123L
  )
  expect_false(obj.max$minimize)
})

test_that("ObjectiveStreamConjoined initialization fails for invalid inputs", {
  obj.stream1 <- DummyObjectiveStream1$new(id = "os1", domain = test.domain, minimize = TRUE, seed = c(1L, 2L))
  obj.stream2 <- DummyObjectiveStream2$new(id = "os2", domain = test.domain, minimize = TRUE, seed = c(3L, 4L))
  objective.streams <- list(os1 = obj.stream1, os2 = obj.stream2)

  # Empty objective streams list
  expect_error(
    ObjectiveStreamConjoined$new(id = "conjoined", objective.streams = list()),
    "Must have length >= 1"
  )

  # Unnamed objective streams list
  expect_error(
    ObjectiveStreamConjoined$new(id = "conjoined", objective.streams = unname(objective.streams)),
    "Must have names"
  )

  # Incompatible minimize settings
  obj.stream3 <- DummyObjectiveStream1$new(id = "os3", domain = test.domain, minimize = FALSE, seed = c(5L, 6L))
  objective.streams.incompatible.min <- list(os1 = obj.stream1, os3 = obj.stream3)  # nolint
  expect_error(
    ObjectiveStreamConjoined$new(id = "conjoined", objective.streams = objective.streams.incompatible.min),
    "All objective streams must have the same 'minimize' setting."
  )

  # Invalid sampling strategy
  expect_error(
    ObjectiveStreamConjoined$new(id = "conjoined",
      objective.streams = objective.streams, sampling.strategy = "invalid"),
    "Must be element of set"
  )

  # Weights of wrong length
  expect_error(
    ObjectiveStreamConjoined$new(id = "conjoined", objective.streams = objective.streams, sampling.strategy = "random",
      seed = 123L, weights = 0.1),
    "Must have length 2, but has length 1"
  )
  expect_error(
    ObjectiveStreamConjoined$new(id = "conjoined", objective.streams = objective.streams, sampling.strategy = "random",
      seed = 123L, weights = c("a", "b")),
    "Must be of type 'numeric'"
  )
  expect_error(
    ObjectiveStreamConjoined$new(id = "conjoined", objective.streams = objective.streams, sampling.strategy = "random",
    seed = 123L, weights = c(-1, 2)),
    "Element 1 is not >= 0"
  )
  expect_error(
    ObjectiveStreamConjoined$new(id = "conjoined", objective.streams = objective.streams, sampling.strategy = "random",
    seed = 123L, weights = c(NA, 1)),
    "Contains missing values"
  )
})

test_that("ObjectiveStreamConjoined random sampling works", {
  obj.stream1 <- DummyObjectiveStream1$new(id = "os1", domain = test.domain, minimize = TRUE, seed = c(1L, 2L))
  obj.stream2 <- DummyObjectiveStream2$new(id = "os2", domain = test.domain, minimize = TRUE, seed = c(3L, 4L))
  objective.streams <- list(os1 = obj.stream1, os2 = obj.stream2)

  obj <- ObjectiveStreamConjoined$new(
    id = "conjoined",
    objective.streams = objective.streams,
    sampling.strategy = "random",
    seed = 123L
  )

  samples <- obj$sample(100) # large sample to check distribution
  expect_data_table(samples, nrows = 100)
  expect_named(samples, c("os1.x1", "os1.x2", "os2.x1", "os2.x2", "subspace", ".id"))
  expect_factor(samples$subspace, levels = c("os1", "os2"))

  # Check approximate distribution of samples from each stream (should be roughly uniform)
  stream.counts <- table(samples$subspace) / nrow(samples)
  expect_equal(unname(stream.counts[["os1"]]), 0.5, tolerance = 0.1)
  expect_equal(unname(stream.counts[["os2"]]), 0.5, tolerance = 0.1)

  # Test with custom weights
  weights <- c(0.2, 0.8)
  obj.weighted <- ObjectiveStreamConjoined$new(
    id = "conjoined_weighted",
    objective.streams = objective.streams,
    sampling.strategy = "random",
    weights = weights,
    seed = 123L
  )
  samples.weighted <- obj.weighted$sample(100)
  stream.counts.weighted <- table(samples.weighted$subspace) / nrow(samples.weighted)
  expect_equal(unname(stream.counts.weighted[["os1"]]), 0.2, tolerance = 0.1)
  expect_equal(unname(stream.counts.weighted[["os2"]]), 0.8, tolerance = 0.1)

  # Test empty sample
  empty.samples <- obj$sample(0)
  expect_data_table(empty.samples, nrows = 0)
  expect_named(empty.samples, c("os1.x1", "os1.x2", "os2.x1", "os2.x2", "subspace", ".id"))
})

test_that("ObjectiveStreamConjoined roundrobin sampling works", {
  obj.stream1 <- DummyObjectiveStream1$new(id = "os1", domain = test.domain, minimize = TRUE, seed = c(1L, 2L))
  obj.stream2 <- DummyObjectiveStream2$new(id = "os2", domain = test.domain, minimize = TRUE, seed = c(3L, 4L))
  objective.streams <- list(os1 = obj.stream1, os2 = obj.stream2)

  obj <- ObjectiveStreamConjoined$new(
    id = "conjoined",
    objective.streams = objective.streams,
    sampling.strategy = "roundrobin",
    seed = 123L
  )

  samples <- obj$sample(100) # large sample to check distribution
  expect_data_table(samples, nrows = 100)
  expect_named(samples, c("os1.x1", "os1.x2", "os2.x1", "os2.x2", "subspace", ".id"))
  expect_factor(samples$subspace, levels = c("os1", "os2"))

  # Check approximate distribution of samples from each stream (should be roughly uniform)
  stream.counts <- table(samples$subspace) / nrow(samples)
  expect_equal(unname(stream.counts[["os1"]]), 0.5, tolerance = 0)
  expect_equal(unname(stream.counts[["os2"]]), 0.5, tolerance = 0)

  # Test with custom weights
  weights <- c(0.2, 0.8)
  obj.weighted <- ObjectiveStreamConjoined$new(
    id = "conjoined_weighted",
    objective.streams = objective.streams,
    sampling.strategy = "roundrobin",
    weights = weights,
    seed = 123L
  )
  samples.weighted <- obj.weighted$sample(100)
  stream.counts.weighted <- table(samples.weighted$subspace) / nrow(samples.weighted)
  expect_equal(unname(stream.counts.weighted[["os1"]]), 0.2, tolerance = 0)
  expect_equal(unname(stream.counts.weighted[["os2"]]), 0.8, tolerance = 0)

  expect_identical(
    samples.weighted$subspace,
    factor(rep(rep(c("os2", "os1", "os2"), c(2, 1, 2)), 20), levels = c("os1", "os2"))
  )

  # Test empty sample
  empty.samples <- obj$sample(0)
  expect_data_table(empty.samples, nrows = 0)
  expect_named(empty.samples, c("os1.x1", "os1.x2", "os2.x1", "os2.x2", "subspace", ".id"))
})

test_that("ObjectiveStreamConjoined evaluation works", {
  obj.stream1 <- DummyObjectiveStream1$new(id = "os1", domain = test.domain, minimize = TRUE, seed = c(1L, 2L))
  obj.stream2 <- DummyObjectiveStream2$new(id = "os2", domain = test.domain, minimize = TRUE, seed = c(3L, 4L))
  objective.streams <- list(os1 = obj.stream1, os2 = obj.stream2)

  obj <- ObjectiveStreamConjoined$new(
    id = "conjoined",
    objective.streams = objective.streams,
    sampling.strategy = "random",
    seed = 123L
  )

  samples <- obj$sample(20)
  values <- obj$eval(samples)
  expect_numeric(values, len = 20)

  # Check that evaluations are correct based on DummyObjectiveStream implementations
  expected.values <- ifelse(samples$subspace == "os1", 1, 2)
  expect_identical(values, expected.values)

  # Test with different order of samples
  reordered.samples <- samples[sample(nrow(samples))]
  reordered.values <- obj$eval(reordered.samples)
  expect_identical(reordered.values, expected.values[match(reordered.samples$.id, samples$.id)])
})

test_that("ObjectiveStreamConjoined respects seeds", {
  obj.stream1 <- DummyObjectiveStream1$new(id = "os1", domain = test.domain, minimize = TRUE, seed = c(1L, 2L))
  obj.stream2 <- DummyObjectiveStream2$new(id = "os2", domain = test.domain, minimize = TRUE, seed = c(3L, 4L))
  objective.streams <- list(os1 = obj.stream1, os2 = obj.stream2)

  # Same seed gives same results
  obj1 <- ObjectiveStreamConjoined$new(
    id = "conjoined",
    objective.streams = objective.streams,
    sampling.strategy = "random",
    seed = 123L
  )

  obj2 <- ObjectiveStreamConjoined$new(
    id = "conjoined",
    objective.streams = objective.streams,
    sampling.strategy = "random",
    seed = 123L
  )

  sample.1 <- obj1$sample(5)
  sample.2 <- obj2$sample(5)

  expect_identical(sample.1, sample.2)

  samples1 <- obj1$sample(3)
  samples2 <- obj2$sample(3)
  expect_identical(obj1$eval(samples1), obj2$eval(samples2))

  # Different seeds give different results
  obj3 <- ObjectiveStreamConjoined$new(
    id = "conjoined",
    objective.streams = objective.streams,
    sampling.strategy = "random",
    seed = 789L
  )

  sample.3 <- obj3$sample(5)
  expect_false(identical(sample.1, sample.3))

  # seed within internal streams not affected
  sample.1.firsts <- sample.1[, first(.SD), keyby = "subspace"][, -".id", with = FALSE]
  sample.3.firsts <- sample.3[, first(.SD), keyby = "subspace"][, -".id", with = FALSE]
  expect_identical(sample.1.firsts, sample.3.firsts)
})

test_that("ObjectiveStreamConjoined handles categorical variables", {
  obj.stream.cat <- DummyObjectiveStreamCat$new(id = "os_cat", domain = test.domain.cat,
    minimize = TRUE, seed = c(1L, 2L))
  objective.streams.cat <- list(os_cat = obj.stream.cat)

  obj.cat <- ObjectiveStreamConjoined$new(
    id = "conjoined_cat",
    objective.streams = objective.streams.cat,
    sampling.strategy = "random",
    seed = 123L
  )

  samples.cat <- obj.cat$sample(5)
  expect_factor(samples.cat$os_cat.cat)
  expect_identical(levels(samples.cat$os_cat.cat), c("a", "b", "c"))
})

test_that("ObjectiveStreamConjoined '.id' field and ordering tests", {
  # Create two dummy objective streams for ordering tests.
  ds1 <- DummyEvalMapStream1$new(id = "os1", domain = test.domain, minimize = TRUE, seed = c(1L, 2L))
  ds2 <- DummyEvalMapStream2$new(id = "os2", domain = test.domain, minimize = TRUE, seed = c(3L, 4L))
  streams <- list(os1 = ds1, os2 = ds2)

  # Use roundrobin sampling; seed can be NULL.
  conjoined <- ObjectiveStreamConjoined$new(
    id = "conjoined_order",
    objective.streams = streams,
    sampling.strategy = "roundrobin",
    choice.param.name = "subspace",
    seed = NULL
  )

  samples <- conjoined$sample(6)
  # Check that the .id field exists and that all .id values are unique.
  expect_true(".id" %in% colnames(samples))
  expect_length(unique(samples[[".id"]]), nrow(samples))

  # Shuffle rows and evaluate to verify ordering does not affect evaluation.
  set.seed(42)
  shuffled <- samples[sample(nrow(samples))]
  evalOrig <- conjoined$eval(samples)
  evalShuffled <- conjoined$eval(shuffled)

  # Align evaluation values using the unique .id field.
  evalShuffledOrdered <- evalShuffled[order(shuffled[[".id"]])]
  evalOrigOrdered <- evalOrig[order(samples[[".id"]])]
  expect_identical(evalOrigOrdered, evalShuffledOrdered)
})

test_that("ObjectiveStreamConjoined evaluation mapping test", {
  # Create two dummy streams that produce fixed sample values.
  ds1 <- DummyEvalMapStream1$new(id = "os1", domain = test.domain, minimize = TRUE, seed = c(1L, 2L))
  ds2 <- DummyEvalMapStream2$new(id = "os2", domain = test.domain, minimize = TRUE, seed = c(3L, 4L))
  streams <- list(os1 = ds1, os2 = ds2)

  # Use random sampling with provided weights.
  conjoined <- ObjectiveStreamConjoined$new(
    id = "conjoined_eval",
    objective.streams = streams,
    sampling.strategy = "random",
    weights = c(0.5, 0.5),
    choice.param.name = "subspace",
    seed = 123L
  )

  samples <- conjoined$sample(10)
  values <- conjoined$eval(samples)

  # Expected evaluations:
  # For stream "os1": 0.1 + 10 * 0.2 = 2.1
  # For stream "os2": 0.3 + 100 * 0.4 = 40.3
  expectedValues <- ifelse(samples[["subspace"]] == "os1", 2.1, 40.3)

  expect_numeric(values, len = nrow(samples))
  expect_identical(values, expectedValues)
})
