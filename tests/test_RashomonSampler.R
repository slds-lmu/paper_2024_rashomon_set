# Dummy implementation for testing
DummyRashomonSampler <- R6Class("DummyRashomonSampler",
  inherit = RashomonSampler,
  public = list(
    next.x.samples = 3L
  ),
  private = list(
    .x.samples = NULL,  # data.table of samples with .id and domain columns
    .y.values = NULL,   # data.table of evaluated samples with .id and .score
    .rashomon.samples = NULL,  # data.table of samples in rashomon set
    .askXSamples = function() {
      self$next.x.samples
    },
    .tellXSamples = function(x) {
      private$.x.samples <- rbind(private$.x.samples, x)
    },
    .askYValues = function() {
      private$.x.samples[is.na(.score)]
    },
    .tellYValues = function(y) {
      private$.y.values <- rbind(private$.y.values, y)
      # Simple rule: add to Rashomon set if score <= 0.5
      rashomon.threshold <- if (self$minimize) 0.5 else -0.5
      rashomon.scores <- if (self$minimize) y$.score else -y$.score
      private$.rashomon.samples <- rbind(
        private$.rashomon.samples,
        y[rashomon.scores <= rashomon.threshold]
      )
    },
    .rashomonSamplesComplete = function() {
      nrow(private$.rashomon.samples %??% data.table())
    },
    .getRashomonSamples = function() {
      private$.rashomon.samples %??% getNullTable(self$domain, include.id = TRUE, include.score = TRUE)
    }
  )
)

# Common test objects
test.domain <- ps(
  x1 = p_dbl(0, 1),
  x2 = p_dbl(0, 1)
)

test_that("RashomonSampler initialization works", {
  sampler <- DummyRashomonSampler$new(
    id = "dummy",
    domain = test.domain,
    minimize = TRUE,
    rashomon.epsilon = 0.1,
    rashomon.is.relative = FALSE,
    seed = 1L,
    n.rashomon.samples = 10L
  )

  expect_identical(sampler$id, "dummy")
  expect_identical(sampler$domain, test.domain)
  expect_true(sampler$minimize)
  expect_identical(sampler$rashomon.epsilon, 0.1)
  expect_false(sampler$rashomon.is.relative)
  expect_identical(sampler$n.rashomon.samples, 10L)

  sampler <- DummyRashomonSampler$new(
    id = "dummy",
    domain = test.domain,
    minimize = FALSE,
    rashomon.epsilon = 0.1,
    rashomon.is.relative = TRUE,
    seed = 1L,
    n.rashomon.samples = 10L
  )
  expect_false(sampler$minimize)
  expect_true(sampler$rashomon.is.relative)
})

test_that("RashomonSampler state machine works correctly", {
  sampler <- DummyRashomonSampler$new(
    id = "dummy",
    domain = test.domain,
    minimize = TRUE,
    rashomon.epsilon = 0.1,
    rashomon.is.relative = FALSE,
    seed = 1L,
    n.rashomon.samples = 10L
  )

  # Initial state: can ask for X samples
  expect_error(
    sampler$tellYValues(data.table(.id = 1:3, .score = c(0.3, 0.7, 0.4))),
    "Cannot tell Y when no Y was asked for"
  )

  n.samples <- sampler$askXSamples()
  expect_identical(n.samples, 3L)

  # Cannot ask for Y values before telling X samples
  expect_data_table(sampler$askYValues(), nrows = 0)
  expect_error(
    sampler$tellYValues(data.table(.id = 1:3, .score = c(0.3, 0.7, 0.4))),
    "Cannot tell Y when no Y was asked for"
  )

  # Tell X samples
  x.samples <- data.table(
    .id = 1:3,
    x1 = runif(3),
    x2 = runif(3)
  )
  remaining <- sampler$tellXSamples(x.samples)
  expect_identical(remaining, 3L)

  # Still in ASKING.X(3) state
  expect_data_table(sampler$askYValues(), nrows = 0)
  expect_error(
    sampler$tellYValues(data.table(.id = 1:3, .score = c(0.3, 0.7, 0.4))),
    "Cannot tell Y when no Y was asked for"
  )

  x.samples <- data.table(
    .id = 4:6,
    x1 = runif(3),
    x2 = runif(3)
  )
  sampler$next.x.samples <- 0L
  remaining <- sampler$tellXSamples(x.samples)
  expect_identical(remaining, 0L)

  # Now can ask for Y values
  y.request <- sampler$askYValues()
  expect_data_table(y.request, nrows = 6)
  expect_named(y.request, c("x1", "x2", ".id"))

  # Cannot ask for X samples while waiting for Y values
  expect_identical(sampler$askXSamples(), 0L)
  expect_error(sampler$tellXSamples(x.samples), "Cannot tell X when Y was asked")

  # Tell Y values
  y.values <- data.table(
    .id = 1:6,
    .score = c(0.3, 0.7, 0.4, 0.2, 0.6, 0.5)
  )
  sampler$next.x.samples <- 3L
  remaining <- sampler$tellYValues(y.values)
  expect_data_table(remaining, nrows = 0)
  expect_named(remaining, c("x1", "x2", ".id"))

  # Back to initial state
  expect_identical(sampler$askXSamples(), 3L)
})

test_that("RashomonSampler handles partial batches", {
  sampler <- DummyRashomonSampler$new(
    id = "dummy",
    domain = test.domain,
    minimize = TRUE,
    rashomon.epsilon = 0.1,
    rashomon.is.relative = FALSE,
    seed = 1L,
    n.rashomon.samples = 10L
  )

  n.samples <- sampler$askXSamples()

  # Tell X samples in two batches
  x.samples.1 <- data.table(
    .id = 1:2,
    x1 = runif(2),
    x2 = runif(2)
  )
  remaining <- sampler$tellXSamples(x.samples.1)
  expect_identical(remaining, 1L)

  x.samples.2 <- data.table(
    .id = 3L,
    x1 = runif(1),
    x2 = runif(1)
  )
  sampler$next.x.samples <- 0L
  remaining <- sampler$tellXSamples(x.samples.2)
  expect_identical(remaining, 0L)

  # Tell Y values in two batches
  y.request <- sampler$askYValues()
  expect_data_table(y.request, nrows = 3)

  y.values.1 <- data.table(
    .id = c(1L, 3L),
    .score = c(0.3, 0.7)
  )
  remaining <- sampler$tellYValues(y.values.1)
  expect_data_table(remaining, nrows = 1)
  expect_identical(remaining$.id, 2L)

  y.values.2 <- data.table(
    .id = 2L,
    .score = 0.4
  )
  sampler$next.x.samples <- 3L
  remaining <- sampler$tellYValues(y.values.2)
  expect_data_table(remaining, nrows = 0)
})

test_that("RashomonSampler tracks Rashomon set correctly", {
  sampler <- DummyRashomonSampler$new(
    id = "dummy",
    domain = test.domain,
    minimize = TRUE,
    rashomon.epsilon = 0.1,
    rashomon.is.relative = FALSE,
    seed = 1L,
    n.rashomon.samples = 10L
  )

  expect_identical(sampler$rashomonSamplesComplete(), 0L)

  # Process first batch
  sampler$askXSamples()
  x.samples <- data.table(
    .id = 1:3,
    x1 = runif(3),
    x2 = runif(3)
  )
  sampler$next.x.samples <- 0L
  sampler$tellXSamples(x.samples)

  y.values <- data.table(
    .id = 1:3,
    .score = c(0.3, 0.7, 0.4)
  )
  sampler$next.x.samples <- 3L
  y.request <- sampler$askYValues()
  expect_data_table(y.request, nrows = 3)
  expect_named(y.request, c("x1", "x2", ".id"))
  sampler$tellYValues(y.values)

  # Check Rashomon set (scores <= 0.5 are included)
  expect_identical(sampler$rashomonSamplesComplete(), 2L)
  rashomon.samples <- sampler$getRashomonSamples()
  expect_data_table(rashomon.samples, nrows = 2)
  expect_named(rashomon.samples, c("x1", "x2", ".id", ".score"))
  expect_true(all(rashomon.samples$.score <= 0.5))
})

test_that("RashomonSampler handles errors appropriately", {
  sampler <- DummyRashomonSampler$new(
    id = "dummy",
    domain = test.domain,
    minimize = TRUE,
    rashomon.epsilon = 0.1,
    rashomon.is.relative = FALSE,
    seed = 1L,
    n.rashomon.samples = 10L
  )

  # Cannot tell Y values when X was not asked
  expect_error(
    sampler$tellYValues(data.table(.id = 1L, .score = 0.5)),
    "Cannot tell Y when no Y was asked for"
  )

  # Cannot tell X values with wrong columns
  expect_error(
    sampler$tellXSamples(data.table(.id = 1L, wrong = 0.5)),
    "must.include"
  )

  # Cannot tell Y values with wrong IDs
  sampler$askXSamples()
  sampler$next.x.samples <- 0L
  sampler$tellXSamples(data.table(.id = 1:3, x1 = runif(3), x2 = runif(3)))
  sampler$askYValues()
  expect_error(
    sampler$tellYValues(data.table(.id = 4L, .score = 0.5)),
    "Gave .id values that were not asked for"
  )
})

test_that("RashomonSampler n.rashomon.samples can be changed", {
  sampler <- DummyRashomonSampler$new(
    id = "dummy",
    domain = test.domain,
    minimize = TRUE,
    rashomon.epsilon = 0.1,
    rashomon.is.relative = FALSE,
    seed = 1L,
    n.rashomon.samples = 10L
  )

  expect_identical(sampler$n.rashomon.samples, 10L)
  sampler$n.rashomon.samples <- 20L
  expect_identical(sampler$n.rashomon.samples, 20L)

  # Can change after samples have been requested
  sampler$askXSamples()
  sampler$n.rashomon.samples <- 30L
  expect_identical(sampler$n.rashomon.samples, 30L)

  # Tell incomplete X samples
  x.samples.1 <- data.table(
    .id = 1:2,
    x1 = runif(2),
    x2 = runif(2)
  )
  sampler$tellXSamples(x.samples.1)

  expect_error(
    sampler$n.rashomon.samples <- 40L,  # nolint
    "Cannot change n.rashomon.samples .* samples .* incompletely told"
  )
  expect_identical(sampler$n.rashomon.samples, 30L)
  x.samples.2 <- data.table(
    .id = 3L,
    x1 = runif(1),
    x2 = runif(1)
  )
  sampler$next.x.samples <- 0L
  sampler$tellXSamples(x.samples.2)
  sampler$n.rashomon.samples <- 40L
  expect_identical(sampler$n.rashomon.samples, 40L)
  sampler$askYValues()

  expect_error(
    sampler$n.rashomon.samples <- 50L,  # nolint
    "Cannot change n.rashomon.samples .* values .* asked"
  )
  expect_identical(sampler$n.rashomon.samples, 40L)
  y.values <- data.table(
    .id = c(1L, 3L),
    .score = c(0.3, 0.7)
  )
  sampler$tellYValues(y.values)
  expect_error(
    sampler$n.rashomon.samples <- 50L,  # nolint
    "Cannot change n.rashomon.samples .* values .* incompletely told"
  )
  expect_identical(sampler$n.rashomon.samples, 40L)

  sampler$next.x.samples <- 3L
  y.values.2 <- data.table(
    .id = 2L,
    .score = 0.4
  )
  sampler$tellYValues(y.values.2)

  sampler$n.rashomon.samples <- 50L
  expect_identical(sampler$n.rashomon.samples, 50L)
})

# Helper functions to check state
isInInitState <- function(sampler) {
  # In INIT state:
  # don't touch the object, just check that no internal functions have been called yet
  identical(
    unname(sampler$counts[c("askXSamples", "tellXSamples", "askYValues", "tellYValues")]),
    list(0L, 0L, 0L, 0L)
  )
}

isInAskingXState <- function(sampler, n = NULL) {
  # In ASKING.X(n) state:
  # - askXSamples() returns n
  # - askYValues() returns empty table
  # - tellYValues() errors
  if (!identical(sampler$askXSamples(), n)) return(FALSE)
  empty.y <- sampler$askYValues()
  if (!is.data.table(empty.y) || nrow(empty.y) > 0) return(FALSE)
  tryCatch({
    sampler$tellYValues(data.table(.id = 1L, .score = 0.5))
    return(FALSE)
  }, error = function(e) {
    if (!grepl("Cannot tell Y when no Y was asked for", e$message, fixed = TRUE)) stop(e)
    NULL
  })
  TRUE
}

isInAskingYState <- function(sampler, n = NULL) {
  # In ASKING.Y(n) state:
  # - askXSamples() returns 0
  # - askYValues() returns table with n rows
  # - tellXSamples() errors
  if (!identical(sampler$askXSamples(), 0L)) return(FALSE)
  y.request <- sampler$askYValues()
  if (!is.data.table(y.request) || nrow(y.request) != n) return(FALSE)
  tryCatch({
    sampler$tellXSamples(data.table(.id = 1L, x1 = 0.5, x2 = 0.5))
    return(FALSE)
  }, error = function(e) {
    if (!grepl("Cannot tell X when Y was asked", e$message, fixed = TRUE)) stop(e)
    NULL
  })
  TRUE
}

# Sampler that counts function calls for testing state transitions
CountingRashomonSampler <- R6Class("CountingRashomonSampler",
  inherit = RashomonSampler,
  public = list(
    initialize = function() {
      super$initialize(
        id = "counter",  # hardcoded test value
        domain = test.domain,  # hardcoded test value (defined at file level)
        minimize = TRUE,  # hardcoded test value
        rashomon.epsilon = 0.1,  # hardcoded test value
        rashomon.is.relative = FALSE,  # hardcoded test value
        seed = 1L,  # hardcoded test value
        n.rashomon.samples = 10L  # hardcoded test value
      )
      private$.counts <- list(
        askXSamples = 0L,
        tellXSamples = 0L,
        askYValues = 0L,
        tellYValues = 0L
      )
      private$.next.x.samples <- 3L
      private$.next.y.samples <- data.table()
      private$.x.samples <- data.table()
      private$.y.values <- data.table()
      private$.rashomon.samples <- data.table()
      private$.last.x.told <- NULL
      private$.last.y.told <- NULL
    }
  ),
  active = list(
    counts = function() private$.counts,
    next.x.samples = function(val) {
      if (!missing(val)) private$.next.x.samples <- val
      private$.next.x.samples
    },
    next.y.samples = function(val) {
      if (!missing(val)) private$.next.y.samples <- val
      private$.next.y.samples
    },
    last.x.told = function() private$.last.x.told,
    last.y.told = function() private$.last.y.told
  ),
  private = list(
    .counts = NULL,
    .next.x.samples = NULL,
    .next.y.samples = NULL,
    .x.samples = NULL,
    .y.values = NULL,
    .rashomon.samples = NULL,
    .last.x.told = NULL,
    .last.y.told = NULL,
    .askXSamples = function() {
      private$.counts$askXSamples <- private$.counts$askXSamples + 1L
      private$.next.x.samples
    },
    .tellXSamples = function(x) {
      private$.counts$tellXSamples <- private$.counts$tellXSamples + 1L
      private$.x.samples <- rbind(private$.x.samples, x)
      private$.last.x.told <- copy(x)  # deep copy to avoid modification
    },
    .askYValues = function() {
      private$.counts$askYValues <- private$.counts$askYValues + 1L
      private$.next.y.samples
    },
    .tellYValues = function(y) {
      private$.counts$tellYValues <- private$.counts$tellYValues + 1L
      private$.y.values <- rbind(private$.y.values, y)
      private$.last.y.told <- copy(y)  # deep copy to avoid modification
    },
    .rashomonSamplesComplete = function() 0L,
    .getRashomonSamples = function() getNullTable(self$domain, include.id = TRUE, include.score = TRUE)
  )
)

test_that("RashomonSampler state transitions from INIT work", {
  sampler <- CountingRashomonSampler$new()

  # Initially in INIT state
  expect_true(isInInitState(sampler))
  expect_identical(sampler$counts$askXSamples, 0L)

  # INIT -> TRANSITION -> ASKING.X(3)
  sampler$next.x.samples <- 3L
  expect_true(isInAskingXState(sampler, 3L))
  expect_identical(sampler$counts$askXSamples, 1L)
  expect_identical(sampler$counts$askYValues, 0L)
  expect_identical(sampler$counts$tellYValues, 0L)
  expect_identical(sampler$counts$tellXSamples, 0L)

  # Reset and test INIT -> TRANSITION -> ASKING.Y(2)
  sampler <- CountingRashomonSampler$new()
  sampler$next.x.samples <- 0L
  sampler$next.y.samples <- data.table(.id = 1:2, x1 = c(0.1, 0.2), x2 = c(0.3, 0.4))
  expect_true(isInAskingYState(sampler, 2L))
  expect_identical(sampler$counts$askXSamples, 1L)
  expect_identical(sampler$counts$askYValues, 1L)
  expect_identical(sampler$counts$tellYValues, 0L)
  expect_identical(sampler$counts$tellXSamples, 0L)
})

test_that("RashomonSampler transitions from ASKING.X work", {
  sampler <- CountingRashomonSampler$new()
  sampler$next.x.samples <- 3L
  expect_identical(sampler$askXSamples(), 3L)  # enter ASKING.X(3)
  expect_true(isInAskingXState(sampler, 3L))

  # ASKING.X(3) -> ASKING.X(1) via partial tellXSamples
  x.samples.1 <- data.table(.id = 1:2, x1 = c(0.1, 0.2), x2 = c(0.3, 0.4))
  remaining <- sampler$tellXSamples(x.samples.1)
  expect_identical(remaining, 1L)
  expect_true(isInAskingXState(sampler, 1L))
  expect_identical(sampler$counts$tellXSamples, 0L)  # private$.tellXSamples() only called when batch is complete

  # ASKING.X(1) -> ASKING.X(0) -> TRANSITION -> ASKING.X(2)
  sampler$next.x.samples <- 2L
  x.samples.2 <- data.table(.id = 3L, x1 = 0.5, x2 = 0.6)
  remaining <- sampler$tellXSamples(x.samples.2)
  expect_identical(remaining, 2L)
  expect_true(isInAskingXState(sampler, 2L))
  expect_identical(sampler$counts$tellXSamples, 1L)
  expect_equal(sampler$last.x.told,
    cbind(rbind(x.samples.1, x.samples.2), .score = NA_real_),
    ignore.col.order = TRUE, ignore.row.order = TRUE
  )

  # Reset and test ASKING.X(2) -> ASKING.X(0) -> TRANSITION -> ASKING.Y(3)
  sampler <- CountingRashomonSampler$new()
  sampler$next.x.samples <- 2L
  sampler$askXSamples()  # enter ASKING.X(2)

  sampler$next.x.samples <- 0L
  sampler$next.y.samples <- data.table(.id = 1:3, x1 = runif(3), x2 = runif(3))
  x.samples.3 <- data.table(.id = 1:2, x1 = c(0.1, 0.2), x2 = c(0.3, 0.4))
  remaining <- sampler$tellXSamples(x.samples.3)
  expect_identical(remaining, 0L)
  expect_true(isInAskingYState(sampler, 3L))
  expect_identical(sampler$counts$tellXSamples, 1L)
  expect_equal(sampler$last.x.told,
    cbind(x.samples.3, .score = NA_real_),
    ignore.col.order = TRUE, ignore.row.order = TRUE
  )
  expect_identical(sampler$counts$askXSamples, 2L)
  expect_identical(sampler$counts$askYValues, 1L)
  expect_identical(sampler$counts$tellYValues, 0L)
})

test_that("RashomonSampler transitions from ASKING.Y work", {
  sampler <- CountingRashomonSampler$new()
  sampler$next.x.samples <- 0L
  sampler$next.y.samples <- data.table(.id = 1:3, x1 = runif(3), x2 = runif(3), boguscol = runif(3))
  expect_identical(sampler$askXSamples(), 0L)  # enter ASKING.Y(3)
  expect_true(isInAskingYState(sampler, 3L))
  expect_equal(sampler$askYValues(), sampler$next.y.samples[, -"boguscol", with = FALSE], ignore.col.order = TRUE)

  # ASKING.Y(3) -> ASKING.Y(1) via partial tellYValues
  y.values.1 <- data.table(
    .id = c(1L, 3L),
    .score = c(0.1, 0.3),
    x1 = c(0.9, 0.8),  # spurious values, should be ignored
    x2 = c(0.7, 0.6)   # spurious values, should be ignored
  )
  remaining <- sampler$tellYValues(y.values.1)
  expect_data_table(remaining, nrows = 1)
  expect_equal(remaining, sampler$next.y.samples[2, -"boguscol", with = FALSE], ignore.col.order = TRUE)
  expect_true(isInAskingYState(sampler, 1L))
  expect_identical(sampler$counts$tellYValues, 0L)  # private$.tellYValues() only called when batch is complete

  # ASKING.Y(1) -> ASKING.Y(0) -> TRANSITION -> ASKING.X(2)
  sampler$next.x.samples <- 2L
  y.values.2 <- data.table(
    .id = 2L,
    .score = 0.2,
    x1 = 0.5,  # spurious value, should be ignored
    x2 = 0.4   # spurious value, should be ignored
  )
  remaining <- sampler$tellYValues(y.values.2)
  expect_data_table(remaining, nrows = 0)
  expect_true(isInAskingXState(sampler, 2L))
  expect_identical(sampler$counts$tellYValues, 1L)
  expect_equal(sampler$last.y.told,
    sampler$next.y.samples[
      rbind(y.values.1[, !c("x1", "x2")], y.values.2[, !c("x1", "x2")]),
      on = ".id"][, -"boguscol", with = FALSE],
    ignore.col.order = TRUE, ignore.row.order = TRUE
  )
  expect_identical(sampler$counts$askXSamples, 2L)
  expect_identical(sampler$counts$askYValues, 1L)
  expect_identical(sampler$counts$tellXSamples, 0L)

  # Reset and test ASKING.Y(2) -> ASKING.Y(0) -> TRANSITION -> ASKING.Y(1)
  sampler <- CountingRashomonSampler$new()
  sampler$next.x.samples <- 0L
  next.y.samples.keep <- data.table(.id = 1:2, x1 = runif(2), x2 = runif(2))
  sampler$next.y.samples <- next.y.samples.keep
  expect_identical(sampler$askXSamples(), 0L)  # enter ASKING.Y(2)
  expect_true(isInAskingYState(sampler, 2L))

  sampler$next.x.samples <- 0L
  sampler$next.y.samples <- data.table(.id = 3L, x1 = runif(1), x2 = runif(1))
  y.values.3 <- data.table(
    .id = c(2L, 1L),  # out of order
    .score = c(0.2, 0.1),
    x1 = c(0.9, 0.8),  # spurious values, should be ignored
    x2 = c(0.7, 0.6)   # spurious values, should be ignored
  )
  remaining <- sampler$tellYValues(y.values.3)
  expect_data_table(remaining, nrows = 1)
  expect_identical(remaining$.id, 3L)
  expect_identical(remaining$x1, sampler$next.y.samples$x1)
  expect_identical(remaining$x2, sampler$next.y.samples$x2)
  expect_true(isInAskingYState(sampler, 1L))
  expect_identical(sampler$counts$tellYValues, 1L)
  expect_equal(sampler$last.y.told,
    next.y.samples.keep[y.values.3[, !c("x1", "x2")], on = ".id"],
    ignore.col.order = TRUE, ignore.row.order = TRUE
  )
  expect_identical(sampler$counts$askXSamples, 2L)
  expect_identical(sampler$counts$askYValues, 2L)
  expect_identical(sampler$counts$tellXSamples, 0L)
})

test_that("RashomonSampler handles ID mismatches", {
  sampler <- CountingRashomonSampler$new()

  # Enter ASKING.Y state
  sampler$next.x.samples <- 0L
  sampler$next.y.samples <- data.table(.id = 1:2, x1 = runif(2), x2 = runif(2))
  expect_identical(sampler$askXSamples(), 0L)
  expect_equal(sampler$askYValues(), sampler$next.y.samples, ignore.col.order = TRUE)

  # Try to tell Y with wrong ID
  expect_error(
    sampler$tellYValues(data.table(.id = 3L, .score = 0.5)),
    "Gave .id values that were not asked for"
  )
})

# Sampler that tracks RNG values for testing RNG state independence
RngTestRashomonSampler <- R6Class("RngTestRashomonSampler",
  inherit = RashomonSampler,
  public = list(
    initialize = function(seed) {
      super$initialize(
        id = "rngtest",  # hardcoded test value
        domain = test.domain,  # hardcoded test value
        minimize = TRUE,  # hardcoded test value
        rashomon.epsilon = 0.1,  # hardcoded test value
        rashomon.is.relative = FALSE,  # hardcoded test value
        seed = seed,
        n.rashomon.samples = 10L  # hardcoded test value
      )
      private$.state <- "asking.x"
      private$.counts <- list(
        askXSamples = 0L,
        tellXSamples = 0L,
        askYValues = 0L,
        tellYValues = 0L
      )
      private$.rng.values <- list(
        askXSamples = numeric(),
        tellXSamples = numeric(),
        askYValues = numeric(),
        tellYValues = numeric()
      )
    }
  ),
  active = list(
    counts = function() private$.counts,
    rng.values = function() private$.rng.values
  ),
  private = list(
    .state = NULL,
    .counts = NULL,
    .rng.values = NULL,
    .askXSamples = function() {
      private$.counts$askXSamples <- private$.counts$askXSamples + 1L
      private$.rng.values$askXSamples <- c(private$.rng.values$askXSamples, runif(1))
      if (private$.state == "asking.x") 1L else 0L
    },
    .tellXSamples = function(x) {
      private$.counts$tellXSamples <- private$.counts$tellXSamples + 1L
      private$.rng.values$tellXSamples <- c(private$.rng.values$tellXSamples, runif(1))
      private$.state <- "asking.y"
    },
    .askYValues = function() {
      private$.counts$askYValues <- private$.counts$askYValues + 1L
      private$.rng.values$askYValues <- c(private$.rng.values$askYValues, runif(1))
      data.table(.id = 1L, x1 = 0.5, x2 = 0.5)
    },
    .tellYValues = function(y) {
      private$.counts$tellYValues <- private$.counts$tellYValues + 1L
      private$.rng.values$tellYValues <- c(private$.rng.values$tellYValues, runif(1))
      private$.state <- "asking.x"
    }
  )
)

test_that("RashomonSampler RNG state is independent", {
  # Same seed gives same results
  set.seed(1)
  sampler1 <- RngTestRashomonSampler$new(seed = 1L)
  set.seed(2)
  sampler2 <- RngTestRashomonSampler$new(seed = 1L)

  # Run through one complete cycle for both samplers
  expect_identical(sampler1$askXSamples(), 1L)  # -> ASKING.X(1)
  expect_identical(sampler2$askXSamples(), 1L)
  expect_identical(sampler1$counts$askXSamples, 1L)
  expect_identical(sampler2$counts$askXSamples, 1L)
  expect_identical(sampler1$rng.values$askXSamples, sampler2$rng.values$askXSamples)
  expect_numeric(sampler1$rng.values$askXSamples, len = 1)

  sampler1$tellXSamples(data.table(.id = 1L, x1 = 0.5, x2 = 0.5))  # -> ASKING.Y(1)
  sampler2$tellXSamples(data.table(.id = 1L, x1 = 0.5, x2 = 0.5))
  expect_identical(sampler1$counts$tellXSamples, 1L)
  expect_identical(sampler2$counts$tellXSamples, 1L)
  expect_identical(sampler1$rng.values$tellXSamples, sampler2$rng.values$tellXSamples)
  expect_numeric(sampler1$rng.values$tellXSamples, len = 1)

  y.request1 <- sampler1$askYValues()  # still ASKING.Y(1)
  y.request2 <- sampler2$askYValues()
  expect_identical(sampler1$counts$askYValues, 1L)
  expect_identical(sampler2$counts$askYValues, 1L)
  expect_identical(sampler1$rng.values$askYValues, sampler2$rng.values$askYValues)
  expect_numeric(sampler1$rng.values$askYValues, len = 1)

  sampler1$tellYValues(data.table(.id = 1L, .score = 0.5))  # -> ASKING.X(1)
  sampler2$tellYValues(data.table(.id = 1L, .score = 0.5))
  expect_identical(sampler1$counts$tellYValues, 1L)
  expect_identical(sampler2$counts$tellYValues, 1L)
  expect_identical(sampler1$rng.values$tellYValues, sampler2$rng.values$tellYValues)
  expect_numeric(sampler1$rng.values$tellYValues, len = 1)


  # Different seeds give different results
  sampler3 <- RngTestRashomonSampler$new(seed = 2L)
  sampler3$askXSamples()
  sampler3$tellXSamples(data.table(.id = 1L, x1 = 0.5, x2 = 0.5))
  sampler3$askYValues()
  sampler3$tellYValues(data.table(.id = 1L, .score = 0.5))

  expect_identical(sampler3$counts, sampler1$counts)

  expect_true(all(sampler1$rng.values$askXSamples != sampler3$rng.values$askXSamples))
  expect_true(all(sampler1$rng.values$tellXSamples != sampler3$rng.values$tellXSamples))
  expect_true(all(sampler1$rng.values$askYValues != sampler3$rng.values$askYValues))
  expect_true(all(sampler1$rng.values$tellYValues != sampler3$rng.values$tellYValues))

  # Outside RNG state does not affect inside RNG state
  set.seed(123)
  outside.rng <- runif(1)
  set.seed(123)
  sampler4 <- RngTestRashomonSampler$new(seed = 1L)
  sampler4$askXSamples()
  sampler4$tellXSamples(data.table(.id = 1L, x1 = 0.5, x2 = 0.5))
  sampler4$askYValues()
  sampler4$tellYValues(data.table(.id = 1L, .score = 0.5))
  expect_identical(runif(1), outside.rng)  # outside RNG state preserved
  expect_identical(sampler1$rng.values, sampler4$rng.values)  # inside RNG state same as sampler1
})

# Sampler that tracks column orders and content for testing
ColumnOrderRashomonSampler <- R6Class("ColumnOrderRashomonSampler",
  inherit = RashomonSampler,
  public = list(
    initialize = function() {
      super$initialize(
        id = "colorder",  # hardcoded test value
        domain = test.domain,  # hardcoded test value (defined at file level)
        minimize = TRUE,  # hardcoded test value
        rashomon.epsilon = 0.1,  # hardcoded test value
        rashomon.is.relative = FALSE,  # hardcoded test value
        seed = 1L,  # hardcoded test value
        n.rashomon.samples = 10L  # hardcoded test value
      )
      private$.state <- "asking.x"
      private$.next.x.samples <- 3L
      private$.next.y.samples <- data.table(
        .id = 1:2,
        x1 = 0.5,
        x2 = 0.6,
        boguscol = "ignore"  # should be ignored by public interface
      )
      private$.last.x.told <- NULL
      private$.last.y.told <- NULL
    }
  ),
  active = list(
    next.x.samples = function(val) {
      if (!missing(val)) private$.next.x.samples <- val
      private$.next.x.samples
    },
    next.y.samples = function(val) {
      if (!missing(val)) private$.next.y.samples <- val
      private$.next.y.samples
    },
    last.x.told = function() private$.last.x.told,
    last.y.told = function() private$.last.y.told
  ),
  private = list(
    .state = NULL,
    .next.x.samples = NULL,
    .next.y.samples = NULL,
    .last.x.told = NULL,
    .last.y.told = NULL,
    .askXSamples = function() {
      if (private$.state == "asking.x") private$.next.x.samples else 0L
    },
    .tellXSamples = function(x) {
      private$.last.x.told <- copy(x)  # deep copy to avoid modification
      private$.state <- "asking.y"
    },
    .askYValues = function() {
      private$.next.y.samples
    },
    .tellYValues = function(y) {
      private$.last.y.told <- copy(y)  # deep copy to avoid modification
      private$.state <- "asking.x"
    }
  )
)

test_that("RashomonSampler column orders are predictable", {
  # Test tellXSamples column order
  sampler1 <- ColumnOrderRashomonSampler$new()
  sampler2 <- ColumnOrderRashomonSampler$new()

  # Enter ASKING.X state
  expect_identical(sampler1$askXSamples(), 3L)
  expect_identical(sampler2$askXSamples(), 3L)

  # Tell X samples with different column orders and extra columns
  x.samples.1 <- data.table(
    x2 = c(0.4, 0.5, 0.6),
    .id = 1:3,
    x1 = c(0.1, 0.2, 0.3),
    extra = letters[1:3]
  )
  x.samples.2 <- data.table(
    x1 = c(0.1, 0.2, 0.3),
    extra2 = LETTERS[1:3],
    .id = 1:3,
    x2 = c(0.4, 0.5, 0.6),
    .score = c(0.7, 0.8, 0.9)  # should be ignored
  )

  sampler1$tellXSamples(x.samples.1)
  sampler2$tellXSamples(x.samples.2)

  # Check that private$.tellXSamples got same columns in same order
  expect_identical(names(sampler1$last.x.told), names(sampler2$last.x.told))  # nolint
  # Check that exactly the expected columns are present
  expect_identical(sort(names(sampler1$last.x.told)), sort(c(".id", ".score", test.domain$ids())))

  # Test askYValues column order
  y.request.1 <- sampler1$askYValues()
  y.request.2 <- sampler2$askYValues()

  # Check that public askYValues returns same columns in same order
  expect_identical(names(y.request.1), names(y.request.2))  # nolint
  # Check that exactly the expected columns are present
  expect_identical(sort(names(y.request.1)), sort(c(".id", test.domain$ids())))
  expect_false("boguscol" %in% names(y.request.1))


  # Test tellYValues column order with different scorecol
  y.values.1 <- data.table(
    x2 = c(0.4, 0.5),  # spurious, should be ignored
    .id = 1:2,
    myscore = c(0.1, 0.2),
    x1 = c(0.1, 0.2)   # spurious, should be ignored
  )
  y.values.2 <- data.table(
    x1 = c(0.1, 0.2),   # spurious, should be ignored
    otherscore = c(0.1, 0.2),
    .id = 1:2,
    x2 = c(0.4, 0.5)    # spurious, should be ignored
  )

  sampler1$tellYValues(y.values.1, scorecol = "myscore")
  sampler2$tellYValues(y.values.2, scorecol = "otherscore")

  # Check that private$.tellYValues got same columns in same order
  expect_identical(names(sampler1$last.y.told), names(sampler2$last.y.told))  # nolint
  # Check that exactly the expected columns are present
  expect_identical(sort(names(sampler1$last.y.told)), sort(c(".id", ".score", test.domain$ids())))

  expect_identical(sampler1$last.y.told$.score, y.values.1[sampler1$last.y.told, myscore, on = ".id"])
  expect_identical(sampler2$last.y.told$.score, y.values.2[sampler2$last.y.told, otherscore, on = ".id"])

  # Test column order consistency across multiple cycles
  sampler1$askXSamples()  # rotate back to ASKING.X
  sampler1$tellXSamples(x.samples.1[, 4:1, with = FALSE])  # second cycle
  # note sampler2 still has same order as before
  expect_identical(names(sampler1$last.x.told), names(sampler2$last.x.told))  # nolint

  sampler1$askYValues()
  # second cycle
  sampler1$tellYValues(setnames(y.values.1[, 4:1, with = FALSE], "myscore", "myscore2"), scorecol = "myscore2")
  expect_identical(names(sampler1$last.y.told), names(sampler2$last.y.told))  # nolint

})

test_that("RashomonSampler tellXSamples handles scorecol correctly", {
  sampler <- ColumnOrderRashomonSampler$new()
  sampler$next.x.samples <- 2L
  expect_identical(sampler$askXSamples(), 2L)  # enter ASKING.X(4)

  # Error on non-existing scorecol
  x.samples.1 <- data.table(.id = 1L, x1 = 0.1, x2 = 0.2)
  expect_error(
    sampler$tellXSamples(x.samples.1, scorecol = "nonexistent"),
    "but is missing elements.*'nonexistent'"
  )

  # Existing scorecol works
  x.samples.2 <- data.table(
    .id = 1:2,
    x1 = c(0.1, 0.2),
    x2 = c(0.3, 0.4),
    myscore = c(0.5, 0.6)
  )
  remaining <- sampler$tellXSamples(x.samples.2, scorecol = "myscore")
  expect_identical(remaining, 0L)
  expect_identical(sampler$last.x.told$.score, x.samples.2$myscore)

  # Incomplete batch without score, followed by batch with score
  sampler <- ColumnOrderRashomonSampler$new()
  sampler$next.x.samples <- 4L
  sampler$askXSamples()  # enter ASKING.X(4)

  x.samples.3 <- data.table(
    .id = 1:2,
    x1 = c(0.1, 0.2),
    x2 = c(0.3, 0.4)
  )
  remaining <- sampler$tellXSamples(x.samples.3)  # no scorecol
  expect_identical(remaining, 2L)
  expect_true(isInAskingXState(sampler, 2L))

  x.samples.4 <- data.table(
    .id = 3:4,
    x1 = c(0.5, 0.6),
    x2 = c(0.7, 0.8),
    score2 = c(0.9, 1.0)
  )
  remaining <- sampler$tellXSamples(x.samples.4, scorecol = "score2")
  expect_identical(remaining, 0L)
  expect_equal(
    sampler$last.x.told,
    data.table(
      .id = 1:4,
      x1 = c(0.1, 0.2, 0.5, 0.6),
      x2 = c(0.3, 0.4, 0.7, 0.8),
      .score = c(NA_real_, NA_real_, 0.9, 1.0)
    ),
    ignore.col.order = TRUE
  )

  # Incomplete batch with score, followed by batch without score
  sampler <- ColumnOrderRashomonSampler$new()
  sampler$next.x.samples <- 4L
  sampler$askXSamples()  # enter ASKING.X(4)

  x.samples.5 <- data.table(
    .id = 1:2,
    x1 = c(0.1, 0.2),
    x2 = c(0.3, 0.4),
    score3 = c(0.5, 0.6)
  )
  remaining <- sampler$tellXSamples(x.samples.5, scorecol = "score3")
  expect_identical(remaining, 2L)
  expect_true(isInAskingXState(sampler, 2L))

  x.samples.6 <- data.table(
    .id = 3:4,
    x1 = c(0.7, 0.8),
    x2 = c(0.9, 1.0)
  )
  remaining <- sampler$tellXSamples(x.samples.6)  # no scorecol
  expect_identical(remaining, 0L)
  expect_equal(
    sampler$last.x.told,
    data.table(
      .id = 1:4,
      x1 = c(0.1, 0.2, 0.7, 0.8),
      x2 = c(0.3, 0.4, 0.9, 1.0),
      .score = c(0.5, 0.6, NA_real_, NA_real_)
    ),
    ignore.col.order = TRUE
  )
})
