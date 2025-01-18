# Dummy implementation for testing
DummyObjectiveStream <- R6Class("DummyObjectiveStream",
  inherit = ObjectiveStream,
  private = list(
    .eval = function(x) {
      # Simple sum of values for testing
      rowSums(as.matrix(x[, -".id", with = FALSE]))
    },
    .sample = function(n) {
      # Generate deterministic samples for testing
      data.table(
        x1 = seq_len(n) / (n + 1),
        x2 = rev(seq_len(n)) / (n + 1)
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

# Common test objects
test.domain <- ps(
  x1 = p_dbl(0, 1),
  x2 = p_dbl(0, 1)
)

test.domain.cat <- ps(
  x1 = p_dbl(0, 1),
  cat = p_fct(c("a", "b", "c"))
)

test_that("ObjectiveStream initialization works", {
  obj <- DummyObjectiveStream$new(
    id = "dummy",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  expect_identical(obj$id, "dummy")
  expect_identical(obj$domain, test.domain)
  expect_true(obj$minimize)

  obj <- DummyObjectiveStream$new(
    id = "dummy",
    domain = test.domain,
    minimize = FALSE,
    seed = c(1L, 2L)
  )

  expect_false(obj$minimize)
})

test_that("ObjectiveStream sampling works", {
  obj <- DummyObjectiveStream$new(
    id = "dummy",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  samples <- obj$sample(3)
  expect_data_table(samples, nrows = 3)
  expect_named(samples, c("x1", "x2", ".id"))
  expect_identical(samples$.id, 1:3)
  expect_true(all(samples$x1 >= 0 & samples$x1 <= 1))
  expect_true(all(samples$x2 >= 0 & samples$x2 <= 1))

  # Test empty sample
  empty.samples <- obj$sample(0)
  expect_data_table(empty.samples, nrows = 0)
  expect_named(empty.samples, c("x1", "x2", ".id"))
  expect_identical(empty.samples$.id, integer(0))
})

test_that("ObjectiveStream evaluation works", {
  obj <- DummyObjectiveStream$new(
    id = "dummy",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  samples <- obj$sample(3)
  values <- obj$eval(samples)
  expect_numeric(values, len = 3)
  expect_identical(values, rowSums(as.matrix(samples[, .(x1, x2)])))
})

test_that("ObjectiveStream row retrieval works", {
  obj <- DummyObjectiveStream$new(
    id = "dummy",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  samples <- obj$sample(3)
  row.1 <- obj$getRow(1)
  expect_data_table(row.1, nrows = 1)
  expect_named(row.1, c("x1", "x2", ".id"))
  expect_identical(row.1$.id, 1L)
  expect_identical(row.1[, .(x1, x2)], samples[1, .(x1, x2)])
})

test_that("ObjectiveStream error handling works", {
  obj <- DummyObjectiveStream$new(
    id = "dummy",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  obj$sample(3)
  expect_error(obj$eval(data.table(wrong.col = 1)), "must include")
  expect_error(obj$getRow(0), "is not >= 1")
  expect_error(obj$getRow(4), "is not <= 3")
  expect_error(obj$sample(-1), "must be >= 0")
})

test_that("ObjectiveStream categorical variables work", {
  obj.cat <- DummyObjectiveStreamCat$new(
    id = "dummy_cat",
    domain = test.domain.cat,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  samples.cat <- obj.cat$sample(5)
  expect_factor(samples.cat$cat)
  expect_identical(levels(samples.cat$cat), c("a", "b", "c"))
})

test_that("ObjectiveStream handles wrong sample columns", {
  # Create a domain with different columns than what DummyObjectiveStream returns
  wrong.domain <- ps(
    x3 = p_dbl(0, 1),
    x4 = p_dbl(0, 1)
  )

  expect_error(
    DummyObjectiveStream$new(
      id = "dummy",
      domain = wrong.domain,
      minimize = TRUE,
      seed = c(1L, 2L)
    )$sample(1),
    "must.include.*x3.*x4"
  )
})

test_that("ObjectiveStream respects domain column order", {
  # Create domain with reversed column order
  reversed.domain <- ps(
    x2 = p_dbl(0, 1),
    x1 = p_dbl(0, 1)
  )

  obj <- DummyObjectiveStream$new(
    id = "dummy",
    domain = reversed.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  samples <- obj$sample(3)
  expect_identical(names(samples)[1:2], c("x2", "x1"))
})

test_that("ObjectiveStream handles wrong eval column order", {
  # Create dummy class that verifies column order
  OrderCheckingObjectiveStream <- R6Class("OrderCheckingObjectiveStream",
    inherit = ObjectiveStream,
    private = list(
      .eval = function(x) {
        # Check that columns are in the exact order specified by domain
        expected.cols <- c(self$domain$ids(), ".id")
        expect_named(
          x,
          expected.cols,
          info = "Columns must be in domain order followed by .id"
        )
        rep(1, nrow(x))
      },
      .sample = function(n) {
        data.table(
          x1 = runif(n),
          x2 = runif(n)
        )
      }
    )
  )

  obj <- OrderCheckingObjectiveStream$new(
    id = "order_check",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  samples <- obj$sample(3)
  # Reorder columns
  reordered <- samples[, .(x2, x1, .id)]

  # Both should work - the public eval() method should reorder columns
  obj$eval(samples)  # original order
  obj$eval(reordered)  # reordered columns
})
