# Common test objects
test.domain <- ps(
  x1 = p_dbl(0, 1),
  x2 = p_dbl(0, 1)
)

test.domain.cat <- ps(
  x1 = p_dbl(0, 1),
  cat = p_fct(c("a", "b", "c"))
)

# Dummy implementation for testing
DummyObjectiveStreamActual <- R6Class("DummyObjectiveStreamActual",
  inherit = ObjectiveStreamActual,
  private = list(
    .eval = function(x) {
      # Order-dependent evaluation: 2 * first_col + second_col
      # This way we can detect if columns are in wrong order
      2 * x[[1]] + x[[2]]
    }
  )
)

test_that("ObjectiveStreamActual initialization works", {
  obj <- ObjectiveStreamActual$new(
    id = "actual",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  expect_identical(obj$id, "actual")
  expect_identical(obj$domain, test.domain)
  expect_true(obj$minimize)

  obj <- ObjectiveStreamActual$new(
    id = "actual",
    domain = test.domain,
    minimize = FALSE,
    seed = c(1L, 2L)
  )

  expect_false(obj$minimize)
})

test_that("ObjectiveStreamActual sampling works", {
  obj <- ObjectiveStreamActual$new(
    id = "actual",
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

test_that("ObjectiveStreamActual samples are uniformly distributed", {
  obj <- ObjectiveStreamActual$new(
    id = "actual",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  # Generate large sample for statistical testing
  n <- 10000
  samples <- obj$sample(n)

  # Test uniformity using Kolmogorov-Smirnov test
  ks.x1 <- ks.test(samples$x1, "punif", 0, 1)
  ks.x2 <- ks.test(samples$x2, "punif", 0, 1)

  expect_gt(ks.x1$p.value, 0.01)
  expect_lt(ks.x1$p.value, 0.99)
  expect_gt(ks.x2$p.value, 0.01)
  expect_lt(ks.x2$p.value, 0.99)

  # Test independence using correlation test
  cor.test <- cor.test(samples$x1, samples$x2)
  expect_gt(cor.test$p.value, 0.01)
})

test_that("ObjectiveStreamActual respects seeds", {
  # Same seed gives same results
  obj1 <- ObjectiveStreamActual$new(
    id = "actual",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  obj2 <- ObjectiveStreamActual$new(
    id = "actual",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  expect_identical(obj1$sample(5), obj2$sample(5))

  # Different seeds give different results
  obj3 <- ObjectiveStreamActual$new(
    id = "actual",
    domain = test.domain,
    minimize = TRUE,
    seed = c(2L, 3L)
  )

  obj4 <- ObjectiveStreamActual$new(
    id = "actual",
    domain = test.domain,
    minimize = TRUE,
    seed = c(3L, 4L)
  )

  expect_false(identical(obj3$sample(5), obj4$sample(5)))
})

test_that("ObjectiveStreamActual sampling is independent of external RNG state", {
  set.seed(123)
  rng.state <- .Random.seed

  obj <- ObjectiveStreamActual$new(
    id = "actual",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  obj$sample(10)
  expect_identical(.Random.seed, rng.state)
})

test_that("ObjectiveStreamActual sequential sampling is equivalent to single sampling", {
  obj1 <- ObjectiveStreamActual$new(
    id = "actual",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  obj2 <- ObjectiveStreamActual$new(
    id = "actual",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  # Sequential sampling
  samples1 <- rbind(obj1$sample(3), obj1$sample(2))

  # Single sampling
  samples2 <- obj2$sample(5)

  expect_identical(samples1, samples2)

  # Test with zero samples
  samples3 <- rbind(obj1$sample(0), obj1$sample(2))
  samples4 <- obj2$sample(2)

  expect_identical(samples3, samples4)

  samples5 <- rbind(obj1$sample(2), obj1$sample(0))
  samples6 <- obj2$sample(2)

  expect_identical(samples5, samples6)
})

test_that("ObjectiveStreamActual handles categorical variables", {
  obj.cat <- ObjectiveStreamActual$new(
    id = "actual_cat",
    domain = test.domain.cat,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  samples <- obj.cat$sample(5)
  expect_factor(samples$cat)
  expect_identical(levels(samples$cat), c("a", "b", "c"))
})

test_that("ObjectiveStreamActual evaluation works", {
  obj <- DummyObjectiveStreamActual$new(
    id = "dummy",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  samples <- obj$sample(3)
  values <- obj$eval(samples)
  expect_numeric(values, len = 3)
  expect_identical(values, 2 * samples$x1 + samples$x2)
})

test_that("ObjectiveStreamActual handles wrong eval column order", {
  obj <- DummyObjectiveStreamActual$new(
    id = "dummy",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  samples <- obj$sample(3)
  # Reorder columns
  reordered <- samples[, .(x2, x1, .id)]

  # Both should give same result because eval() should reorder columns to match domain
  values1 <- obj$eval(samples)  # original order
  values2 <- obj$eval(reordered)  # reordered columns

  expect_identical(values1, values2)
  expect_identical(values1, 2 * samples$x1 + samples$x2)

  test.domain.reordered <- ps(
    x2 = p_dbl(0, 1),
    x1 = p_dbl(0, 1)
  )

  # Verify that without reordering, results would be different
  obj2 <- DummyObjectiveStreamActual$new(
    id = "dummy",
    domain = test.domain.reordered,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  samples.reordered <- obj2$sample(3)

  # In some sense we are cheating here, because `samples` were not generated using the reordered domain, and the
  # contract of the class would require us to only pass samples generated using the domain.
  values3 <- obj2$eval(samples)

  # values1 and values3 were generated from the same samples, but the different order of parameter names in the domain
  # should give different values to the evaluation function. If the evaluation function happens to be column order
  # independent, then values1 and values3 would be identical.
  expect_false(identical(values1, values3))
})
