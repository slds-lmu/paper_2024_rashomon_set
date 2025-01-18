# Common test objects
test.domain <- ps(
  x1 = p_dbl(0, 1),
  x2 = p_dbl(0, 1)
)

test.domain.cat <- ps(
  x1 = p_dbl(0, 1),
  cat = p_fct(c("a", "b", "c"))
)

extra.trafo <- function(x) {
  x$x1 <- 2 * x$x1  # scale x1 to [0, 2]
  x$x2 <- x$x2 + 1  # shift x2 to [1, 2]
  x
}

test.domain.trafo <- ps(
  x1 = p_dbl(0, 1),
  x2 = p_dbl(0, 1),
  .extra_trafo = extra.trafo
)

test_that("ObjectiveStreamSynthetic initialization works", {
  obj <- ObjectiveStreamSynthetic$new(
    objective = function(x) sum(unlist(x)),
    id = "synthetic",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  expect_identical(obj$id, "synthetic")
  expect_identical(obj$domain, test.domain)
  expect_true(obj$minimize)
  expect_function(obj$objective)

  # Test with non-minimization
  obj.max <- ObjectiveStreamSynthetic$new(
    objective = function(x) sum(unlist(x)),
    id = "synthetic",
    domain = test.domain,
    minimize = FALSE,
    seed = c(1L, 2L)
  )

  expect_false(obj.max$minimize)
})

test_that("ObjectiveStreamSynthetic sampling works", {
  obj <- ObjectiveStreamSynthetic$new(
    objective = function(x) sum(unlist(x)),
    id = "synthetic",
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
})

test_that("ObjectiveStreamSynthetic evaluation works", {
  # Simple sum objective
  obj.sum <- ObjectiveStreamSynthetic$new(
    objective = function(x) sum(unlist(x)),
    id = "synthetic",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  samples <- obj.sum$sample(3)
  values <- obj.sum$eval(samples)
  expect_numeric(values, len = 3)
  expect_identical(values, rowSums(as.matrix(samples[, .(x1, x2)])))

  # Test with more complex objective
  obj.complex <- ObjectiveStreamSynthetic$new(
    objective = function(x) x$x1^2 + cos(x$x2),
    id = "synthetic",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  samples <- obj.complex$sample(3)
  values <- obj.complex$eval(samples)
  expect_numeric(values, len = 3)
  expect_identical(values, samples$x1^2 + cos(samples$x2))
})

test_that("ObjectiveStreamSynthetic handles domain transformations", {
  obj <- ObjectiveStreamSynthetic$new(
    objective = function(x) sum(unlist(x)),
    id = "synthetic",
    domain = test.domain.trafo,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  samples <- obj$sample(3)
  values <- obj$eval(samples)

  # Manual transformation to verify results
  transformed <- lapply(seq_len(nrow(samples)), function(i) {
    extra.trafo(as.list(samples[i, .(x1, x2)]))
  })
  expected <- vapply(transformed, function(x) sum(unlist(x)), numeric(1))

  expect_identical(values, expected)

  # check that trafo is not actually a no-op: using a domain without trafo should give different results
  obj.notrafo <- ObjectiveStreamSynthetic$new(
    objective = function(x) sum(unlist(x)),
    id = "synthetic",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  samples.notrafo <- obj.notrafo$sample(3)
  expect_identical(samples, samples.notrafo)
  expect_false(identical(values, obj.notrafo$eval(samples)))
})

test_that("ObjectiveStreamSynthetic respects seeds", {
  # Same seed gives same results
  obj1 <- ObjectiveStreamSynthetic$new(
    objective = function(x) sum(unlist(x)),
    id = "synthetic",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  obj2 <- ObjectiveStreamSynthetic$new(
    objective = function(x) sum(unlist(x)),
    id = "synthetic",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  expect_identical(obj1$sample(5), obj2$sample(5))

  samples1 <- obj1$sample(3)
  samples2 <- obj2$sample(3)
  expect_identical(obj1$eval(samples1), obj2$eval(samples2))

  # Different seeds give different results
  obj3 <- ObjectiveStreamSynthetic$new(
    objective = function(x) sum(unlist(x)),
    id = "synthetic",
    domain = test.domain,
    minimize = TRUE,
    seed = c(2L, 3L)
  )

  expect_false(identical(obj1$sample(5), obj3$sample(5)))
})

test_that("ObjectiveStreamSynthetic handles categorical variables", {
  obj <- ObjectiveStreamSynthetic$new(
    objective = function(x) as.numeric(x$cat == "a"),  # 1 if cat is "a", 0 otherwise
    id = "synthetic_cat",
    domain = test.domain.cat,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  samples <- obj$sample(10)
  expect_factor(samples$cat)
  expect_identical(levels(samples$cat), c("a", "b", "c"))

  values <- obj$eval(samples)
  expect_numeric(values, len = 10)
  expect_true(all(values %in% c(0, 1)))
  expect_identical(values, as.numeric(samples$cat == "a"))
  expect_length(unique(values), 2L)  # make sure we got both 0 and 1, need to change seed if this fails.
})

test_that("ObjectiveStreamSynthetic handles errors appropriately", {
  # Test with invalid objective function
  expect_error(
    ObjectiveStreamSynthetic$new(
      objective = "not a function",
      id = "synthetic",
      domain = test.domain,
      minimize = TRUE,
      seed = c(1L, 2L)
    ),
    "Must be a function"
  )

  # Test with objective function returning invalid values
  obj.invalid <- ObjectiveStreamSynthetic$new(
    objective = function(x) "not a number",
    id = "synthetic",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  samples <- obj.invalid$sample(2)
  expect_error(obj.invalid$eval(samples), "must be type 'double'")

  # Test with objective function returning wrong length
  obj.wrong.length <- ObjectiveStreamSynthetic$new(
    objective = function(x) c(1, 2),
    id = "synthetic",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  samples <- obj.wrong.length$sample(1)
  expect_error(obj.wrong.length$eval(samples), "must be length 1")
})

test_that("ObjectiveStreamSynthetic evaluation has independent RNG state", {
  # Objective that generates random values
  random.objective <- function(x) runif(length(x$x1))

  # Test that sequential evaluation equals combined evaluation
  obj1 <- ObjectiveStreamSynthetic$new(
    objective = random.objective,
    id = "synthetic",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  obj2 <- ObjectiveStreamSynthetic$new(
    objective = random.objective,
    id = "synthetic",
    domain = test.domain,
    minimize = TRUE,
    seed = c(1L, 2L)
  )

  samples1 <- obj1$sample(3)
  samples2 <- obj2$sample(3)
  values1 <- obj1$eval(samples1)
  values2 <- obj2$eval(samples2)
  expect_identical(values1, values2)

  # Sequential sampling and evaluation
  samples1a <- obj1$sample(3)
  values1a <- obj1$eval(samples1a)
  samples1b <- obj1$sample(2)
  values1b <- obj1$eval(samples1b)
  sequential.values <- c(values1a, values1b)

  # Combined sampling and evaluation
  samples2 <- obj2$sample(5)
  combined.values <- obj2$eval(samples2)

  expect_identical(sequential.values, combined.values)

  # Test that different seeds give different results
  obj3 <- ObjectiveStreamSynthetic$new(
    objective = random.objective,
    id = "synthetic",
    domain = test.domain,
    minimize = TRUE,
    seed = c(2L, 3L)
  )

  samples3 <- obj3$sample(3)
  values3 <- obj3$eval(samples3)

  expect_false(identical(values1, values3))

  # Test that eval RNG state is independent of session RNG state
  set.seed(123)
  rng.state <- .Random.seed

  samples7 <- obj1$sample(3)
  values7 <- obj1$eval(samples7)
  expect_identical(.Random.seed, rng.state)
})
