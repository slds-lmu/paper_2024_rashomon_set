

test_that("ObjectiveStreamGP initialization works", {
  obj <- ObjectiveStreamGP$new(
    lengthscales = 0.1,
    noise = 0.01,
    id = "gp",
    seed = c(1L, 2L)
  )

  expect_identical(obj$id, "gp")
  # some internals do differ because of the way the paramset is constructed
  expect_identical(as.data.table(obj$domain), as.data.table(ps(x.d1 = p_dbl(0, 1))))
  expect_true(obj$minimize)
  expect_identical(obj$lengthscales, 0.1)
  expect_identical(obj$noise, 0.01)
  expect_identical(obj$kernel, "matern52")

  # Test with different kernel
  obj.se <- ObjectiveStreamGP$new(
    lengthscales = 0.1,
    noise = 0.01,
    id = "gp",
    seed = c(1L, 2L),
    kernel = "se"
  )
  expect_identical(obj.se$kernel, "se")

  # Test with multiple dimensions
  obj.2d <- ObjectiveStreamGP$new(
    lengthscales = c(0.1, 0.2),
    noise = 0.01,
    id = "gp_2d",
    seed = c(1L, 2L)
  )
  expect_identical(obj.2d$lengthscales, c(0.1, 0.2))
  expect_identical(obj.2d$dimensions, 2L)
  expect_identical(as.data.table(obj.2d$domain), as.data.table(ps(x.d1 = p_dbl(0, 1), x.d2 = p_dbl(0, 1))))

  expect_error(ObjectiveStreamGP$new(
    lengthscales = -0.1,  # negative lengthscale
    noise = 0.01,
    id = "gp",
    seed = c(1L, 2L)
  ), "Element 1 is not >= 0")

  expect_error(ObjectiveStreamGP$new(
    lengthscales = 0.1,
    noise = -0.01,  # negative noise
    id = "gp",
    seed = c(1L, 2L)
  ), "Element 1 is not >= 0")
})

test_that("ObjectiveStreamGP sampling works", {
  obj <- ObjectiveStreamGP$new(
    lengthscales = 0.1,
    noise = 0.01,
    id = "gp",
    seed = c(1L, 2L)
  )

  samples <- obj$sample(3)
  expect_data_table(samples, nrows = 3)
  expect_named(samples, c("x.d1", ".id"))
  expect_identical(samples$.id, 1:3)
  expect_true(all(samples$x.d1 >= 0 & samples$x.d1 <= 1))

  # Test empty sample
  empty.samples <- obj$sample(0)
  expect_data_table(empty.samples, nrows = 0)
  expect_named(empty.samples, c("x.d1", ".id"))

  # Test 2D sampling
  obj.2d <- ObjectiveStreamGP$new(
    lengthscales = c(0.1, 0.2),
    noise = 0.01,
    id = "gp_2d",
    seed = c(1L, 2L)
  )

  samples.2d <- obj.2d$sample(3)
  expect_data_table(samples.2d, nrows = 3)
  expect_named(samples.2d, c("x.d1", "x.d2", ".id"))
  expect_true(all(samples.2d$x.d1 >= 0 & samples.2d$x.d1 <= 1))
  expect_true(all(samples.2d$x.d2 >= 0 & samples.2d$x.d2 <= 1))
})

test_that("ObjectiveStreamGP evaluation works with different kernels", {
  kernels <- c("matern32", "matern52", "se")

  for (kernel in kernels) {
    obj <- ObjectiveStreamGP$new(
      lengthscales = 0.1,
      noise = 0.01,
      id = "gp",
      seed = c(1L, 2L),
      kernel = kernel
    )

    samples <- obj$sample(5)
    values <- obj$eval(samples)
    expect_numeric(values, len = 5, finite = TRUE)
  }
})

test_that("ObjectiveStreamGP respects seeds", {
  # Same seed gives same results
  obj1 <- ObjectiveStreamGP$new(
    lengthscales = 0.1,
    noise = 0.01,
    id = "gp",
    seed = c(1L, 2L)
  )

  obj2 <- ObjectiveStreamGP$new(
    lengthscales = 0.1,
    noise = 0.01,
    id = "gp",
    seed = c(1L, 2L)
  )

  expect_identical(obj1$sample(5), obj2$sample(5))

  samples1 <- obj1$sample(3)
  samples2 <- obj2$sample(3)
  expect_identical(obj1$eval(samples1), obj2$eval(samples2))

  # Different sampling seed gives different samples
  obj3 <- ObjectiveStreamGP$new(
    lengthscales = 0.1,
    noise = 0.01,
    id = "gp",
    seed = c(2L, 2L)
  )

  expect_false(identical(obj1$sample(5), obj3$sample(5)))

  # Different evaluation seed gives different results (when noise > 0)
  obj4 <- ObjectiveStreamGP$new(
    lengthscales = 0.1,
    noise = 0.01,
    id = "gp",
    seed = c(1L, 3L)
  )

  obj5 <- ObjectiveStreamGP$new(
    lengthscales = 0.1,
    noise = 0.01,
    id = "gp",
    seed = c(1L, 4L)
  )

  samples4 <- obj4$sample(3)
  samples5 <- obj5$sample(3)
  expect_identical(samples4, samples5)  # same sampling seed
  expect_false(identical(obj4$eval(samples4), obj5$eval(samples5)))  # different eval seed
})

test_that("ObjectiveStreamGP produces correlated samples", {
  obj <- ObjectiveStreamGP$new(
    lengthscales = 0.1,
    noise = 0,  # no noise for clearer correlation
    id = "gp",
    seed = c(2L, 3L)
  )

  # Generate samples at regular intervals
  x.test <- data.table(
    x.d1 = seq(0, 1, length.out = 100),
    .id = seq_len(100)
  )

  # this is strictly speaking illegal, since we are testing with values that were not sampled
  y.test <- obj$eval(x.test)

  # Check autocorrelation at different lags
  acf.result <- acf(y.test, plot = FALSE)

  # Values should be correlated at small lags
  expect_gt(acf.result$acf[2], 0.8)  # lag 1 correlation should be substantial
  # Correlation should decrease with lag
  expect_numeric(diff(acf.result$acf[2:15]), upper = 0)
})

test_that("ObjectiveStreamGP respects lengthscales", {
  obj.short <- ObjectiveStreamGP$new(
    lengthscales = 0.1,  # short lengthscale
    noise = 0,
    id = "gp_short",
    seed = c(2L, 3L)
  )

  obj.long <- ObjectiveStreamGP$new(
    lengthscales = 0.5,  # long lengthscale
    noise = 0,
    id = "gp_long",
    seed = c(2L, 3L)
  )

  x.test <- data.table(
    x.d1 = seq(0, 1, length.out = 100),
    .id = seq_len(100)
  )

  y.short <- obj.short$eval(x.test)
  y.long <- obj.long$eval(x.test)

  # Shorter lengthscale should result in more variation
  expect_gt(mean(abs(diff(y.short))), mean(abs(diff(y.long))))

  # Check autocorrelation
  acf.short <- acf(y.short, plot = FALSE)
  acf.long <- acf(y.long, plot = FALSE)

  # Longer lengthscale should maintain correlation for more lags
  expect_true(all(acf.long$acf[4:15] > acf.short$acf[4:15]))
})

test_that("Re-evaluating ObjectiveStreamGP with same samples gives similar results (up to tolerance)", {
  obj <- ObjectiveStreamGP$new(
    lengthscales = 0.1,
    noise = 0,  # no noise to ensure exact matching
    id = "gp",
    seed = c(2L, 3L)
  )

  # First evaluation
  samples1 <- obj$sample(3)
  values1 <- obj$eval(samples1)

  # Second evaluation with some overlap
  samples2 <- rbind(samples1[1:2], obj$sample(1))
  values2 <- obj$eval(samples2)

  # Check that overlapping points have identical values
  expect_equal(values1[1:2], values2[1:2], tolerance = 1e-3)
})

test_that("ObjectiveStreamGP evaluation is independent of external RNG state", {
  set.seed(123)
  rng.state <- .Random.seed

  obj <- ObjectiveStreamGP$new(
    lengthscales = 0.1,
    noise = 0.01,
    id = "gp",
    seed = c(1L, 2L)
  )

  samples <- obj$sample(3)
  values <- obj$eval(samples)
  expect_identical(.Random.seed, rng.state)
})

test_that("Evaluating ObjectiveStreamGP one-by-one gives the same result as batch evaluation", {
  obj1 <- ObjectiveStreamGP$new(
    lengthscales = 0.1,
    noise = 0.01,
    id = "gp",
    seed = c(2L, 3L)
  )

  obj2 <- ObjectiveStreamGP$new(
    lengthscales = 0.1,
    noise = 0.01,
    id = "gp",
    seed = c(2L, 3L)
  )

  obj3 <- ObjectiveStreamGP$new(
    lengthscales = 0.1,
    noise = 0.01,
    id = "gp",
    seed = c(2L, 3L)
  )

  samples1 <- obj1$sample(3)
  values1 <- obj1$eval(samples1)

  samples2.1 <- obj2$sample(1)
  values2.1 <- obj2$eval(samples2.1)

  samples2.2 <- obj2$sample(2)
  values2.2 <- obj2$eval(samples2.2)

  samples3 <- obj3$sample(3)
  values3.1 <- obj3$eval(samples3[1])
  values3.2 <- obj3$eval(samples3[2])
  values3.3 <- obj3$eval(samples3[3])

  expect_identical(samples1, rbind(samples2.1, samples2.2))

  expect_equal(values1, c(values2.1, values2.2), tolerance = 1e-9)

  expect_equal(values1, c(values3.1, values3.2, values3.3), tolerance = 1e-9)

  # the same with 3 dimensions
  obj1.3d <- ObjectiveStreamGP$new(
    lengthscales = c(0.1, 0.2, 0.3),
    noise = 0.01,
    id = "gp",
    seed = c(2L, 3L)
  )
  obj2.3d <- ObjectiveStreamGP$new(
    lengthscales = c(0.1, 0.2, 0.3),
    noise = 0.01,
    id = "gp",
    seed = c(2L, 3L)
  )
  obj3.3d <- ObjectiveStreamGP$new(
    lengthscales = c(0.1, 0.2, 0.3),
    noise = 0.01,
    id = "gp",
    seed = c(2L, 3L)
  )

  samples1.3d <- obj1.3d$sample(3)
  values1.3d <- obj1.3d$eval(samples1.3d)

  samples2.1.3d <- obj2.3d$sample(1)
  values2.1.3d <- obj2.3d$eval(samples2.1.3d)

  samples2.2.3d <- obj2.3d$sample(2)
  values2.2.3d <- obj2.3d$eval(samples2.2.3d)

  samples3.3d <- obj3.3d$sample(3)
  values3.1.3d <- obj3.3d$eval(samples3.3d[1])
  values3.2.3d <- obj3.3d$eval(samples3.3d[2])
  values3.3.3d <- obj3.3d$eval(samples3.3d[3])

  expect_identical(samples1.3d, rbind(samples2.1.3d, samples2.2.3d))
  expect_equal(values1.3d, c(values2.1.3d, values2.2.3d), tolerance = 1e-9)
  expect_equal(values1.3d, c(values3.1.3d, values3.2.3d, values3.3.3d), tolerance = 1e-9)
})

test_that("ObjectiveStreamGP noise scaling works as expected", {
  obj.nonoise <- ObjectiveStreamGP$new(
    lengthscales = 0.1,
    noise = 0,
    id = "gp",
    seed = c(2L, 3L)
  )
  obj.lownoise <- ObjectiveStreamGP$new(
    lengthscales = 0.1,
    noise = 0.01,
    id = "gp",
    seed = c(2L, 3L)
  )
  obj.highnoise <- ObjectiveStreamGP$new(
    lengthscales = 0.1,
    noise = 1,
    id = "gp",
    seed = c(2L, 3L)
  )
  samples.nonoise <- obj.nonoise$sample(500)
  samples.lownoise <- obj.lownoise$sample(500)
  samples.highnoise <- obj.highnoise$sample(500)

  values.nonoise.1 <- obj.nonoise$eval(samples.nonoise[1:400])
  values.nonoise.2 <- obj.nonoise$eval(samples.nonoise[401:500])

  values.lownoise.1 <- obj.lownoise$eval(samples.lownoise[1:400])
  values.lownoise.2 <- obj.lownoise$eval(samples.lownoise[401:500])

  values.highnoise.1 <- obj.highnoise$eval(samples.highnoise[1:400])
  values.highnoise.2 <- obj.highnoise$eval(samples.highnoise[401:500])

  expect_equal(sd(values.nonoise.1 - values.lownoise.1), 0.01, tolerance = 1e-4)
  expect_equal(sd(values.nonoise.1 - values.highnoise.1), 1, tolerance = 1e-2)

  expect_equal(sd(values.nonoise.2 - values.lownoise.2), 0.01, tolerance = 1e-3)
  expect_equal(sd(values.nonoise.2 - values.highnoise.2), 1, tolerance = 1e-1)

  expect_equal(mean(values.nonoise.1 - values.lownoise.1), 0, tolerance = 0.05 / sqrt(400))
  expect_equal(mean(values.nonoise.1 - values.highnoise.1), 0, tolerance = 5 / sqrt(400))

  expect_equal(mean(values.nonoise.2 - values.lownoise.2), 0, tolerance = 0.05 / sqrt(100))
  expect_equal(mean(values.nonoise.2 - values.highnoise.2), 0, tolerance = 5 / sqrt(100))

  # noise is just added to the values, so difference to no-noise-condition scales with `noise`
  expect_equal((values.nonoise.1 - values.lownoise.1) * 100, (values.nonoise.1 - values.highnoise.1), tolerance = 1e-10)
  expect_equal((values.nonoise.2 - values.lownoise.2) * 100, (values.nonoise.2 - values.highnoise.2), tolerance = 1e-10)

})
