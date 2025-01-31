test_that("RashomonSamplerRandom initialization works", {
  domain <- ps(
    x1 = p_dbl(0, 1),
    x2 = p_dbl(0, 1)
  )

  sampler <- RashomonSamplerRandom$new(
    id = "random",
    domain = domain,
    minimize = TRUE,
    rashomon.epsilon = 0.1,
    rashomon.is.relative = FALSE,
    seed = 1L,
    n.rashomon.samples = 10L,
    initial.sample.size = 100L,
    batchsize = 50L
  )

  expect_identical(sampler$id, "random")
  expect_identical(sampler$domain, domain)
  expect_true(sampler$minimize)
  expect_identical(sampler$rashomon.epsilon, 0.1)
  expect_false(sampler$rashomon.is.relative)
  expect_identical(sampler$n.rashomon.samples, 10L)

  sampler <- RashomonSamplerRandom$new(
    id = "random",
    domain = domain,
    minimize = FALSE,
    rashomon.epsilon = 0.1,
    rashomon.is.relative = TRUE,
    seed = 1L,
    n.rashomon.samples = 10L,
    initial.sample.size = 100L,
    batchsize = 50L
  )
  expect_false(sampler$minimize)
  expect_true(sampler$rashomon.is.relative)
})

test_that("RashomonSamplerRandom handles relative epsilon correctly", {
  domain <- ps(
    x1 = p_dbl(0, 1),
    x2 = p_dbl(0, 1)
  )

  sampler <- RashomonSamplerRandom$new(
    id = "random",
    domain = domain,
    minimize = TRUE,
    rashomon.epsilon = 0.1,
    rashomon.is.relative = TRUE,
    seed = 1L,
    n.rashomon.samples = 10L,
    initial.sample.size = 100L,
    batchsize = 50L
  )

  # Add some samples with known scores
  samples <- data.table(
    .id = 1:100,
    x1 = seq(0, 1, length.out = 100),
    x2 = seq(0, 1, length.out = 100),
    .score = seq(10, 20, length.out = 100)
  )

  expect_identical(sampler$tellXSamples(samples), 0L)
  y.values <- sampler$askYValues()
  expect_equal(y.values, samples[, -".score", with = FALSE], ignore.col.order = TRUE)
  expect_identical(sampler$rashomonSamplesComplete(), 0L)
  sampler$tellYValues(samples)

  expect_equal(sampler$getRashomonSamples(), samples[.score <= 11][, -".id", with = FALSE], ignore.col.order = TRUE, ignore.row.order = TRUE)
  expect_identical(sampler$rashomonSamplesComplete(), nrow(samples[.score <= 11]))
  expect_gt(sampler$rashomonSamplesComplete(), 1)  # make sure we don't accidentally have a case where this is 0 or 1

  samples.2 <- data.table(
    .id = 101:150,
    x1 = seq(0, 1, length.out = 50),
    x2 = seq(1, 0, length.out = 50),
    .score = seq(9.8, 20, length.out = 50)
  )

  expect_identical(sampler$tellXSamples(samples.2), 0L)
  y.values <- sampler$askYValues()
  expect_equal(y.values, samples.2[, -".score", with = FALSE], ignore.col.order = TRUE)
  sampler$tellYValues(samples.2)

  expect_equal(
    sampler$getRashomonSamples(),
    rbind(samples[.score <= 9.8 * 1.1], samples.2[.score <= 9.8 * 1.1])[, -".id", with = FALSE],
    ignore.col.order = TRUE, ignore.row.order = TRUE
  )
  expect_identical(
    sampler$rashomonSamplesComplete(),
    nrow(samples[.score <= 9.8 * 1.1]) + nrow(samples.2[.score <= 9.8 * 1.1])
  )
  expect_gt(sampler$rashomonSamplesComplete(), 1)  # make sure we don't accidentally have a case where this is 0 or 1
})

test_that("RashomonSamplerRandom handles absolute epsilon correctly", {
  domain <- ps(
    x1 = p_dbl(0, 1),
    x2 = p_dbl(0, 1)
  )

  sampler <- RashomonSamplerRandom$new(
    id = "random",
    domain = domain,
    minimize = TRUE,
    rashomon.epsilon = 2,
    rashomon.is.relative = FALSE,  # absolute epsilon
    seed = 1L,
    n.rashomon.samples = 10L,
    initial.sample.size = 100L,
    batchsize = 50L
  )

  # Add some samples with known scores
  samples <- data.table(
    .id = 1:100,
    x1 = seq(0, 1, length.out = 100),
    x2 = seq(0, 1, length.out = 100),
    .score = seq(10, 20, length.out = 100)
  )

  expect_identical(sampler$tellXSamples(samples), 0L)
  y.values <- sampler$askYValues()
  expect_equal(y.values, samples[, -".score", with = FALSE], ignore.col.order = TRUE)
  expect_identical(sampler$rashomonSamplesComplete(), 0L)
  sampler$tellYValues(samples)

  # With absolute epsilon of 2, samples with scores up to 12 should be in Rashomon set
  expect_equal(sampler$getRashomonSamples(), samples[.score <= 12][, -".id", with = FALSE], ignore.col.order = TRUE, ignore.row.order = TRUE)
  expect_identical(sampler$rashomonSamplesComplete(), nrow(samples[.score <= 12]))
  expect_gt(sampler$rashomonSamplesComplete(), 1)  # make sure we don't accidentally have a case where this is 0 or 1

  samples.2 <- data.table(
    .id = 101:150,
    x1 = seq(0, 1, length.out = 50),
    x2 = seq(1, 0, length.out = 50),
    .score = seq(9.8, 20, length.out = 50)
  )

  expect_identical(sampler$tellXSamples(samples.2), 0L)
  y.values <- sampler$askYValues()
  expect_equal(y.values, samples.2[, -".score", with = FALSE], ignore.col.order = TRUE)
  sampler$tellYValues(samples.2)

  expect_equal(
    sampler$getRashomonSamples(),
    rbind(samples[.score <= 11.8], samples.2[.score <= 11.8])[, -".id", with = FALSE],
    ignore.col.order = TRUE, ignore.row.order = TRUE
  )
  expect_identical(
    sampler$rashomonSamplesComplete(),
    nrow(samples[.score <= 11.8]) + nrow(samples.2[.score <= 11.8])
  )
  expect_gt(sampler$rashomonSamplesComplete(), 1)  # make sure we don't accidentally have a case where this is 0 or 1
})

test_that("RashomonSamplerRandom handles maximization correctly (absolute epsilon)", {
  domain <- ps(
    x1 = p_dbl(0, 1),
    x2 = p_dbl(0, 1)
  )

  sampler <- RashomonSamplerRandom$new(
    id = "random",
    domain = domain,
    minimize = FALSE,  # maximize
    rashomon.epsilon = 2,  # large enough to include multiple samples
    rashomon.is.relative = FALSE,
    seed = 1L,
    n.rashomon.samples = 10L,
    initial.sample.size = 100L,
    batchsize = 50L
  )

  # Add some samples with known scores
  samples <- data.table(
    .id = 1:100,
    x1 = seq(0, 1, length.out = 100),
    x2 = seq(0, 1, length.out = 100),
    .score = seq(10, 20, length.out = 100)
  )

  expect_identical(sampler$tellXSamples(samples), 0L)
  y.values <- sampler$askYValues()
  expect_equal(y.values, samples[, -".score", with = FALSE], ignore.col.order = TRUE)
  expect_identical(sampler$rashomonSamplesComplete(), 0L)
  sampler$tellYValues(samples)

  # With absolute epsilon of 2, samples with scores down to 18 should be in Rashomon set
  expect_equal(sampler$getRashomonSamples(), samples[.score >= 18][, -".id", with = FALSE],
    ignore.col.order = TRUE, ignore.row.order = TRUE)
  expect_identical(sampler$rashomonSamplesComplete(), nrow(samples[.score >= 18]))
  expect_gt(sampler$rashomonSamplesComplete(), 1)  # make sure we don't accidentally have a case where this is 0 or 1

  samples.2 <- data.table(
    .id = 101:150,
    x1 = seq(0, 1, length.out = 50),
    x2 = seq(1, 0, length.out = 50),
    .score = seq(14, 21, length.out = 50)  # overlaps with first batch but includes better values
  )

  expect_identical(sampler$tellXSamples(samples.2), 0L)
  y.values <- sampler$askYValues()
  expect_equal(y.values, samples.2[, -".score", with = FALSE], ignore.col.order = TRUE)
  sampler$tellYValues(samples.2)

  expect_equal(
    sampler$getRashomonSamples(),
    rbind(samples[.score >= 19], samples.2[.score >= 19])[, -".id", with = FALSE],  # both batches contribute
    ignore.col.order = TRUE, ignore.row.order = TRUE
  )
  expect_identical(
    sampler$rashomonSamplesComplete(),
    nrow(samples[.score >= 19]) + nrow(samples.2[.score >= 19])
  )
  expect_gt(sampler$rashomonSamplesComplete(), 1)  # make sure we don't accidentally have a case where this is 0 or 1
})


test_that("RashomonSamplerRandom handles maximization correctly (relative epsilon)", {
  domain <- ps(
    x1 = p_dbl(0, 1),
    x2 = p_dbl(0, 1)
  )

  sampler <- RashomonSamplerRandom$new(
    id = "random",
    domain = domain,
    minimize = FALSE,  # maximize
    rashomon.epsilon = 0.1,  # relative epsilon of 10%
    rashomon.is.relative = TRUE,
    seed = 1L,
    n.rashomon.samples = 10L,
    initial.sample.size = 100L,
    batchsize = 50L
  )

  # Add some samples with known scores
  samples <- data.table(
    .id = 1:100,
    x1 = seq(0, 1, length.out = 100),
    x2 = seq(0, 1, length.out = 100),
    .score = seq(10, 20, length.out = 100)
  )

  expect_identical(sampler$tellXSamples(samples), 0L)
  y.values <- sampler$askYValues()
  expect_equal(y.values, samples[, -".score", with = FALSE], ignore.col.order = TRUE)
  expect_identical(sampler$rashomonSamplesComplete(), 0L)
  sampler$tellYValues(samples)

  # With relative epsilon of 0.1, samples with scores down to 18 should be in Rashomon set
  expect_equal(sampler$getRashomonSamples(), samples[.score >= 18][, -".id", with = FALSE],
    ignore.col.order = TRUE, ignore.row.order = TRUE)
  expect_identical(sampler$rashomonSamplesComplete(), nrow(samples[.score >= 18]))
  expect_gt(sampler$rashomonSamplesComplete(), 1)  # make sure we don't accidentally have a case where this is 0 or 1

  samples.2 <- data.table(
    .id = 101:150,
    x1 = seq(0, 1, length.out = 50),
    x2 = seq(1, 0, length.out = 50),
    .score = seq(14, 21, length.out = 50)  # overlaps with first batch but includes better values
  )

  expect_identical(sampler$tellXSamples(samples.2), 0L)
  y.values <- sampler$askYValues()
  expect_equal(y.values, samples.2[, -".score", with = FALSE], ignore.col.order = TRUE)
  sampler$tellYValues(samples.2)

  expect_equal(
    sampler$getRashomonSamples(),
    rbind(samples[.score >= 18.9], samples.2[.score >= 18.9])[, -".id", with = FALSE],  # both batches contribute
    ignore.col.order = TRUE, ignore.row.order = TRUE
  )
  expect_identical(
    sampler$rashomonSamplesComplete(),
    nrow(samples[.score >= 18.9]) + nrow(samples.2[.score >= 18.9])
  )
  expect_gt(sampler$rashomonSamplesComplete(), 1)  # make sure we don't accidentally have a case where this is 0 or 1
})

test_that("RashomonSamplerRandom requests correct batch sizes", {
  domain <- ps(
    x1 = p_dbl(0, 1),
    x2 = p_dbl(0, 1)
  )

  sampler <- RashomonSamplerRandom$new(
    id = "random",
    domain = domain,
    minimize = TRUE,
    rashomon.epsilon = 0.1,
    rashomon.is.relative = FALSE,
    seed = 1L,
    n.rashomon.samples = 10L,
    initial.sample.size = 100L,
    batchsize = 50L
  )

  # First request should be initial.sample.size
  expect_identical(sampler$askXSamples(), 100L)

  # Provide samples but no scores yet
  x.samples <- data.table(
    .id = 1:100,
    x1 = runif(100),
    x2 = runif(100)
  )
  expect_identical(sampler$tellXSamples(x.samples), 0L)

  # Request Y values for all samples
  y.request <- sampler$askYValues()
  expect_identical(nrow(y.request), 100L)

  # Provide scores that don't yield enough Rashomon samples
  y.values <- data.table(.id = 1:100, .score = runif(100, 0.5, 1.5))
  expect_identical(nrow(sampler$tellYValues(y.values)), 0L)

  # Next X request should be batchsize
  expect_identical(sampler$askXSamples(), 50L)
})

test_that("RashomonSamplerRandom handles partial scores in tellXSamples correctly", {
  domain <- ps(
    x1 = p_dbl(0, 1),
    x2 = p_dbl(0, 1)
  )

  sampler <- RashomonSamplerRandom$new(
    id = "random",
    domain = domain,
    minimize = TRUE,
    rashomon.epsilon = 2,
    rashomon.is.relative = FALSE,
    seed = 1L,
    n.rashomon.samples = 10L,
    initial.sample.size = 100L,
    batchsize = 50L
  )

  # Create samples where every second row has a score
  samples <- data.table(
    .id = 1:100,
    x1 = seq(0, 1, length.out = 100),
    x2 = seq(0, 1, length.out = 100),
    .score = NA_real_
  )
  samples[seq(1, 100, by = 2), ".score" := seq(10, 20, length.out = 50)]

  expect_identical(sampler$tellXSamples(samples, scorecol = ".score"), 0L)
  y.values <- sampler$askYValues()
  # Should only request rows without scores
  expect_identical(nrow(y.values), 50L)
  expect_equal(y.values, samples[is.na(.score), -".score", with = FALSE], ignore.col.order = TRUE)

  # Provide remaining scores
  remaining.scores <- data.table(
    .id = samples[is.na(.score)]$.id,
    .score = seq(9.8, 19.8, length.out = 50)
  )
  # order in which we pass rows is ignored
  sampler$tellYValues(remaining.scores[rev(seq_len(nrow(remaining.scores)))])

  samples[is.na(.score), .score := remaining.scores$.score]
  # With absolute epsilon of 2, samples with scores up to 11.8 should be in Rashomon set
  expect_equal(
    sampler$getRashomonSamples(),
    samples[.score <= 11.8][, -".id", with = FALSE],
    ignore.col.order = TRUE, ignore.row.order = TRUE
  )
  expect_identical(sampler$rashomonSamplesComplete(), nrow(samples[.score <= 11.8]))
  expect_gt(sampler$rashomonSamplesComplete(), 1)
})
