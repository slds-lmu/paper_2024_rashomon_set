


test_that("generateCanonicalDataSplits() RNG state is reset correctly", {
  rm(".Random.seed", envir = .GlobalEnv)
  expect_false(".Random.seed" %in% names(.GlobalEnv))

  splits <- generateCanonicalDataSplits(task.bh, seed = 1)
  expect_false(".Random.seed" %in% names(.GlobalEnv))

  set.seed(2)
  xngs <- .Random.seed
  splits2 <- generateCanonicalDataSplits(task.bh, seed = 1)
  expect_equal(.Random.seed, xngs)

  expect_equal(lapply(splits, as.data.table), lapply(splits2, as.data.table))
})

test_that("generateCanonicalDataSplits() produces correct split sizes", {
  splits <- generateCanonicalDataSplits(task.bh, ratio = 0.7, seed = 42)

  expect_equal(splits$training$nrow, round(task.bh$nrow * 0.7))
  expect_equal(splits$validation$nrow, task.bh$nrow - round(task.bh$nrow * 0.7))
  expect_equal(splits$training$nrow + splits$validation$nrow, task.bh$nrow)
})

test_that("generateCanonicalDataSplits() produces consistent results with same seed", {
  splits1 <- generateCanonicalDataSplits(task.bh, ratio = 0.8, seed = 123)
  splits2 <- generateCanonicalDataSplits(task.bh, ratio = 0.8, seed = 123)

  expect_equal(splits1$training$row_ids, splits2$training$row_ids)
  expect_equal(splits1$validation$row_ids, splits2$validation$row_ids)
})

test_that("generateCanonicalDataSplits() produces different results with different seeds", {
  splits1 <- generateCanonicalDataSplits(task.bh, ratio = 0.75, seed = 456)
  splits2 <- generateCanonicalDataSplits(task.bh, ratio = 0.75, seed = 789)

  expect_false(identical(splits1$training$row_ids, splits2$training$row_ids))
  expect_false(identical(splits1$validation$row_ids, splits2$validation$row_ids))
})

test_that("generateCanonicalDataSplits() works with different tasks", {
  splits_gc <- generateCanonicalDataSplits(task.gc, ratio = 0.6, seed = 101)
  splits_cs <- generateCanonicalDataSplits(task.cs, ratio = 0.6, seed = 101)
  splits_bs <- generateCanonicalDataSplits(task.bs, ratio = 0.6, seed = 101)

  expect_equal(sort(c(splits_gc$training$row_ids, splits_gc$validation$row_ids)), sort(task.gc$row_ids))
  expect_equal(sort(c(splits_cs$training$row_ids, splits_cs$validation$row_ids)), sort(task.cs$row_ids))
  expect_equal(sort(c(splits_bs$training$row_ids, splits_bs$validation$row_ids)), sort(task.bs$row_ids))
})

test_that("generateCanonicalDataSplits() handles edge cases", {
  expect_error(generateCanonicalDataSplits(task.bh, ratio = -1), "is not >= 0")
  expect_error(generateCanonicalDataSplits(task.bh, ratio = 2), "is not <= 1")

  splits_min <- generateCanonicalDataSplits(task.bh, ratio = 1e-10, seed = 999)
  expect_equal(splits_min$training$nrow, 1)
  expect_equal(splits_min$validation$nrow, task.bh$nrow - 1)

  splits_max <- generateCanonicalDataSplits(task.bh, ratio = 1 - 1e-10, seed = 999)
  expect_equal(splits_max$training$nrow, task.bh$nrow - 1)
  expect_equal(splits_max$validation$nrow, 1)
})



