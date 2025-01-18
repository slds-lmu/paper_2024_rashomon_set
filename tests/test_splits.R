test_that("generateCanonicalDataSplits() RNG state is reset correctly", {
  rm(".Random.seed", envir = .GlobalEnv)
  expect_false(".Random.seed" %in% names(.GlobalEnv))

  splits <- generateCanonicalDataSplits(task.bh, seed = 1)
  expect_false(".Random.seed" %in% names(.GlobalEnv))

  set.seed(2)
  xngs <- .Random.seed
  splits2 <- generateCanonicalDataSplits(task.bh, seed = 1)
  expect_identical(.Random.seed, xngs)

  expect_identical(lapply(splits, as.data.table), lapply(splits2, as.data.table))
})

test_that("generateCanonicalDataSplits() produces correct split sizes", {
  splits <- generateCanonicalDataSplits(task.bh, ratio = 0.7, seed = 42)

  expect_identical(splits$training$nrow, as.integer(round(task.bh$nrow * 0.7)))
  expect_identical(splits$validation$nrow, as.integer(task.bh$nrow - round(task.bh$nrow * 0.7)))
  expect_identical(splits$training$nrow + splits$validation$nrow, task.bh$nrow)
})

test_that("generateCanonicalDataSplits() produces consistent results with same seed", {
  splits1 <- generateCanonicalDataSplits(task.bh, ratio = 0.8, seed = 123)
  splits2 <- generateCanonicalDataSplits(task.bh, ratio = 0.8, seed = 123)

  expect_identical(splits1$training$row_ids, splits2$training$row_ids)
  expect_identical(splits1$validation$row_ids, splits2$validation$row_ids)
})

test_that("generateCanonicalDataSplits() produces different results with different seeds", {
  splits1 <- generateCanonicalDataSplits(task.bh, ratio = 0.75, seed = 456)
  splits2 <- generateCanonicalDataSplits(task.bh, ratio = 0.75, seed = 789)

  expect_false(identical(splits1$training$row_ids, splits2$training$row_ids))
  expect_false(identical(splits1$validation$row_ids, splits2$validation$row_ids))
})

test_that("generateCanonicalDataSplits() works with different tasks", {
  splits.gc <- generateCanonicalDataSplits(task.gc, ratio = 0.6, seed = 101)
  splits.cs <- generateCanonicalDataSplits(task.cs, ratio = 0.6, seed = 101)
  splits.bs <- generateCanonicalDataSplits(task.bs, ratio = 0.6, seed = 101)

  expect_identical(sort(c(splits.gc$training$row_ids, splits.gc$validation$row_ids)), sort(task.gc$row_ids))
  expect_identical(sort(c(splits.cs$training$row_ids, splits.cs$validation$row_ids)), sort(task.cs$row_ids))
  expect_identical(sort(c(splits.bs$training$row_ids, splits.bs$validation$row_ids)), sort(task.bs$row_ids))
})

test_that("generateCanonicalDataSplits() handles edge cases", {
  expect_error(generateCanonicalDataSplits(task.bh, ratio = -1), "is not >= 0")
  expect_error(generateCanonicalDataSplits(task.bh, ratio = 2), "is not <= 1")

  splits.min <- generateCanonicalDataSplits(task.bh, ratio = 1e-10, seed = 999)
  expect_identical(splits.min$training$nrow, 1L)
  expect_identical(splits.min$validation$nrow, task.bh$nrow - 1L)

  splits.max <- generateCanonicalDataSplits(task.bh, ratio = 1 - 1e-10, seed = 999)
  expect_identical(splits.max$training$nrow, task.bh$nrow - 1L)
  expect_identical(splits.max$validation$nrow, 1L)
})
