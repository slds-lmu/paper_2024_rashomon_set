# Common test objects
test.domain <- ps(
  x1 = p_dbl(0, 1),
  x2 = p_dbl(0, 1)
)

test.domain.cat <- ps(
  x1 = p_dbl(0, 1),
  cat = p_fct(c("a", "b", "c"))
)

test_that("ObjectiveStreamRecorded initialization works", {
  test.table <- data.table(
    x1 = c(0.1, 0.2, 0.3),
    x2 = c(0.4, 0.5, 0.6),
    score = c(1, 2, 3)
  )

  obj <- ObjectiveStreamRecorded$new(
    id = "recorded",
    domain = test.domain,
    minimize = TRUE,
    table = test.table
  )

  expect_identical(obj$id, "recorded")
  expect_identical(obj$domain, test.domain)
  expect_true(obj$minimize)

  # Test custom score column name
  test.table.custom <- data.table(
    x1 = c(0.1, 0.2),
    x2 = c(0.4, 0.5),
    custom_score = c(1, 2)
  )

  obj.custom <- ObjectiveStreamRecorded$new(
    id = "recorded",
    domain = test.domain,
    minimize = TRUE,
    table = test.table.custom,
    scorecol = "custom_score"
  )

  expect_error(
    ObjectiveStreamRecorded$new(
      id = "recorded",
      domain = test.domain,
      minimize = TRUE,
      table = test.table,
      scorecol = "nonexistent"
    ),
    "Must be element of set"
  )
})

test_that("ObjectiveStreamRecorded sequential sampling works", {
  test.table <- data.table(
    x1 = c(0.1, 0.2, 0.3),
    x2 = c(0.4, 0.5, 0.6),
    score = c(1, 2, 3)
  )

  obj <- ObjectiveStreamRecorded$new(
    id = "recorded",
    domain = test.domain,
    minimize = TRUE,
    table = test.table
  )

  # First sample
  samples.1 <- obj$sample(2)
  expect_data_table(samples.1, nrows = 2)
  expect_named(samples.1, c("x1", "x2", ".id"))
  expect_identical(samples.1$x1, c(0.1, 0.2))
  expect_identical(samples.1$x2, c(0.4, 0.5))
  expect_identical(samples.1$.id, c(1L, 2L))

  # Second sample
  samples.2 <- obj$sample(1)
  expect_data_table(samples.2, nrows = 1)
  expect_identical(samples.2$x1, 0.3)
  expect_identical(samples.2$x2, 0.6)
  expect_identical(samples.2$.id, 3L)

  # No more samples available
  expect_error(obj$sample(1), "No more samples available")
})

test_that("ObjectiveStreamRecorded evaluation works", {
  test.table <- data.table(
    x1 = c(0.1, 0.2, 0.3),
    x2 = c(0.4, 0.5, 0.6),
    score = c(1, 2, 3)
  )

  obj <- ObjectiveStreamRecorded$new(
    id = "recorded",
    domain = test.domain,
    minimize = TRUE,
    table = test.table
  )

  samples <- obj$sample(2)
  values <- obj$eval(samples)
  expect_numeric(values, len = 2)
  expect_identical(values, c(1, 2))

  # Test evaluation of rows in different order
  reordered <- samples[c(2, 1)]
  values.reordered <- obj$eval(reordered)
  expect_identical(values.reordered, c(2, 1))

  # Test custom score column
  test.table.custom <- data.table(
    x1 = c(0.1, 0.2, 0.3),
    x2 = c(0.4, 0.5, 0.6),
    custom_score = c(10, 20, 30)
  )

  obj.custom <- ObjectiveStreamRecorded$new(
    id = "recorded",
    domain = test.domain,
    minimize = TRUE,
    table = test.table.custom,
    scorecol = "custom_score"
  )

  samples.custom <- obj.custom$sample(2)
  values.custom <- obj.custom$eval(samples.custom)
  expect_identical(values.custom, c(10, 20))
})

test_that("ObjectiveStreamRecorded handles categorical variables", {
  test.table.cat <- data.table(
    x1 = c(0.1, 0.2, 0.3),
    cat = c("a", "b", "c"),
    score = c(1, 2, 3)
  )

  obj.cat <- ObjectiveStreamRecorded$new(
    id = "recorded_cat",
    domain = test.domain.cat,
    minimize = TRUE,
    table = test.table.cat
  )

  samples <- obj.cat$sample(2)
  expect_factor(samples$cat)
  expect_identical(levels(samples$cat), c("a", "b", "c"))
})

test_that("ObjectiveStreamRecorded handles empty samples", {
  test.table <- data.table(
    x1 = c(0.1, 0.2),
    x2 = c(0.4, 0.5),
    score = c(1, 2)
  )

  obj <- ObjectiveStreamRecorded$new(
    id = "recorded",
    domain = test.domain,
    minimize = TRUE,
    table = test.table
  )

  empty.samples <- obj$sample(0)
  expect_data_table(empty.samples, nrows = 0)
  expect_named(empty.samples, c("x1", "x2", ".id"))

  samples.1 <- obj$sample(2)
  expect_data_table(samples.1, nrows = 2)
  expect_named(samples.1, c("x1", "x2", ".id"))
  expect_identical(samples.1$x1, c(0.1, 0.2))
  expect_identical(samples.1$x2, c(0.4, 0.5))
  expect_identical(samples.1$.id, c(1L, 2L))
})

test_that("ObjectiveStreamRecorded validates input table", {
  # Missing required column
  test.table.missing <- data.table(
    x1 = c(0.1, 0.2),
    score = c(1, 2)
  )

  expect_error(
    ObjectiveStreamRecorded$new(
      id = "recorded",
      domain = test.domain,
      minimize = TRUE,
      table = test.table.missing
    ),
    "must.include"
  )

  # Score column conflicts with domain
  test.domain.conflict <- ps(
    x1 = p_dbl(0, 1),
    score = p_dbl(0, 1)
  )

  test.table <- data.table(
    x1 = c(0.1, 0.2),
    score = c(1, 2)
  )

  expect_error(
    ObjectiveStreamRecorded$new(
      id = "recorded",
      domain = test.domain.conflict,
      minimize = TRUE,
      table = test.table
    ),
    "TRUE"  # checks !any(self$domain$ids() == scorecol)
  )
})

test_that("ObjectiveStreamRecorded respects domain column order", {
  # Create table with reversed column order
  test.table.reordered <- data.table(
    x2 = c(0.4, 0.5, 0.6),
    x1 = c(0.1, 0.2, 0.3),
    score = c(1, 2, 3)
  )

  obj <- ObjectiveStreamRecorded$new(
    id = "recorded",
    domain = test.domain,  # domain has x1, then x2
    minimize = TRUE,
    table = test.table.reordered
  )

  samples <- obj$sample(2)

  # Check that columns are in domain order, not table order
  expect_named(samples, c("x1", "x2", ".id"))
  expect_identical(samples$x1, c(0.1, 0.2))
  expect_identical(samples$x2, c(0.4, 0.5))

  # Check that evaluation still works with reordered columns
  values <- obj$eval(samples)
  expect_identical(values, c(1, 2))
})
