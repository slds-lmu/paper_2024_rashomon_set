test_that("SpaceDisjoiner initialization works", {
  param.set <- ps(
    x = p_dbl(0, 1),
    y = p_dbl(0, 1),
    a = p_fct(c("a", "b", "c")),
    b = p_lgl(depends = a == "b"),
    c = p_lgl(depends = a %in% c("b", "c") && b == TRUE)
  )

  space.disjoiner <- SpaceDisjoiner$new(param.set)

  expect_r6(space.disjoiner, "SpaceDisjoiner")
  expect_identical(
    as.data.table(space.disjoiner$param.set),
    as.data.table(param.set)
  )
  expect_type(space.disjoiner$subspaces, "list")
  expect_type(space.disjoiner$subspace.names, "character")
  expect_type(space.disjoiner$disjoiner.levels, "list")
})

test_that("SpaceDisjoiner handles simple parameter dependencies", {
  param.set <- ps(
    x = p_dbl(0, 1),
    a = p_fct(c("a", "b")),
    b = p_lgl(depends = a == "b")
  )

  space.disjoiner <- SpaceDisjoiner$new(param.set)

  # Should create 2 subspaces: one for a="a" and one for a="b"
  expect_length(space.disjoiner$subspaces, 2)
  expect_setequal(
    space.disjoiner$subspace.names,
    c("a=a", "a=b")
  )

  # Check subspace contents
  subspace.a <- space.disjoiner$subspaces[["a=a"]]
  expect_identical(subspace.a$ids(), "x")

  subspace.b <- space.disjoiner$subspaces[["a=b"]]
  expect_identical(subspace.b$ids(), c("b", "x"))
})

test_that("SpaceDisjoiner handles complex parameter dependencies", {
  param.set <- ps(
    x = p_dbl(0, 1),
    a = p_fct(c("a", "b", "c")),
    b = p_lgl(depends = a == "b"),
    c = p_lgl(depends = a %in% c("b", "c") && b == TRUE),
    d = p_fct(c("x", "y"), depends = a == "c")
  )

  space.disjoiner <- SpaceDisjoiner$new(param.set)

  expect_identical(space.disjoiner$disjoiner.levels,
    list(a = c("a", "b", "c"), b = c(TRUE, FALSE))
  )

  # Check number of subspaces
  expect_length(space.disjoiner$subspaces, 4)

  expect_setequal(
    space.disjoiner$subspace.names,
    c("a=a,b=NA", "a=b,b=TRUE", "a=b,b=FALSE", "a=c,b=NA")
  )

  # Check that each subspace has valid parameter combinations
  for (subspace in space.disjoiner$subspaces) {
    expect_identical(nrow(subspace$deps), 0L)  # No dependencies within subspaces
  }

  # Check that each subspace has valid parameter combinations
  expect_identical(as.data.table(space.disjoiner$subspaces[["a=a,b=NA"]]),
    as.data.table(ps(x = p_dbl(0, 1))))
  expect_identical(as.data.table(space.disjoiner$subspaces[["a=b,b=TRUE"]]),
    as.data.table(ps(c = p_lgl(), x = p_dbl(0, 1))))
  expect_identical(as.data.table(space.disjoiner$subspaces[["a=b,b=FALSE"]]),
    as.data.table(ps(x = p_dbl(0, 1))))
  expect_identical(as.data.table(space.disjoiner$subspaces[["a=c,b=NA"]]),
    as.data.table(ps(d = p_fct(c("x", "y")), x = p_dbl(0, 1))))
})

test_that("SpaceDisjoiner disjoinTable works", {
  param.set <- ps(
    x = p_dbl(0, 1),
    a = p_fct(c("a", "b")),
    b = p_lgl(depends = a == "b")
  )

  space.disjoiner <- SpaceDisjoiner$new(param.set)

  # Create test data
  test.data <- data.table(
    x = c(0.1, 0.2, 0.3, 0.4),
    a = c("a", "b", "a", "b"),
    b = c(NA, TRUE, NA, TRUE)
  )

  disjoined <- space.disjoiner$disjoinTable(test.data)

  expect_type(disjoined, "list")
  expect_named(disjoined, c("a=a", "a=b"))

  # Check contents of disjoined tables
  expect_identical(nrow(disjoined[["a=a"]]), 2L)
  expect_identical(nrow(disjoined[["a=b"]]), 2L)

  # Check that values are correctly separated
  expect_equal(disjoined[["a=a"]], data.table(x = c(0.1, 0.3)), ignore.col.order = TRUE)
  expect_equal(disjoined[["a=b"]], data.table(x = c(0.2, 0.4), b = c(TRUE, TRUE)), ignore.col.order = TRUE)
})

test_that("SpaceDisjoiner handles additional disjoin parameters", {
  param.set <- ps(
    x = p_dbl(0, 1),
    a = p_fct(c("a", "b")),
    b = p_lgl(depends = a == "b"),
    c = p_fct(c("x", "y"))
  )

  space.disjoiner <- SpaceDisjoiner$new(
    param.set,
    also.disjoin.on = "c"
  )

  # Should create subspaces for each combination of a and c
  expect_length(space.disjoiner$subspaces, 4)
  expect_setequal(
    space.disjoiner$subspace.names,
    c("a=a,c=x", "a=a,c=y", "a=b,c=x", "a=b,c=y")
  )
  test.data <- data.table(
    x = c(0.1, 0.2, 0.3, 0.4),
    a = c("a", "b", "a", "b"),
    b = c(NA, TRUE, NA, TRUE),
    c = c("x", "y", "x", "y")
  )

  disjoined <- space.disjoiner$disjoinTable(test.data)

  expect_type(disjoined, "list")
  expect_named(disjoined, space.disjoiner$subspace.names)

  # Check that values are correctly separated
  expect_equal(disjoined[["a=a,c=x"]], data.table(x = c(0.1, 0.3)), ignore.col.order = TRUE)
  expect_equal(disjoined[["a=b,c=y"]], data.table(x = c(0.2, 0.4), b = c(TRUE, TRUE)), ignore.col.order = TRUE)
})

test_that("SpaceDisjoiner validates inputs", {
  param.set <- ps(
    x = p_dbl(0, 1),
    y = p_dbl(-Inf, Inf)  # Unbounded parameter
  )

  # Should error on unbounded parameters
  expect_error(
    SpaceDisjoiner$new(param.set, also.disjoin.on = "y"),
    "Disjoining on non-factor.*logical parameters.*allow.int to TRUE"
  )

  # Should error on non-existent parameters
  expect_error(
    SpaceDisjoiner$new(param.set, also.disjoin.on = "z"),
    "Must be a subset of"
  )

  # Should error on non-categorical/logical parameters without allow.int
  param.set <- ps(
    x = p_dbl(0, 1),
    y = p_int(1, 10)
  )
  expect_error(
    SpaceDisjoiner$new(param.set, also.disjoin.on = "y"),
    "Disjoining on non-factor / logical parameters"
  )
})

test_that("SpaceDisjoiner handles integer parameters correctly", {
  param.set <- ps(
    x = p_dbl(0, 1),
    y = p_dbl(0, 1),
    a = p_int(0, 2),  # Integer parameter with 3 levels
    b = p_lgl(depends = a == 1),  # Similar to a=="b" in other tests
    c = p_lgl(depends = a %in% c(1, 2) && b == TRUE),  # Similar to previous c dependency
    d = p_fct(c("x", "y"), depends = a == 2)  # Similar to previous d dependency
  )

  space.disjoiner <- SpaceDisjoiner$new(param.set, allow.int = TRUE)

  # Check disjoiner levels
  expect_identical(space.disjoiner$disjoiner.levels,
    list(a = 0:2, b = c(TRUE, FALSE))
  )

  # Check number and names of subspaces
  expect_length(space.disjoiner$subspaces, 4)
  expect_setequal(
    space.disjoiner$subspace.names,
    c("a=0,b=NA", "a=1,b=TRUE", "a=1,b=FALSE", "a=2,b=NA")
  )

  # Test disjoinTable functionality
  test.data <- data.table(
    x = seq(0.1, 0.5, 0.1),
    y = seq(0.2, 0.6, 0.1),
    a = c(0, 1, 1, 2, 2),
    b = c(NA, TRUE, FALSE, NA, NA),
    c = c(NA, TRUE, NA, NA, NA),
    d = c(NA, NA, NA, "x", "y")
  )

  disjoined <- space.disjoiner$disjoinTable(test.data)

  # Check that values are correctly separated
  expect_equal(
    disjoined[["a=0,b=NA"]],
    data.table(x = 0.1, y = 0.2),
    ignore.col.order = TRUE
  )
  expect_equal(
    disjoined[["a=1,b=TRUE"]],
    data.table(x = 0.2, y = 0.3, c = TRUE),
    ignore.col.order = TRUE
  )
  expect_equal(
    disjoined[["a=1,b=FALSE"]],
    data.table(x = 0.3, y = 0.4),
    ignore.col.order = TRUE
  )
  expect_equal(
    disjoined[["a=2,b=NA"]],
    data.table(x = c(0.4, 0.5), y = c(0.5, 0.6), d = c("x", "y")),
    ignore.col.order = TRUE
  )

  # Test that it errors without allow.int
  expect_error(
    SpaceDisjoiner$new(param.set),
    "Disjoining on non-factor.*logical parameters.*allow.int to TRUE"
  )
})

