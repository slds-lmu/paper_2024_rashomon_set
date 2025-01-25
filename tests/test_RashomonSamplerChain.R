# Dummy sampler that logs all interactions and allows controlling outputs
LoggingRashomonSampler <- R6Class("LoggingRashomonSampler",
  inherit = RashomonSampler,
  public = list(
    initialize = function(id, domain, minimize, rashomon.epsilon, rashomon.is.relative,
        n.rashomon.samples = 10L) {
      super$initialize(id, domain, minimize, rashomon.epsilon, rashomon.is.relative,
        seed = 1L, n.rashomon.samples = n.rashomon.samples)
      private$.log <- list(
        askXSamples = list(),
        tellXSamples = list(),
        askYValues = list(),
        tellYValues = list()
      )
      # Controlled outputs
      private$.next.x.samples <- 0L
      private$.next.y.samples <- data.table()
      private$.rashomon.samples <- data.table()
    }
  ),
  active = list(
    log = function() private$.log,
    next.x.samples = function(val) {
      if (!missing(val)) private$.next.x.samples <- val
      private$.next.x.samples
    },
    next.y.samples = function(val) {
      if (!missing(val)) private$.next.y.samples <- val
      private$.next.y.samples
    },
    next.rashomon.samples = function(val) {
      if (!missing(val)) private$.rashomon.samples <- val
      private$.rashomon.samples
    }
  ),
  private = list(
    .log = NULL,
    .next.x.samples = NULL,
    .next.y.samples = NULL,
    .rashomon.samples = NULL,
    .askXSamples = function() {
      private$.log$askXSamples <- c(private$.log$askXSamples, list(Sys.time()))
      private$.next.x.samples
    },
    .tellXSamples = function(x) {
      private$.log$tellXSamples <- c(private$.log$tellXSamples, list(copy(x)))
    },
    .askYValues = function() {
      private$.log$askYValues <- c(private$.log$askYValues, list(Sys.time()))
      private$.next.y.samples
    },
    .tellYValues = function(y) {
      private$.log$tellYValues <- c(private$.log$tellYValues, list(copy(y)))
    },
    .getRashomonSamples = function() private$.rashomon.samples,
    .rashomonSamplesComplete = function() nrow(private$.rashomon.samples)
  )
)

test_that("RashomonSamplerChain initialization works", {
  domain <- ps(x = p_dbl(0, 1))

  # Create samplers with different epsilon/relative settings
  sampler1 <- LoggingRashomonSampler$new(
    id = "s1", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.1, rashomon.is.relative = FALSE, n.rashomon.samples = 10L
  )
  sampler2 <- LoggingRashomonSampler$new(
    id = "s2", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.2, rashomon.is.relative = TRUE, n.rashomon.samples = 10L
  )

  # Basic initialization
  chain <- RashomonSamplerChain$new(
    id = "chain",
    samplers = list(sampler1, sampler2),
    ask.y.each = c(5, Inf),
    n.rashomon.samples = 10L
  )

  expect_identical(chain$id, "chain")
  expect_identical(chain$domain, domain)
  expect_true(chain$minimize)
  # Should inherit from last sampler
  expect_identical(chain$rashomon.epsilon, 0.2)
  expect_true(chain$rashomon.is.relative)
  expect_identical(chain$sampler.index, 1L)
  expect_identical(chain$ask.y.each, c(5, Inf))

  # Error on non-matching domain
  domain2 <- ps(y = p_dbl(0, 1))
  sampler3 <- LoggingRashomonSampler$new(
    id = "s3", domain = domain2, minimize = TRUE,
    rashomon.epsilon = 0.1, rashomon.is.relative = FALSE
  )
  expect_error(
    RashomonSamplerChain$new("chain", list(sampler1, sampler3), c(5, Inf), 10L),
    "samplers\\[\\[2\\]\\]\\$domain == samplers\\[\\[1\\]\\]\\$domain"
  )

  # Error on non-matching minimize
  sampler4 <- LoggingRashomonSampler$new(
    id = "s4", domain = domain, minimize = FALSE,
    rashomon.epsilon = 0.1, rashomon.is.relative = FALSE
  )
  expect_error(
    RashomonSamplerChain$new("chain", list(sampler1, sampler4), c(5, Inf), 10L),
    "samplers\\[\\[2\\]\\]\\$minimize == samplers\\[\\[1\\]\\]\\$minimize"
  )

  # Error on invalid ask.y.each
  expect_error(
    RashomonSamplerChain$new("chain", list(sampler1, sampler2), c(5, 10), 10L),
    "tail.*ask.y.each.*== Inf"
  )
  expect_error(
    RashomonSamplerChain$new("chain", list(sampler1, sampler2), c(-1, Inf), 10L),
    "Element 1 is not >= 0"
  )

  # Warning on smaller n.rashomon.samples
  expect_warning(
    RashomonSamplerChain$new("chain", list(sampler1, sampler2), c(5, Inf), 15L),
    "smaller n.rashomon.samples.*may lead to incomplete results"
  )
})

test_that("RashomonSamplerChain transitions correctly", {
  domain <- ps(x = p_dbl(0, 1))

  sampler1 <- LoggingRashomonSampler$new(
    id = "s1", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.1, rashomon.is.relative = FALSE
  )
  sampler2 <- LoggingRashomonSampler$new(
    id = "s2", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.2, rashomon.is.relative = TRUE
  )

  chain <- RashomonSamplerChain$new(
    id = "chain",
    samplers = list(sampler1, sampler2),
    ask.y.each = c(4, Inf),
    n.rashomon.samples = 10L
  )

  # samplers are cloned by RashomonSamplerChain, so we need to get them from the chain
  sampler1 <- chain$samplers[[1]]
  sampler2 <- chain$samplers[[2]]

  # Set up first sampler behavior
  sampler1$next.x.samples <- 3L
  expect_identical(chain$askXSamples(), 3L)

  # Tell X samples
  x.samples <- data.table(.id = 1:3, x = c(0.1, 0.2, 0.3))
  expect_length(sampler1$log$askXSamples, 1L)
    # Set up first sampler behavior
  sampler1$next.x.samples <- 0L

  chain$tellXSamples(x.samples)
  expect_length(sampler1$log$askXSamples, 2L)
  expect_length(sampler1$log$tellXSamples, 1L)
  expect_equal(sampler1$log$tellXSamples[[1]], cbind(x.samples, .score = NA_real_), ignore.col.order = TRUE)

  # Set up Y samples for first sampler
  sampler1$next.y.samples <- x.samples
  expect_length(sampler1$log$askYValues, 0L)
  y.request <- chain$askYValues()
  expect_length(sampler1$log$askYValues, 1L)
  expect_equal(y.request, x.samples, ignore.col.order = TRUE)

  # Tell Y values (first batch)
  y.values <- data.table(.id = 1:3, .score = c(0.5, 0.6, 0.7))
  sampler1$next.x.samples <- 2L
  expect_length(sampler1$log$tellYValues, 0L)
  expect_length(sampler1$log$askXSamples, 2L)
  chain$tellYValues(y.values)
  expect_length(sampler1$log$tellYValues, 1L)
  expect_length(sampler1$log$askXSamples, 3L)
  expect_equal(sampler1$log$tellYValues[[1]], x.samples[y.values, on = ".id"], ignore.col.order = TRUE)

  # Should still be on first sampler
  expect_identical(chain$sampler.index, 1L)

  # Second batch of X samples
  expect_identical(chain$askXSamples(), 2L)

  x.samples2 <- data.table(.id = 4:5, x = c(0.4, 0.5))
  sampler1$next.x.samples <- 0L
  expect_length(sampler1$log$askXSamples, 3L)
  chain$tellXSamples(x.samples2)
  expect_length(sampler1$log$askXSamples, 4L)

  sampler1$next.y.samples <- x.samples2
  expect_length(sampler1$log$askYValues, 1L)
  y.request2 <- chain$askYValues()
  expect_length(sampler1$log$askYValues, 2L)
  expect_equal(y.request2, x.samples2, ignore.col.order = TRUE)

  # Tell Y values (second batch) - should trigger transition
  y.values2 <- data.table(.id = 4:5, .score = c(0.8, 0.9))
  expect_length(sampler1$log$askYValues, 2L)
  expect_length(sampler1$log$tellYValues, 1L)
  expect_length(sampler1$log$askXSamples, 4L)
  expect_length(sampler1$log$tellXSamples, 2L)
  expect_identical(unname(lengths(sampler2$log)), c(0L, 0L, 0L, 0L))
  expect_identical(chain$sampler.index, 1L)

  sampler2$next.x.samples <- 3L
  chain$tellYValues(y.values2)  # should trigger transition

  # s1 was not touched any more
  expect_length(sampler1$log$askYValues, 2L)
  expect_length(sampler1$log$tellYValues, 1L)
  expect_length(sampler1$log$askXSamples, 4L)
  expect_length(sampler1$log$tellXSamples, 2L)

  expect_length(sampler2$log$askXSamples, 2L)  # asked 2x for 3: given 3 the first time, 2 the second time
  expect_length(sampler2$log$askYValues, 0L)
  expect_length(sampler2$log$tellYValues, 0L)
  expect_length(sampler2$log$tellXSamples, 1L)  # 2nd call to tellXSamples() has not reached base class yet
  expect_identical(sampler2$askXSamples(), 1L)
  expect_equal(sampler2$log$tellXSamples[[1]], x.samples[y.values, on = ".id"], ignore.col.order = TRUE)

  # Should now be on second sampler
  expect_identical(chain$sampler.index, 2L)

  x.samples3 <- data.table(.id = 6, x = 0.6)
  chain$tellXSamples(x.samples3)

  expect_length(sampler2$log$tellXSamples, 2L)
  # remaining known y values were given, plus the one extra row from now
  expect_equal(
    sampler2$log$tellXSamples[[2]],
    rbind(x.samples2[y.values2, on = ".id"], cbind(x.samples3, .score = NA_real_)),
    ignore.col.order = TRUE
  )
})

test_that("RashomonSamplerChain inherits parameters from last sampler correctly", {
  domain <- ps(x = p_dbl(0, 1))

  # Create samplers with different epsilon/relative settings
  sampler1 <- LoggingRashomonSampler$new(
    id = "s1", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.1, rashomon.is.relative = FALSE, n.rashomon.samples = 10L
  )
  sampler2 <- LoggingRashomonSampler$new(
    id = "s2", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.2, rashomon.is.relative = TRUE, n.rashomon.samples = 10L
  )
  sampler3 <- LoggingRashomonSampler$new(
    id = "s3", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.3, rashomon.is.relative = FALSE, n.rashomon.samples = 10L
  )

  # Test with 2 samplers
  chain2 <- RashomonSamplerChain$new(
    id = "chain2",
    samplers = list(sampler1, sampler2),
    ask.y.each = c(5, Inf),
    n.rashomon.samples = 10L
  )

  expect_identical(chain2$rashomon.epsilon, 0.2)
  expect_true(chain2$rashomon.is.relative)

  # Test with 3 samplers
  chain3 <- RashomonSamplerChain$new(
    id = "chain3",
    samplers = list(sampler1, sampler2, sampler3),
    ask.y.each = c(5, 10, Inf),
    n.rashomon.samples = 10L
  )

  expect_identical(chain3$rashomon.epsilon, 0.3)
  expect_false(chain3$rashomon.is.relative)
})

test_that("RashomonSamplerChain works with single sampler", {
  domain <- ps(x = p_dbl(0, 1))

  sampler <- LoggingRashomonSampler$new(
    id = "s1", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.1, rashomon.is.relative = FALSE, n.rashomon.samples = 10L
  )

  chain <- RashomonSamplerChain$new(
    id = "chain",
    samplers = list(sampler),
    ask.y.each = Inf,
    n.rashomon.samples = 10L
  )

  # Basic initialization checks
  expect_identical(chain$id, "chain")
  expect_identical(chain$domain, domain)
  expect_true(chain$minimize)
  expect_identical(chain$rashomon.epsilon, 0.1)
  expect_false(chain$rashomon.is.relative)
  expect_identical(chain$sampler.index, 1L)
  expect_identical(chain$ask.y.each, Inf)
  expect_length(chain$samplers, 1L)

  # Get sampler from chain since it was cloned
  sampler <- chain$samplers[[1]]

  # Set up sampler behavior and request X samples
  sampler$next.x.samples <- 3L
  expect_identical(chain$askXSamples(), 3L)

  # Tell X samples
  x.samples <- data.table(.id = 1:3, x = c(0.1, 0.2, 0.3))
  sampler$next.x.samples <- 0L
  expect_length(sampler$log$askXSamples, 1L)
  expect_identical(chain$tellXSamples(x.samples), 0L)
  expect_length(sampler$log$askXSamples, 2L)
  expect_length(sampler$log$tellXSamples, 1L)
  expect_equal(sampler$log$tellXSamples[[1]], cbind(x.samples, .score = NA_real_), ignore.col.order = TRUE)

  # Request Y values
  sampler$next.y.samples <- x.samples
  expect_length(sampler$log$askYValues, 0L)
  y.request <- chain$askYValues()
  expect_length(sampler$log$askYValues, 1L)
  expect_equal(y.request, x.samples, ignore.col.order = TRUE)

  # Tell Y values
  y.values <- data.table(.id = 1:3, .score = c(0.5, 0.6, 0.7))
  sampler$next.x.samples <- 2L
  expect_length(sampler$log$tellYValues, 0L)
  expect_equal(chain$tellYValues(y.values), data.table(x = numeric(0), .id = integer(0)), ignore.col.order = TRUE)
  expect_length(sampler$log$tellYValues, 1L)
  expect_equal(sampler$log$tellYValues[[1]], x.samples[y.values, on = ".id"], ignore.col.order = TRUE)

  # Verify sampler index is still sampler1
  expect_identical(chain$sampler.index, 1L)

  # Request more X samples
  expect_identical(chain$askXSamples(), 2L)
  x.samples2 <- data.table(.id = 4:5, x = c(0.4, 0.5))
  sampler$next.x.samples <- 0L
  expect_identical(chain$tellXSamples(x.samples2), 0L)

  # Verify log contents
  expect_identical(lengths(sampler$log), c(askXSamples = 4L, tellXSamples = 2L, askYValues = 1L, tellYValues = 1L))
  expect_equal(sampler$log$tellXSamples[[1]], cbind(x.samples, .score = NA_real_), ignore.col.order = TRUE)
  expect_equal(sampler$log$tellXSamples[[2]], cbind(x.samples2, .score = NA_real_), ignore.col.order = TRUE)
})

test_that("RashomonSamplerChain works with three samplers", {
  domain <- ps(x = p_dbl(0, 1))

  # Create samplers with different epsilon/relative settings
  sampler1 <- LoggingRashomonSampler$new(
    id = "s1", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.1, rashomon.is.relative = FALSE, n.rashomon.samples = 10L
  )
  sampler2 <- LoggingRashomonSampler$new(
    id = "s2", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.2, rashomon.is.relative = TRUE, n.rashomon.samples = 10L
  )
  sampler3 <- LoggingRashomonSampler$new(
    id = "s3", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.3, rashomon.is.relative = FALSE, n.rashomon.samples = 10L
  )

  chain <- RashomonSamplerChain$new(
    id = "chain",
    samplers = list(sampler1, sampler2, sampler3),
    ask.y.each = c(5, 10, Inf),
    n.rashomon.samples = 10L
  )

  expect_identical(chain$id, "chain")
  expect_identical(chain$domain, domain)
  expect_true(chain$minimize)
  expect_identical(chain$rashomon.epsilon, 0.3)
  expect_false(chain$rashomon.is.relative)
  expect_identical(chain$sampler.index, 1L)
  expect_identical(chain$ask.y.each, c(5, 10, Inf))
  expect_length(chain$samplers, 3L)

  # Test error on invalid ask.y.each length
  expect_error(
    RashomonSamplerChain$new("chain", list(sampler1, sampler2, sampler3), c(5, Inf), 10L),
    "Must have length 3, but has length 2"
  )
})

test_that("RashomonSamplerChain handles multiple transitions in single tellYValues", {
  domain <- ps(x = p_dbl(0, 1))

  sampler1 <- LoggingRashomonSampler$new(
    id = "s1", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.1, rashomon.is.relative = FALSE
  )
  sampler2 <- LoggingRashomonSampler$new(
    id = "s2", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.2, rashomon.is.relative = TRUE
  )
  sampler3 <- LoggingRashomonSampler$new(
    id = "s3", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.3, rashomon.is.relative = FALSE
  )

  chain <- RashomonSamplerChain$new(
    id = "chain",
    samplers = list(sampler1, sampler2, sampler3),
    ask.y.each = c(5, 10, Inf),
    n.rashomon.samples = 10L
  )

  # samplers are cloned, so we need to get them back from the chain
  sampler1 <- chain$samplers[[1]]
  sampler2 <- chain$samplers[[2]]
  sampler3 <- chain$samplers[[3]]

  # Set up first sampler behavior to request 15 samples at once
  x.samples <- data.table(.id = 1:15, x = seq(0, 1, length.out = 15))
  sampler1$next.x.samples <- 15L
  expect_identical(chain$askXSamples(), 15L)

  sampler1$next.x.samples <- 0L
  chain$tellXSamples(x.samples)
  expect_identical(chain$askXSamples(), 0L)

  # Tell Y values that exceed both first and second sampler thresholds
  y.values <- data.table(.id = 1:15, .score = seq(0.1, 1.5, length.out = 15))

  # Before transition
  expect_identical(chain$sampler.index, 1L)
  expect_length(sampler1$log$tellYValues, 0L)
  expect_length(sampler2$log$tellXSamples, 0L)
  expect_length(sampler3$log$tellXSamples, 0L)

  sampler2$next.x.samples <- 10L
  sampler3$next.x.samples <- 10L

  sampler1$next.y.samples <- x.samples
  expect_equal(chain$askYValues(), x.samples, ignore.col.order = TRUE)
  chain$tellYValues(y.values)

  expect_identical(lengths(sampler1$log), c(askXSamples = 2L, tellXSamples = 1L, askYValues = 1L, tellYValues = 0L))
  expect_identical(lengths(sampler2$log), c(askXSamples = 0L, tellXSamples = 0L, askYValues = 0L, tellYValues = 0L))

  # sampler3 is asked twice for X-samples: it asks for 10, receives 10, then asks for 10 again and receives 5.
  # The latter 5 are not forwarded to the concrete class yet, hence tellXSamples is 1.
  expect_identical(lengths(sampler3$log), c(askXSamples = 2L, tellXSamples = 1L, askYValues = 0L, tellYValues = 0L))

  # Should have skipped sampler2 entirely and moved to sampler3
  expect_identical(chain$sampler.index, 3L)
  x.samples.2 <- data.table(.id = 16:20, x = seq(0, 1, length.out = 5))
  expect_identical(chain$askXSamples(), 5L)
  chain$tellXSamples(x.samples.2)
  expect_identical(lengths(sampler3$log), c(askXSamples = 3L, tellXSamples = 2L, askYValues = 0L, tellYValues = 0L))

  expect_equal(
    sampler3$log$tellXSamples[[1]],
    y.values[x.samples, on = ".id"][1:10],
    ignore.col.order = TRUE
  )
  expect_equal(
    sampler3$log$tellXSamples[[2]],
    # this merge correctly sets .score to NA for the 16:20 rows
    y.values[rbind(x.samples, x.samples.2), on = ".id"][11:20],
    ignore.col.order = TRUE
  )
})

test_that("RashomonSamplerChain transitions exactly at threshold", {
  domain <- ps(x = p_dbl(0, 1))

  sampler1 <- LoggingRashomonSampler$new(
    id = "s1", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.1, rashomon.is.relative = FALSE
  )
  sampler2 <- LoggingRashomonSampler$new(
    id = "s2", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.2, rashomon.is.relative = TRUE
  )

  chain <- RashomonSamplerChain$new(
    id = "chain",
    samplers = list(sampler1, sampler2),
    ask.y.each = c(5, Inf),
    n.rashomon.samples = 10L
  )

  # samplers are cloned, so we need to get them back from the chain
  sampler1 <- chain$samplers[[1]]
  sampler2 <- chain$samplers[[2]]

  # Request exactly 5 samples
  sampler1$next.x.samples <- 5L
  expect_identical(chain$askXSamples(), 5L)

  sampler1$next.x.samples <- 0L
  x.samples <- data.table(.id = 1:5, x = seq(0, 1, length.out = 5))
  expect_identical(chain$tellXSamples(x.samples), 0L)

  # Tell exactly 5 Y values
  y.values <- data.table(.id = 1:5, .score = seq(0.1, 0.5, length.out = 5))
  sampler1$next.y.samples <- x.samples
  expect_equal(chain$askYValues(), x.samples, ignore.col.order = TRUE)

  expect_identical(chain$sampler.index, 1L)

  sampler2$next.x.samples <- 2L
  chain$tellYValues(y.values)
  expect_identical(chain$sampler.index, 2L)

  expect_identical(lengths(sampler1$log), c(askXSamples = 2L, tellXSamples = 1L, askYValues = 1L, tellYValues = 0L))
  expect_identical(lengths(sampler2$log), c(askXSamples = 3L, tellXSamples = 2L, askYValues = 0L, tellYValues = 0L))
})

test_that("RashomonSamplerChain element complains if an element that got only complete samples does not ask for X", {
  domain <- ps(x = p_dbl(0, 1))

  sampler1 <- LoggingRashomonSampler$new(
    id = "s1", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.1, rashomon.is.relative = FALSE
  )
  sampler2 <- LoggingRashomonSampler$new(
    id = "s2", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.2, rashomon.is.relative = TRUE
  )

  chain <- RashomonSamplerChain$new(
    id = "chain",
    samplers = list(sampler1, sampler2),
    ask.y.each = c(5, Inf),
    n.rashomon.samples = 10L
  )

  # samplers are cloned, so we need to get them back from the chain
  sampler1 <- chain$samplers[[1]]
  sampler2 <- chain$samplers[[2]]

  # Request exactly 5 samples
  sampler1$next.x.samples <- 5L
  expect_identical(chain$askXSamples(), 5L)

  sampler1$next.x.samples <- 0L
  x.samples <- data.table(.id = 1:5, x = seq(0, 1, length.out = 5))
  expect_identical(chain$tellXSamples(x.samples), 0L)

  # Tell exactly 5 Y values
  y.values <- data.table(.id = 1:5, .score = seq(0.1, 0.5, length.out = 5))
  sampler1$next.y.samples <- x.samples
  expect_equal(chain$askYValues(), x.samples, ignore.col.order = TRUE)

  expect_error(chain$tellYValues(y.values), "Sampler s2 askXSamples\\(\\) returned 0 even though.* complete samples")
})

test_that("RashomonSamplerChain preserves sample order during transition within groups of Y given / not given", {
  domain <- ps(x = p_dbl(0, 1))

  sampler1 <- LoggingRashomonSampler$new(
    id = "s1", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.1, rashomon.is.relative = FALSE
  )
  sampler2 <- LoggingRashomonSampler$new(
    id = "s2", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.2, rashomon.is.relative = TRUE
  )

  chain <- RashomonSamplerChain$new(
    id = "chain",
    samplers = list(sampler1, sampler2),
    ask.y.each = c(5, Inf),
    n.rashomon.samples = 10L
  )

  # samplers are cloned, so we need to get them back from the chain
  sampler1 <- chain$samplers[[1]]
  sampler2 <- chain$samplers[[2]]

  # First batch: mix of scored and unscored samples
  sampler1$next.x.samples <- 8L
  x.samples <- data.table(
    .id = 1:8,
    x = seq(0, 1, length.out = 8),
    .score = NA_real_
  )
  x.samples[c(2, 4, 6), .score := c(0.2, 0.4, 0.6)]
  expect_identical(chain$askXSamples(), 8L)
  sampler1$next.x.samples <- 0L
  expect_identical(chain$tellXSamples(x.samples, scorecol = ".score"), 0L)

  # Tell remaining Y values to trigger transition
  y.values <- data.table(.id = c(1, 5, 8), .score = c(0.1, 0.5, 0.8))
  sampler1$next.y.samples <- x.samples[c(1, 5, 8)]
  sampler2$next.x.samples <- 4L
  expect_equal(chain$askYValues(), x.samples[c(1, 5, 8), .(.id, x)], ignore.col.order = TRUE)
  chain$tellYValues(y.values)

  # Verify that sampler2 received samples with known Y values first
  expect_identical(lengths(sampler1$log), c(askXSamples = 2L, tellXSamples = 1L, askYValues = 1L, tellYValues = 0L))
  expect_identical(lengths(sampler2$log), c(askXSamples = 3L, tellXSamples = 2L, askYValues = 0L, tellYValues = 0L))

  # Verify that sampler2 received samples with known Y values first
  x.samples.expected <- rbind(
    x.samples[!is.na(.score)],
    x.samples[, .(.id, x)][y.values, on = ".id"],
    use.names = TRUE
  )[order(.id)]
  x.samples.expected <- rbind(x.samples.expected, x.samples[is.na(.score) & !.id %in% y.values$.id], use.names = TRUE)

  expect_equal(
    sampler2$log$tellXSamples[[1]],
    x.samples.expected[1:4],
    ignore.col.order = TRUE
  )
  expect_equal(
    sampler2$log$tellXSamples[[2]],
    x.samples.expected[5:8],
    ignore.col.order = TRUE
  )

})

test_that("RashomonSamplerChain transitions when enough Y-values are given to tellXSamples()", {
  domain <- ps(x = p_dbl(0, 1))

  sampler1 <- LoggingRashomonSampler$new(
    id = "s1", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.1, rashomon.is.relative = FALSE
  )
  sampler2 <- LoggingRashomonSampler$new(
    id = "s2", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.2, rashomon.is.relative = TRUE
  )

  chain <- RashomonSamplerChain$new(
    id = "chain",
    samplers = list(sampler1, sampler2),
    ask.y.each = c(2, Inf),
    n.rashomon.samples = 10L
  )

  # samplers are cloned, so we need to get them back from the chain
  sampler1 <- chain$samplers[[1]]
  sampler2 <- chain$samplers[[2]]

  # First batch: mix of scored and unscored samples
  sampler1$next.x.samples <- 8L
  x.samples <- data.table(
    .id = 1:8,
    x = seq(0, 1, length.out = 8),
    .score = NA_real_
  )
  x.samples[c(2, 4, 6), .score := c(0.2, 0.4, 0.6)]
  expect_identical(chain$askXSamples(), 8L)
  sampler1$next.x.samples <- 0L
  sampler2$next.x.samples <- 4L
  expect_identical(chain$tellXSamples(x.samples, scorecol = ".score"), 4L)

  expect_identical(lengths(sampler1$log), c(askXSamples = 1L, tellXSamples = 0L, askYValues = 0L, tellYValues = 0L))
  expect_identical(lengths(sampler2$log), c(askXSamples = 3L, tellXSamples = 2L, askYValues = 0L, tellYValues = 0L))

  # Verify that sampler2 received samples with known Y values first
  x.samples.expected <- rbind(
    x.samples[!is.na(.score)],
    x.samples[is.na(.score)]
  )

  expect_equal(
    sampler2$log$tellXSamples[[1]],
    x.samples.expected[1:4],
    ignore.col.order = TRUE
  )
  expect_equal(
    sampler2$log$tellXSamples[[2]],
    x.samples.expected[5:8],
    ignore.col.order = TRUE
  )
})

test_that("RashomonSamplerChain handles cache with multiple batches", {
  domain <- ps(x = p_dbl(0, 1))

  sampler1 <- LoggingRashomonSampler$new(
    id = "s1", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.1, rashomon.is.relative = FALSE
  )
  sampler2 <- LoggingRashomonSampler$new(
    id = "s2", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.2, rashomon.is.relative = TRUE
  )

  chain <- RashomonSamplerChain$new(
    id = "chain",
    samplers = list(sampler1, sampler2),
    ask.y.each = c(5, Inf),
    n.rashomon.samples = 10L
  )

  # samplers are cloned, so we need to get them back from the chain
  sampler1 <- chain$samplers[[1]]
  sampler2 <- chain$samplers[[2]]

  # Set up behavior before first tellXSamples
  sampler1$next.x.samples <- 3L
  expect_identical(chain$askXSamples(), 3L)

  # First batch
  x.samples.1 <- data.table(.id = 1:3, x = seq(0, 1, length.out = 3))
  sampler1$next.x.samples <- 3L
  chain$tellXSamples(x.samples.1)

  # Second batch
  x.samples.2 <- data.table(.id = 4:6, x = seq(0, 1, length.out = 3))
  sampler1$next.x.samples <- 0L
  chain$tellXSamples(x.samples.2)

  # Get Y values for all samples
  sampler1$next.y.samples <- rbind(x.samples.1, x.samples.2)
  y.request <- chain$askYValues()
  expect_equal(y.request, rbind(x.samples.1, x.samples.2), ignore.col.order = TRUE)

  # Set up sampler2 behavior before telling Y values
  sampler2$next.x.samples <- 4L
  y.values <- data.table(.id = 1:6, .score = seq(0.1, 0.6, length.out = 6))
  chain$tellYValues(y.values)

  # Verify sampler2 got all samples in correct order
  expect_identical(chain$sampler.index, 2L)
  expect_identical(lengths(sampler2$log), c(askXSamples = 2L, tellXSamples = 1L, askYValues = 0L, tellYValues = 0L))

  expect_equal(
    sampler2$log$tellXSamples[[1]],
    rbind(x.samples.1, x.samples.2)[y.values[1:4], on = ".id"],
    ignore.col.order = TRUE
  )
})

test_that("RashomonSamplerChain errors on invalid askYValues requests", {
  domain <- ps(x = p_dbl(0, 1))

  sampler1.original <- LoggingRashomonSampler$new(
    id = "s1", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.1, rashomon.is.relative = FALSE
  )
  sampler2 <- LoggingRashomonSampler$new(
    id = "s2", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.2, rashomon.is.relative = TRUE
  )

  chain <- RashomonSamplerChain$new(
    id = "chain",
    samplers = list(sampler1.original, sampler2),
    ask.y.each = c(5, Inf),
    n.rashomon.samples = 10L
  )

  # samplers are cloned, so we need to get them back from the chain
  sampler1 <- chain$samplers[[1]]

  # Set up first batch of X samples
  sampler1$next.x.samples <- 3L
  x.samples <- data.table(.id = 1:3, x = seq(0, 1, length.out = 3))
  expect_identical(chain$askXSamples(), 3L)
  sampler1$next.x.samples <- 0L
  expect_identical(chain$tellXSamples(x.samples), 0L)

  # Try to request Y values for unseen .id
  sampler1$next.y.samples <- data.table(.id = c(1, 4), x = c(0.1, 0.4))
  expect_error(
    chain$askYValues(),
    "Sampler s1 asked for .id values that were not seen yet"
  )

  chain <- RashomonSamplerChain$new(
    id = "chain",
    samplers = list(sampler1.original, sampler2),
    ask.y.each = c(5, Inf),
    n.rashomon.samples = 10L
  )

  # samplers are cloned, so we need to get them back from the chain
  sampler1 <- chain$samplers[[1]]

  # Set up first batch of X samples
  sampler1$next.x.samples <- 3L
  x.samples <- data.table(.id = 1:3, x = seq(0, 1, length.out = 3), .score = c(0.1, NA, NA))
  expect_identical(chain$askXSamples(), 3L)
  sampler1$next.x.samples <- 0L
  expect_identical(chain$tellXSamples(x.samples), 0L)

  # Try to request Y values for unseen .id
  sampler1$next.y.samples <- data.table(.id = c(1, 4), x = c(0.1, 0.4))


  # Request Y values for valid .ids
  sampler1$next.y.samples <- x.samples
  y.request <- chain$askYValues()
  y.values <- data.table(.id = 1:3, .score = seq(0.1, 0.3, length.out = 3))
  chain$tellYValues(y.values)

  # Try to request Y values that were already answered
  sampler1$next.y.samples <- data.table(.id = c(1, 2), x = c(0.1, 0.2))
  expect_error(
    chain$askYValues(),
    "Sampler s1 asked for .id values that should have been answered already"
  )
})

test_that("RashomonSamplerChain does not error on invalid askYValues requests if last sampler is currently active", {
  domain <- ps(x = p_dbl(0, 1))

  sampler1.original <- LoggingRashomonSampler$new(
    id = "s1", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.1, rashomon.is.relative = FALSE
  )
  sampler2.original <- LoggingRashomonSampler$new(
    id = "s2", domain = domain, minimize = TRUE,
    rashomon.epsilon = 0.2, rashomon.is.relative = TRUE
  )

  chain <- RashomonSamplerChain$new(
    id = "chain",
    samplers = list(sampler1.original, sampler2.original),
    ask.y.each = c(5, Inf),
    n.rashomon.samples = 10L
  )

  # samplers are cloned, so we need to get them back from the chain
  sampler1 <- chain$samplers[[1]]
  sampler2 <- chain$samplers[[2]]

  # Send in batches of X samples

  # Set up first batch of X samples
  sampler1$next.x.samples <- 8L
  sampler2$next.x.samples <- 3L
  x.samples <- data.table(.id = 1:9, x = seq(0, 1, length.out = 9), .score = c(0.1, 0.2, 0.3, 0.4, 0.5, NA, NA, NA, NA))
  expect_identical(chain$askXSamples(), 8L)
  expect_identical(chain$tellXSamples(x.samples[1:8], scorecol = ".score"), 1L)

  sampler2$next.x.samples <- 0L
  expect_identical(chain$tellXSamples(x.samples[9]), 0L)

  # Try to request Y values for unseen .id
  sampler2$next.y.samples <- data.table(.id = c(1, 10), x = c(0.1, 0.4))
  # no error, even though we ask for .id with known Y-value as well as non-existent .id
  # this test may need to go if we ever put these tests in the base class.
  expect_equal(chain$askYValues(), data.table(.id = c(1, 10), x = c(0.1, 0.4)), ignore.col.order = TRUE)

})
