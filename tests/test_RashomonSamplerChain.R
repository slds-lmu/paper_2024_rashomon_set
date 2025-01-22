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
