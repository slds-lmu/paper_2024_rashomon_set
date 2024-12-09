
RashomonSamplerChain <- R6Class("RashomonSamplerChain",
  inherit = RashomonSampler,
  public = list(
    initialize = function(id, samplers, ask.y.each, n.rashomon.samples) {
      assertList(samplers, types = "RashomonSampler", min.len = 1L, any.missing = FALSE)
      assertNumeric(ask.y.each, len = length(samplers), lower = 0, any.missing = FALSE)
      assertTRUE(tail(ask.y.each, 1) == Inf)
      assertIntegerish(head(ask.y.each, -1), lower = 0, any.missing = FALSE)
      domain <- samplers[[1]]$domain
      minimize <- samplers[[1]]$minimize
      rashomon.epsilon <- samplers[[1]]$rashomon.epsilon
      rashomon.is.relative <- samplers[[1]]$rashomon.is.relative
      for (i in seq_along(samplers)) {
        for (element in c("domain", "minimize", "rashomon.epsilon", "rashomon.is.relative")) {
          assertTRUE(samplers[[i]]$element == samplers[[1]]$element,
            .var.name = sprintf("samplers[[%d]]$%s == samplers[[1]]$%s", i, element, element)
          )
        }
      }
      private$.samplers <- lapply(samplers, function(x) {
        if (x$n.rashomon.samples < n.rashomon.samples) {
          warning("Chaining samplers with smaller n.rashomon.samples than requested in total may lead to incomplete results.")  # nolint
        }
        x$clone(deep = TRUE)
      })
      private$.ask.y.each <- ask.y.each
      super$initialize(id, domain, minimize, rashomon.epsilon, rashomon.is.relative, 0, n.rashomon.samples)
    }
  ),
  active = list(
    samplers = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.samplers)) {
        stop("samplers cannot be changed")
      }
      private$.samplers
    },
    ask.y.each = function() private$.ask.y.each
  ),
  private = list(
    .samplers = NULL,
    .ask.y.each = NULL,
    .table.cache = NULL,
    .askXSamples = function() {
      # TODO
    },
    .tellXSamples = function(x) {
      # TODO
    },
    .askYValues = function() {
      # TODO
    },
    .tellYValues = function(y) {
      # TODO
    },
    .getRashomonSamples = function() {
      # TODO
    },
    .rashomonSamplesComplete = function() {
      # TODO
    }
  )
)

