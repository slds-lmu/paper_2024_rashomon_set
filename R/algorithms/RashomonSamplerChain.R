
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
      private$.ask.y.each <- cumsum(ask.y.each)
      private$.sampler.index <- min(which(ask.y.each > 0))
      private$.still.caching <- length(samplers) > private$.sampler.index
      private$.cache.index <- 0L
      private$.table.cache <- getNullTable(domain, include.id = TRUE, include.score = TRUE)
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
    sampler.index = function() private$.sampler.index,
    ask.y.each = function() private$.ask.y.each
  ),
  private = list(
    .samplers = NULL,
    .ask.y.each = NULL,
    .table.cache = NULL,
    .sampler.index = NULL,
    .still.caching = NULL,
    .cache.index = NULL,
    .askXSamples = function() {
      assertTRUE(NROW(private$.table.cache) == 0)  # if not empty, we could have given more to the current sampler
      private$.samplers[[private$.sampler.index]]$askXSamples()
    },
    .tellXSamples = function(x) {
      if (private$.still.caching) {
        private$.table.cache <- rbind(private$.table.cache, x, fill = TRUE)
      }
      private$.samplers[[private$.sampler.index]]$tellXSamples(x, scorecol = ".score")
      ## The following should not happen, since (1) we can only ask for Ys that were given as Xs before, and
      ## (2) when we were providing these Xes, we already gave .score columns.
      # if (private$.still.caching) {
      #   now.asking <- private$.samplers[[private$.sampler.index]]$askYValues()
      #   if (nrow(now.asking[!private$.table.cache, on = ".id"])) {
      #     stop("Asked for .id values that were not seen yet.")
      #   }
      #   known.vals <- private$.table.cache[now.asking, on = ".id"][!is.na(.score)]
      #   if (nrow(known.vals)) {
      #     warning(sprintf("Telling Y values that were already given to tellXSamples. This is likely a bug in %s.",
      #       head(class(private$.samplers[[private$.sampler.index]]), 1)
      #     ))
      #     private$.samplers[[private$.sampler.index]]$tellYValues(known.vals, scorecol = ".score")
      #   }
      # }
    },
    .askYValues = function() {
      asking <- private$.samplers[[private$.sampler.index]]$askYValues()
      if (private$.still.caching) {
        if (nrow(asking[!private$.table.cache, on = ".id"])) {
          stop("Asked for .id values that were not seen yet.")
        }
        if (!all(is.na(private$.table.cache[asking, on = ".id", score]))) {
          stop("Asked for .id values that should have been answered already. This is a bug.")
        }
      }
      asking
    },
    .tellYValues = function(y) {
      if (private$.still.caching) {
        update <- y[, .(.id, .score.new = .score)]
        private$.table.cache[update, .score := .score.new, on = ".id"]
        seen.y <- sum(!is.na(private$.table.cache$.score))
      } else {
        seen.y <- 0  # don't care about updating the cache any more
      }
      if (seen.y < private$.ask.y.each[private$.sampler.index]) {
        private$.samplers[[private$.sampler.index]]$tellYValues(y)
      } else {
        # switch over to next sampler
        while (seen.y <= private$.ask.y.each[private$.sampler.index]) {
          private$.sampler.index <- private$.sampler.index + 1
        }
        private$.still.caching <- length(private$.samplers) > private$.sampler.index
        private$.cache.index <- 0L
        sampler.next <- private$.samplers[[private$.sampler.index]]
      }
      while (sampler.next$askXSamples() != 0 && private$.cache.index < NROW(private$.table.cache)) {
        asking <- sampler.next$askXSamples()
        rows <- seq.int(private$.cache.index + 1, length.out = asking)
        sampler.next$tellXSamples(private$.table.cache[rows, , nomatch = NULL])
        private$.cache.index <- private$.cache.index + asking
      }
      if (!private$.still.caching && private$.cache.index >= NROW(private$.table.cache)) {
        # not going to need the cache any more
        private$.table.cache <- NULL
      }
    },
    .getRashomonSamples = function() {
      private$.samplers[[private$.sampler.index]]$getRashomonSamples()
    },
    .rashomonSamplesComplete = function() {
      private$.samplers[[private$.sampler.index]]$rashomonSamplesComplete()
    }
  )
)

