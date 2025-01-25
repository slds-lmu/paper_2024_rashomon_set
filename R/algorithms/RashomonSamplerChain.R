#' @title Chained Rashomon Set Sampler
#'
#' @description
#' A meta-sampler that manages a sequence of individual [`RashomonSampler`] objects, activating each one until it has
#' requested or received a specified number of scored evaluations (Y-values). As soon as this quota is met or exceeded
#' for the current sampler, control transitions to the next sampler in the chain. Previously evaluated configurations
#' and their scores are passed along to each subsequent sampler so that all samplers share the same historical data.
#' The values of `rashomon.epsilon` and `rashomon.is.relative` exposed by the chain reflect those of the final sampler.
#'
#' @details
#' This class extends [RashomonSampler] but internally defers to multiple child samplers. Each child sampler must have:
#'
#' * An identical `domain`.
#' * An identical `minimize` value.
#' * A defined `n.rashomon.samples` parameter (except possibly set to `Inf` for the final sampler).
#'
#' The primary mechanism is as follows. When a sampler `samplers[[i]]` in the chain is active, it manages "ask" and
#' "tell" operations (for X- and Y-values) until the new Y-values that `samplers[[i]]` specifically requested or
#' received reach or exceed its own `n.rashomon.samples[i]` threshold. At that point, or if an update call surpasses
#' that threshold immediately, the chain automatically transitions to the next sampler `samplers[[i+1]]`.
#' In the process:
#'
#' * All previously known configurations and scores (including any surplus Y-values that exceed the threshold) are
#'   passed to sampler `samplers[[i+1]]` via its `tellXSamples()` method.
#' * Hence, `samplers[[i+1]]` is initialized as if it had received the same data interactively from the start.
#' * If multiple thresholds are exceeded at once (e.g., a single "tell" call brings total Y-values from below
#'   `samplers[[i]]`'s threshold to above `samplers[[i+1]]`s threshold as well), the chain moves forward until it finds
#'   the next sampler whose threshold is not fully satisfied.
#' * Samples with known Y-values are passed on before samples without Y-values, so some reordering of samples will take
#'   place unless the samples with known Y-values happen to come before any samples without known Y-values from the
#'   outside.
#' * Once the `RashomonSamplerChain` received enough Y-values to switch over to `samplers[[i+1]]`, `samplers[[i]]` is
#'   not called again. This meains the last call made to `samplers[[i]]` will have been `askXSamples()` or
#'   `askYValues()`.
#'
#' The final sampler in the chain must have `n.rashomon.samples[i] == Inf`. The overall `rashomon.epsilon` and
#' `rashomon.is.relative` parameters for the chain are inherited from the last sampler, so any final decisions
#' about set membership are governed by the configuration of that sampler.
#'
#' @section Rationale:
#' The purpose of a chained sampler is to use multiple sampling strategies in succession while preserving all
#' accumulated knowledge. Each sampler is allowed to drive the exploration and scoring process up to its own limit,
#' then smoothly hands off control to the next sampler. This can be useful when different sampling heuristics are
#' desired at different stages of the optimization or when a coarse initial sampling strategy should be followed by a
#' finer or more specialized search.
#'
#' @section Implementation Notes:
#' Internally, `RashomonSamplerChain` delegates calls to the active sampler for `askXSamples()`, `tellXSamples()`,
#' `askYValues()`, and `tellYValues()`. Whenever the active sampler's threshold (given by its `n.rashomon.samples`
#' value) is reached or exceeded, all further "ask"/"tell" operations are diverted to the next sampler in the list.
#' Because the new sampler receives a complete copy of the known configurations and their scores, it behaves
#' as if it had been active from the beginning. This facilitates a transparent transition between different sampling
#' approaches.
#'
#' @section Example:
#' Consider a chain of three samplers with `n.rashomon.samples = c(5, 2, Inf)`. Here's how the sampling process flows:
#'
#' ### First Sampler (`n.rashomon.samples` = 5)
#'
#' 1. The first sampler begins active and can:
#'    * Request X samples through `askXSamples()`
#'    * (Afterwards) request Y values for these samples through `askYValues()`
#' 2. All requests are forwarded to the caller for evaluation
#' 3. The sampler remains active until at least 5 Y values have been received (through either `tellXSamples()` with a
#'    scorecol, or through `tellYValues()`)
#'
#' ### Transition to Second Sampler (n.rashomon.samples = 2)
#'
#' 1. Once 5 or more Y values are received, control transitions to the second sampler
#' 2. During transition:
#'    * All known X samples and Y values are passed to the second sampler via `tellXSamples()`
#'    * Samples with known Y values are provided first, followed by any pending X samples
#'    * The first sampler is never called again after transition
#'
#' ### Second Sampler Operation
#'
#' 1. The second sampler begins with all historical data
#' 2. It needs 2 new Y values (beyond the initial 5) before control transitions again
#' 3. It can request new X samples or Y values for pending (or new) samples
#' 4. It is possible that a total number of Y-values jumped from below 5 to at least 7 in one call, in which case
#'    control is never given to the second sampler and the third sampler starts right away, instead.
#'
#' ### Final Sampler (n.rashomon.samples = Inf)
#' 1. After 7 total Y values, control moves to the final sampler
#' 2. The final sampler receives all historical data, where complete cases with Y-values are provided first.
#' 3. It continues indefinitely (Inf) and determines final set membership
#'
#' ### Important Notes:
#'
#' * Skip transitions: If a single call provides more Y values than needed (e.g., jumping from 4 to 7 total Y values),
#'   the chain may skip samplers entirely
#' * Data preservation: Each sampler inherits all previous X samples and Y values
#' * Order preservation: Y-valued samples are always provided before pending X samples during transitions
#' * One-way transitions: Once a sampler transitions, it is never called again
#'
#' @export
RashomonSamplerChain <- R6Class("RashomonSamplerChain",
  inherit = RashomonSampler,
  public = list(
    #' @description
    #' Initialize the chained Rashomon sampler.
    #' @param id (`character(1)`) Identifier for this sampler instance, used for logging and printing
    #' @param samplers (`list`) List of RashomonSampler objects to chain together
    #' @param ask.y.each (`numeric`) Vector of length equal to `samplers` specifying how many Y-values
    #'   each sampler should evaluate before transitioning to the next. The last value must be `Inf`.
    #' @param n.rashomon.samples (`integer(1)` | `Inf`) Target number of Rashomon set samples.
    #'   Note that individual samplers in the chain may have different n.rashomon.samples values,
    #'   which could lead to incomplete results if they are smaller than this value.
    initialize = function(id, samplers, ask.y.each, n.rashomon.samples) {
      assertList(samplers, types = "RashomonSampler", min.len = 1L, any.missing = FALSE)
      assertNumeric(ask.y.each, len = length(samplers), lower = 0, any.missing = FALSE)
      assertTRUE(tail(ask.y.each, 1) == Inf)
      assertIntegerish(head(ask.y.each, -1), lower = 0, any.missing = FALSE)
      domain <- samplers[[1]]$domain
      minimize <- samplers[[1]]$minimize
      for (i in seq_along(samplers)) {
        for (element in c("domain", "minimize")) {
          assertTRUE(all.equal(as.data.table(samplers[[i]][[element]]), as.data.table(samplers[[1]][[element]])),
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
      private$.known.y.count <- 0L
      # .table.cache is NULL if there is only one sampler
      private$.table.cache <- if (private$.still.caching) getNullTable(domain, include.id = TRUE, include.score = TRUE)

      # Inherit from last sampler
      rashomon.epsilon <- samplers[[length(samplers)]]$rashomon.epsilon
      rashomon.is.relative <- samplers[[length(samplers)]]$rashomon.is.relative

      super$initialize(id, domain, minimize, rashomon.epsilon, rashomon.is.relative, 0, n.rashomon.samples)
    }
  ),
  active = list(
    #' @field samplers (`list`) List of RashomonSampler objects in the chain
    samplers = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.samplers)) {
        stop("samplers cannot be changed")
      }
      private$.samplers
    },
    #' @field sampler.index (`integer(1)`) Index of the currently active sampler
    sampler.index = function() private$.sampler.index,
    #' @field ask.y.each (`numeric`) Number of Y-values at which transitions occur
    # internally, .ask.y.each is cumulative
    ask.y.each = function() c(private$.ask.y.each[[1]], diff(private$.ask.y.each))
  ),
  private = list(
    # samplers being wrapped
    .samplers = NULL,
    # index of the currently active sampler
    .sampler.index = NULL,
    # cumulative number of Y-values at which transitions occur. .known.y.count >= .ask.y.each[i] => transition to i+1
    .ask.y.each = NULL,
    # cache of X-values seen so far. Set to NULL when no longer needed, which happens when:
    # .sampler.index == length(.samplers) AND the last sampler has seen all cached X-samples.
    .table.cache = NULL,
    # number of Y-values that are in the .table.cache
    .known.y.count = NULL,
    # whether we are still recording Y-values. Set to FALSE when .sampler.index == length(.samplers)
    .still.caching = NULL,
    # index of the last row that was fed to the currently active sampler.
    # .askYValues() returns 0 until .cache.index == NROW(.table.cache)
    .cache.index = NULL,
    .askXSamples = function() {
      # use NROW here since .table.cache may be NULL
      if (private$.cache.index < NROW(private$.table.cache)) {
        # we still have X-values that we could give to the currently active sampler
        return(0L)
      }
      private$.samplers[[private$.sampler.index]]$askXSamples()
    },
    .tellXSamples = function(x) {
      if (private$.still.caching) {
        # we should only get here when the current sampler has seen all cached X-samples
        assertTRUE(private$.cache.index == nrow(private$.table.cache))
        private$.table.cache <- rbind(private$.table.cache, x, use.names = TRUE)
        private$.cache.index <- nrow(private$.table.cache)
        private$.known.y.count <- private$.known.y.count + sum(!is.na(x$.score))
        if (private$.known.y.count >= private$.ask.y.each[private$.sampler.index]) {
          private$.switchToNextSampler()
          return(invisible(NULL))
        }
      }
      private$.samplers[[private$.sampler.index]]$tellXSamples(x, scorecol = ".score")
    },
    .askYValues = function() {
      asking <- private$.samplers[[private$.sampler.index]]$askYValues()
      if (!is.null(private$.table.cache)) {
        # match() here gives NA if asking$.id is not in the first .cache.index entries of .table.cache$.ids.
        # This also means we only look at .table.cache lines that were already given to the currently active sampler.
        rows <- match(asking$.id, private$.table.cache$.id[seq_len(private$.cache.index)])
        if (anyNA(rows)) {
          stop(sprintf("Sampler %s asked for .id values that were not seen yet.",
            private$.samplers[[private$.sampler.index]]$id
          ))
        }
        if (!all(is.na(private$.table.cache[rows, .score]))) {
          stop(sprintf("Sampler %s asked for .id values that should have been answered already. This is a bug.",
            private$.samplers[[private$.sampler.index]]$id
          ))
        }
      }
      asking
    },
    .tellYValues = function(y) {
      if (private$.still.caching) {
        # not yet using the last sampler, so still recording y-values
        row <- match(y$.id, private$.table.cache$.id)
        set(private$.table.cache, i = row, j = ".score", value = y$.score)
        private$.known.y.count <- private$.known.y.count + nrow(y)
        if (private$.known.y.count >= private$.ask.y.each[[private$.sampler.index]]) {
          private$.switchToNextSampler()
          return(invisible(NULL))
        }
      }
      private$.samplers[[private$.sampler.index]]$tellYValues(y)
      if (!is.null(private$.table.cache)) {
        private$.feedCacheToSampler()
      }
    },
    .getRashomonSamples = function() {
      private$.samplers[[private$.sampler.index]]$getRashomonSamples()
    },
    .rashomonSamplesComplete = function() {
      private$.samplers[[private$.sampler.index]]$rashomonSamplesComplete()
    },
    .switchToNextSampler = function() {
      while (private$.known.y.count >= private$.ask.y.each[[private$.sampler.index]]) {
        private$.sampler.index <- private$.sampler.index + 1L
      }
      private$.table.cache <- private$.table.cache[order(is.na(.score))]  # put rows with known scores first
      private$.cache.index <- 0L
      private$.feedCacheToSampler()
      if (private$.cache.index < private$.known.y.count) {
        stop(sprintf("Sampler %s askXSamples() returned 0 even though it was only supplied with complete samples",
          private$.samplers[[private$.sampler.index]]$id
        ))
      }
      private$.still.caching <- length(private$.samplers) > private$.sampler.index
      if (!private$.still.caching && private$.cache.index >= nrow(private$.table.cache)) {
        # not going to need the cache any more
        private$.table.cache <- NULL
      }
    },
    .feedCacheToSampler = function() {
      sampler.current <- private$.samplers[[private$.sampler.index]]
      max.cache.index <- nrow(private$.table.cache)
      while (sampler.current$askXSamples() != 0 && private$.cache.index < max.cache.index) {
        asking <- sampler.current$askXSamples()
        next.cache.index <- min(private$.cache.index + asking, max.cache.index)
        rows <- seq.int(private$.cache.index + 1L, next.cache.index)
        sampler.current$tellXSamples(private$.table.cache[rows, , nomatch = NULL], scorecol = ".score")
        private$.cache.index <- next.cache.index
      }
    }
  )
)
