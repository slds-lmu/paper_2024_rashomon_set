#' @title Rashomon Sampler Learner Wrapper
#'
#' @description
#' A RashomonSampler that inherits from [RashomonSamplerLearnerBased].
#' Its primary purpose is to ensure a `Learner` (as managed by `RashomonSamplerLearnerBased`)
#' is trained on the available data. However, all core sampling decisions
#' (asking for X samples, telling X samples, asking for Y values, telling Y values,
#' and retrieving the Rashomon set) are delegated to a wrapped `RashomonSampler` instance.
#'
#' This allows a `Learner` to be fitted and inspected (via `lastmodel`) even when the
#' underlying sampling strategy does not inherently require a surrogate model.
#'
#' @export
RashomonSamplerLearnerWrapper <- R6Class("RashomonSamplerLearnerWrapper",
  inherit = RashomonSamplerLearnerBased,
  public = list(
    #' @description
    #' Initialize the RashomonSamplerLearnerWrapper.
    #' @param id (`character(1)`) Identifier for this sampler instance.
    #' @param learner (`Learner`) An `mlr3` learner instance to be trained.
    #' @param search.grid.size (`integer(1)`) Number of samples for the initial grid for learner training.
    #' @param seed (`integer(1)`) Random seed for reproducibility.
    #' @param n.rashomon.samples (`integer(1)` | `Inf`) Target number of Rashomon set samples for the wrapper.
    #' @param wrapped.sampler (`RashomonSampler`) The `RashomonSampler` instance to wrap and delegate to.
    initialize = function(wrapped.sampler, learner, seed, id = sprintf("learnerwrapped_%s", wrapped.sampler$id)) {
      private$.wrapped.sampler <- assertR6(wrapped.sampler, "RashomonSampler")$clone(deep = TRUE)

      super$initialize(id,
        domain = wrapped.sampler$domain,
        minimize = wrapped.sampler$minimize,
        rashomon.epsilon = wrapped.sampler$rashomon.epsilon,
        rashomon.is.relative = wrapped.sampler$rashomon.is.relative,
        learner = learner,
        search.grid.size = 1L,  # does not matter, wrapped RS decides this
        seed = seed,
        n.rashomon.samples = wrapped.sampler$n.rashomon.samples
      )
    }
  ),
  active = list(
    #' @field wrapped.sampler (`RashomonSampler`) The wrapped sampler instance.
    wrapped.sampler = function(rhs) {
      if (!missing(rhs)) {
        stop("wrapped.sampler is read-only")
      }
      private$.wrapped.sampler
    }
  ),
  private = list(
    .wrapped.sampler = NULL,

    .askXSamples = function() {
      private$.wrapped.sampler$askXSamples()
    },

    .tellXSamples = function(x) {
      # Allow LearnerBased to process X samples (set or extend `private$.search.grid`)
      super$.tellXSamples(x)
      # Delegate to the wrapped sampler's public `tellXSamples`.
      # The `x` data.table here has a `.score` column (potentially all NA).
      private$.wrapped.sampler$tellXSamples(x, scorecol = ".score")
    },

    .askYValuesWithLearner = function(mean.pred, sd.pred, known.y, known.y.predicted, known.y.predicted.sd,
        grid.known, grid.unknown, learner) {

      # Delegate to the wrapped sampler for actual Y value selection without doing anything with the predictions
      table.asking <- private$.wrapped.sampler$askYValues()
      match(table.asking$.id, grid.unknown$.id)
    },

    .tellYValues = function(y) {
      # Allow LearnerBased to process Y values (e.g., update `private$.search.grid`)
      super$.tellYValues(y)
      # Delegate to the wrapped sampler's public `tellYValues`.
      # `y` has .id and .score columns.
      private$.wrapped.sampler$tellYValues(y, scorecol = ".score")
    },

    .getRashomonSamples = function() {
      # Delegate entirely to the wrapped sampler for retrieving the Rashomon set.
      # This means the Rashomon set definition (epsilon, relative, etc.) of the
      # wrapped sampler will be used.
      private$.wrapped.sampler$getRashomonSamples()
    }
  )
)
