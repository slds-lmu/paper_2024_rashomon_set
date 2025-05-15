#' @title Using the Straddle Heuristic to Find the Rashomon Set
#'
#' @description
#' This sampler uses the straddle heuristic (Bryan et al. 2005) to find the Rashomon set.
#'
#' @export
RashomonSamplerStraddle <- R6Class("RashomonSamplerStraddle",
  inherit = RashomonSamplerOptimize,
  public = list(
    #' @description
    #' Initialize the Rashomon sampler.
    #' @param id (`character(1)`) Identifier for this sampler instance, used for logging and printing
    #' @param domain (`ParamSet`) Parameter space to search over
    #' @param minimize (`logical(1)`) Whether to minimize (`TRUE`) or maximize (`FALSE`) the objective
    #' @param rashomon.epsilon (`numeric(1)`) Epsilon threshold for Rashomon set membership
    #' @param rashomon.is.relative (`logical(1)`) Whether epsilon is relative to optimal score (`TRUE`) or absolute
    #'   (`FALSE`)
    #' @param learner (`Learner`) Learner to optimize
    #' @param search.grid.size (`integer(1)`) Number of samples to request in the initial batch
    #' @param seed (`integer(1)`) Random seed for reproducibility
    #' @param n.rashomon.samples (`integer(1)` | `Inf`) Target number of Rashomon set samples.
    #'   RashomonSampler implementations may optimize their behaviour to hit this target specifically.
    initialize = function(id, domain, minimize, rashomon.epsilon, rashomon.is.relative, learner, search.grid.size, seed,
        n.rashomon.samples) {
      super$initialize(id, domain, minimize, rashomon.epsilon, rashomon.is.relative, learner,
        aqf = AqfStraddle(rashomon.epsilon, rashomon.is.relative), search.grid.size, seed)
    }
  )
)

#' @title Straddle Acquisition Function
#'
#' @description
#' Acquisition function that implements the straddle heuristic.
#'
#' @param rashomon.epsilon (`numeric(1)`) Epsilon threshold for Rashomon set membership
#' @param rashomon.is.relative (`logical(1)`) Whether epsilon is relative to optimal score (`TRUE`) or absolute
#'   (`FALSE`)
#' @return Straddle Acquisition Function
#'
#' @family Acquisition Functions
#' @export
AqfStraddle <- function(rashomon.epsilon, rashomon.is.relative) {
  assertNumber(rashomon.epsilon, lower = 0, finite = TRUE)
  assertLogical(rashomon.is.relative)
  makeAqf(function(mean, sd, known.y, known.y.predicted) {
    # remember we are minimizing
    threshold <- min(known.y.predicted, mean)
    if (rashomon.is.relative) {
      # for negative scores (when maximizing) we need abs here
      rashomon.epsilon <- abs(threshold) * rashomon.epsilon
    }
    threshold <- threshold + rashomon.epsilon
    - (1.96 * sd - abs(mean - threshold))  # we need to return a small value for favourable points
  }, sprintf("AqfStraddle(ε = %s, %s)", rashomon.epsilon, if (rashomon.is.relative) "relative" else "absolute"))
}
