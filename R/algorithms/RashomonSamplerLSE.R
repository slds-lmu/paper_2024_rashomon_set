#' @title Rashomon Sampler using LSE from Gotovos et al. 2013
#'
#' @description
#' This sampler uses the LSE (Local Surrogate Estimation) method from Gotovos et al. 2013 to find the Rashomon set.
#'
#' TODO: this implementation is broken, use the RashomonSamplerLSEImp with implicit.threshold.method = FALSE instead
#'
#' @export
RashomonSamplerLSE <- R6Class("RashomonSamplerLSE",
  inherit = RashomonSamplerLearnerBased,
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
    #' @param lse.beta (`numeric(1)`) Beta (scaling) parameter for LSE
    #' @param lse.epsilon (`numeric(1)`) Epsilon (accuracy) parameter for LSE
    #' @param search.grid.size (`integer(1)`) Number of samples to request in the initial batch
    #' @param seed (`integer(1)`) Random seed for reproducibility
    #' @param n.rashomon.samples (`integer(1)` | `Inf`) Target number of Rashomon set samples.
    #'   RashomonSampler implementations may optimize their behaviour to hit this target specifically.
    initialize = function(id, domain, minimize, rashomon.epsilon, rashomon.is.relative, learner, lse.beta, lse.epsilon,
        search.grid.size, seed, n.rashomon.samples) {
      super$initialize(id, domain, minimize, rashomon.epsilon, rashomon.is.relative, learner,
        search.grid.size, seed, n.rashomon.samples)
      private$.lse.beta <- assertNumber(lse.beta, lower = 0, upper = 1, finite = TRUE)
      private$.lse.epsilon <- assertNumber(lse.epsilon, lower = 0, upper = 1, finite = TRUE)
      private$.threshold <- if (minimize) Inf else -Inf
      private$.resultset <- getNullTable(domain, include.id = TRUE, include.score = TRUE)
    }
  ),
  active = list(
    #' @field lse.beta (`numeric(1)`) Beta parameter for LSE
    lse.beta = function() private$.lse.beta,
    #' @field lse.epsilon (`numeric(1)`) Epsilon parameter for LSE
    lse.epsilon = function() private$.lse.epsilon
  ),
  private = list(
    .lse.beta = NULL,
    .lse.epsilon = NULL,
    .metainfo = NULL,
    .resultset = NULL,
    .threshold = NULL,
    .tellXSamples = function(x) {
      super$.tellXSamples(x)
    },
    .tellYValues = function(y) {
      super$.tellYValues(y)
    },
    .askYValuesWithLearner = function(mean.pred, sd.pred, known.y, known.y.predicted, known.y.predicted.sd,
        grid.known, grid.unknown, learner) {

      pred.index.to.fullindex <- private$.search.grid[, which(is.na(.score))]
      known.index.to.fullindex <- private$.search.grid[, which(!is.na(.score))]

      if (is.null(private$.metainfo)) {
        private$.metainfo <- data.table(
          lower = rep(-Inf, length(mean.pred) + length(known.y.predicted)), # "min(C(t))"
          upper = rep(Inf, length(mean.pred) + length(known.y.predicted)), # "max(C(t))"
          in.solution.set = FALSE
        )
      }

      sd.pred.full <- numeric(length(mean.pred) + length(known.y.predicted))
      sd.pred.full[pred.index.to.fullindex] <- sd.pred
      sd.pred.full[known.index.to.fullindex] <- known.y.predicted.sd
      mean.pred.full <- numeric(length(mean.pred) + length(known.y.predicted))
      mean.pred.full[pred.index.to.fullindex] <- mean.pred
      mean.pred.full[known.index.to.fullindex] <- known.y.predicted

      # Get confidence intervals
      interval.width <- sqrt(self$lse.beta) * sd.pred.full
      new.lower <- mean.pred.full - interval.width
      new.upper <- mean.pred.full + interval.width
      # update C(t) = Intersection(C(t-1), Q(t))
      private$.metainfo[lower < new.lower, lower := new.lower]
      private$.metainfo[upper > new.upper, upper := new.upper]

      threshold <- min(mean.pred.full)
      if (self$rashomon.is.relative) {
        threshold <- threshold + abs(threshold * self$rashomon.epsilon)  # abs since we could have negatives
      } else {
        threshold <- threshold + self$rashomon.epsilon
      }

      private$.metainfo[, in.solution.set := upper - self$lse.epsilon <= threshold]

      ambiguity <- private$.metainfo[is.na(.score), pmin(upper - threshold, threshold - lower)]

      index.to.keep[which.max(ambiguity)]
    },
    .getRashomonSamples = function() {
      self$askYValues()  # trigger model update. Fails if we don't have all samples, as it should.
      private$.resultset[in.solution.set == TRUE]
    }
  )
)
