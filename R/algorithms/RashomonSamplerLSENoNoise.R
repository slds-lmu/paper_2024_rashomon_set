#' @title Rashomon Sampler using LSE from Gotovos et al. 2013
#'
#' @description
#' This sampler uses the LSE (Local Surrogate Estimation) method from Gotovos et al. 2013 to find the Rashomon set.
#'
#' The sampler assumes no noise and uses the supplied Y-values to calculate the threshold directly.
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
      private$.updateResultSet(x[!is.na(.score)])
    },
    .tellYValues = function(y) {
      super$.tellYValues(y)
      private$.updateResultSet(y)
    },
    .updateResultSet = function(y) {
      if (!nrow(y)) return()  # no new samples, happens if .tellXSamples was called without scores
      # get the threshold from current best known y
      # This only makes sense with non-implicity LSE if we optimized before and are reasonably sure about the optimal y
      if (self$rashomon.is.relative) {
        epsilon <- abs(y) * self$rashomon.epsilon
      } else {
        epsilon <- self$rashomon.epsilon
      }
      if (self$minimize) {
        threshold.candidate <- min(y$.score) + epsilon
        if (threshold.candidate < private$.threshold) {
          # threshold got more strict -> resultset shrinks
          private$.resultset <- private$.resultset[.score <= private$.threshold]
          private$.threshold <- threshold.candidate
        }
        private$.resultset <- rbind(private$.resultset, y[.score <= private$.threshold])
      } else {
        threshold.candidate <- max(y$.score) - epsilon
        if (threshold.candidate > private$.threshold) {
          private$.resultset <- private$.resultset[.score >= private$.threshold]
          private$.threshold <- threshold.candidate
        }
        private$.resultset <- rbind(private$.resultset, y[.score >= private$.threshold])
      }
    },
    .askYValuesWithLearner = function(mean.pred, sd.pred, known.y, known.y.predicted, known.y.predicted.sd,
        grid.known, grid.unknown, learner) {
      if (is.null(private$.metainfo)) {
        private$.metainfo <- data.table(
          lower = rep(-Inf, length(mean.pred)), # "min(C(t))"
          upper = rep(Inf, length(mean.pred)) # "max(C(t))"
        )
      }


      # Get confidence intervals
      interval.width <- sqrt(self$lse.beta) * sd.pred
      new.lower <- mean.pred - interval.width
      new.upper <- mean.pred + interval.width
      # update C(t) = Intersection(C(t-1), Q(t))
      private$.metainfo[lower < new.lower, lower := new.lower]
      private$.metainfo[upper > new.upper, upper := new.upper]

      lgl.to.solution <- private$.metainfo[, upper - self$lse.epsilon <= private$.threshold]
      lgl.to.discard <- private$.metainfo[, lower + self$lse.epsilon >= private$.threshold]
      index.to.solution <- which(lgl.to.solution)
      index.to.discard <- which(lgl.to.discard)
      index.to.keep <- which(!(lgl.to.solution | lgl.to.discard))
      index.to.fullindex <- private$.search.grid[, which(is.na(.score))]
      fullindex.to.keep <- index.to.fullindex[index.to.keep]

      # update solution set, "L"
      private$.resultset <- rbind(
        private$.resultset,
        grid.unknown[index.to.solution]
      )
      # update candidate set, "U"
      # ... of the total archive (need to use "fullindex", since samples with known Ys are also in here)
      private$.search.grid <- private$.search.grid[fullindex.to.keep]
      # ... of the learner predictions that we still need to aqf-fun optimize over
      mean.pred <- mean.pred[index.to.keep]
      sd.pred <- sd.pred[index.to.keep]
      # ... of the intervals that we are tracking
      private$.metainfo <- private$.metainfo[index.to.keep]

      ambiguity <- pmin(private$.metainfo$upper - private$.threshold, private$.threshold - private$.metainfo$lower)

      index.to.keep[which.max(ambiguity)]
    },
    .getRashomonSamples = function() {
      private$.resultset
    }
  )
)
