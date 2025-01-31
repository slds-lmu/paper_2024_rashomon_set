#' @title Rashomon Sampler using LSE-impl from Gotovos et al. 2013
#'
#' @description
#' This sampler uses the LSE (Local Surrogate Estimation) method with implicit threshold from Gotovos et al. 2013 to
#' find the Rashomon set.
#'
#' @export
RashomonSamplerLSEImplicit <- R6Class("RashomonSamplerLSEImplicit",
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
    #' @param learner (`Learner`) Surrogate model to use
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
      private$.lse.beta <- assertNumber(lse.beta, lower = 0, finite = TRUE)
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
#if (sum(private$.search.grid[, !is.na(.score)]) == 132) browser()


      pred.index.to.fullindex <- private$.search.grid[, which(is.na(.score))]
      known.index.to.fullindex <- private$.search.grid[, which(!is.na(.score))]

      if (is.null(private$.metainfo)) {
        private$.metainfo <- data.table(
          lower = rep(-Inf, length(mean.pred) + length(known.y.predicted)),  # "min(C(t))"
          upper = rep(Inf, length(mean.pred) + length(known.y.predicted)),  # "max(C(t))"
          in.candidate.set = TRUE  # "U" if TRUE, "M^H" or "M^L" if FALSE
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
      private$.metainfo[, lower := pmin(pmax(lower, new.lower), upper)]
      private$.metainfo[, upper := pmax(pmin(upper, new.upper), lower)]

      opt.optimistic <- min(private$.metainfo$lower, private$.threshold)
      opt.pessimistic <- min(private$.metainfo$upper)

      if (self$rashomon.is.relative) {
        threshold.optimistic <- opt.optimistic + abs(opt.optimistic) * self$rashomon.epsilon
        threshold.pessimistic <- opt.pessimistic + abs(opt.pessimistic) * self$rashomon.epsilon
      } else {
        threshold.optimistic <- opt.optimistic + self$rashomon.epsilon
        threshold.pessimistic <- opt.pessimistic + self$rashomon.epsilon
      }


      # candidate with upper bound (+ eps) below the worst-case threshold whose lower bound is above the worst-case
      # optimum is in the solution set, but not interesting any more for threshold change -> remove from search grid
      # altogether, but put into solution set.
      lgl.move.to.solution <- private$.metainfo[,
        in.candidate.set &
        upper - self$lse.epsilon <= threshold.optimistic &
        lower > opt.pessimistic
      ]
      lgl.move.to.solution <- FALSE  # temporary measure

      # candidate with upper bound (+ eps) below the worst-case threshold whose lower bound is below the worst-case
      # optimum also goes into the solution set, but could still be interesting for updating threshold
      lgl.copy.to.solution <- private$.metainfo[,
        in.candidate.set &
        upper - self$lse.epsilon <= threshold.optimistic &
        lower <= opt.pessimistic
      ]

      lgl.discard.altogether <- private$.metainfo[,
        in.candidate.set &
        lower + self$lse.epsilon > threshold.pessimistic &
        lower > opt.pessimistic
      ]

      # the following is in case lse.epsilon is large
      lgl.no.candidate.but.potential.opt <- private$.metainfo[,
        in.candidate.set &
        lower + self$lse.epsilon > threshold.pessimistic &
        lower <= opt.pessimistic
      ]

      # this one may not be necessary
      lgl.drop.completely <- private$.metainfo[,
        !in.candidate.set & lower > opt.pessimistic
      ]

      # stuff is monotonic, can do these things separately
      fullindex.to.solution <- which(lgl.move.to.solution | lgl.copy.to.solution)

      # don't discard known samples
      fullindex.to.discard <- which( (lgl.move.to.solution | lgl.discard.altogether | lgl.drop.completely) & private$.search.grid[, is.na(.score)])
      index.to.keep <- which(!(lgl.move.to.solution | lgl.discard.altogether | lgl.drop.completely)[private$.search.grid[, is.na(.score)]])


      index.to.fullindex <- private$.search.grid[, which(is.na(.score))]
      # fullindex.to.solution <- index.to.fullindex[index.to.solution]
      # fullindex.to.discard <- index.to.fullindex[index.to.discard]

      # update solution set, "L"
      private$.resultset <- rbind(
        private$.resultset,
        private$.search.grid[fullindex.to.solution]
      )
#      if (length(fullindex.to.solution) > 0) {
#        write.table(private$.search.grid[fullindex.to.solution], file = stdout(), row.names = FALSE, col.names = FALSE, sep = ",")
#      }

      # remove from candidate set if copied or discarded, but could still be minimizers
      private$.metainfo[lgl.no.candidate.but.potential.opt | lgl.copy.to.solution, in.candidate.set := FALSE]

      # update candidate set, "U"
      # ... of the total archive (need to use "fullindex", since samples with known Ys are also in here)
      if (length(fullindex.to.discard)) {
        private$.search.grid <- private$.search.grid[-fullindex.to.discard]
        private$.metainfo <- private$.metainfo[-fullindex.to.discard]
      }

      index.to.keep[which.max(  (private$.metainfo$upper - private$.metainfo$lower)[private$.search.grid[, is.na(.score)]]   )]
    },
    .getRashomonSamples = function() {
      self$askYValues()  # trigger model update. Fails if we don't have all samples, as it should.
      private$.resultset
    }
  )
)
