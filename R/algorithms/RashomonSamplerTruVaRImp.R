
#' @title Rashomon Sampler for Truncated Variance with Implicitly Defined Level
#'
#' @description
#'
#' @export
RashomonSamplerTruVaRImp <- R6Class("RashomonSamplerTruVaRImp",
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
    #' @param delta.bar (`numeric(1)`) delta bar (confidence bound parameter)
    #' @param r (`numeric(1)`) eta reduction factor
    #' @param beta (`numeric`) beta (SD scaling) parameter. Vector with element for each epoch;
    #'   last value is repeated indefinitely.
    #' @param eta (`numeric(1)`) eta (target uncertainty) parameter
    #' @param search.grid.size (`integer(1)`) Number of samples to request in the initial batch
    #' @param seed (`integer(1)`) Random seed for reproducibility
    #' @param n.rashomon.samples (`integer(1)` | `Inf`) Target number of Rashomon set samples.
    #'   RashomonSampler implementations may optimize their behaviour to hit this target specifically.
    #' @param chunk.size (`integer(1)` | `Inf`) Size of chunks to process at a time when calculating
    #'   scaled excess variance reduction. If `Inf`, the entire grid is processed at once.
    initialize = function(id, domain, minimize, rashomon.epsilon, rashomon.is.relative, learner,
        delta.bar, r, beta, eta, search.grid.size, seed, n.rashomon.samples, chunk.size = Inf) {
      super$initialize(id, domain, minimize, rashomon.epsilon, rashomon.is.relative, learner,
        search.grid.size, seed, n.rashomon.samples)
      private$.delta.bar <- assertNumber(delta.bar, lower = 0, finite = TRUE)
      private$.r <- assertNumber(r, lower = 0, upper = 1)
      if (r == 1) {
        stop("r must be smaller than 1")
      }
      private$.beta <- assertNumeric(beta, min.len = 1, any.missing = FALSE, lower = 0, finite = TRUE)
      private$.eta <- assertNumber(eta, lower = 0, finite = TRUE)
      private$.epoch <- 1
      private$.chunk.size <- assert(
        checkCount(chunk.size, positive = TRUE, tol = 0),
        checkNumber(chunk.size, lower = Inf)
      )
    }
  ),
  active = list(
    #' @field delta.bar (`numeric(1)`) delta bar (confidence bound parameter)
    delta.bar = function() private$.delta.bar,
    #' @field r (`numeric(1)`) eta reduction factor
    r = function() private$.r,
    #' @field beta (`numeric`) beta (SD scaling) parameter. Vector with element for each epoch;
    #'   last value is repeated indefinitely.
    beta = function() private$.beta,
    #' @field eta (`numeric(1)`) eta (target uncertainty) parameter
    eta = function() private$.eta,
    #' @field epoch (`integer(1)`) Current epoch
    epoch = function() private$.epoch,
    #' @field chunk.size (`integer(1)`) Size of chunks to process at a time when calculating
    #'   scaled excess variance reduction.
    chunk.size = function() private$.chunk.size
  ),
  private = list(
    .delta.bar = NULL,
    .r = NULL,
    .beta = NULL,
    .eta = NULL,
    .epoch = NULL,
    .chunk.size = NULL,
    .L = NULL,  # points in the low set
    .H = NULL,  # points in the high set
    .U = NULL,  # unclassified points
    .M = NULL,  # potential minimizers
    .askYValuesWithLearner = function(mean.pred, sd.pred, known.y, known.y.predicted, known.y.predicted.sd,
        grid.known, grid.unknown, learner) {

      if (is.null(private$.L)) {
        private$.L <- integer(0)
        private$.H <- integer(0)
        private$.U <- seq_len(nrow(private$.search.grid))
        private$.M <- seq_len(nrow(private$.search.grid))
      }

      sqrt.beta.i <- if (private$.epoch <= length(private$.beta)) {
        sqrt(private$.beta[private$.epoch])
      } else {
        sqrt(private$.beta[length(private$.beta)])
      }
      eta.i <- private$.eta * private$.r^(private$.epoch - 1)

      # map index within 'grid.known' to index within 'private$.search.grid'
      pred.index.to.fullindex <- private$.search.grid[, which(is.na(.score))]
      known.index.to.fullindex <- private$.search.grid[, which(!is.na(.score))]

      sd.pred.full <- numeric(length(mean.pred) + length(known.y.predicted))
      sd.pred.full[pred.index.to.fullindex] <- sd.pred
      sd.pred.full[known.index.to.fullindex] <- known.y.predicted.sd

      mean.pred.full <- numeric(length(mean.pred) + length(known.y.predicted))
      mean.pred.full[pred.index.to.fullindex] <- mean.pred
      mean.pred.full[known.index.to.fullindex] <- known.y.predicted

      # get confidence intervals
      interval.width <- sqrt.beta.i * sd.pred.full
      current.l <- mean.pred.full - interval.width
      current.u <- mean.pred.full + interval.width

      f.pes <- max(current.u[private$.M])
      f.opt <- min(current.l[private$.M])
      if (self$rashomon.is.relative) {
        h.opt <- f.opt + abs(f.opt) * self$rashomon.epsilon
        h.pes <- f.pes + abs(f.pes) * self$rashomon.epsilon
        m.interval.factor <- 1 / (1 + self$rashomon.epsilon)
      } else {
        h.opt <- f.opt + self$rashomon.epsilon
        h.pes <- f.pes + self$rashomon.epsilon
        m.interval.factor <- 1
      }

      # update (un-)decided level sets
      u.to.l <- current.u[private$.U] <= h.opt
      u.to.h <- current.l[private$.U] > h.pes
      private$.L <- c(private$.L, private$.U[u.to.l])
      private$.H <- c(private$.H, private$.U[u.to.h])
      private$.U <- private$.U[!(u.to.l | u.to.h)]

      # update M
      private$.M <- private$.M[current.l[private$.M] <= f.pes]

      # advance epoch as long as interval widths are within bounds
      while (length(private$.U) > 0 &&
        max(interval.width[private$.U]) <= eta.i * (1 + private$.delta.bar) &&
        max(interval.width[private$.M]) <=
          eta.i * (1 - private$.delta.bar) * m.interval.factor
      ) {
        # advance epoch
        private$.epoch <- private$.epoch + 1
        sqrt.beta.i <- if (private$.epoch <= length(private$.beta)) {
          sqrt(private$.beta[private$.epoch])
        } else {
          sqrt(private$.beta[length(private$.beta)])
        }
        eta.i <- private$.eta * private$.r^(private$.epoch - 1)

        # update confidence intervals
        interval.width <- sqrt.beta.i * sd.pred.full
      }

      task.m <- as_task_regr(private$.search.grid[private$.M, -".id", with = FALSE], target = ".score", id = "M")
      task.u <- as_task_regr(private$.search.grid[private$.U, -".id", with = FALSE], target = ".score", id = "U")

      chunks <- split(seq_len(nrow(grid.unknown)), 1 + floor((seq_len(nrow(grid.unknown)) - 1) / private$.chunk.size))

      newpoints <- as_task_regr(grid.unknown[, -".id", with = FALSE], target = ".score", id = "unknown")
      scaled.excess.variance.improvement <- (
        # excess variance improvement for maximum
        colSums(learner$totalScaledExcessVarianceReduction(newpoints, task.m,
          sqrt.beta.i^2, eta.i, private$.chunk.size)) +
        # excess variance improvement for threshold
        colSums(learner$totalScaledExcessVarianceReduction(newpoints, task.u,
          sqrt.beta.i^2, eta.i, private$.chunk.size)) / m.interval.factor
      )

      which.max(scaled.excess.variance.improvement)
    }
  )
)
