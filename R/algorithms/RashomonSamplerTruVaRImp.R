
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
        delta.bar, r, beta, eta, search.grid.size, seed, n.rashomon.samples, chunk.size = Inf,
        implicit.threshold.method = TRUE) {
      super$initialize(id, domain, minimize, rashomon.epsilon, rashomon.is.relative, learner,
        search.grid.size, seed, n.rashomon.samples)
      private$.delta.bar <- assertNumber(delta.bar, lower = 0, finite = TRUE)
      private$.r <- assertNumber(r, lower = 0, upper = 1)
      if (r == 1) {
        stop("r must be smaller than 1")
      }
      assert(
        checkNumeric(beta, min.len = 1, any.missing = FALSE, lower = 0, finite = TRUE),
        checkFunction(beta)
      )
      private$.beta <- beta
      private$.eta <- assertNumber(eta, lower = 0, finite = TRUE)
      private$.epoch <- 1
      assert(
        checkCount(chunk.size, positive = TRUE, tol = 0),
        checkNumber(chunk.size, lower = Inf)
      )
      private$.chunk.size <- chunk.size
      private$.implicit.threshold.method <- implicit.threshold.method
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
    chunk.size = function() private$.chunk.size,
    #' @field metainfo (`list`) Additional information about the sampler
    metainfo = function() {
      row <- seq_len(nrow(private$.search.grid))
      cbind(
        private$.search.grid,
        .is.in.M = row %in% private$.M,
        .lse.set = fcase(
          row %in% private$.L, "L",
          row %in% private$.H, "H",
          row %in% private$.U, "U",
          default = NA_character_
        )
      )
    }
  ),
  private = list(
    .delta.bar = NULL,
    .r = NULL,
    .beta = NULL,
    .eta = NULL,
    .epoch = NULL,
    .chunk.size = NULL,
    .last.epoch.switch = NULL,
    .implicit.threshold.method = NULL,
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
        if (private$.implicit.threshold.method) {
          private$.M <- seq_len(nrow(private$.search.grid))
        } else {
          private$.M <- integer(0)
        }
      }

      if (is.null(private$.last.epoch.switch)) {
        private$.last.epoch.switch <- length(known.y)
      }

      sqrt.beta.i <- if (is.function(private$.beta)) {
        sqrt(private$.beta(i = private$.epoch, t = private$.last.epoch.switch, D = nrow(private$.search.grid)))
      } else if (private$.epoch <= length(private$.beta)) {
        sqrt(private$.beta[private$.epoch])
      } else {
        sqrt(private$.beta[length(private$.beta)])
      }
      cat(sprintf("***** sqrt.beta.i: %s\n", sqrt.beta.i))
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

      if (private$.implicit.threshold.method) {
        f.pes <- min(current.u[private$.M])
        f.opt <- min(current.l[private$.M])
      } else {
        f.pes <- min(known.y)
        f.opt <- min(known.y)
      }

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
        max(interval.width[private$.M], 0) <=
          eta.i * (1 - private$.delta.bar) * m.interval.factor
      ) {
        # advance epoch
        private$.epoch <- private$.epoch + 1
        private$.last.epoch.switch <- length(known.y)
        sqrt.beta.i <- if (is.function(private$.beta)) {
          sqrt(private$.beta(i = private$.epoch, t = private$.last.epoch.switch, D = nrow(private$.search.grid)))
        } else if (private$.epoch <= length(private$.beta)) {
          sqrt(private$.beta[private$.epoch])
        } else {
          sqrt(private$.beta[length(private$.beta)])
        }
        eta.i <- private$.eta * private$.r^(private$.epoch - 1)

        # update confidence intervals
        interval.width <- sqrt.beta.i * sd.pred.full
      }

      unclassified.unknowns <- which(pred.index.to.fullindex %in% c(private$.M, private$.U))
      if (!length(unclassified.unknowns) || !length(private$.U)) {
        return(1L)
      }

      if (private$.implicit.threshold.method) {
        task.m <- as_task_regr(private$.search.grid[private$.M, -".id", with = FALSE], target = ".score", id = "M")
      }
      task.u <- as_task_regr(private$.search.grid[private$.U, -".id", with = FALSE], target = ".score", id = "U")

      newpoints <- as_task_regr(grid.unknown[unclassified.unknowns, -".id", with = FALSE], target = ".score", id = "unknown")
      scaled.excess.variance.improvement <- (
        (if (private$.implicit.threshold.method) {
          # excess variance improvement for maximum
          learner$totalScaledExcessVarianceReduction(newpoints, task.m,
            (sqrt.beta.i / m.interval.factor)^2, eta.i, private$.chunk.size)
        } else {
          0
        }) +
        # excess variance improvement for threshold
        learner$totalScaledExcessVarianceReduction(newpoints, task.u,
          sqrt.beta.i^2, eta.i, private$.chunk.size)
      )

      unclassified.unknowns[which.max(scaled.excess.variance.improvement)]
    }
  )
)
