#' @title Base Class for RashomonSamplers that use a Surrogate Model
#'
#' @description
#' This class provides a base class for RashomonSamplers that use a Surrogate Model.
#'
#' @details
#' Subclasses must implement the `.askYValuesWithLearner` method.
#' Its parameters are:
#' - `mean.pred`: The mean predictions of the learner.
#' - `sd.pred`: The standard deviation predictions of the learner.
#' - `known.y`: The known scores of the points in the search grid.
#'
#' It should return the index of the point with the minimum acquisition value.
#'
#' @export
RashomonSamplerLearnerBased <- R6Class("RashomonSamplerLearnerBased",
  inherit = RashomonSampler,
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

    initialize = function(id, domain, minimize, rashomon.epsilon, rashomon.is.relative, learner, search.grid.size, seed,
        n.rashomon.samples) {
      private$.search.grid.size <- assertCount(search.grid.size, positive = TRUE, tol = 0, coerce = TRUE)
      super$initialize(id, domain, minimize, rashomon.epsilon, rashomon.is.relative,
        seed, n.rashomon.samples)
      private$.learner <- assert_r6(learner, "Learner")$clone(deep = TRUE)
      if ("se" %in% private$.learner$predict_types) {
        private$.learner$predict_type <- "se"
      }
    }
  ),
  active = list(
    #' @field search.grid.size (`integer(1)`) Number of samples to request in the initial batch
    search.grid.size = function() private$.search.grid.size,
    #' @field learner (`Learner`) Learner to optimize
    learner = function(rhs) {
      if (!missing(rhs) && !identical(private$.learner, rhs)) {
        stop("learner is read-only")
      }
      private$.learner
    }
  ),
  private = list(
    .search.grid.size = NULL,
    .search.grid = NULL,
    .learner = NULL,
    .askXSamples = function() {
      if (is.null(private$.search.grid)) {
        private$.search.grid.size
      } else {
        0
      }
    },
    .tellXSamples = function(x) {
      private$.search.grid <- copy(x)
    },
    .askYValues = function() {
      # Split into known and unknown points
      grid.known <- private$.search.grid[!is.na(.score)]
      grid.unknown <- private$.search.grid[is.na(.score)]

      # If no known points yet, pick first point randomly
      if (!nrow(grid.known)) {
        warning("No known points yet, picking first point randomly")
        row <- sample.int(nrow(grid.unknown), 1L)
        return(grid.unknown[row])
      }

      if (!nrow(grid.unknown)) {
        stop("No unknown points left to evaluate")
      }

      # Create regression tasks
      task.known <- as_task_regr(grid.known[, -".id", with = FALSE], target = ".score", id = "known")
      task.unknown <- as_task_regr(grid.unknown[, -".id", with = FALSE], target = ".score", id = "unknown")

      # Train learner and make predictions
      learner <- private$.learner$clone(deep = TRUE)
      learner$train(task.known)

      pred.unknown <- learner$predict(task.unknown)

      # Get mean and sd predictions
      mean.pred <- assertNumeric(pred.unknown$response, len = nrow(grid.unknown), any.missing = FALSE, finite = TRUE,
        .var.name = "mean.pred made by learner")
      sd.pred <- assertNumeric(pred.unknown$se, len = nrow(grid.unknown), finite = TRUE,
        .var.name = "sd.pred made by learner")

      pred.known <- learner$predict(task.known)
      known.y.predicted <- assertNumeric(pred.known$response,
        len = nrow(grid.known), any.missing = FALSE, finite = TRUE,
        .var.name = "known.y.predicted made by learner")
      known.y.predicted.sd <- assertNumeric(pred.known$se,
        len = nrow(grid.known), finite = TRUE,
        .var.name = "known.y.predicted.sd made by learner")

      # If minimizing, keep as is. If maximizing, negate means
      multiplier <- 1
      if (!self$minimize) {
        mean.pred <- -mean.pred
        grid.known$.score <- -grid.known$.score
        known.y.predicted <- -known.y.predicted
        multiplier <- -1
      }

      row <- private$.askYValuesWithLearner(mean.pred, sd.pred, grid.known$.score * multiplier,
        known.y.predicted, known.y.predicted.sd, grid.known, grid.unknown, learner)
      assertInt(row, lower = 1, upper = nrow(grid.unknown), .var.name = "row returned by .askYValuesWithLearner")
      grid.unknown[row]
    },
    .tellYValues = function(y) {
      row <- match(y$.id, private$.search.grid$.id)
      set(private$.search.grid, i = row, j = ".score", value = y$.score)
    },
    .getRashomonSamples = function() {
      non.nas <- which(!is.na(private$.search.grid$.score))
      indices <- private$.getRashomonIndices(private$.search.grid[non.nas, .score])
      private$.search.grid[non.nas[indices], ]
    },
    .askYValuesWithLearner = function(mean.pred, sd.pred, known.y, grid.unknown) {
      stop("Not implemented")
    }
  )
)
