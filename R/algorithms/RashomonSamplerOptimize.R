#' @title Optimization Sampler
#'
#' @description
#' This sampler does not actually do Rashomon set inference, it just optimizes the given learner on the given task.
#'
#' It asks for X-samples exactly once and shoud therefore be provided with a large enough sample to optimize over.
#' It will then ask Y values in batches of 1, trying to optimize over the provided grid.
#'
#' @export
RashomonSamplerOptimize <- R6Class("RashomonSamplerOptimize",
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
    #' @param aqf (`function`) Acquisition function to optimize.
    #'   Takes vectors of same length `mean` and `sd` and returns a vector of scores.
    #'   Besides that, argument `known.y` and `known.y.predicted` are passed to the acquisition function.
    #'   This vector contains information about the values of the objective function that were already evaluated.
    #'   The acquisition function should always minimize:
    #'   It should assume that a lower mean is better, and should therefore tend to return lower values for lower means.
    #' @param search.grid.size (`integer(1)`) Number of samples to request in the initial batch
    #' @param seed (`integer(1)`) Random seed for reproducibility
    initialize = function(id, domain, minimize, rashomon.epsilon, rashomon.is.relative, learner, aqf, search.grid.size,
        seed) {
      super$initialize(id, domain, minimize, rashomon.epsilon, rashomon.is.relative, learner, search.grid.size,
        seed, n.rashomon.samples = Inf)
      private$.aqf <- assert_function(aqf)
    }
  ),
  active = list(
    #' @field aqf (`function`) Acquisition function to optimize.
    aqf = function() private$.aqf
  ),
  private = list(
    .aqf = NULL,
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
    .askYValuesWithLearner = function(mean.pred, sd.pred, known.y, known.y.predicted, ...) {
      # Calculate acquisition function values
      acq.values <- private$.aqf(
        mean = mean.pred,
        sd = sd.pred,
        known.y = known.y,
        known.y.predicted = known.y.predicted
      )

      assertNumeric(acq.values, len = length(mean.pred), any.missing = FALSE, finite = TRUE,
        .var.name = "acquisition function result")

      # Return the point with minimum acquisition value
      which.min(acq.values)
    }
  )
)

makeAqf <- function(fun, description, ...) {
  structure(fun,
    repr = sprintf(description, ...),
    class = c("Aqf", "function")
  )
}

# Representation of character vector
# letters[1]   --> '"a"'
# letters[1:2] --> 'c("a", "b")'
charRepr <- function(x) {
  if (length(x) == 0) {
    return("character(0)")
  }
  output <- paste0('"', x, '"', collapse = ", ", recycle0 = TRUE)
  if (length(x) == 1) {
    output
  } else {
    sprintf("c(%s)", output)
  }
}

# Representation for a function that may or may not be an `Aqf`.
# If it is not, we just use deparse(), otherwise we use the repr as
# reported by that Aqf.
aqfRepr <- function(aqf) {
  if (testString(attr(aqf, "repr"))) {
    attr(aqf, "repr")
  } else {
    paste0(deparse(aqf), collapse = "\n")
  }
}

#' @export
print.Aqf <- function(x, ...) {
  if (inherits(x, "R6")) return(NextMethod("print"))
  cat(aqfRepr(x), "\n")
  invisible(x)
}



#' @title Mean Acquisition Function
#'
#' @description
#' Acquisition function that returns the mean.
#'
#' @return Mean Acquisition Function
#'
#' @family Acquisition Functions
#' @export
AqfMean <- function() makeAqf(function(mean, sd, known.y, known.y.predicted) mean, "AqfMean()")

#' @title Standard Deviation Acquisition Function
#'
#' @description
#' Acquisition function that returns the negative standard deviation.
#'
#' It results in an optimizer that evaluates points with greatest uncertainty.
#'
#' @return Negative Standard Deviation Acquisition Function
#'
#' @family Acquisition Functions
#' @export
AqfSd <- function() makeAqf(function(mean, sd, known.y, known.y.predicted) -sd, "AqfSd()")

#' @title Lower Confidence Bound Acquisition Function
#'
#' @description
#' Acquisition function that returns the mean minus lambda times the standard deviation.
#'
#' @param lambda (`numeric(1)`) Lambda parameter
#' @return Lower Confidence Bound Acquisition Function with given lambda value.
#'
#' @family Acquisition Functions
#' @export
AqfLcb <- function(lambda) {
  assertNumber(lambda, finite = TRUE)
  makeAqf(function(mean, sd, known.y, known.y.predicted) mean - lambda * sd, sprintf("AqfLcb(%s)", lambda))
}

#' @title Expected Improvement Acquisition Function
#'
#' @description
#' Acquisition function that calculates the expected improvement over the current best observed value.
#'
#' Expected improvement measures the expected value of the improvement over the current best,
#' taking into account both the predicted mean and uncertainty at each point.
#'
#' @return Expected Improvement Acquisition Function
#'
#' @family Acquisition Functions
#' @export
AqfEi <- function() {
  makeAqf(function(mean, sd, known.y, known.y.predicted) {
    # Get current best value (minimum since we're always minimizing)
    best.f <- min(known.y)

    # Standardize the improvement (always minimizing)
    d <- best.f - mean
    z <- d / sd

    # Calculate EI using the analytical formula
    ei <- d * pnorm(z) + sd * dnorm(z)

    # Return negative EI since we want to minimize
    -ei
  }, "AqfEi()")
}
