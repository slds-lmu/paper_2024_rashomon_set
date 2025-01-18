
# does not actually do rashomon set inference, just optimized
RashomonSamplerOptimize <- R6Class("RashomonSamplerOptimize",
  inherit = RashomonSampler,
  public = list(
    initialize = function(id, domain, minimize, learner, search.grid.size, seed) {
      private$.search.grid.size <- assertCount(search.grid.size, positive = TRUE, tol = 0, coerce = TRUE)
      super$initialize(id, domain, minimize, rashomon.epsilon = 0, rashomon.is.relative = FALSE,
        n.rashomon.samples = Inf, seed = seed)
      private$.learner <- learner$clone(deep = TRUE)
    }
  ),
  active = list(
    search.grid.size = function() private$.search.grid.size,
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
      private$.search.grid <- private$.search.grid
    },
    .askYValues = function() {
      private$.search.grid
    },
    .tellYValues = function(y) {
      private$.search.grid <- private$.search.grid
    }
  )
)
