#' @title Base Class for Objective Function with Uniform Samples
#'
#' @description
#' This is a base class for objective functions that sample uniformly from the domain.
#'
#' @details
#' This base class implements the `$sample()` method.
#'
#' Concrete classes must implement the `$eval()` method.
#' @export
ObjectiveStreamActual <- R6Class("ObjectiveStreamActual",
  inherit = ObjectiveStream,
  public = list(
    #' @description
    #' Initialize the objective function.
    #' @param id (`character(1)`) The id of the objective function, used to identify the objective when printing.
    #' @param domain (`ParamSet`) The domain of the objective function.
    #' @param minimize (`logical(1)`) Whether the objective function should be minimized.
    #' @param seed (`integer(2)`) Seed used both to initialize the sample stream (first element) and the evaluation
    #'   function (second element).
    initialize = function(id, domain, minimize, seed) {
      super$initialize(id, domain, minimize = minimize, seed = seed)
    }
  ),
  private = list(
    .sample = function(n) {
      ss <- self$domain
      inmat <- matrix(runif(n * ss$length), ncol = ss$length, byrow = TRUE)
      colnames(inmat) <- ss$ids()
      ss$qunif(inmat)
    }
  )
)
