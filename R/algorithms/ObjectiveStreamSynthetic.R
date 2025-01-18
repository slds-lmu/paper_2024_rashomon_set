#' @title Synthetic Test Function Objective
#'
#' @description
#' Generates samples uniformly from the given domain (see `ObjectiveStreamActual`) and evaluates the given objective
#' function.
#' The objective function should accept a single argument: a list of parameter values sampled from the domain
#' (and possibly transformed by that domain).
#'
#' @details
#' This is a concrete class, currently not meant to be subclassed.
#' @export
ObjectiveStreamSynthetic <- R6Class("ObjectiveStreamSynthetic",
  inherit = ObjectiveStreamActual,
  public = list(
    #' @description
    #' Initialize the objective function.
    #' @param objective (`function`) The objective function to evaluate.
    #'   Should accept a single argument: a named list sampled from `domain` (possibly transformed by `domain$trafo`)
    #' @param id (`character(1)`) The id of the objective function, used to identify the objective when printing.
    #' @param domain (`ParamSet`) The domain of the objective function.
    #' @param minimize (`logical(1)`) Whether the objective function should be minimized.
    #' @param seed (`integer(2)`) Seed used both to initialize the sample stream (first element) and the evaluation
    #'   function (second element).
    initialize = function(objective, id, domain, minimize, seed) {
      private$.objective <- assertFunction(objective)
      super$initialize(id, domain, minimize = minimize, seed = seed)
    }
  ),
  active = list(
    #' @field objective (`function`) The objective function to evaluate.
    objective = function() private$.objective
  ),
  private = list(
    .objective = NULL,
    .eval = function(x) {
      vapply(seq_len(nrow(x)), function(i) {
        x.trafo <- self$domain$trafo(as.list(x[i, ]))
        self$objective(x.trafo)
      }, numeric(1))
    }
  )
)
