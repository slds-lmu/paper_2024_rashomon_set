
ObjectiveStreamSynthetic <- R6Class("ObjectiveStreamSynthetic",
  inherit = ObjectiveStreamActual,
  public = list(
    initialize = function(objective, id, domain, minimize, seed) {
      private$.objective <- assertFunction(objective)
      super$initialize(id, domain, minimize = minimize, seed = seed)
    }
  ),
  active = list(
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
