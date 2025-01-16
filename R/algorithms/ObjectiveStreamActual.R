
ObjectiveStreamActual <- R6Class("ObjectiveStreamActual",
  inherit = ObjectiveStream,
  public = list(
    initialize = function(id, domain, minimize, seed) {
      super$initialize(id, domain, minimize = measure$minimize, seed = seed)
    }
  )
  private = list(
    .sample = function(n) {
      ss <- self$domain
      inmat <- matrix(runif(n * ss$length), ncol = ss$length, byrow = TRUE)
      colnames(inmat) <- ss$ids()
      ss$qunif(inmat)
    }
  )
)
