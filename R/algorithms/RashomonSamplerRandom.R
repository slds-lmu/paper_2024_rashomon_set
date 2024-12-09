
RashomonSamplerRandom <- R6Class("RashomonSamplerRandom",
  inherit = RashomonSampler,
  public = list(
    initialize = function(id, domain, minimize, rashomon.epsilon, rashomon.is.relative, seed,
        n.rashomon.samples, initial.sample.size = 100L, batchsize = 1L) {
      private$.initial.sample.size <- assertCount(initial.sample.size, positive = TRUE, tol = 0, coerce = TRUE)
      private$.batchsize <- assertCount(batchsize, positive = TRUE, tol = 0, coerce = TRUE)
      super$initialize(id, domain, minimize, rashomon.epsilon, rashomon.is.relative, seed, n.rashomon.samples)
      private$.archive <- getNullTable(domain, include.id = TRUE)
    }
  ),
  active = list(
    initial.sample.size = function() private$.initial.sample.size,
    batchsize = function() private$.batchsize
  ),
  private = list(
    .initial.sample.size = NULL,
    .xcoord = NULL,
    .archive = NULL,
    .scorecol = NULL,
    .batchsize = NULL,
    .askXSamples = function() {
      if (is.null(private$.archive)) private$.initial.sample.size else private$.batchsize
    },
    .tellXSamples = function(x) {
      private$.xcoord <- x
    },
    .askYValues = function() {
      private$.xcoord
    },
    .tellYValues = function(y, scorecol) {
      if (is.null(private$.scorecol)) {
        private$.scorecol <- scorecol
      }
      assertTRUE(private$.scorecol == scorecol)
      private$.archive <- rbind(private$.archive, y)
    },
    .getRashomonSamples = function() {
      if (is.null(private$.archive)) {
        return(getNullTable(private$.domain, include.id = TRUE)[, .score := NA_real_][])
      }
      indices <- private$.getRashomonIndices()
      private$.archive[indices, ]
    },
    .rashomonSamplesComplete = function() {
      sum(private$.getRashomonIndices())
    },
    .getRashomonIndices = function() {
      if (is.null(private$.archive)) {
        return(FALSE)
      }
      scores <- private$.archive$.score
      epsilon <- self$rashomon.epsilon
      if (!private$minimize) {
        scores <- -scores
        epsilon <- -epsilon
      }
      if (self$rashomon.is.relative) {
        optimum <- min(scores)
        cutoff <- optimum * (1 + epsilon)
        if (cutoff < optimum) {
          stop("rashomon.is.relative does not work for negative scores")
        }
      } else {
        cutoff <- optimum + epsilon
      }
      scores <= cutoff
    }
  )
)
