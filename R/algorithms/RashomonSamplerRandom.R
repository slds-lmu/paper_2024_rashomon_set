#' @title Naive Rashomon Set Inference
#'
#' @description
#' Decides whether a sample belongs to the Rashomon set by naively evaluating all samples.
#'
#' @details
#' This sampler requests an initial batch of samples from the parameter space, then continues requesting smaller batches
#' until the desired number of Rashomon set members is found.
#'
#' @export
RashomonSamplerRandom <- R6Class("RashomonSamplerRandom",
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
    #' @param seed (`integer(1)`) Random seed for reproducibility
    #' @param n.rashomon.samples (`integer(1)` | `Inf`) Target number of Rashomon set samples.
    #' @param initial.sample.size (`integer(1)`) Number of samples to request in the initial batch
    #' @param batchsize (`integer(1)`) Number of samples to request in each subsequent batch
    #'   RashomonSampler implementations may optimize their behaviour to hit this target specifically.
    initialize = function(id, domain, minimize, rashomon.epsilon, rashomon.is.relative, seed,
        n.rashomon.samples, initial.sample.size = 100L, batchsize = 1L) {
      private$.initial.sample.size <- assertCount(initial.sample.size, positive = TRUE, tol = 0, coerce = TRUE)
      private$.batchsize <- assertCount(batchsize, positive = TRUE, tol = 0, coerce = TRUE)
      super$initialize(id, domain, minimize, rashomon.epsilon, rashomon.is.relative, seed, n.rashomon.samples)
      private$.archive <- getNullTable(domain, include.id = TRUE, include.score = TRUE)
    }
  ),
  active = list(
    #' @field initial.sample.size (`integer(1)`) Number of samples to request in the initial batch
    initial.sample.size = function() private$.initial.sample.size,
    #' @field batchsize (`integer(1)`) Number of samples to request in each subsequent batch
    batchsize = function() private$.batchsize
  ),
  private = list(
    .initial.sample.size = NULL,
    .xcoord = NULL,
    .archive = NULL,
    .batchsize = NULL,
    .askXSamples = function() {
      if (!is.null(private$.xcoord)) return(0)
      if (nrow(private$.archive)) private$.batchsize else private$.initial.sample.size
    },
    .tellXSamples = function(x) {
      if (anyNA(x$.score)) {
        private$.xcoord <- x
      } else {
        private$.archive <- rbind(private$.archive, x)
      }
    },
    .askYValues = function() {
      private$.xcoord[is.na(.score)]
    },
    .tellYValues = function(y) {
      set(private$.xcoord, which(is.na(private$.xcoord$.score)), ".score", y$.score)
      private$.archive <- rbind(private$.archive, private$.xcoord, use.names = TRUE)
      private$.xcoord <- NULL
    },
    .getRashomonSamples = function() {
      indices <- private$.getRashomonIndices(private$.archive$.score)
      private$.archive[indices, ]
    }
  )
)
