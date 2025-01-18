#' @title Objective Function based on a Learner
#'
#' @description
#' This objective function is evaluated by resampling a given learner on a given task and evaluating the given measure.
#'
#' @details
#' This is a concrete class, currently not meant to be subclassed.
#' @export
ObjectiveStreamLearner <- R6Class("ObjectiveStreamLearner",
  inherit = ObjectiveStreamActual,
  public = list(
    #' @description
    #' Initialize the objective function.
    #' @param learner (`Learner`) The learner to evaluate.
    #' @param task (`Task`) The task to evaluate on.
    #' @param resampling (`Resampling`) The resampling to use.
    #' @param measure (`Measure`) The measure to evaluate the resampling result with.
    #' @param id (`character(1)`) The id of the objective function, used to identify the objective when printing.
    #'   By default, this is constructed from the learner, task, resampling, and measure IDs.
    #' @param domain (`ParamSet`) The domain of the objective function.
    #'   By default, this is the search space of the learner, defined by "`to_tune()`" tokens in the learner's
    #'   `ParamSet`.
    #' @param seed (`integer(2)`) Seed used both to initialize the sample stream (first element) and the evaluation
    #'   function (second element).
    initialize = function(learner, task, resampling, measure,
        id = sprintf("lrn_%s_task_%s_rsmp_%s_msr_%s", learner$id, task$id, resampling$id, measure$id),
        domain = learner$param_set$search_space(), seed = NULL) {
      private$.learner <- learner$clone(deep = TRUE)
      private$.learner$predict_type <- measure$predict_type
      private$.task <- task$clone(deep = TRUE)
      private$.resampling <- resampling$clone(deep = TRUE)
      private$.measure <- measure$clone(deep = TRUE)
      super$initialize(id, domain, minimize = measure$minimize, seed = seed)
    }
  ),
  active = list(
    #' @field learner (`Learner`) The learner to evaluate.
    learner = function() private$.learner,
    #' @field task (`Task`) The task to evaluate on.
    task = function() private$.task,
    #' @field resampling (`Resampling`) The resampling to use.
    resampling = function() private$.resampling,
    #' @field measure (`Measure`) The measure to evaluate the resampling result with.
    measure = function() private$.measure
  ),
  private = list(
    .learner = NULL,
    .task = NULL,
    .resampling = NULL,
    .measure = NULL,
    .eval = function(x) {
      x$.id <- NULL  # remove ID column, but do not change x in-place
      oldparams <- private$.learner$param_set$values
      oldstate <- private$.learner$state
      on.exit({
        private$.learner$param_set$values <- oldparams
        private$.learner$state <- oldstate
      })
      measure <- private$.measure
      learner <- private$.learner
      hpcs <- lapply(seq_len(nrow(x)), function(i) {
        self$domain$trafo(as.list(x[i, ]))
      })
      to.overwrite <- intersect(names(hpcs[[1]]), learner$param_set$ids())
      learner$param_set$values[to.overwrite] <- NULL
      bmg <- benchmark_grid(private$.task, learner, private$.resampling, param_values = list(hpcs))
      bmr <- benchmark(bmg,
        store_models = "requires_model" %in% measure$properties,
        store_backends = "requires_task" %in% measure$properties
      )
      bmr$aggregate(measure)[order(nr), measure$id, with = FALSE][[1]]
    }
  )
)
