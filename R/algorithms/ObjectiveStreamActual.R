
ObjectiveStreamActual <- R6Class("ObjectiveStreamActual",
  inherit = ObjectiveStream,
  public = list(
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
    learner = function() private$.learner,
    task = function() private$.task,
    resampling = function() private$.resampling,
    measure = function() private$.measure
  ),
  private = list(
    .learner = NULL,
    .task = NULL,
    .resampling = NULL,
    .measure = NULL,
    .eval = function(x) {
      x$.id <- NULL  # remove ID column, but do not change x in place
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
    },
    .sample = function(n) {
      ss <- self$domain
      inmat <- matrix(runif(n * ss$length), ncol = ss$length, byrow = TRUE)
      colnames(inmat) <- ss$ids()
      ss$qunif(inmat)
    }
  )
)
