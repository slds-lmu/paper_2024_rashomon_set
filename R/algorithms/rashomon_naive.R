

tuneRashomonNaive <- function(
    task,
    learner,
    epsilon,
    iters = 100,
    measure = NULL,
    resampling = rsmp("cv"),
    search.space = NULL,
    epsilon.is.absolute = TRUE) {
  tr <- tune(tuner = tnr("random_search", batch_size = min(iters, 1000)),
    task = task, learner = learner, resampling = resampling,
    measures = if (!is.null(measure)) list(measure), term_evals = iters,
    search_space = search.space, store_models = TRUE)

  cod <- tr$archive$codomain

  targetname <- cod$ids()
  minimize <- "minimize" %in% cod$tags[[targetname]]

  obj.vals <- tr$archive$data[[targetname]]

  if (minimize) {
    keep <- obj.vals < min(obj.vals) + epsilon
  } else {
    keep <- obj.vals > max(obj.vals) - epsilon
  }

  tr$archive$benchmark_result$learners[keep, .(obj.vals, learner)]
}



