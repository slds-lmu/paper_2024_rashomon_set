
task.getter.problem.fun <- function(data, job, taskname) {
  task <- list.tasks[[taskname]]
  generateCanonicalDataSplits(task, ratio = 2 / 3, seed = 1)$training
}


perf.evaluation.fun <- function(data, instance, job, ...) {
  resampling <- resampling.inner$clone(deep = TRUE)$instantiate(instance)
  if ("TaskClassif" %in% class(instance)) {  # nolint
    learnerlist <- list.learners.classif
    measure <- measure.classif
  } else {
    learnerlist <- list.learners.regr
    measure <- measure.regr
  }
  learner <- learnerlist[[job$algo.name]]$clone(deep = TRUE)

  hpvals <- job$algo.pars
  design <- Design$new(param_set = learner$param_set$search_space(),
    data = as.data.table(hpvals),
    remove_dupl = FALSE
  )
  hp.transposed <- design$transpose()[[1]]
  learner$param_set$set_values(.values = hp.transposed)
  rr <- resample(instance, learner, resampling, store_backends = FALSE)
  list(
    result = rr$aggregate(measure),
    config = hp.transposed
  )
}

addProblemTaskGetter <- function(reg) {
  addProblem("task_getter_problem", fun = task.getter.problem.fun, reg = reg)
}

addAlgorithmPerfEvaluation <- function(reg, learnername) {
  addAlgorithm(learnername, fun = perf.evaluation.fun, reg = reg)
}

# resolution is per-HP if grid = TRUE, otherwise total number of random samples
addExperimentsPerfEvaluation <- function(
    reg,
    learner,
    resolution,
    grid = FALSE,
    tasks = names(list.tasks),
    repls = resampling.reps.inner,
    seed = 1
) {
  assertChoice(learner, names(list.learners.regr))
  assertSubset(tasks, names(list.tasks))

  learner.object <- list.learners.regr[[learner]]
  ss <- learner.object$param_set$search_space()

  if (grid) {
    design <- generate_design_grid(ss, resolution)
  } else {
    set.seed(seed)
    design <- generate_design_random(ss, resolution)
  }

  algo.designs <- list(design$data)
  names(algo.designs) <- learner

  addExperiments(
    prob.designs = list(task_getter_problem = data.table(taskname = tasks)),
    algo.designs = algo.designs,
    repls = repls
  )
}
