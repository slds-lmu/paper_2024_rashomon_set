
rm(list = ls(all.names = TRUE))
source("init.R")



reg.treefarms <- getRegistry("../registry_paper_2024_rashomon_set_treefarms/registry3", make.default = TRUE)


treefitter <- function(data, instance, job, offset, balance, use.adder = TRUE,...) {
  learner.tf <- LearnerClassifTreeFarms$new()
  learner.tf$predict_type <- "prob"
  learner.tf$param_set$values$regularization <- 0.01
  learner.tf$param_set$values$balance <- balance
  if (use.adder) {
    learner.tf$param_set$values$rashomon_bound_adder <- offset
  } else {
    learner.tf$param_set$values$rashomon_bound_multiplier <- offset
  }

  learner.tf$train(instance)
}


addAlgorithm("treefitter", fun = treefitter, reg = reg.treefarms)

addProblemTaskGetter(reg.treefarms)

addExperiments(
  prob.designs = list(task_getter_problem = data.table(taskname = names(list.tasks.binarized))),
  algo.designs = list(treefitter = CJ(offset = c(0.05, 0.1, 0.15), balance = c(TRUE, FALSE))),
  repls = 1,
  reg = reg.treefarms
)

addExperiments(
  prob.designs = list(task_getter_problem = data.table(taskname = names(list.tasks.binarized))),
  algo.designs = list(treefitter = CJ(offset = c(0.05, 0.1, 0.15), balance = c(TRUE, FALSE), use.adder = FALSE)),
  repls = 1,
  reg = reg.treefarms
)

submitJobs(resources = list(ncpus = 1, memory = 204800, walltime = 3600 * 3), reg = reg.treefarms)
