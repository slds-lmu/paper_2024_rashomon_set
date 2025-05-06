
rm(list = ls(all.names = TRUE))
source("init.R")



reg.treefarms <- getRegistry("../registry_paper_2024_rashomon_set_treefarms/registry", make.default = TRUE)


treefitter <- function(data, instance, job, offset, ...) {
  learner.tf <- LearnerClassifTreeFarms$new()
  learner.tf$predict_type = "prob"
  learner.tf$param_set$values$regularization <- 0.01
  learner.tf$param_set$values$balance <- TRUE
  learner.tf$param_set$values$rashomon_bound_adder <- offset

  learner.tf$train(instance)
}


addAlgorithm("treefitter", fun = treefitter, reg = reg.treefarms)

addProblemTaskGetter(reg.treefarms)

addExperiments(
  prob.designs = list(task_getter_problem = data.table(taskname = names(list.tasks.binarized))),
  algo.designs = list(treefitter = data.table(offset = c(0.05, 0.1, 0.15))),
  repls = 1,
  reg = reg.treefarms
)

submitJobs(findExperiments(prob.pars = taskname == "mk", reg = reg.treefarms),
  resources = list(ncpus = 1, memory = 102400, walltime = 3600 * 3), reg = reg.treefarms)

submitJobs(findExperiments(prob.pars = taskname == "fc.bin", reg = reg.treefarms),
  resources = list(ncpus = 1, memory = 204800, walltime = 3600 * 3), reg = reg.treefarms)

submitJobs(resources = list(ncpus = 1, memory = 50000, walltime = 3600 * 3), reg = reg.treefarms)


# # most take around 16 gb; fico needs 64 gb
# # most runtime for fc.bin, then cr
# for (offset in c(0.05, 0.1, 0.15)) {
#   learner.tf$param_set$values$rashomon_bound_adder <- offset
#   for (tn in names(list.tasks.binarized)) {
#     if (tn != "fc.bin") next
#     outpath <- file.path("data", paste0("treefarms_", tn, "_", offset, ".rds"))
#
#     if (file.exists(outpath)) {
#       cat(sprintf("Skipping %s (already exists)\n", outpath))
#       next
#     }
#
#     set.seed(1)
#     cat(sprintf("Fitting TreeFarms for %s with offset %s...\n", tn, offset))
#     task <- generateCanonicalDataSplits(list.tasks.binarized[[tn]])$training
#     learner.tf.clone <- learner.tf$clone(deep = TRUE)
#     runtime <- system.time(learner.tf.clone$train(task), gcFirst = FALSE)
#     cat(sprintf("Done. Runtime: %s ; Trees: %s\n",
#       runtime["elapsed"], learner.tf.clone$modelcontainer$get_tree_count()
#     ))
#
#
#     saveRDS(learner.tf.clone, outpath)
#     cat(sprintf("Saved to %s\n", outpath))
#     gc() ; gc() ; gc() ; gc()
#   }
# }

