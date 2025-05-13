
rm(list = ls(all.names = TRUE))
source("init.R")

oldtasks <- setdiff(names(list.tasks), c("fc", names(list.tasks.binarized)))
newtasks <- names(list.tasks.binarized)

# ------------------------------------------------------------------------------

reg <- getRegistry("../registry_paper_2024_rashomon_set", make.default = TRUE)
addProblemTaskGetter(reg)

# glmnet: 1e4, plus 1e2^2 grid
## 1e3 points per chunk
addExperimentsPerfEvaluation(reg, "glmnet", 1e4, grid = FALSE, tasks = oldtasks)
addExperimentsPerfEvaluation(reg, "glmnet", 1e2, grid = TRUE, tasks = oldtasks)

# tree: 1e4, plus 1e2^2 grid
## 1e3 points per chunk
addExperimentsPerfEvaluation(reg, "tree", 8e3, grid = FALSE, tasks = oldtasks)
addExperimentsPerfEvaluation(reg, "tree", 20, grid = TRUE, tasks = oldtasks)

# nnet: 2e4, plus 2 * 1e2^2 grid
## size > 100: 1 point per chunk
## size > 10: 50 points per chunk
## size < 10: 500 points per chunk
addExperimentsPerfEvaluation(reg, "nnet", 2e4, grid = FALSE, tasks = oldtasks)
addExperimentsPerfEvaluation(reg, "nnet", 1e2, grid = TRUE, tasks = oldtasks)

# xgb: 5e5 points for 1e4 cpuh (x10 reps)
## 5e1 points per chunk
addExperimentsPerfEvaluation(reg, "xgb", 5e5, grid = FALSE, tasks = oldtasks)

# ------------------------------------------------------------------------------

reg.svm.primary <- getRegistry("../registry_paper_2024_rashomon_set_svm", make.default = TRUE)
addProblemTaskGetter(reg.svm.primary)
addAlgorithmPerfEvaluation(reg.svm.primary, "svm")
# svm: takes about .5h per eval.
# grid size 50 -> 12550 configs * 4 tasks * 10 repls -> 251k cpuh
addExperimentsPerfEvaluation(reg.svm.primary, "svm", 50, grid = TRUE)
addExperimentsPerfEvaluation(reg.svm.primary, "svm", 12550, grid = FALSE)

# ------------------------------------------------------------------------------

reg.glmnet <- getRegistry("../registry_paper_2024_rashomon_set_glmnet/registry3", make.default = TRUE)
addProblemTaskGetter(reg.glmnet)
addAlgorithmPerfEvaluation(reg.glmnet, "glmnet")
addExperimentsPerfEvaluation(reg.glmnet, "glmnet", 1e4, grid = FALSE, tasks = newtasks)

# ------------------------------------------------------------------------------

reg.tree <- getRegistry("../registry_paper_2024_rashomon_set_tree/registry3", make.default = TRUE)
addProblemTaskGetter(reg.tree)
addAlgorithmPerfEvaluation(reg.tree, "tree")
addExperimentsPerfEvaluation(reg.tree, "tree", 1e4, grid = FALSE, tasks = newtasks)

# ------------------------------------------------------------------------------

reg.nnet <- getRegistry("../registry_paper_2024_rashomon_set_nnet/registry4", make.default = TRUE)
addProblemTaskGetter(reg.nnet)
addAlgorithmPerfEvaluation(reg.nnet, "nnet")
addExperimentsPerfEvaluation(reg.nnet, "nnet", 2e4, grid = FALSE, tasks = newtasks)

# ------------------------------------------------------------------------------

reg.xgb <- getRegistry("../registry_paper_2024_rashomon_set_xgb/registry4", make.default = TRUE)
addProblemTaskGetter(reg.xgb)
addAlgorithmPerfEvaluation(reg.xgb, "xgb")
addExperimentsPerfEvaluation(reg.xgb, "xgb", 1e5, grid = FALSE, tasks = newtasks)

# ------------------------------------------------------------------------------

reg.gosdt <- getRegistry("../registry_paper_2024_rashomon_set_gosdt/registry5", make.default = TRUE)
addProblemTaskGetter(reg.gosdt)
addAlgorithmPerfEvaluation(reg.gosdt, "gosdt")
addExperimentsPerfEvaluation(reg.gosdt, "gosdt", 1e4, grid = FALSE, tasks = newtasks)

# ------------------------------------------------------------------------------

reg.svm <- getRegistry("../registry_paper_2024_rashomon_set_svm/registry4", make.default = TRUE)
addProblemTaskGetter(reg.svm)
addAlgorithmPerfEvaluation(reg.svm, "svm")
addExperimentsPerfEvaluation(reg.svm, "svm", 1.5e4, grid = FALSE, tasks = newtasks)
