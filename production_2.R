
rm(list = ls(all.names = TRUE))
source("init.R")

reg.glmnet <- getRegistry("../registry_paper_2024_rashomon_set_glmnet/registry", make.default = FALSE)
reg.tree <- getRegistry("../registry_paper_2024_rashomon_set_tree/registry", make.default = FALSE)
reg.nnet <- getRegistry("../registry_paper_2024_rashomon_set_nnet/registry", make.default = FALSE)
reg.xgb <- getRegistry("../registry_paper_2024_rashomon_set_xgb/registry", make.default = FALSE)
reg.gosdt <- getRegistry("../registry_paper_2024_rashomon_set_gosdt/registry", make.default = FALSE)
reg.svm <- getRegistry("../registry_paper_2024_rashomon_set_svm/registry", make.default = FALSE)

addProblemTaskGetter(reg.glmnet)
addProblemTaskGetter(reg.tree)
addProblemTaskGetter(reg.nnet)
addProblemTaskGetter(reg.xgb)
addProblemTaskGetter(reg.gosdt)
addProblemTaskGetter(reg.svm)

addAlgorithmPerfEvaluation(reg.glmnet, "glmnet")
addAlgorithmPerfEvaluation(reg.tree, "tree")
addAlgorithmPerfEvaluation(reg.nnet, "nnet")
addAlgorithmPerfEvaluation(reg.xgb, "xgb")
addAlgorithmPerfEvaluation(reg.gosdt, "gosdt")
addAlgorithmPerfEvaluation(reg.svm, "svm")


newtasks <- setdiff(names(list.tasks), names(list.tasks.binarized))

# glmnet: 1e4, plus 1e2^2 grid --> 90 cpuh
## 1e3 points per chunk


## addExperimentsPerfEvaluation(reg.glmnet, "glmnet", 1e4, grid = FALSE, tasks = newtasks)

tosubmit.glmnet <- (findNotDone(reg = reg.glmnet) |> findExperiments(reg = reg.glmnet))[,
    chunk := chunk(job.id, chunk.size = 1e3)]
submitJobs(tosubmit.glmnet, resources = list(walltime = 3600, memory = 4000, ncpus = 1), reg = reg.glmnet)

# addExperimentsPerfEvaluation(reg.glmnet, "glmnet", 1e2, grid = TRUE)

# tree: 1e4, plus 1e2^2 grid --> 40 cpuh
## 1e3 points per chunk

## addExperimentsPerfEvaluation(reg.tree, "tree", 8e3, grid = FALSE, tasks = newtasks)


# addExperimentsPerfEvaluation(reg.tree, "tree", 20, grid = TRUE)
tosubmit.tree <- (findNotDone(reg = reg.tree) |> findExperiments(reg = reg.tree))[,
    chunk := chunk(job.id, chunk.size = 1e3)]
submitJobs(tosubmit.tree, resources = list(walltime = 3600, memory = 4000, ncpus = 1), reg = reg.tree)


# nnet: 2e4, plus 2 * 1e2^2 grid --> 2000 cpuh
## size > 100: 1 point per chunk
## size > 10: 50 points per chunk
## size < 10: 500 points per chunk

## addExperimentsPerfEvaluation(reg.nnet, "nnet", 2e4, grid = FALSE, tasks = newtasks)

# addExperimentsPerfEvaluation(reg.nnet, "nnet", 1e2, grid = TRUE)
tosubmit <- list(
#   findExperiments(algo.name = "nnet", algo.pars = exp(nnet.size) > 200, reg = reg.nnet),  # no chunking
   (findNotDone(reg = reg.nnet) |> findExperiments(algo.name = "nnet", algo.pars = exp(nnet.size) <= 200 & exp(nnet.size) > 50, reg = reg.nnet))[,
     chunk := chunk(job.id, chunk.size = 1e2)],
   (findNotDone(reg = reg.nnet) |> findExperiments(algo.name = "nnet", algo.pars = exp(nnet.size) <= 50, reg = reg.nnet))[,
     chunk := chunk(job.id, chunk.size = 1e3)]
)
lapply(tosubmit, submitJobs, resources = list(walltime = 3600, memory = 4000, ncpus = 1), reg = reg.nnet)


# xgb: 5e5 points for 1e4 cpuh (x10 reps)
## 5e1 points per chunk

## addExperimentsPerfEvaluation(reg.xgb, "xgb", 5e5, grid = FALSE, tasks = newtasks)

tosubmit.xgb <- (findNotDone(reg = reg.xgb) |> findExperiments(reg = reg.xgb))[,
    chunk := chunk(job.id, chunk.size = 1e3)]
submitJobs(tosubmit.xgb, resources = list(walltime = 3600, memory = 4000, ncpus = 1), reg = reg.xgb)








# svm: takes about .5h per eval.
# grid size 50 -> 12550 configs * 4 tasks * 10 repls -> 251k cpuh
# addExperimentsPerfEvaluation(reg.svm, "svm", 50, grid = TRUE)



addExperimentsPerfEvaluation(reg.svm, "svm", 1e4, grid = FALSE)
# 1e3 points per chunk

tosubmit <- findExperiments(reg = reg.svm, algo.pars = svm.kernel != "polynomial")[, .(job.id, chunk = chunk(job.id, chunk.size = 500))]

submitJobs(tosubmit, resources = list(walltime = 2 * 3600, memory = 4000, ncpus = 1), reg = reg.svm)
