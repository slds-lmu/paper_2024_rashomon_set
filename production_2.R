
rm(list = ls(all.names = TRUE))
source("init.R")

newtasks <- names(list.tasks.binarized)

reg.glmnet <- getRegistry("../registry_paper_2024_rashomon_set_glmnet/registry3", make.default = TRUE)
addProblemTaskGetter(reg.glmnet)
addAlgorithmPerfEvaluation(reg.glmnet, "glmnet")
addExperimentsPerfEvaluation(reg.glmnet, "glmnet", 1e4, grid = FALSE, tasks = newtasks)

reg.tree <- getRegistry("../registry_paper_2024_rashomon_set_tree/registry3", make.default = TRUE)
addProblemTaskGetter(reg.tree)
addAlgorithmPerfEvaluation(reg.tree, "tree")
addExperimentsPerfEvaluation(reg.tree, "tree", 1e4, grid = FALSE, tasks = newtasks)




reg.nnet <- getRegistry("../registry_paper_2024_rashomon_set_nnet/registry4", make.default = TRUE)
addProblemTaskGetter(reg.nnet)
addAlgorithmPerfEvaluation(reg.nnet, "nnet")
addExperimentsPerfEvaluation(reg.nnet, "nnet", 2e4, grid = FALSE, tasks = newtasks)

reg.xgb <- getRegistry("../registry_paper_2024_rashomon_set_xgb/registry4", make.default = TRUE)
addProblemTaskGetter(reg.xgb)
addAlgorithmPerfEvaluation(reg.xgb, "xgb")
addExperimentsPerfEvaluation(reg.xgb, "xgb", 1e5, grid = FALSE, tasks = newtasks)

reg.gosdt <- getRegistry("../registry_paper_2024_rashomon_set_gosdt/registry5", make.default = TRUE)
addProblemTaskGetter(reg.gosdt)
addAlgorithmPerfEvaluation(reg.gosdt, "gosdt")
addExperimentsPerfEvaluation(reg.gosdt, "gosdt", 1e4, grid = FALSE, tasks = newtasks)


reg.svm <- getRegistry("../registry_paper_2024_rashomon_set_svm/registry4", make.default = TRUE)
addProblemTaskGetter(reg.svm)
addAlgorithmPerfEvaluation(reg.svm, "svm")
addExperimentsPerfEvaluation(reg.svm, "svm", 1.5e4, grid = FALSE, tasks = newtasks)





# glmnet: 1e4, plus 1e2^2 grid --> 90 cpuh
## 1e3 points per chunk




tosubmit.glmnet <- (findNotDone(reg = reg.glmnet) |> findExperiments(reg = reg.glmnet))[,
    chunk := chunk(job.id, chunk.size = 1e3)]
submitJobs(tosubmit.glmnet, resources = list(walltime = 360000, memory = 4000, ncpus = 1), reg = reg.glmnet)

# addExperimentsPerfEvaluation(reg.glmnet, "glmnet", 1e2, grid = TRUE)

# tree: 1e4, plus 1e2^2 grid --> 40 cpuh
## 1e3 points per chunk




# addExperimentsPerfEvaluation(reg.tree, "tree", 20, grid = TRUE)
tosubmit.tree <- (findNotDone(reg = reg.tree) |> findExperiments(reg = reg.tree))[,
    chunk := chunk(job.id, chunk.size = 1e3)]
submitJobs(tosubmit.tree, resources = list(walltime = 360000, memory = 4000, ncpus = 1), reg = reg.tree)


# nnet: 2e4, plus 2 * 1e2^2 grid --> 2000 cpuh
## size > 100: 1 point per chunk
## size > 10: 50 points per chunk
## size < 10: 500 points per chunk

##

# addExperimentsPerfEvaluation(reg.nnet, "nnet", 1e2, grid = TRUE)
tosubmit <- list(
#   findExperiments(algo.name = "nnet", algo.pars = exp(nnet.size) > 200, reg = reg.nnet),  # no chunking
   (findNotDone(reg = reg.nnet) |> findExperiments(algo.name = "nnet", algo.pars = exp(nnet.size) <= 200 & exp(nnet.size) > 50, reg = reg.nnet))[,
     chunk := chunk(job.id, chunk.size = 1e1)],
   (findNotDone(reg = reg.nnet) |> findExperiments(algo.name = "nnet", algo.pars = exp(nnet.size) <= 50, reg = reg.nnet))[,
     chunk := chunk(job.id, chunk.size = 1e1)]
)
lapply(tosubmit, submitJobs, resources = list(walltime = 360000, memory = 4000, ncpus = 1), reg = reg.nnet)

tosubmit.nnet <- (findNotDone(reg = reg.nnet) |> findExperiments(algo.name = "nnet", algo.pars = exp(nnet.size) > 200, prob.pars = taskname != "fc.bin", reg = reg.nnet))[,
     chunk := chunk(job.id, chunk.size = 1e1)]
submitJobs(tosubmit.nnet, resources = list(walltime = 360000, memory = 4000, ncpus = 1), reg = reg.nnet)

tosubmit.nnet <- (findNotDone(rgosdeg = reg.nnet) |> findExperiments(algo.name = "nnet", algo.pars = exp(nnet.size) > 200, prob.pars = taskname == "fc.bin", reg = reg.nnet))


# xgb: 5e5 points for 1e4 cpuh (x10 reps)
## 5e1 points per chunk

## addExperimentsPerfEvaluation(reg.xgb, "xgb", 5e5, grid = FALSE, tasks = newtasks)

tosubmit.xgb <- (findNotDone(reg = reg.xgb) |> findExperiments(reg = reg.xgb))[,
    chunk := chunk(job.id, chunk.size = 1e2)]
submitJobs(tosubmit.xgb, resources = list(walltime = 360000, memory = 4000, ncpus = 1), reg = reg.xgb)

tosubmit.xgb <- (findNotDone(reg = reg.xgb) |> findExperiments(reg = reg.xgb))[,
    chunk := chunk(job.id, chunk.size = 1e1)]
submitJobs(tosubmit.xgb, resources = list(walltime = 360000, memory = 4000, ncpus = 1), reg = reg.xgb)






tosubmit.gosdt <- (findNotDone(reg = reg.gosdt) |> findExperiments(reg = reg.gosdt))[,
    chunk := chunk(job.id, chunk.size = 1e2)]
submitJobs(tosubmit.gosdt, resources = list(walltime = 360000, memory = 4000, ncpus = 1), reg = reg.gosdt)


tosubmit.gosdt <- (findNotDone(reg = reg.gosdt) |> findExperiments(reg = reg.gosdt, prob.pars = taskname == "cs.bin"))[,
    chunk := chunk(job.id, chunk.size = 1e2)]

# cs.bin: need around 1.xG
# mk: 1G
# bc: 1G
# cr: need around 8G
# fc.bin: 100G

submitJobs(tosubmit.gosdt, resources = list(walltime = 360000, memory = 4000, ncpus = 1), reg = reg.gosdt)


# svm: takes about .5h per eval.
# grid size 50 -> 12550 configs * 4 tasks * 10 repls -> 251k cpuh
# addExperimentsPerfEvaluation(reg.svm, "svm", 50, grid = TRUE)




# 1e3 points per chunk

tosubmit.svm <- (findNotDone(reg = reg.svm) |> findExperiments(reg = reg.svm, algo.pars = svm.kernel != "polynomial"))[,
    chunk := chunk(job.id, chunk.size = 1e1)]
submitJobs(tosubmit.svm, resources = list(walltime = 2 * 360000, memory = 4000, ncpus = 1), reg = reg.svm)


tosubmit.svm <- (findNotDone(reg = reg.svm) |> findExperiments(reg = reg.svm, algo.pars = svm.kernel != "polynomial"))[,
    chunk := chunk(job.id, chunk.size = 1e1)]
submitJobs(tosubmit.svm, resources = list(walltime = 2 * 360000, memory = 4000, ncpus = 1), reg = reg.svm)

# TODO:
#  - chunking parallelization
#  - chunk creation parallelization