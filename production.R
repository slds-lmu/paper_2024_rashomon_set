
rm(list = ls(all.names = TRUE))
source("init.R")

reg <- getRegistry("../registry_paper_2024_rashomon_set", make.default = TRUE)

addProblemTaskGetter(reg)

for (lname in names(list.learners.regr)) {
  addAlgorithmPerfEvaluation(reg, lname)
}

# glmnet: 1e4, plus 1e2^2 grid --> 90 cpuh
## 1e3 points per chunk
addExperimentsPerfEvaluation(reg, "glmnet", 1e4, grid = FALSE)
addExperimentsPerfEvaluation(reg, "glmnet", 1e2, grid = TRUE)

# tree: 1e4, plus 1e2^2 grid --> 40 cpuh
## 1e3 points per chunk
addExperimentsPerfEvaluation(reg, "tree", 1e6, grid = FALSE)
addExperimentsPerfEvaluation(reg, "tree", 1e2, grid = TRUE)

# nnet: 2e4, plus 2 * 1e2^2 grid --> 2000 cpuh
## size > 100: 1 point per chunk
## size > 10: 50 points per chunk
## size < 10: 500 points per chunk
addExperimentsPerfEvaluation(reg, "nnet", 2e4, grid = FALSE)
addExperimentsPerfEvaluation(reg, "nnet", 1e2, grid = TRUE)


# xgb: 5e5 points for 1e4 cpuh (x10 reps)
## 5e1 points per chunk
addExperimentsPerfEvaluation(reg, "xgb", 5e5, grid = FALSE)


resources.default <- list(walltime = 48 * 3600, memory = 4000, ncpus = 1)
# 1e3 points per chunk

tosubmit <- list(
  findExperiments(algo.pattern = "^glmnet$|^tree$")[, chunk := chunk(job.id, chunk.size = 1e3)],
  findExperiments(algo.name = "xgb")[, chunk := chunk(job.id, chunk.size = 5e1)],
  findExperiments(algo.name = "nnet", algo.pars = exp(nnet.size) > 100),  # no chunking
  findExperiments(algo.name = "nnet", algo.pars = exp(nnet.size) <= 100 & exp(nnet.size) > 10)[,
    chunk := chunk(job.id, chunk.size = 5e1)],
  findExperiments(algo.name = "nnet", algo.pars = exp(nnet.size) <= 10)[,
    chunk := chunk(job.id, chunk.size = 5e2)]
)

lapply(tosubmit, submitJobs, resources = resources.default)