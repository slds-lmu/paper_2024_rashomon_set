
rm(list = ls(all.names = TRUE))
source("init.R")

reg <- getRegistry("../registry_paper_2024_rashomon_set_svm", make.default = TRUE)

addProblemTaskGetter(reg)

addAlgorithmPerfEvaluation(reg, "svm")

# svm: takes about .5h per eval.
# grid size 50 -> 12550 configs * 4 tasks * 10 repls -> 251k cpuh
addExperimentsPerfEvaluation(reg, "svm", 50, grid = TRUE)
addExperimentsPerfEvaluation(reg, "svm", 12550, grid = FALSE)

resources.default <- list(walltime = 2 * 48 * 3600, memory = 2048, ncpus = 1)
# 1e3 points per chunk

tosubmit <- findJobs()[, .(job.id, chunk = chunk(job.id, chunk.size = 100))]

submitJobs(tosubmit, resources = resources.default)
