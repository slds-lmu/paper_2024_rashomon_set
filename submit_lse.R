

rm(list = ls(all.names = TRUE))
source("init.R")

reg.lse <- getRegistry("../registry_paper_2024_rashomon_set_lseruns/registry2", make.default = TRUE)



addProblem("osr", fun = osGenerator, reg = reg.lse)

addAlgorithm("runOptimize", fun = runOptimize, reg = reg.lse)

addExperiments(reg = reg.lse,
  algo.designs = list(runOptimize = CJ(
    optimizer = optimizer.scenarios,
    logscale = c(TRUE, FALSE)
  )),
  prob.design = list(osr = CJ(
    dataset = c("gc", "cs", "bs", "st", "cr", "mk", "bc", "cs.bin", "fc.bin"),
    learners = c("all", names(getLearnersMeta()))
  )),
  repls = 30
)


testJob(721)

