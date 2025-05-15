
rm(list = ls(all.names = TRUE))

source("init.R")

reg <- makeRegistry(file.dir = NA, seed = 1,
  source = c("init.R", "experiments/model_evaluations/aux_load_evalinfo.R"),
  make.default = TRUE
)

tables.to.run <- c("torun.minima", "torun.rashomon.learnerwise.1k")

jobs <- lapply(tables.to.run, function(x) {
  lapply(names(allinfo[[x]]), function(y) {
    allinfo[[x]][[y]][, .(ttr = x, learner = y, row = seq_len(.N))]
  })
}) |> unlist(recursive = FALSE) |> rbindlist()

runfun <- function(ttr, learner, row) {
  evaluating <- allinfo[[ttr]][[learner]][row]
  outfile <- file.path(datapath, evaluating$filename)
  if (file.exists(outfile)) {
    return(NULL)
  }
  model <- trainLearnerFromInfoRow(learner, evaluating)
  saveRDS(model, file = outfile)
  NULL
}

batchMap(fun = runfun, args = jobs, reg = reg)

