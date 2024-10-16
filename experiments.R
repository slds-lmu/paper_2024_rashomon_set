


source("init.R")

future::plan(list("sequential", future::tweak("multisession", workers = future::availableCores())))
options("mlr3.exec_chunk_bins" = future::nbrOfWorkers() * 5)

tr <- tuneRashomonNaive(task.bh, learner.tree, 10, iters = 100)



tr[order(obj.vals), lapply(learner, function(l) l$param_set$values)]


task.bh


ranges von statistiken explorieren
 - jointly?

vs.

uniformly sampling
