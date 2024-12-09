source("init.R")

# future::plan(list("sequential", future::tweak("multisession", workers = future::availableCores())))
# options("mlr3.exec_chunk_bins" = future::nbrOfWorkers() * 5)

tr <- tuneRashomonNaive(task.bh, learner.tree, 10, iters = 4)
list.tasks



tr[order(obj.vals), lapply(learner, function(l) l$param_set$values)]

tr

library("batchtools")

regr = makeExperimentRegistry(file.dir = NA,
  source = "init.R"
)

regr$cluster.functions = makeClusterFunctionsSocket(ncpus = 40)

addProblem("fromlist", fun = function(data, job, taskname) {
  task = list.tasks[[taskname]]
  generateCanonicalDataSplits(task, ratio = 2 / 3, seed = 1)$training
})

addAlgorithm("resample", fun = function(data, instance, job, learnername) {
  learnerlist <- if ("TaskClassif" %in% class(instance)) list.learners.classif else list.learners.regr
  learner <- learnerlist[[learnername]]
  learner <- learner$clone(deep = TRUE)
  learner$param_set$set_values(.values = generate_design_random(learner$param_set$search_space(), 1)$transpose()[[1]])
  resample(instance, learner, rsmp("cv", folds = 10), store_backends = FALSE)
})


addExperiments(
  prob.designs = list(fromlist = data.table(taskname = names(list.tasks))),
  algo.designs = list(resample = data.table(learnername = names(list.learners.regr))),
  repls = 10
)

testJob(1)

submitJobs(findErrors())

getJobTable() |> unwrap() -> jt

jt[learnername == "nnet" & taskname == "bs", ]

library("ggplot2")
ggplot(jt, aes(y = as.numeric(time.running), x = learnername, color = taskname)) +
  geom_jitter(alpha = 1) + scale_y_log10() +
  theme_minimal()

# New ggplot with mean runtime bars
ggplot(jt, aes(y = as.numeric(time.running), x = learnername, color = taskname)) +
  geom_jitter(alpha = 0.5) +
  stat_summary(fun = function(x) log(mean(exp(x))), geom = "crossbar", width = 0.5) +
  scale_y_log10() +
  theme_minimal() +
  labs(title = "Runtime by Learner and Task",
       x = "Learner Name",
       y = "Runtime (log scale)",
       color = "Task Name")


hours.per.point <- jt[, .(meanruntime = mean(time.running, na.rm = TRUE)),
  by = c("taskname", "learnername")][,
  .(worstcasemean = sum(meanruntime)), by = "learnername"][,
  as.numeric(sum(worstcasemean)) / 3600]

prospective.points <- 1e5 / hours.per.point

prospective.points

options(datatable.print.nrows = 120)

getStatus()


task.bh


#ranges von statistiken explorieren
# - jointly?
#
#vs.
#
# uniformly sampling

jt[, mean(as.numeric(time.running), na.rm = TRUE), by = c("taskname", "learnername")][,
  sum(V1) / 3600 * 1e4, by = "learnername"]

# around 1e6 pts per learner
# glmnet: 1e4, plus 1e2^2 grid --> 90 cpuh
## 4e2 points per chunk
# tree: 1e6, plus 1e2^3 grid --> 4000 cpuh
## 4e2 points per chunk
# nnet: 2e4, plus 2 * 1e2^2 grid --> 2000 cpuh
## size > 100: 1 point per chunk
## size > 10: 50 points per chunk
## size < 10: 500 points per chunk
jt[learnername == "xgb", mean(as.numeric(time.running), na.rm = TRUE), by = "taskname"][,
  .(points = 1e4 / (sum(V1) / 3600))]
# xgb: 5e5 points for 1e4 cpuh (x10 reps)
## 5e1 points per chunk


regr2 = makeExperimentRegistry(file.dir = NA,
  source = "init.R",
  make.default = FALSE
)

addProblem("fromlist", fun = function(data, job, taskname) {
  task = list.tasks[[taskname]]
  generateCanonicalDataSplits(task, ratio = 2 / 3, seed = 1)$training
}, reg = regr2)

addAlgorithm("resample", fun = function(data, instance, job, ...) {
  browser()
}, reg = regr2)

addExperiments(
  prob.designs = list(fromlist = data.table(taskname = names(list.tasks))),
  algo.designs = list(resample = data.table(learnername = names(list.learners.regr))),
  repls = 10,
  reg = regr2
)

testJob(1, reg = regr2)




################

reg <- getRegistry("../registry_production", make.default = TRUE)
reg$cluster.functions <- makeClusterFunctionsSocket(ncpus = 40)

reg$cluster.functions <- makeClusterFunctionsSocket(ncpus = 40)

addProblemTaskGetter(reg)

for (lname in names(list.learners.regr)) {
  addAlgorithmPerfEvaluation(reg, lname)
}

addExperimentsPerfEvaluation(reg, "tree", 1, grid = FALSE, repls = 1)
addExperimentsPerfEvaluation(reg, "xgb", 1, grid = FALSE, repls = 1)
addExperimentsPerfEvaluation(reg, "glmnet", 1, grid = FALSE, repls = 1)
addExperimentsPerfEvaluation(reg, "nnet", 1, grid = FALSE, repls = 1)

getJobTable() |> unwrap()

submitJobs()


getStatus()

getJobTable() |> unwrap()

res <- reduceResultsList(findDone())

res



# glmnet: 1e4, plus 1e2^2 grid --> 90 cpuh
## 4e2 points per chunk
addExperimentsPerfEvaluation(reg, "glmnet", 1e2, grid = FALSE)
addExperimentsPerfEvaluation(reg, "glmnet", 1e1, grid = TRUE)

# tree: 1e4, plus 1e2^2 grid --> 40 cpuh
## 4e2 points per chunk
addExperimentsPerfEvaluation(reg, "tree", 1e2, grid = FALSE)
addExperimentsPerfEvaluation(reg, "tree", 1e1, grid = TRUE)

# nnet: 2e4, plus 2 * 1e2^2 grid --> 2000 cpuh
## size > 100: 1 point per chunk
## size > 10: 50 points per chunk
## size < 10: 500 points per chunk
addExperimentsPerfEvaluation(reg, "nnet", 2e2, grid = FALSE)
addExperimentsPerfEvaluation(reg, "nnet", 1e1, grid = TRUE)


# xgb: 5e5 points for 1e4 cpuh (x10 reps)
## 5e1 points per chunk
addExperimentsPerfEvaluation(reg, "xgb", 5e2, grid = FALSE)


