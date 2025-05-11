library("reticulate")
library("paradox")



ps.treefarms = ps(
  # Key Parameters
  regularization = p_dbl(lower = 0, upper = 1, default = 0.05, tags = "train"),  # shoud be larger than 1 / #samples
  rashomon = p_lgl(default = TRUE, tags = "train"),  # if false, only fit best tree

  # Rashomon-specific configs
  # The following are mutually exclusive
  rashomon_bound_multiplier = p_dbl(lower = 0, default = 0.05, tags = "train"),
  rashomon_bound = p_dbl(lower = 0, tags = "train"),
  rashomon_bound_adder = p_dbl(lower = 0, tags = "train"),

  # File Output
  # the following generates files, generally not needed
  output_accuracy_model_set = p_lgl(default = FALSE, tags = "train"),
  output_covered_sets = p_uty(default = list(), tags = "train"), # Array of strings e.g., c('f1', 'bacc', 'auc')
  covered_sets_thresholds = p_uty(default = list(), tags = "train"), # Array of doubles
  rashomon_model_set_suffix = p_uty(default = "", tags = "train"),
  rashomon_ignore_trivial_extensions = p_lgl(default = TRUE, tags = "train"),
  rashomon_trie = p_uty(default = "", tags = "train"), # Path to output trie file


  # Limits
  depth_budget = p_int(lower = 1L, default = 0L, special_vals = list(0L), tags = "train"), # max tree depth, 0 = unlimited
  tile_limit = p_int(lower = 1L, default = 0L, special_vals = list(0L), tags = "train"), # Undocumented in README text, assuming 0 = default/special
  precision_limit = p_int(lower = 1L, default = 0L, special_vals = list(0L), tags = "train"), # 0 = no limit
  stack_limit = p_int(lower = 1L, default = 0L, special_vals = list(0L), tags = "train"), # 0 = use heap
  minimum_captured_points = p_int(lower = 0L, default = 0L, tags = "train"), # Undocumented in README text

  # Tuners
  uncertainty_tolerance = p_dbl(lower = 0, upper = 1, default = 0.0, tags = "train"),  # allow early termination
  upperbound = p_dbl(lower = 0, upper = 1, default = 0.0, tags = "train"), # limit risk of model search space

  # Execution Control
  time_limit = p_dbl(lower = 0, default = 0, tags = "train"), # 0 = no limit
  worker_limit = p_int(lower = 1L, default = 1L, special_vals = list(0L), tags = "train"), # 0 = #cores

  # Flags
  balance = p_lgl(default = FALSE, tags = "train"),  # sample importance: equal weights to all classes
  cancellation = p_lgl(default = TRUE, tags = "train"),  # propagate up the dependency graph of task cancellations
  look_ahead = p_lgl(default = TRUE, tags = "train"),  # one-step look-ahead bound via scopes
  similar_support = p_lgl(default = TRUE, tags = "train"),  # similar support bound via distance index
  feature_exchange = p_lgl(default = FALSE, tags = "train"),  # prune pairs of features using subset comparison
  continuous_feature_exchange = p_lgl(default = FALSE, tags = "train"),  # prune pairs of continuous features
  feature_transform = p_lgl(default = TRUE, tags = "train"),  # equivalence discovery through simple feature transformations
  rule_list = p_lgl(default = FALSE, tags = "train"),  # rule-list constraints on models
  non_binary = p_lgl(default = FALSE, tags = "train"),  # non-binary encoding (support for non-binary features?)

  costs = p_uty(default = "", tags = "train"), # input: cost matrix; must contain path to cost matrix CSV

  # Output & Diagnostics
  diagnostics = p_lgl(default = FALSE, tags = "train"),
  verbose = p_lgl(default = TRUE, tags = "train"),

  # Low-Level File Output
  model = p_uty(default = "", tags = "train"), # Path for output model file
  profile = p_uty(default = "", tags = "train"), # Path for analytics log file
  timing = p_uty(default = "", tags = "train"), # Path for timing log file
  trace = p_uty(default = "", tags = "train"), # Path for trace visualization dir
  tree = p_uty(default = "", tags = "train"), # Path for trace-tree visualization dir
  datatset_encoding = p_uty(default = "", tags = "train"), # Undocumented in README text
  memory_checkpoints = p_uty(default = list(), tags = "train") # Undocumented in README text
)



ps.treefarms$set_values(
  regularization = 0.05,
  rashomon = TRUE,
  output_covered_sets = c("f1", "bacc", "auc"),
  covered_sets_thresholds = c(0.8, 0.8, 0.8),
  rashomon_model_set_suffix = "rashomon",
  depth_budget = 0L
)

parvals <- ps.treefarms$get_values()
for (vector.params in c("output_covered_sets", "covered_sets_thresholds", "memory_checkpoints")) {
  if (vector.params %in% names(parvals)) {
    parvals[[vector.params]] <- as.list(parvals[[vector.params]])
  }
}

config.json <- jsonlite::toJSON(parvals, auto_unbox = TRUE)

treefarms.model <- reticulate::import("treefarms.libgosdt")

treefarms.model$configure(config.json)


testdata <- data.table(x1 = runif(200) > .5, x2 = as.numeric(runif(200) > 0.3), target = sample(c("plus", "minus"), 200, TRUE))
testdata[x1 == 1 & x2 == 1, target := "plus"]

task <- as_task_classif(testdata, target = "target", id = "test")


feats = as.matrix(task$data(cols = c(task$feature_names)))

mode(feats) <- "numeric"

assertIntegerish(feats, tol = 0)


data.csv <- paste(capture.output(fwrite(cbind(feats, task$data(cols = task$target_names)))), collapse = "\n")

timing <- system.time(result <- treefarms.model$fit(data.csv), gcFirst = FALSE)

info <- list(timing.raw = timing, result = result)

if (treefarms.model$status() == 0) {
  # no timeout
  info$timeout <- FALSE
  info$timing <- timing
} else if (treefarms.model$status() == 2) {
  timing[] <- -1
  info$timing <- timing
  info$timeout = TRUE
} else {
  stop(sprintf("TREEFARMS error:\n%s", result))
}





py <- reticulate::py
py$jsonloader <- import("json")$loads
py$ModelSetContainer <- import("treefarms.model.model_set")$ModelSetContainer

reticulate::py_run_string("mscgen = lambda result: ModelSetContainer(jsonloader(result))")

container <- py$mscgen(result)

container$get_tree_count()

container[[0]]

tosave <- list(container = container)

saveRDS(tosave, "/tmp/treefarms_model.rds")

loaded <- readRDS("/tmp/treefarms_model.rds")

reticulate::py_is_null_xptr(loaded$container)



source("R/learners/trees_utils.R")
source("R/learners/LearnerTreeFarms.R")
source("R/learners/LearnerGosdt.R")

compas.example <- fread("https://raw.githubusercontent.com/ubc-systopia/treeFarms/refs/heads/main/experiments/datasets/compas/binned.csv")

colnames(compas.example) <- make.names(colnames(compas.example))

compas.example[[length(compas.example)]] <- factor(c("zero", "one")[compas.example[[length(compas.example)]] + 1], levels = c("zero", "one"))

tname <- colnames(compas.example)[length(compas.example)]

task.compas <- as_task_classif(compas.example, target = tname, id = "compas.binned")

learner.treefarms <- LearnerClassifTreeFarms$new()

learner.treefarms$param_set$values$regularization <- 0.00001
learner.treefarms$param_set$values$rashomon_bound_multiplier <- 0.00001


learner.treefarms$param_set$values$regularization <- 0.1
learner.treefarms$param_set$values$rashomon_bound_multiplier <- 0.1


learner.treefarms$train(task.compas)
learner.treefarms$modelcontainer$get_tree_count()


learner.treefarms$param_set$values$regularization <- 0.05
learner.treefarms$param_set$values$rashomon_bound_multiplier <- 0.1


learner.treefarms$train(task.compas)
learner.treefarms$modelcontainer$get_tree_count()

learner.treefarms$param_set$values$regularization <- 0.02
learner.treefarms$param_set$values$rashomon_bound_multiplier <- 0.1


learner.treefarms$train(task.compas)
learner.treefarms$modelcontainer$get_tree_count()



cat(reticulate::py_repr(learner.treefarms$modelcontainer[[0]]))

table(task.compas$data(cols = task.compas$target_names)[[1]],
  learner.treefarms$modelcontainer[[0]]$predict(task.compas$data(cols = task.compas$feature_names))
)

table(task.compas$data(cols = task.compas$target_names)[[1]],
  learner.treefarms$modelcontainer[[1]]$predict(task.compas$data(cols = task.compas$feature_names))
)

table(task.compas$data(cols = task.compas$target_names)[[1]],
  learner.treefarms$modelcontainer[[2]]$predict(task.compas$data(cols = task.compas$feature_names))
)

image(lrn("classif.rpart")$train(task.compas)$predict(task.compas)$confusion)


manualpreds <- sapply(seq_len(nrow(task.compas$data())), function(i) {
  learner.treefarms$modelcontainer[[0]]$classify(as.numeric(task.compas$data(cols = task.compas$feature_names)[i]))[[1]]
})

table(task.compas$data(cols = task.compas$target_names)[[1]], manualpreds)




cm <- cor(task.compas$data(cols = c(task.compas$feature_names, task.compas$target_names))[, lapply(.SD, as.numeric)])
cm[2, 12] <- cm[12, 2] <- 1

image(cm)


lrn("classif.rpart")$train(task.compas)$model

task.compas$feature_names


test.ncol <- length(task.compas$feature_names)
test.nrow <- 10000

test.data <- matrix(sample(0:1, test.ncol * test.nrow, TRUE), nrow = test.nrow, ncol = test.ncol)

test.preds <- learner.treefarms$modelcontainer[[0]]$predict(as.data.frame(test.data))

cat(reticulate::py_repr(learner.treefarms$modelcontainer[[0]]), "\n")

plot(test.data[, 12] + rnorm(test.nrow, 0, 0.1), test.preds + rnorm(test.nrow, 0, 0.1))
plot(test.data[, 2] + rnorm(test.nrow, 0, 0.1), test.preds + rnorm(test.nrow, 0, 0.1))

learner.treefarms$modelcontainer[[0]]$classify(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1))  # feature 12 == 1 -> 1
learner.treefarms$modelcontainer[[0]]$classify(c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))  # feature 2 == 1 -> 0
learner.treefarms$modelcontainer[[0]]$classify(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0))  # feature 10 == 1 -> 0
learner.treefarms$modelcontainer[[0]]$classify(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))  # feature 10 == 0 -> 1


test.preds[(test.data[, 12] == 0 & test.data[, 2] == 0 & test.data[, 10] == 0)]
test.preds[(test.data[, 12] == 0 & test.data[, 2] == 0 & test.data[, 10] == 1)]


rootfeats <- sapply(seq_len(learner.treefarms$modelcontainer$get_tree_count()), function(i) {
  jsonlite::fromJSON(learner.treefarms$modelcontainer[[i - 1]]$json())$feature
})

sort(table(rootfeats))

conftables <- sapply(seq_len(learner.treefarms$modelcontainer$get_tree_count()), function(i) {
  table(task.compas$data(cols = task.compas$target_names)[[1]],
    learner.treefarms$modelcontainer[[i - 1]]$predict(task.compas$data(cols = task.compas$feature_names))
  )
}, simplify = FALSE)

mces <- (sapply(conftables, function(x) {
  sum(diag(x)) / sum(x)
}))

range(mces)

image(conftables[[1]])
conftables[[1]]




trees <- jsonlite::fromJSON(learner.treefarms$model$result)

names(trees)

as.data.table(trees$storage)

plot(sort(trees$storage$prediction))

bytes.01 <- sprintf("%04X", as.integer(unique(sort(trees$storage$prediction)) %% 2^16))
bytes.02 <- sprintf("%04X", as.integer(unique(sort(trees$storage$prediction)) %/% 2^16))

do.call(rbind, paste0(bytes.02, bytes.01) |> strsplit(""))[, 8] |> table()
do.call(rbind, paste0(bytes.02, bytes.01) |> strsplit(""))[, 7] |> table()
do.call(rbind, paste0(bytes.02, bytes.01) |> strsplit(""))[, 6] |> table()
do.call(rbind, paste0(bytes.02, bytes.01) |> strsplit(""))[, 5] |> table()
do.call(rbind, paste0(bytes.02, bytes.01) |> strsplit(""))[, 4] |> table()
do.call(rbind, paste0(bytes.02, bytes.01) |> strsplit(""))[, 3] |> table()
do.call(rbind, paste0(bytes.02, bytes.01) |> strsplit(""))[, 2] |> table()
do.call(rbind, paste0(bytes.02, bytes.01) |> strsplit(""))[, 1] |> table()

sort(table(trees$storage$prediction))
paste0(sprintf("%04X", as.integer(2841603296 %/% 2^16)), sprintf("%04X", as.integer(2841603296 %% 2^16)))







## TODO:
# - [X] target level confusion
# - [X] are trees redundant?
#   - they are not the same, but some may express the same function
# - [X] does it handle continuous features?
#   - in theory yes, but produces a lot of trees, crashes sometimes, gives weird behaviour
# - [ ] time limit
# - [X] how do we disable verbosity? ("diagnostics", "verbose"?)
#   - not possible (C++)

#%%  Are trees redundant?

set.seed(2)
data.testing <- data.table(
  feature1 = as.numeric(runif(1000) > 0.5),
  feature2 = as.numeric(runif(1000) > 0.75),
  feature3 = as.numeric(runif(1000) > 0.25)
)

data.testing[, target := rnorm(1000, 0.5 * feature1 + feature2 * (1 - feature3)) > 0.5]
levelnames <- c("0", "1")
data.testing[, target := factor(levelnames[target + 1], levels = levelnames)]
task.testing <- as_task_classif(data.testing, target = "target", id = "testing")

learner.treefarms <- LearnerClassifTreeFarms$new()
learner.treefarms$param_set$values$regularization <- 0
learner.treefarms$param_set$values$rashomon_bound_multiplier <- 1

learner.treefarms$train(task.testing)

learner.treefarms$modelcontainer$get_tree_count()

conftables <- sapply(seq_len(learner.treefarms$modelcontainer$get_tree_count()), function(i) {
  table(task.testing$data(cols = task.testing$target_names)[[1]],
    learner.treefarms$modelcontainer[[i - 1]]$predict(task.testing$data(cols = task.testing$feature_names))
  )
}, simplify = FALSE)
mces <- (sapply(conftables, function(x) {
  sum(diag(x)) / sum(x)
}))

sametrees <- which(abs(mces - as.numeric(names(which.max(table(mces))))) < 0.001)

# tree [1] is 1 when 0 == TRUE || (1 == TRUE && 2 == FALSE)
learner.treefarms$modelcontainer[[sametrees[1] - 1]]  # [1] same as [2]
learner.treefarms$modelcontainer[[sametrees[2] - 1]]

learner.treefarms$modelcontainer[[sametrees[3] - 1]]  # differs from [1]: 2 == TRUE -> 0 regardless of 0
learner.treefarms$modelcontainer[[sametrees[5] - 1]]  # same as above
learner.treefarms$modelcontainer[[sametrees[4] - 1]]  # redundant split, same as above otherwise
learner.treefarms$modelcontainer[[sametrees[6] - 1]]  # same as above

learner.treefarms$modelcontainer[[sametrees[7] - 1]]  # also seems redundant
learner.treefarms$modelcontainer[[sametrees[8] - 1]]  # same as above

learner.treefarms$modelcontainer[[sametrees[9] - 1]]
learner.treefarms$modelcontainer[[sametrees[10] - 1]]  # same as above

#%%  Continuous features

set.seed(2)
data.testing <- data.table(
  feature1 = as.numeric(runif(10)),
  feature2 = as.numeric(runif(10)),
  feature3 = as.numeric(runif(10))
)

data.testing[, target := rnorm(10, 0.5 * feature1 + feature2 * (1 - feature3)) > 0.5]
levelnames <- c("0", "1")
data.testing[, target := factor(levelnames[target + 1], levels = levelnames)]
task.testing <- as_task_classif(data.testing, target = "target", id = "testing")

learner.treefarms <- LearnerClassifTreeFarms$new()
learner.treefarms$param_set$values$regularization <- 0.05
learner.treefarms$param_set$values$rashomon_bound_multiplier <- 0.2

learner.treefarms$train(task.testing)

learner.treefarms$modelcontainer$get_tree_count()
learner.treefarms$modelcontainer[[15]]  # 'null'?!

#%%  target level confusion

set.seed(2)
data.testing <- data.table(
  feature1 = as.numeric(runif(1000) > 0.5),
  feature2 = as.numeric(runif(1000) > 0.75),
  feature3 = as.numeric(runif(1000) > 0.25)
)

data.testing[, target := rnorm(1000, 0.5 * feature1 + feature2 * (1 - feature3)) > 0.5]
levelnames <- c("0", "1")
data.testing[, target := factor(levelnames[target + 1], levels = levelnames)]
task.testing <- as_task_classif(data.testing, target = "target", id = "testing")

learner.treefarms <- LearnerClassifTreeFarms$new()
learner.treefarms$param_set$values$regularization <- 0
learner.treefarms$param_set$values$rashomon_bound_multiplier <- 1

learner.treefarms$train(task.testing)

data.testing2 <- copy(data.testing)
levels(data.testing2$target) <- c("b", "a")
task.testing2 <- as_task_classif(data.testing2, target = "target", id = "testing2")

learner.treefarms2 <- LearnerClassifTreeFarms$new()
learner.treefarms2$param_set$values$regularization <- 0
learner.treefarms2$param_set$values$rashomon_bound_multiplier <- 1

learner.treefarms2$train(task.testing2)

learner.treefarms$modelcontainer[[1]]
learner.treefarms2$modelcontainer[[1]]

learner.treefarms3 <- LearnerClassifTreeFarms$new()
learner.treefarms3$param_set$values$regularization <- 0
learner.treefarms3$param_set$values$rashomon_bound_multiplier <- 1
learner.treefarms3$param_set$values$verbose <- FALSE

learner.treefarms3$train(task.testing2)

jsonlite::fromJSON(learner.treefarms3$model$result)




#%%  predictions


learner.treefarms <- LearnerClassifTreeFarms$new()

learner.treefarms$param_set$values$regularization <- 0
learner.treefarms$param_set$values$rashomon_bound_multiplier <- 0.07

learner.treefarms$train(task.testing)

learner.treefarms$modelcontainer$get_tree_count()

conftables <- sapply(seq_len(learner.treefarms$modelcontainer$get_tree_count()), function(i) {
  table(task.testing$data(cols = task.testing$target_names)[[1]],
    learner.treefarms$modelcontainer[[i - 1]]$predict(task.testing$data(cols = task.testing$feature_names))
  )
}, simplify = FALSE)

mces <- (sapply(conftables, function(x) {
  sum(diag(x)) / sum(x)
}))

mces2 <- sapply(seq_len(learner.treefarms$modelcontainer$get_tree_count()), function(i) {
  learner.treefarms$param_set$values$selected_tree <- i
  learner.treefarms$predict(task.testing)$score(msr("classif.acc"))
})

all(mces == mces2)


mces3 <- sapply(seq_len(learner.treefarms$modelcontainer$get_tree_count()), function(i) {
  learner.treefarms$modelcontainer$get_tree_metric_at_idx(i - 1)$loss
})

all.equal(mces, 1 - mces3)


#%% tree conversion

set.seed(2)
data.testing <- data.table(
  feature1 = as.numeric(runif(1000) > 0.5),
  feature2 = as.numeric(runif(1000) > 0.75),
  feature3 = as.numeric(runif(1000) > 0.25)
)

data.testing[, target := rnorm(1000, 0.5 * feature1 + feature2 * (1 - feature3)) > 0.5]
levelnames <- c("0", "1")
data.testing[, target := factor(levelnames[target + 1], levels = levelnames)]
task.testing <- as_task_classif(data.testing, target = "target", id = "testing")

learner.treefarms <- LearnerClassifTreeFarms$new()
learner.treefarms$predict_type = "prob"
learner.treefarms$param_set$values$regularization <- 0
learner.treefarms$param_set$values$rashomon_bound_multiplier <- 1

learner.treefarms$train(task.testing)



#%% gosdt

set.seed(2)
data.testing <- data.table(
  feature1 = as.numeric(runif(1000) > 0.5),
  feature2 = as.numeric(runif(1000) > 0.75),
  feature3 = as.numeric(runif(1000) > 0.25)
)

data.testing[, target := rnorm(1000, 0.5 * feature1 + feature2 * (1 - feature3)) > 0.5]
levelnames <- c("0", "1")
data.testing[, target := factor(levelnames[target + 1], levels = levelnames)]
task.testing <- as_task_classif(data.testing, target = "target", id = "testing")



learner.treefarms <- LearnerClassifTreeFarms$new()
learner.treefarms$param_set$values$regularization <- 0
learner.treefarms$param_set$values$rashomon_bound_multiplier <- 0.07
learner.treefarms$train(task.testing)
learner.treefarms$modelcontainer$get_tree_count()

learner.gosdt <- LearnerClassifGosdt$new()
learner.gosdt$param_set$values$regularization <- 0
learner.gosdt$param_set$values$allow_small_reg <- TRUE
learner.gosdt$train(task.testing)

learner.gosdt$model

learner.gosdt$predict(task.testing)$score(msr("classif.acc"))

mces <- sapply(seq_len(learner.treefarms$modelcontainer$get_tree_count()), function(i) {
  learner.treefarms$modelcontainer$get_tree_metric_at_idx(i - 1)$loss
})

max(mces)
which(mces == min(mces))


#%% short tasks



set.seed(2)
data.testing <- data.table(
  feature1 = c(1, 1, 0),
  feature2 = c(1, 0, 0),
  feature3 = c(0, 1, 0)
)

data.testing[, target := c(1, 0, 1)]
levelnames <- c("0", "1")
data.testing[, target := factor(levelnames[target + 1], levels = levelnames)]
task.testing <- as_task_classif(data.testing, target = "target", id = "testing")

learner.treefarms <- LearnerClassifTreeFarms$new()
learner.treefarms$predict_type = "prob"
learner.treefarms$param_set$values$regularization <- 0
learner.treefarms$param_set$values$rashomon_bound_multiplier <- 0.07
learner.treefarms$train(task.testing)
learner.treefarms$modelcontainer$get_tree_count()

learner.gosdt <- LearnerClassifGosdt$new()
learner.gosdt$predict_type = "prob"
learner.gosdt$param_set$values$regularization <- 0
learner.gosdt$param_set$values$allow_small_reg <- TRUE
learner.gosdt$train(task.testing)


learner.gosdt$predict(task.testing)
learner.treefarms$predict(task.testing)
tpmodel

data.testing[, target := c(1, 0, 1)]
levelnames <- c("20", "1")
data.testing[, target := factor(levelnames[target + 1], levels = levelnames)]
task.testing <- as_task_classif(data.testing, target = "target", id = "testing")

learner.treefarms <- LearnerClassifTreeFarms$new()
learner.treefarms$predict_type = "prob"
learner.treefarms$param_set$values$regularization <- 0
learner.treefarms$param_set$values$rashomon_bound_multiplier <- 0.07
learner.treefarms$train(task.testing)
learner.treefarms$modelcontainer$get_tree_count()

learner.gosdt <- LearnerClassifGosdt$new()
learner.gosdt$predict_type = "prob"
learner.gosdt$param_set$values$regularization <- 0
learner.gosdt$param_set$values$allow_small_reg <- TRUE
learner.gosdt$train(task.testing)


learner.gosdt$predict(task.testing)
learner.treefarms$predict(task.testing)

learner.treefarms$modelcontainer[[0]]

learner.treefarms$modelcontainer[[0]]$json() |> cat()

learner.treefarms$.__enclos_env__$private$.partycache
learner.treefarms$model$target.levels
learner.treefarms$model$data


learner.gosdt$param_set$values$regularization <- 0.01
learner.gosdt$train(task.fico.binarized)
learner.gosdt$train(task.compas.binarized)

learner.treefarms$param_set$values$regularization <- 0.01
learner.treefarms$train(task.compas.binarized)

learner.treefarms$modelcontainer$get_tree_count()

objes <- sapply(seq_len(learner.treefarms$modelcontainer$get_tree_count()), function(i) {
  learner.treefarms$modelcontainer$get_tree_metric_at_idx(i - 1)$objective
})


metrices <- sapply(seq_len(learner.treefarms$modelcontainer$get_tree_count()), function(i) {
  learner.treefarms$modelcontainer$get_tree_metric_at_idx(i - 1)$loss
})

learner.rpart <- LearnerClassifRpart$new()
learner.rpart$predict_type = "prob"
learner.rpart$train(task.compas.binarized)



learner.rpart$predict(task.compas.binarized)$score(msrs(c("classif.acc", "classif.auc")))

learner.treefarms$param_set$values$selected_tree <- which.min(metrices)
learner.treefarms$predict(task.compas.binarized)$score(msrs(c("classif.acc", "classif.auc")))


learner.treefarms$param_set$values$selected_tree <- which.min(objes)
learner.treefarms$predict(task.compas.binarized)$score(msrs(c("classif.acc", "classif.auc")))
learner.gosdt$predict(task.compas.binarized)$score(msrs(c("classif.acc", "classif.auc")))


1 - min(metrices)



learner.gosdt$model$models_string |> cat()


learner.rpart$model

task.compas.binarized$feature_names


plot(sort(mces))
abline(max(mces) * (1 - learner.treefarms$param_set$values$rashomon_bound_multiplier), 0)
length(mces)



plot(mces[mces > .565])
points(mces.old, col = "red")

plot(sort(mces[mces > .565]))
points(sort(mces.old), col = "red")

max(mces) * 0.9

1 - min(mces) / max(mces)


mces * .2


data.testing[, .(oneness = mean(target == "1")), by = c("feature1", "feature2", "feature3")][order(oneness)]








task.compas$nrow
trees$metadata

trees$available_metric_values$metric_pointers
trees$available_metric_values$metric_values




debugonce(learner.treefarms$.__enclos_env__$private$.predict)

learner.treefarms$predict(task.compas)
