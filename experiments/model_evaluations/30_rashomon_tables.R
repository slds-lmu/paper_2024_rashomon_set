


rm(list = ls(all.names = TRUE))
source("init.R")

rashomon.offset.multiplier <- 0.05  # min * (1 + offset)

reduceTables <- function(table, lname, params = NULL) {
  if (is.null(params)) {
    bycols <- c("taskname", grep(sprintf("^(config\\.)?%s\\.", lname), colnames(table), value = TRUE))
  } else {
    bycols <- c("taskname", params, paste0("config.", params))
  }
  table[, .(
      bbrier = mean(result.classif.bbrier),
      bbrier.se = sd(result.classif.bbrier),
      rmse = mean(result.regr.rmse),
      rmse.se = sd(result.regr.rmse),
      .N,
      is.grid = all(is.grid),
      is.in.grid = any(is.grid)
    ),
    by = bycols
  ]
}


tglmnet <- readRDS("data/rashomon.table.glmnet.rds")
tglmnet[, is.grid := seq_len(.N) > nrow(.SD) / 2, by = taskname]
tglmnet[, max.concurrent.jobs := NULL]
tglmnet2 <- readRDS("data/rashomon.table.glmnet_2.rds")
tglmnet2[, is.grid := FALSE]
reduced.glmnet <- reduceTables(rbind(tglmnet, tglmnet2, use.names = TRUE, fill = TRUE), "glmnet")
## check:
# reduced.glmnet[is.grid == FALSE, table(config.glmnet.lambda) |> length()]
# reduced.glmnet[is.grid == TRUE, table(config.glmnet.lambda) |> length()]

ttree <- readRDS("data/rashomon.table.tree.rds")
ttree[, is.grid := seq_len(.N) > nrow(.SD) / 2, by = taskname]
ttree[, max.concurrent.jobs := NULL]
ttree2 <- readRDS("data/rashomon.table.tree_2.rds")
ttree2[, is.grid := FALSE]
reduced.tree <- reduceTables(rbind(ttree, ttree2, use.names = TRUE, fill = TRUE), "tree",
  params = c("cp", "minbucket", "minsplit"))
## check:
# reduced.tree[is.grid == FALSE, table(config.cp) |> length()]
# reduced.tree[is.grid == TRUE, table(config.cp) |> length()]

txgb <- readRDS("data/rashomon.table.xgb.rds")
txgb[, is.grid := FALSE]
txgb[, max.concurrent.jobs := NULL]
txgb[, memory := NULL]
txgb2 <- readRDS("data/rashomon.table.xgb_2.rds")
txgb2[, is.grid := FALSE]
reduced.xgb <- reduceTables(rbind(txgb, txgb2, use.names = TRUE, fill = TRUE), "xgb")

## no check necessary, there is no grid

tsvm <- readRDS("data/rashomon.table.svm.rds")
tsvm[, is.grid := FALSE]
tsvm[tsvm[, .N, by = "svm.cost"][N > 100], is.grid := TRUE, on = "svm.cost"]
tsvm2 <- readRDS("data/rashomon.table.svm_2.rds")
tsvm2[, is.grid := FALSE]
reduced.svm <- reduceTables(rbind(tsvm, tsvm2, use.names = TRUE, fill = TRUE), "svm")

reduced.svm.linear <- reduced.svm[config.svm.kernel == "linear"]
all.nas.linear <- names(Filter(function(x) all(is.na(x)), reduced.svm.linear))
reduced.svm.linear[, (all.nas.linear) := NULL]

reduced.svm.radial <- reduced.svm[config.svm.kernel == "radial"]
all.nas.radial <- names(Filter(function(x) all(is.na(x)), reduced.svm.radial))
reduced.svm.radial[, (all.nas.radial) := NULL]

## check:
# reduced.svm[is.grid == FALSE, table(svm.cost) |> length()]
# reduced.svm[is.grid == TRUE, table(svm.cost) |> length()]

tnnet <- readRDS("data/rashomon.table.nnet.rds")
tnnet[, is.grid := FALSE]
tnnet[tnnet[, .N, by = "nnet.decay"][N > 100], is.grid := TRUE, on = "nnet.decay"]
tnnet2 <- readRDS("data/rashomon.table.nnet_2.rds")
tnnet2[, is.grid := FALSE]
reduced.nnet <- reduceTables(rbind(tnnet, tnnet2, use.names = TRUE, fill = TRUE), "nnet")

## check:
# reduced.nnet[is.grid == FALSE, table(nnet.size) |> length()]
# reduced.nnet[is.grid == TRUE, table(nnet.size) |> length()]

tgosdt <- readRDS("data/rashomon.table.gosdt_2.rds")
tgosdt[, is.grid := FALSE]
tgosdt[, result.regr.rmse := NA_real_]
tgosdt[, ncpus := NULL]
tgosdt[, memory := NULL]
tgosdt <- tgosdt[taskname != "fc.bin"]  # remove partial result for fc.bin
reduced.gosdt <- reduceTables(tgosdt, "gosdt", params = c("regularization", "depth_budget", "balance"))

scores.used <- sapply(list.tasks, function(x) ifelse(inherits(x, "TaskClassif"), "bbrier", "rmse"))

allred <- list(
  xgb = reduced.xgb[is.grid == FALSE][, first(.SD, 100000), by = "taskname"][N >= 10],
  tree = reduced.tree[is.grid == FALSE][, first(.SD, 8000), by = "taskname"][N >= 10],
  glmnet = reduced.glmnet[is.grid == FALSE][, first(.SD, 8000), by = "taskname"][N >= 10],
  svm.linear = reduced.svm.linear[is.grid == FALSE][, first(.SD, 4000), by = "taskname"][N >= 10],
  svm.radial = reduced.svm.radial[is.grid == FALSE][, first(.SD, 4000), by = "taskname"][N >= 10],
  nnet = reduced.nnet[is.grid == FALSE][, first(.SD, 8000), by = "taskname"][N >= 10],
  gosdt = reduced.gosdt[is.grid == FALSE][, first(.SD, 8000), by = "taskname"][N >= 10]
)

# search space sizes:
# xgb: 8 cts
# tree: 3 cts
# glmnet: 2 cts
# svm.radial: 2 cts
# svm.linear: 1 cts
# nnet: 2 cts, 1 lgl
# gosdt: 2 cts, 1 lgl

### ----------------------------------------------------------------------------
## save dataset infos
### ----------------------------------------------------------------------------

runstats <- lapply(names(allred), function(x) {
  allred[[x]][, .N, by = "taskname"][, learner := x]
}) |> rbindlist()

saveRDS(runstats, "data/runstats.rds")

allds <- lapply(allred, function(x) unique(x$taskname)) |> unlist() |> unique()

alltoeval <- sapply(names(allred), function(ln) {
  ut <- allred[[ln]]
  lapply(names(scores.used), function(su) {
    outcome <- scores.used[[su]]
    ut[taskname == su, `:=`(score = get(outcome), score.se = get(paste0(outcome, ".se")))]
  }) |> rbindlist()
}, simplify = FALSE)


allds.pivot <- sapply(allds, function(x) {
  Filter(function(x) nrow(x) > 0, lapply(alltoeval, function(y) {
    y[taskname == x]
  }))
}, simplify = FALSE)

saveRDS(allds.pivot, "data/allds.pivot.rds")

# for LSE evaluation:
saveRDS(lapply(allds.pivot, function(x) lapply(x, first, 8000)), "data/allds.pivot.8k.rds")  # nolint

### ----------------------------------------------------------------------------
## run models
### ----------------------------------------------------------------------------

torun.minima <- lapply(alltoeval, function(x) x[, .SD[which.min(score)], by = "taskname"])

torun.rashomon.learnerwise <- lapply(alltoeval, function(x) {
  x[, .SD[score <= min(score) * (1 + rashomon.offset.multiplier)], by = "taskname"][,
    modelid := seq_len(.N), by = "taskname"]
})

torun.rashomon.learnerwise.1k <- lapply(torun.rashomon.learnerwise, function(x) {
  x[, first(.SD, 1000), by = "taskname"]
})

minima.global <- (lapply(torun.minima, function(x) {
  x[, .(taskname, score)]
}) |> rbindlist())[, .(optimum = min(score)), keyby = taskname]

torun.rashomon.global <- lapply(torun.rashomon.learnerwise, function(x) {
  x[, .SD[score <= minima.global[taskname, optimum] * (1 + rashomon.offset.multiplier)], by = "taskname"]
})

rashomon.size.global <- (lapply(torun.rashomon.global, function(x) x[, .N, by = "taskname"]) |> rbindlist())[,
  .(N = sum(N)), by = "taskname"]

downsize.factor <- rashomon.size.global[, .(reduction.ratio = min(1000 / N, 1)), keyby = "taskname"]

torun.rashomon.global.1k <- lapply(torun.rashomon.global, function(x) {
  x[, first(.SD, round(downsize.factor[taskname, reduction.ratio] * nrow(.SD))), by = "taskname"]
})

# number of models in global RS
(lapply(torun.rashomon.global.1k, function(x) x[, .N, by = "taskname"]) |> rbindlist())[,
  .(N = sum(N)), by = "taskname"]

lapply(names(torun.minima), function(x) {
  torun.minima[[x]][, filename := sprintf("minmodel_%s_%s.rds", x, taskname)]
}) |> invisible()

lapply(names(torun.rashomon.learnerwise.1k), function(x) {
  torun.rashomon.learnerwise.1k[[x]][, filename := sprintf("samplemodel_%s_%s_%04d.rds", x, taskname, modelid)]
}) |> invisible()

lapply(names(torun.rashomon.global.1k), function(x) {
  torun.rashomon.global.1k[[x]][, filename := sprintf("samplemodel_%s_%s_%04d.rds", x, taskname, modelid)]
}) |> invisible()

saveRDS(list(
  torun.minima = torun.minima,
  torun.rashomon.learnerwise.1k = torun.rashomon.learnerwise.1k,
  torun.rashomon.global.1k = torun.rashomon.global.1k,
  torun.rashomon.learnerwise = torun.rashomon.learnerwise,
  torun.rashomon.global = torun.rashomon.global,
  minima.global = minima.global
), "data/run_models.rds")

