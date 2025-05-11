## create models

source("init.R")

# reg_models <- getRegistry("../registry_paper_2024_rashomon_set", make.default = FALSE)
#
# addProblemTaskGetter(reg_models)
#
# for (lname in names(list.learners.regr)) {
#   addAlgorithmPerfEvaluation(reg, lname)
# }


readRDS("data/rashomon.table.glmnet.rds") -> tglmnet
readRDS("data/rashomon.table.tree.rds") -> ttree
readRDS("data/rashomon.table.xgb.rds") -> txgb
readRDS("data/rashomon.table.svm.rds") -> tsvm
readRDS("data/rashomon.table.nnet.rds") -> tnnet

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

sliceTables <- function(table, lname, params = NULL) {
  if (is.null(params)) {
    bycols <- c("taskname", grep(sprintf("^(config\\.)?%s\\.", lname), colnames(table), value = TRUE))
  } else {
    bycols <- c("taskname", params, paste0("config.", params))
  }
  lapply(1:10, function(i) {
    slice <- table[seq(i, nrow(table), by = 10), c(
        "result.classif.bbrier",
        "result.regr.rmse",
        bycols,
        "is.grid"
      ), with = FALSE
    ]
    setnames(slice, "result.classif.bbrier", "bbrier")
    setnames(slice, "result.regr.rmse", "rmse")
    slice
  })
}

tglmnet[, is.grid := seq_len(.N) > nrow(.SD) / 2, by = taskname]
reduced.glmnet <- reduceTables(tglmnet, "glmnet")
sliced.glmnet <- sliceTables(tglmnet, "glmnet")


ttree[, is.grid := seq_len(.N) > nrow(.SD) / 2, by = taskname]
reduced.tree <- reduceTables(ttree, "tree", params = c("cp", "minbucket", "minsplit"))
sliced.tree <- sliceTables(ttree, "tree", params = c("cp", "minbucket", "minsplit"))

txgb[, is.grid := FALSE]
reduced.xgb <- reduceTables(txgb, "xgb")
sliced.xgb <- sliceTables(txgb, "xgb")

tsvm[, is.grid := FALSE]
tsvm[tsvm[, .N, by = "svm.cost"][N > 100], is.grid := TRUE, on = "svm.cost"]

reduced.svm <- reduceTables(tsvm, "svm")
sliced.svm <- sliceTables(tsvm, "svm")

tnnet[, is.grid := FALSE]
tnnet[tnnet[, .N, by = "nnet.decay"][N > 100], is.grid := TRUE, on = "nnet.decay"]
reduced.nnet <- reduceTables(tnnet, "nnet")
sliced.nnet <- sliceTables(tnnet, "nnet")

reduced.tree[is.grid == TRUE, table(config.cp) |> length()]
reduced.tree[is.grid == FALSE, table(config.cp) |> length()]
#reduced.xgb[is.grid == TRUE, table(config.xgb.subsample) |> length()]
reduced.glmnet[is.grid == FALSE, table(config.glmnet.lambda) |> length()]
reduced.glmnet[is.grid == TRUE, table(config.glmnet.lambda) |> length()]

reduced.svm[is.grid == FALSE, table(svm.cost) |> length()]
reduced.svm[is.grid == TRUE, table(svm.cost) |> length()]

reduced.nnet[is.grid == FALSE, table(nnet.size) |> length()]
reduced.nnet[is.grid == TRUE, table(nnet.size) |> length()]

scores.used = c(gc = "bbrier", cs = "bbrier", bs = "rmse", st = "rmse")

allred <- list(
  xgb = reduced.xgb,
  tree = reduced.tree,
  glmnet = reduced.glmnet,
  svm = reduced.svm,
  nnet = reduced.nnet
)

alltoeval <- sapply(names(allred), function(ln) {
  ut <- allred[[ln]]
  lapply(names(scores.used), function(su) {
    outcome <- scores.used[[su]]
    ut[taskname == su, `:=`(score = get(outcome), score.se = get(paste0(outcome, ".se")))]
  }) |> rbindlist()
}, simplify = FALSE)

torun.minima <- lapply(alltoeval, function(x) x[, .SD[which.min(score)], by = "taskname"])
# torun.minima <- lapply(alltoeval, function(x) x[taskname %in% c("cs", "st"), .SD[which.min(score)], by = "taskname"])


lapply(alltoeval, function(x) x[is.grid == FALSE, .(.SD[score <= min(score) * 1.05] |> nrow()), by = "taskname"])

set.seed(1)
torun.samples <- lapply(alltoeval, function(x) x[is.grid == FALSE, .SD[score <= min(score) * 1.05][sample(.N, min(1000, .N))], by = "taskname"])
# torun.samples <- lapply(alltoeval, function(x) x[is.grid == FALSE & taskname %in% c("cs", "st"), .SD[score <= min(score) * 1.05][sample(.N, min(1000, .N))], by = "taskname"])


lapply(names(torun.minima), function(x) torun.minima[[x]][, filename := sprintf("minmodel_%s_%s.rds", x, taskname) ])
lapply(names(torun.samples), function(x) torun.samples[[x]][, filename := sprintf("samplemodel_%s_%s_%04d.rds", x, taskname, seq_len(.N)), by = taskname])

# saveRDS(list(torun.minima = torun.minima, torun.samples = torun.samples), "data/run_models.rds")


traintasks <- lapply(list.tasks, function(x) {
  generateCanonicalDataSplits(x, ratio = 2 / 3, seed = 1)$training
})

trainLearnerFromInfoRow <- function(learnername, inforow) {
  task <- traintasks[[inforow$taskname]]
  if ("TaskClassif" %in% class(task)) {
    learnerlist <- list.learners.classif
  } else {
    learnerlist <- list.learners.regr
  }
  learner <- learnerlist[[learnername]]$clone(deep = TRUE)
  config <- inforow[, grep("config\\.", colnames(inforow), value = TRUE), with = FALSE]
  colnames(config) <- sub("^config\\.", "", colnames(config))
  config <- Filter(function(x) !is.na(x), config)
  learner$param_set$set_values(.values = as.list(config))
  learner$train(task)
  learner
}

datapath <- "data/trained_models"

#future::plan(future::multisession(), workers = 45)
#
#for (evaluating in list(torun.minima, torun.samples)) {
#  for (learnername in names(evaluating)) {
#    cat(sprintf("Training %s models\n", learnername))
#    future.apply::future_lapply(seq_len(nrow(evaluating[[learnername]])), function(i) {
#      model <- trainLearnerFromInfoRow(learnername, evaluating[[learnername]][i])
#      saveRDS(model, file = file.path(datapath, evaluating[[learnername]]$filename[[i]]))
#      NULL
#    }, future.seed = TRUE)
#  }
#}

for (evaluating in list(torun.minima, torun.samples)) {
  for (learnername in names(evaluating)) {
    cat(sprintf("Training %s models\n", learnername))
    parallel::mclapply(seq_len(nrow(evaluating[[learnername]])), function(i) {
      model <- trainLearnerFromInfoRow(learnername, evaluating[[learnername]][i])
      outfile <- file.path(datapath, evaluating[[learnername]]$filename[[i]])
      saveRDS(model, file = outfile)
      NULL
    }, mc.cores = 90, mc.preschedule = FALSE)
  }
}


runners <- 5
for (evaluating in list(torun.minima, torun.samples)) {
  for (learnername in names(evaluating)) {
    cat(sprintf("Training %s models\n", learnername))
    parallel::mclapply(rev(seq.int(runner.id + 1, to = nrow(evaluating[[learnername]]), by = runners)), function(i) {
      outfile <- file.path(datapath, evaluating[[learnername]]$filename[[i]])
      if (file.exists(outfile)) {
        cat(sprintf("Skipping %s model %d\n", learnername, i))
        return(NULL)
      }
      cat(sprintf("Training %s model %d\n", learnername, i))
      model <- trainLearnerFromInfoRow(learnername, evaluating[[learnername]][i])
      cat(sprintf("Saving %s model %d\n", learnername, i))
      saveRDS(model, file = outfile)
      NULL
    }, mc.cores = 90, mc.preschedule = FALSE)
  }
}

# simulation files

set.seed(2)
lapply(allred, function(x) x[is.grid == FALSE, .SD[sample.int(.N, min(8000, .N))], by = taskname]) -> allred.headed

allds <- lapply(allred.headed, function(x) unique(x$taskname)) |> unlist() |> unique()

allds.pivot <- sapply(allds, function(x) {
  lapply(allred.headed, function(y) {
    y[taskname == x]
  })
}, simplify = FALSE)