
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 3) {
  stop("Please provide a path to the dataset id, the seed, and the optimizer as arguments")
}

source("init.R")
datasetid <- checkmate::assertChoice(args[1], c("st", "bs", "cs", "gc"))
genseed <- checkmate::assertInt(as.integer(args[2]), lower = 1, upper = 1000000)
optimizer <- checkmate::assertChoice(args[3], c("lse", "straddle", "truvarimp", "maxsd"))



cat(sprintf("datasetid: %s, genseed: %d, optimizer: %s\n", datasetid, genseed, optimizer))

allds.pivot <- readRDS("data/allds.pivot.rds")

list.learners.meta <- lapply(list.learners.regr, function(x) x$clone(deep = TRUE))
# list.learners.meta$svm$param_set$search_space()$levels$svm.kernel
list.learners.meta$svm <- NULL
list.learners.meta$svmlinear <- list.learners.regr$svm$clone(deep = TRUE)
list.learners.meta$svmlinear$param_set$set_values(svm.kernel = "linear", svm.degree = NULL, svm.gamma = NULL)
list.learners.meta$svmradial <- list.learners.regr$svm$clone(deep = TRUE)
list.learners.meta$svmradial$param_set$set_values(svm.kernel = "radial", svm.degree = NULL)
#list.learners.meta$svmpolynomial <- list.learners.regr$svm$clone(deep = TRUE)
#list.learners.meta$svmpolynomial$param_set$set_values(svm.kernel = "polynomial")

considered.dataset <- copy(allds.pivot[[datasetid]])
scoreby <- c(gc = "bbrier", cs = "bbrier", bs = "rmse", st = "rmse")[[datasetid]]

lapply(names(list.learners.meta), function(x) {
  ss <- list.learners.meta[[x]]$param_set$search_space()$ids()
  if (x %like% "^svm") {
    ds <- copy(considered.dataset[["svm"]])
    ds <- ds[svm.kernel == sub("^svm", "", x), ]
    ds <- ds[, c(ss, scoreby), with = FALSE]
  } else  {
    ds <- copy(considered.dataset[[x]][, c(ss, scoreby), with = FALSE])
  }
  colnames(ds) <- paste0(x, ".", colnames(ds))

  ds[, learner := x]
  ds[, score := get(paste0(x, ".", scoreby))]
  ds[, -paste0(x, ".", scoreby), with = FALSE]
}) |> rbindlist(use.names = TRUE, fill = TRUE) -> considered.dataset.merged


considered.searchspace <- ps_union(c(
  lapply(list.learners.meta, function(x) x$param_set$search_space()),
  ps(learner = p_fct(levels = names(list.learners.meta)))
))


# considered.dataset.merged[score < min(score) * 1.05]

osr <- ObjectiveStreamRecorded$new(
  id = paste0("recorded_", datasetid, "_", genseed),
  domain = considered.searchspace,
  minimize = TRUE,
  table = {
    set.seed(genseed)
    considered.dataset.merged[sample.int(nrow(considered.dataset.merged))]
  },
  scorecol = "score"
)



# osr$sample(10)

lrx <- lrn("regr.km", predict_type = "se")
lrx$encapsulate(method = "try", lrn("regr.featureless", predict_type = "se"))

baselearner <- as_learner(po("encode", method = "treatment") %>>% po("imputeoor") %>>% lrx)
baselearner$param_set$values$regr.km.nugget.estim <- TRUE
baselearner$param_set$values$regr.km.jitter <- 0.0001

LearnerRegrMultiKm <- R6Class("LearnerRegrMultiKm",
  inherit = LearnerRegr,
  public = list(
    learnerlevels = NULL,
    subsets = NULL,
    baselearner = NULL,
    initialize = function(learnerlevels, subsets) {
      assertCharacter(learnerlevels, any.missing = FALSE, unique = TRUE, min.len = 1)
      self$learnerlevels <- learnerlevels
      self$subsets <- subsets
      assertNames(names(subsets), permutation.of = learnerlevels)
      self$baselearner <- baselearner$clone(deep = TRUE)
      self$baselearner$predict_type <- "se"
      super$initialize("regr.multikm", packages = "DiceKriging",
        feature_types = c("numeric", "integer", "logical", "factor"),
        param_set = baselearner$param_set$clone(deep = TRUE),
        properties = c(baselearner$properties, "missings"),
        predict_types = baselearner$predict_types,
        label = "Multi-Kriging"
      )
    }
  ),
  private = list(
    .train = function(task) {
      pars <- self$param_set$values
      sapply(self$learnerlevels, function(x) {
        subtask <- task$clone(deep = TRUE)
        interestingrows <- subtask$row_ids[subtask$data(cols = "learner") == x]
        subtask$filter(rows = interestingrows)$select(self$subsets[[x]])
        self$baselearner$clone(deep = TRUE)$train(subtask)
      }, simplify = FALSE)
    },
    .predict = function(task) {
      rowids <- task$row_ids
      indiv.preds <- sapply(names(self$model), function(x) {
        subtask <- task$clone(deep = TRUE)
        interestingrows <- subtask$row_ids[subtask$data(cols = "learner") == x]
        subtask$filter(rows = interestingrows)$select(self$subsets[[x]])
        self$model[[x]]$predict(subtask)
      }, simplify = FALSE)
      allpreds <- lapply(indiv.preds, as.data.table) |> rbindlist()
      allpreds <- allpreds[J(rowids), on = "row_ids"]
      as.list(allpreds)[c("response", "se")]
    }
  )
)


lls <- names(list.learners.meta)
lkm <- LearnerRegrMultiKm$new(
  learnerlevels = lls,
  subsets = sapply(lls, function(x) paste0(x, ".", list.learners.meta[[x]]$param_set$search_space()$ids()))
)

# uninformedscore <- considered.dataset.merged[, .N, by = .(score = round(score, 2))][which.max(N), score]
uninformedscore <- c(bs = 181.4, st = 1.753, cs = 0.5, gc = 0.42)[[datasetid]]

osr <- ObjectiveStreamRecorded$new(
  id = paste0("recorded_", datasetid, "_", genseed),
  domain = considered.searchspace,
  minimize = TRUE,
  table = {
    set.seed(genseed)
    copy(considered.dataset.merged[sample.int(nrow(considered.dataset.merged))])[, score := log(pmin(uninformedscore * 2, score))]
  },
  scorecol = "score"
)

initsamplesize <- 10

initsample <- osr$sample(nrow(considered.dataset.merged))

evaling <- initsample[, head(.SD, initsamplesize), by = learner]

initscore <- osr$eval(evaling)

alldata <- copy(initsample)

alldata[evaling, on = ".id", score := initscore]


if (optimizer == "lse") {
  optim <- RashomonSamplerLSEImplicit$new(
    id = "optim",
    domain = considered.searchspace,
    minimize = TRUE,
    rashomon.epsilon = log(1.05),
    rashomon.is.relative = FALSE,
    learner = lkm,
    search.grid.size = nrow(considered.dataset.merged),
    seed = 1,
    lse.beta = 9,
    lse.epsilon = 0.0001,
    n.rashomon.samples = 1000
  )
} else if (optimizer == "straddle") {
  optim <- RashomonSamplerStraddle$new(
    id = "optim",
    domain = considered.searchspace,
    minimize = TRUE,
    rashomon.epsilon = log(1.05),
    rashomon.is.relative = FALSE,
    learner = lkm,
    search.grid.size = nrow(considered.dataset.merged),
    seed = 1
  )
} else if (optimizer == "truvarimp") {
  cspace <- conjoinSpaces(lapply(list.learners.meta, function(x) x$param_set$search_space()), choice.param.name = "learner")
  learner <- LearnerRegrKMExtraConjoined$new(cspace)
  learner$predict_type <- "se"
  learner$baselearner$predict_type <- "se"
  learner$baselearner$encapsulate(method = "try", lrn("regr.featureless", predict_type = "se"))
  # learner <- as_learner(po("encode", method = "treatment", affect_columns = selector_name("nnet.nnet.skip")) %>>% learner)
  optim <- RashomonSamplerTruVaRImp$new(
    id = "optim",
    domain = considered.searchspace,
    minimize = TRUE,
    rashomon.epsilon = log(1.05),
    rashomon.is.relative = FALSE,
    learner = learner,
    search.grid.size = nrow(considered.dataset.merged),
    seed = 1,
    beta = 9,
    delta.bar = 0,
    r = 0.1,
    eta = 1,
    n.rashomon.samples = 1000
  )
} else if (optimizer == "maxsd") {
  optim <- RashomonSamplerOptimize$new(
    id = "optim",
    domain = considered.searchspace,
    minimize = TRUE,
    rashomon.epsilon = log(1.05),
    rashomon.is.relative = FALSE,
    learner = lkm,
    aqf = AqfSd(),
    search.grid.size = nrow(considered.dataset.merged),
    seed = 1
  )
}

invisible(optim$askXSamples())
if (optimizer == "truvarimp") {
  alldata[, nnet.nnet.skip := as.numeric(nnet.nnet.skip)]
}
invisible(optim$tellXSamples(alldata, scorecol = "score"))




restrace <- data.table()
ist <- TRUE
repeat {
  capture.output(yv <- optim$askYValues())

  result <- osr$eval(yv)

  fullres <- cbind(yv, score = result)

  write.table(fullres, file = stdout(), row.names = FALSE, col.names = ist, sep = ",")
  ist <- FALSE

  optim$tellYValues(fullres, scorecol = "score")

  restrace <- rbind(restrace, fullres)
}

