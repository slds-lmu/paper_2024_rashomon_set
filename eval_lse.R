




lkm <- lrn("regr.km")



dr <- generate_design_random(list.learners.classif$xgb$param_set$search_space(), n = 10)$data

dr[, target := rnorm(nrow(dr)) * 0.1 + xgb.nrounds + xgb.eta^2]

tt <- as_task_regr(dr, target = "target")

lkm$param_set$values$nugget.estim <- TRUE

lkm$train(tt)

lkm$param_set


1 + 1
names(lkm$model)

attributes(lkm$model)

class(lkm$model@covariance)

myc <- DiceKriging::covMatrix(lkm$model@covariance, as.matrix(dr[, -"target", with = FALSE]))$C


# cholesky decomposition of myc:
chol(myc)
lkm$model@T

t(lkm$model@T) %*% lkm$model@T
myc


allds.pivot <- readRDS("data/allds.pivot.rds")


allds.pivot[[1]]$svm[, min(bbrier), by = svm.kernel][V1 <= min(V1) * 1.05]
allds.pivot[[2]]$svm[, min(bbrier), by = svm.kernel][V1 <= allds.pivot[[2]]$xgb[, min(bbrier)] * 1.05]
allds.pivot[[3]]$svm[, min(rmse), by = svm.kernel][V1 <= min(V1) * 1.05]
allds.pivot[[4]]$svm[, min(rmse), by = svm.kernel][V1 <= min(V1) * 1.05]


list.learners.meta <- lapply(list.learners.regr, function(x) x$clone(deep = TRUE))

list.learners.meta$svm$param_set$search_space()$levels$svm.kernel
list.learners.meta$svm <- NULL
list.learners.meta$svmlinear <- list.learners.regr$svm$clone(deep = TRUE)
list.learners.meta$svmlinear$param_set$set_values(svm.kernel = "linear", svm.degree = NULL, svm.gamma = NULL)
list.learners.meta$svmradial <- list.learners.regr$svm$clone(deep = TRUE)
list.learners.meta$svmradial$param_set$set_values(svm.kernel = "radial", svm.degree = NULL)
#list.learners.meta$svmpolynomial <- list.learners.regr$svm$clone(deep = TRUE)
#list.learners.meta$svmpolynomial$param_set$set_values(svm.kernel = "polynomial")

datasetid <- "st"
genseed <- 1
considered.dataset <- copy(allds.pivot[[datasetid]])
scoreby <- "rmse"

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


plot(log(seq_len(nrow(considered.dataset.merged))), considered.dataset.merged[, sort(log(score))])

ggplot(considered.dataset.merged, aes(x = log(rank(score)), y = log(score), color = learner)) +
  geom_point() +
  theme_minimal()

considered.dataset.merged[score < min(score) * 1.05]

ggplot(considered.dataset.merged[score < min(score) * 1.05, ], aes(x = log(rank(score)), y = log(score) + (learner == "svm") * 0.1, color = learner)) +
  geom_point() +
  theme_minimal()


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



baselearner <- as_learner(po("encode", method = "treatment") %>>% po("imputeoor") %>>% lrn("regr.km", predict_type = "se"))
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


indata <- osr$sample(3000)

indata <- copy(indata[learner %in% lls])

indata[, score := osr$eval(indata)]

indata <- indata[, sapply(indata, function(x) !all(is.na(x))), with = FALSE]
indata <- indata[, -".id", with = FALSE]

totry <- as_task_regr(indata, target = "score", id = "multikm")
totry2 <- as_task_regr(indata[sample.int(nrow(indata))], target = "score", id = "multikm")

part <- partition(totry, ratio = 0.5)
trainset <- part$train
testset <- part$test

lkm$train(totry, trainset)

pr <- lkm$predict(totry, testset)


# lkm$train(totry)
# pr <- lkm$predict(totry2)


ggplot(cbind(as.data.table(pr), learner = indata$learner), aes(y = log(abs(response - truth)), x = log(se), color = learner)) +
  geom_point() +
  theme_minimal() +
  geom_smooth() +
  geom_abline(slope = 1, intercept = 0)




osr <- ObjectiveStreamRecorded$new(
  id = paste0("recorded_", datasetid, "_", genseed),
  domain = considered.searchspace,
  minimize = TRUE,
  table = {
    set.seed(genseed)
    copy(considered.dataset.merged[sample.int(nrow(considered.dataset.merged))])[, score := log(score)]
  },
  scorecol = "score"
)

initsamplesize <- 10

initsample <- osr$sample(nrow(considered.dataset.merged))

evaling <- initsample[, head(.SD, initsamplesize), by = learner]

initscore <- osr$eval(evaling)

alldata <- copy(initsample)

alldata[evaling, on = ".id", score := initscore]


optim <- RashomonSamplerOptimize$new(
  id = "optim",
  domain = considered.searchspace,
  minimize = TRUE,
  rashomon.epsilon = 0.05,
  rashomon.is.relative = TRUE,
  learner = lkm,
  aqf = AqfEi(),
  search.grid.size = nrow(considered.dataset.merged),
  seed = 1
)


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

optim$askXSamples()

optim$tellXSamples(alldata, scorecol = "score")


restrace <- data.table()
for (i in 1:100) {


yv <- optim$askYValues()

result <- osr$eval(yv)

fullres <- cbind(yv, score = result)

optim$tellYValues(fullres, scorecol = "score")

restrace <- rbind(restrace, fullres)


ggplot(restrace, aes(x = seq_along(score), y = (score), color = learner)) +
  geom_point() +
  theme_minimal() + geom_hline(yintercept = log(min(considered.dataset.merged$score) * c(1, 1.05)))
  ggsave("plotoutput.pdf")

}

optim$.__enclos_env__$private$.metainfo

optim$.__enclos_env__$private$.solution.set

optim$.__enclos_env__$private$.search.grid[!is.na(.score)]
optim$.__enclos_env__$private$.search.grid[is.na(.score), table(learner)]

groundtruth <- osr$eval(alldata)

alldata.known <- copy(alldata)[, score := groundtruth]
alldata.known[, in.rashomon.set := score <= min(score) + log(1.05)]
alldata.known[, in.search.grid := .id %in% optim$.__enclos_env__$private$.search.grid$.id]
alldata.known[, table(in.rashomon.set, in.search.grid)]
alldata.known[in.search.grid == FALSE & in.rashomon.set == TRUE, table(learner)]

alldata.known[optim$.__enclos_env__$private$.search.grid[!is.na(.score), .(.id, .score)], on = ".id"][, .(score, .score)]


ggplot(considered.dataset.merged[log(score) < -1.5], aes(x = log(rank(score)), y = log(score), color = learner)) +
  geom_point() +
  theme_minimal()

initscore


ggplot(restrace, aes(x = seq_along(score), y = log(score), color = svm.kernel)) +
  geom_point() +
  theme_minimal() + geom_hline(yintercept = log(min(considered.dataset.merged$score) * c(1, 1.05)))


ggplot(considered.dataset.merged[learner %in% c("svm", "nnet")][score < 0.15], aes(x = seq_along(score), y = log(score), color = learner)) +
  geom_point() +
  theme_minimal() + geom_hline(yintercept = log(min(considered.dataset.merged$score) * c(1, 1.05)))



debugonce(optim$.__enclos_env__$private$.askYValues)



