


traintasks <- lapply(list.tasks, function(x) {
  generateCanonicalDataSplits(x, ratio = 2 / 3, seed = 1)$training
})

datapath <- "data/trained_models"

allinfo <- readRDS("data/run_models.rds")



trainLearnerFromInfoRow <- function(learnername, inforow) {
  task <- traintasks[[inforow$taskname]]
  if ("TaskClassif" %in% class(task)) {
    learnerlist <- list.learners.classif
  } else {
    learnerlist <- list.learners.regr
  }
  learnerlist$svm.linear <- learnerlist$svm
  learnerlist$svm.radial <- learnerlist$svm
  learner <- learnerlist[[learnername]]$clone(deep = TRUE)
  config <- inforow[, grep("config\\.", colnames(inforow), value = TRUE), with = FALSE]
  colnames(config) <- sub("^config\\.", "", colnames(config))
  config <- Filter(function(x) !is.na(x), config)
  learner$param_set$set_values(.values = as.list(config))
  learner$train(task)
  learner
}
