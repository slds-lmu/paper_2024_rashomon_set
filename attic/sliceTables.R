
## ~ production_models.R
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

sliced.glmnet <- sliceTables(tglmnet, "glmnet")
sliced.tree <- sliceTables(ttree, "tree", params = c("cp", "minbucket", "minsplit"))
sliced.xgb <- sliceTables(txgb, "xgb")
sliced.svm <- sliceTables(tsvm, "svm")
sliced.nnet <- sliceTables(tnnet, "nnet")
