learner.xgb.regr.base <- lrn("regr.xgboost")
learner.xgb.regr.base$param_set$set_values(
  eta = to_tune(1e-4, 1, logscale = TRUE),
  nrounds = to_tune(1, 5000, logscale = TRUE),
  max_depth = to_tune(1, 20, logscale = TRUE),
  lambda = to_tune(1e-3, 1e3, logscale = TRUE),
  alpha = to_tune(1e-3, 1e3, logscale = TRUE),
  colsample_bytree = to_tune(.1, 1),
  colsample_bylevel = to_tune(.1, 1),
  subsample = to_tune(1e-1, 1)
)
learner.xgb.regr <- as_learner(po("encode", method = "treatment") %>>!%
  po("learner", learner.xgb.regr.base, id = "xgb"))

learner.xgb.classif.base <- lrn("classif.xgboost", predict_type = "prob")
learner.xgb.classif.base$param_set$set_values(
  eta = to_tune(1e-4, 1, logscale = TRUE),
  nrounds = to_tune(1, 5000, logscale = TRUE),
  max_depth = to_tune(1, 20, logscale = TRUE),
  lambda = to_tune(1e-3, 1e3, logscale = TRUE),
  alpha = to_tune(1e-3, 1e3, logscale = TRUE),
  colsample_bytree = to_tune(.1, 1),
  colsample_bylevel = to_tune(.1, 1),
  subsample = to_tune(1e-1, 1)
)
learner.xgb.classif <- as_learner(po("encode", method = "treatment") %>>!%
  po("learner", learner.xgb.classif.base, id = "xgb"))


learner.tree.regr <- lrn("regr.rpart")
learner.tree.regr$param_set$set_values(
  minsplit = to_tune(2, 2^7, logscale = TRUE),
  minbucket = to_tune(1, 2^6, logscale = TRUE),
  cp = to_tune(1e-4, .2, logscale = TRUE)
)

learner.tree.classif <- lrn("classif.rpart", predict_type = "prob")
learner.tree.classif$param_set$set_values(
  minsplit = to_tune(2, 2^7, logscale = TRUE),
  minbucket = to_tune(1, 2^6, logscale = TRUE),
  cp = to_tune(1e-4, .2, logscale = TRUE)
)

learner.nnet.classif <- lrn("classif.nnet", predict_type = "prob")
learner.nnet.classif$param_set$set_values(
  MaxNWts = 1000000,
  maxit = 5000,
  trace = FALSE,
  decay = to_tune(1e-6, 1, logscale = TRUE),
  size = to_tune(8, 2^9, logscale = TRUE),
  skip = to_tune()
)
learner.nnet.classif$feature_types <- union(learner.nnet.classif$feature_types, "logical")
learner.nnet.classif <- as_learner(po("scale") %>>!%
  po("learner", learner.nnet.classif, id = "nnet"))

learner.nnet.regr <- lrn("regr.nnet")
learner.nnet.regr$param_set$set_values(
  MaxNWts = 1000000,
  maxit = 5000,
  trace = FALSE,
  decay = to_tune(1e-6, 1, logscale = TRUE),
  size = to_tune(8, 2^9, logscale = TRUE),
  skip = to_tune()
)
learner.nnet.regr$feature_types <- union(learner.nnet.regr$feature_types, "logical")
learner.nnet.regr <- as_learner(po("scale") %>>!%
  po("learner", learner.nnet.regr, id = "nnet"))

learner.classif.glmnet <- lrn("classif.glmnet", predict_type = "prob")
learner.classif.glmnet$param_set$set_values(
  alpha = to_tune(0, 1),
  lambda = to_tune(p_dbl(1e-4, 1e3, logscale = TRUE))
)
learner.classif.glmnet <- as_learner(po("encode", method = "treatment") %>>!%
  po("learner", learner.classif.glmnet, id = "glmnet"))

learner.regr.glmnet <- lrn("regr.glmnet")
learner.regr.glmnet$param_set$set_values(
  alpha = to_tune(0, 1),
  lambda = to_tune(p_dbl(1e-4, 1e3, logscale = TRUE))
)
learner.regr.glmnet <- as_learner(po("encode", method = "treatment") %>>!%
  po("learner", learner.regr.glmnet, id = "glmnet"))

learner.svm.regr <- lrn("regr.svm")
learner.svm.regr$param_set$set_values(
  kernel = to_tune(c("linear", "polynomial", "radial")),
  cost = to_tune(1e-4, 1e4, logscale = TRUE),
  gamma = to_tune(1e-4, 1e4, logscale = TRUE),
  tolerance = 1e-4,
  degree = to_tune(2, 5),
  fitted = FALSE,
  type = "eps-regression"
)
learner.svm.regr <- as_learner(po("encode", method = "treatment") %>>!% po("learner", learner.svm.regr, id = "svm"))

learner.svm.classif <- lrn("classif.svm", predict_type = "prob")
learner.svm.classif$param_set$set_values(
  kernel = to_tune(c("linear", "polynomial", "radial")),
  cost = to_tune(1e-4, 1e4, logscale = TRUE),
  gamma = to_tune(1e-4, 1e4, logscale = TRUE),
  tolerance = 1e-4,
  degree = to_tune(2, 5),
  fitted = FALSE,
  type = "C-classification"
)
learner.svm.classif <- as_learner(po("encode", method = "treatment") %>>!% po("learner", learner.svm.classif, id = "svm"))

