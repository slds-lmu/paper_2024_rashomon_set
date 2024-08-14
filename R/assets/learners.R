

learner.xgb.base <- lrn("regr.xgboost")
learner.xgb.base$param_set$set_values(
  eta = to_tune(1e-4, 1, logscale = TRUE),
  nrounds = to_tune(1, 5000, logscale = TRUE),
  max_depth = to_tune(1, 20, logscale = TRUE),
  lambda = to_tune(1e-3, 1e3, logscale = TRUE),
  alpha = to_tune(1e-3, 1e3, logscale = TRUE),
  colsample_bytree = to_tune(.1, 1),
  colsample_bylevel = to_tune(.1, 1),
  subsample = to_tune(1e-1, 1)
)
learner.xgb <- as_learner(po("encode", method = "treatment") %>>!% learner.xgb.base)

learner.tree <- lrn("regr.rpart")
learner.tree$param_set$set_values(
  minsplit = to_tune(2, 2^7, logscale = TRUE),
  minbucket = to_tune(1, 2^6, logscale = TRUE),
  cp = to_tune(1e-4, .2, logscale = TRUE)
)
