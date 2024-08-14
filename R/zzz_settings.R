

resampling.tuning.inner <- rsmp("cv", folds = 5)


list.resampling.tuning.outer <- list(
  repcv = rsmp("repeated_cv", folds = 3, repeats = 3),
  cv = rsmp("cv", folds = 10)
)

list.learners <- list(
  learner.xgb = learner.xgb,
  learner.tree = learner.tree
)
