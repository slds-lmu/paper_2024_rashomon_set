
resampling.inner <- rsmp("cv", folds = 10)
resampling.reps.inner <- 10

measure.regr <- msr("regr.rmse")
measure.classif <- msr("classif.bbrier")



list.tasks <- list(
  gc = task.gc,
  cs = task.cs,
  bs = task.bs,
  st = task.st,
  cr = task.car,
  mk = task.monk2,
  bc = task.breast.cancer,
  cs.bin = task.compas.binarized,
  fc = task.fico,
  fc.bin = task.fico.binarized
)


list.learners.regr <- list(
  xgb = learner.xgb.regr,
  tree = learner.tree.regr,
  nnet = learner.nnet.regr,
  glmnet = learner.regr.glmnet,
  svm = learner.svm.regr
)

list.learners.classif <- list(
  xgb = learner.xgb.classif,
  tree = learner.tree.classif,
  nnet = learner.nnet.classif,
  glmnet = learner.classif.glmnet,
  svm = learner.svm.classif,
  gosdt = learner.gosdt
)

list.tasks.binarized <- list(
  cr = task.car,
  mk = task.monk2,
  bc = task.breast.cancer,
  cs.bin = task.compas.binarized,
  fc.bin = task.fico.binarized
)
