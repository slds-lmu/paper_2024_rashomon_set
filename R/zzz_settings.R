
resampling.inner <- rsmp("cv", folds = 10)
resampling.reps.inner <- 10

measure.regr <- msr("regr.rmse")
measure.classif <- msr("classif.bbrier")



list.tasks <- list(
  gc = task.gc,
  cs = task.cs,
  bs = task.bs,
  st = task.st,
  cr = task.cr,
  mk = task.mk,
  bc = task.bc,
  cs.bin = task.cs.bin,
  fc = task.fc,
  fc.bin = task.fc.bin
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
  cr = task.cr,
  mk = task.mk,
  bc = task.bc,
  cs.bin = task.cs.bin,
  fc.bin = task.fc.bin
)

DEBUG <- FALSE