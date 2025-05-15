

jt.all <- getJobTable()

jt.glmnet <- jt.all[algorithm == "glmnet"]
jt.glmnet.unwrap <- unwrap(jt.glmnet)
results.glmnet <- reduceResultsList(jt.glmnet.unwrap$job.id)
results.glmnet.table <- lapply(results.glmnet, unlist, recursive = FALSE) |> rbindlist(fill = TRUE)
fulltable.glmnet <- cbind(jt.glmnet.unwrap, results.glmnet.table)

collens <- sapply(fulltable.glmnet, function(x) length(unique(x, useNA = "ifany")) == 1)

fulltable.glmnet[, (names(collens[collens == TRUE])) := NULL]
fulltable.glmnet[, job.hash := NULL]
fulltable.glmnet[, batch.id := NULL]

saveRDS(fulltable.glmnet, "data/rashomon.table.glmnet.rds")

jt.tree <- jt.all[algorithm == "tree"]
jt.tree.unwrap <- unwrap(jt.tree)
results.tree <- reduceResultsList(jt.tree.unwrap$job.id)
results.tree.table <- lapply(results.tree, unlist, recursive = FALSE) |> rbindlist(fill = TRUE)
fulltable.tree <- cbind(jt.tree.unwrap, results.tree.table)
collens <- sapply(fulltable.tree, function(x) length(unique(x, useNA = "ifany")) == 1)
fulltable.tree[, (names(collens[collens == TRUE])) := NULL]
fulltable.tree[, batch.id := NULL]
fulltable.tree[, job.hash := NULL]

saveRDS(fulltable.tree, "data/rashomon.table.tree.rds")

jt.xgb <- jt.all[algorithm == "xgb"]
jt.xgb.unwrap <- unwrap(jt.xgb)
results.xgb <- reduceResultsList(jt.xgb.unwrap[is.na(error), job.id])
results.xgb.table <- lapply(results.xgb, unlist, recursive = FALSE) |> rbindlist(fill = TRUE)
fulltable.xgb <- cbind(jt.xgb.unwrap[is.na(error)], results.xgb.table)
collens <- sapply(fulltable.xgb, function(x) length(unique(x, useNA = "ifany")) == 1)
fulltable.xgb[, (names(collens[collens == TRUE])) := NULL]
fulltable.xgb[, batch.id := NULL]
fulltable.xgb[, job.hash := NULL]

saveRDS(fulltable.xgb, "data/rashomon.table.xgb.rds")


jt.nnet <- jt.all[algorithm == "nnet"]
jt.nnet.unwrap <- unwrap(jt.nnet)
results.nnet <- reduceResultsList(jt.nnet.unwrap[!is.na(done), job.id])
results.nnet.table <- lapply(results.nnet, unlist, recursive = FALSE) |> rbindlist(fill = TRUE)
fulltable.nnet <- cbind(jt.nnet.unwrap[!is.na(done)], results.nnet.table)

collens <- sapply(fulltable.nnet, function(x) length(unique(x, useNA = "ifany")) == 1)
fulltable.nnet[, (names(collens[collens == TRUE])) := NULL]
fulltable.nnet[, batch.id := NULL]
fulltable.nnet[, job.hash := NULL]
fulltable.nnet[, log.file := NULL]
fulltable.nnet[, walltime := NULL]
fulltable.nnet[, chunks.as.arrayjobs := NULL]
fulltable.nnet[, max.concurrent.jobs := NULL]
saveRDS(fulltable.nnet, "data/rashomon.table.nnet.rds")


### svm: different registry

jt.svm <- getJobTable()
jt.svm.unwrap <- unwrap(jt.svm)
results.svm <- reduceResultsList(jt.svm.unwrap[is.na(error), job.id])
results.svm.table <- lapply(results.svm, unlist, recursive = FALSE) |> rbindlist(fill = TRUE)
fulltable.svm <- cbind(jt.svm.unwrap[is.na(error)], results.svm.table)

collens <- sapply(fulltable.svm, function(x) length(unique(x, useNA = "ifany")) == 1)
fulltable.svm[, (names(collens[collens == TRUE])) := NULL]
fulltable.svm[, batch.id := NULL]
fulltable.svm[, job.hash := NULL]
fulltable.svm[, log.file := NULL]
fulltable.svm[, clusters := NULL]
fulltable.svm[, walltime := NULL]
fulltable.svm[, account := NULL]
fulltable.svm[, partition := NULL]
fulltable.svm[, chunks.as.arrayjobs := NULL]
fulltable.svm[, max.concurrent.jobs := NULL]

saveRDS(fulltable.svm, "data/rashomon.table.svm.rds")
