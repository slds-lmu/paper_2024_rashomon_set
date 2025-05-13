


rm(list = ls(all.names = TRUE))
source("init.R")


reduceJobTable <- function(jt, algorithm) {
  assertString(algorithm)
  assertDataTable(jt)
  jt.subset <- jt[algorithm, on = "algorithm"]
  jt.subset.unwrap <- unwrap(jt.subset)
  results <- reduceResultsList(jt.subset.unwrap[!is.na(done) & is.na(error), job.id])
  results.table <- lapply(results, unlist, recursive = FALSE) |> rbindlist(fill = TRUE)
  fulltable <- cbind(jt.subset.unwrap[!is.na(done) & is.na(error)], results.table)
  collens <- sapply(fulltable, function(x) length(unique(x, useNA = "ifany")) == 1)
  fulltable[, (names(collens[collens == TRUE])) := NULL]
  fulltable[, batch.id := NULL]
  fulltable[, job.hash := NULL]
  fulltable[, log.file := NULL]
  fulltable[, clusters := NULL]
  fulltable[, walltime := NULL]
  fulltable[, account := NULL]
  fulltable[, partition := NULL]
  fulltable[, chunks.as.arrayjobs := NULL]
  fulltable[, max.concurrent.jobs := NULL]
  fulltable
}

reg <- getRegistry("../registry_paper_2024_rashomon_set", make.default = TRUE)
jt.all <- getJobTable(reg = reg)
for (algorithm in c("glmnet", "tree", "xgb", "nnet")) {
  saveRDS(reduceJobTable(jt.all, algorithm), sprintf("data/rashomon.table.%s.rds", algorithm))
}

reg.svm.primary <- getRegistry("../registry_paper_2024_rashomon_set_svm", make.default = TRUE)
jt.svm <- getJobTable(reg = reg.svm.primary)
saveRDS(reduceJobTable(jt.svm, "svm"), "data/rashomon.table.svm.rds")

reg.glmnet <- getRegistry("../registry_paper_2024_rashomon_set_glmnet/registry3", make.default = TRUE)
jt.glmnet <- getJobTable(reg = reg.glmnet)
saveRDS(reduceJobTable(jt.glmnet, "glmnet"), "data/rashomon.table.glmnet_2.rds")

reg.tree <- getRegistry("../registry_paper_2024_rashomon_set_tree/registry3", make.default = TRUE)
jt.tree <- getJobTable(reg = reg.tree)
saveRDS(reduceJobTable(jt.tree, "tree"), "data/rashomon.table.tree_2.rds")

reg.nnet <- getRegistry("../registry_paper_2024_rashomon_set_nnet/registry4", make.default = TRUE)
jt.nnet <- getJobTable(reg = reg.nnet)
saveRDS(reduceJobTable(jt.nnet, "nnet"), "data/rashomon.table.nnet_2.rds")

reg.xgb <- getRegistry("../registry_paper_2024_rashomon_set_xgb/registry4", make.default = TRUE)
jt.xgb <- getJobTable(reg = reg.xgb)
saveRDS(reduceJobTable(jt.xgb, "xgb"), "data/rashomon.table.xgb_2.rds")

reg.gosdt <- getRegistry("../registry_paper_2024_rashomon_set_gosdt/registry5", make.default = TRUE)
jt.gosdt <- getJobTable(reg = reg.gosdt)
saveRDS(reduceJobTable(jt.gosdt, "gosdt"), "data/rashomon.table.gosdt_2.rds")

reg.svm <- getRegistry("../registry_paper_2024_rashomon_set_svm/registry4", make.default = TRUE)
jt.svm <- getJobTable(reg = reg.svm)
saveRDS(reduceJobTable(jt.svm, "svm"), "data/rashomon.table.svm_2.rds")
