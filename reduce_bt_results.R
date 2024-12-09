

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
