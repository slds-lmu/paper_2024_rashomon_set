# (glmnet,tree)x(bs,gc)
run_models = readRDS("/media/external/rashomon/datafiles/model_info/run_models.rds")
# (glmnet,tree)x(cs,st)
run_models_2 = readRDS("/media/external/rashomon/datafiles/model_info/run_models_2.rds")
# (xgb) x (bs,gc, cs,st)
run_models_3 = readRDS("/media/external/rashomon/datafiles/model_info/run_models_3.rds")
# (nnet) x all
run_models_4 = readRDS("/media/external/rashomon/datafiles/model_info/run_models_nnet.rds")
# (svm) x all
run_models_5 = readRDS("/media/external/rashomon/datafiles/model_info/run_models_svm.rds")

# (glmnet,tree)x(bs,gc)
names(run_models$torun.samples)
# (glmnet,tree)x(cs,st)
names(run_models_2$torun.samples)
# (xgb) x (bs,gc, cs,st)
names(run_models_3$torun.samples)

# replace xgb in run_models by data from run_models_3
run_models$torun.samples$xgb = run_models_3$torun.samples$xgb

# merge data from run_models_2 in run_models
unique(run_models$torun.samples$tree$taskname) # data "cs" is old
# delete "cs"
run_models$torun.samples$tree <- run_models$torun.samples$tree[taskname != "cs"]
run_models$torun.samples$glmnet <- run_models$torun.samples$glmnet[taskname != "cs"]
# merge tree
tree_1 = run_models$torun.samples$tree
tree_2 = run_models_2$torun.samples$tree
merged_tree = merge(tree_1, tree_2, all = TRUE)
run_models$torun.samples$tree = merged_tree
# merge glmnet
glmnet_1 = run_models$torun.samples$glmnet
glmnet_2 = run_models_2$torun.samples$glmnet
merged_glmnet = merge(glmnet_1, glmnet_2, all = TRUE)
run_models$torun.samples$glmnet = merged_glmnet

# add nnet from run_models_4
run_models$torun.samples$nnet = run_models_4$torun.samples$nnet
# add svm from run_models_5
run_models$torun.samples$svm = run_models_5$torun.samples$svm

# check new dimensions
run_models_dims = lapply(run_models$torun.samples, function(x) table(x$taskname))
save(run_models_dims, file = "data/run_models_dims.RData")

saveRDS(run_models, file = "data/run_models_merged.rds")
