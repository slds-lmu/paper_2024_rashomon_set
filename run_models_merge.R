run_models = readRDS("run_models.rds")
run_models_2 = readRDS("run_models_2.rds")
run_models_3 = readRDS("run_models_3.rds")

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

# check new dimensions
run_models_dims = lapply(run_models$torun.samples, function(x) table(x$taskname))
save(run_models_dims, file = "run_models_dims.RData")

saveRDS(run_models, file = "run_models_merged.rds")
