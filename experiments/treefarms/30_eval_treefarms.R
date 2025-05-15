

treefarms.info <- fread("data/treefarms/treefarms_info.csv")

# inspect this to see what we have:
treefarms.info

# example model

model.id <- treefarms.info[taskname == "bc" & offset == 0.05 & balance == FALSE, job.id]

# need at least 64 GB for some of the models:
model <- readRDS(sprintf("data/treefarms/treefarms_%s.rds", model.id))

try(model$modelcontainer)  # sometimes fails on the first evaluation b/c of reticulate


# number of total trees found:
model$tree.count

# getting a random tree:
# note this is a string! do not convert it to numeric / integer, since it may be too large for R to handle
model$sampleTreeIndex()

# activating a specific, random tree:

set.seed(1)
model$param_set$values$selected_tree <- model$sampleTreeIndex()

# predict on the validation set
fulltask <- list.tasks.binarized$bc
valset <- generateCanonicalDataSplits(fulltask)$validation


preds <- model$predict(valset)

# classification error:
preds$score()


# example:
# get 100 predictions:

set.seed(2)
preds <- replicate(100, {
  model$param_set$values$selected_tree <- model$sampleTreeIndex()
  preds <- model$predict(valset)
  preds$score()
})

# plot the distribution of false predictions:
plot(table(preds * valset$nrow))
