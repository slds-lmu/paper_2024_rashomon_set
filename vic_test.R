source("init.R")

library(iml)
library(ggplot2)
library(tidyr)
library(GGally)
library(patchwork)


## General settings ############################################################
task.keys = names(list.tasks) # german credit, compas, bike sharing, synthetic
learner.keys = c("glmnet", "xgb") # c("tree", "glmnet", "xgb")
no.models = 50 # number of models within Rashomon set 100
perm.reps = 10 # number of permutations in PFI


## Preliminaries ###############################################################
# Logical features cannot be handled by Predictor$new() in the iml package.
# Fix this by changing logicals to factors.
for(task.key in task.keys){
  if("logical" %in% list.tasks[[task.key]]$feature_types$type){
    data = as.data.frame(list.tasks[[task.key]]$data())
    id = list.tasks[[task.key]]$id
    target = list.tasks[[task.key]]$target_names
    for(i in 1:ncol(data)){
      if(class(data[,i]) == "logical") data[,i] = as.factor(data[,i])
    }
    list.tasks[[task.key]] = as_task_regr(data, target = target, id = id)
  }
}

task = list.tasks[[task.key]]

## Extra functions #############################################################

# calculate the PFI values for a specific model for each feature
calculate_pfi = function(task, model, seed, permutation.repetitions){
  set.seed(seed)
  X = task$data(cols = task$feature_names)
  if(model$task_type == "regr"){
    y = task$data(cols = task$target_names)
    predictor = Predictor$new(model = model, data = X, y = y)
    # PFI via ratio (default). Alternative: compare = "difference"
    FeatureImp$new(predictor, loss = "rmse", compare = "difference",
                   n.repetitions = permutation.repetitions)
  } else if (model$task_type == "classif") {  # only binary targets
    y = task$data(cols = task$target_names)[[1]]
    y = ifelse(y == task$positive, 1, 0)
    # Define a custom prediction function that returns class labels (not probabilities)
    predict_function_class = function(model, newdata) {
      model$predict_newdata(newdata)$prob[,1]
    }
    predictor = Predictor$new(model = model, data = X, y = y, predict.function = predict_function_class)
    FeatureImp$new(predictor, loss = "mse", compare = "difference",
                   n.repetitions = permutation.repetitions)
  } else {
    stop("Unsupported task type")
  }
}

# General LOCO implementation
# loco = function(task, learner, resampling, measure) {
#   if (!resampling$is_instantiated)
#     resampling$instantiate(task)
#   fnames = task$feature_names
#   p = length(fnames)
#   res = numeric(p); names(res) = fnames
#   rr0 = resample(task = task, learner = learner, resampling = resampling)
#   v0 = rr0$aggregate(measure)
#   for (j in 1:p) {
#     fnames2 = fnames[-j]
#     task2 = task$clone()$select(fnames2)
#     rr = resample(task = task2, learner = learner, resampling = resampling)
#     res[j] = rr$aggregate(measure) - v0
#   }
#   return(res)
# }

# LOCO for our use case
# calculate_loco = function(task, model, seed, permutation.repetitions){
#   set.seed(seed)
#   loss = ifelse(model$task_type == "regr", "regr.rmse", "classif.ce")
#   msr = msr(loss)
#   loco(task = task, learner = model, measure = msr,
#        resampling = rsmp("subsampling", ratio = 0.7,
#                          repeats = permutation.repetitions))
# }


## Read in data ################################################################

# read in models
models = list()
for(task.key in task.keys){
  models[[task.key]] = list()
  tmp = 0
  for(learner.key in learner.keys){
    for(i in 1:no.models){
      if(i<10) name = paste0("trained_models/samplemodel_", learner.key, "_", task.key, "_000",i,".rds")
      else if(i<100) name = paste0("trained_models/samplemodel_", learner.key, "_", task.key, "_00",i,".rds")
      else if(i<1000) name = paste0("trained_models/samplemodel_", learner.key, "_", task.key, "_0",i,".rds")
      else name = paste0("trained_models/samplemodel_", learner.key, "_", task.key, "_",i,".rds")
      models[[task.key]][[tmp*no.models+i]] = readRDS(name)
    }
    tmp = tmp+1
  }
}
rm(tmp)

# read in AutoML information on models
run_models = readRDS("run_models_merged.rds")
# save performance for considered models
performance = list()
for(task.key in task.keys){
  for(learner.key in learner.keys){
    dt = run_models$torun.samples[[learner.key]]
    if(rlang::is_empty(performance[[task.key]])){
      performance[[task.key]] = dt[taskname == task.key]$score[1:no.models]
    } else {
      performance[[task.key]] = c(performance[[task.key]],
                                        dt[taskname == task.key,]$score[1:no.models])
    }
  }
}


# Further fix problem with logical features
if("bs" %in% task.keys){
  holiday.special <- ppl("convert_types", "factor", "logical", selector_name(c("holiday", "working_day")), id = "holiday.special")
  invisible(holiday.special$train(list.tasks$bs))
  models$bs <- lapply(models$bs, function(x) {
    xstate <- x$state
    gr <- holiday.special$clone(deep = TRUE) %>>% x$clone(deep = TRUE)
    lr <- as_learner(gr$clone(deep = TRUE))
    lr$state <- xstate
    lr$state$train_task <- list.tasks$bs$clone(deep = TRUE)$filter(0)
    lr$model <- gr$state
    lr$model[[gr$ids()[[2]]]] <- xstate
    lr
  })
}


## Importance Analysis #########################################################

####### PFI #######
vic = list()
timestamp()
for(task.key in task.keys){
  task = list.tasks[[task.key]]
  task_test = generateCanonicalDataSplits(task, ratio = 2 / 3, seed = 1)$validation
  list.pfi = lapply(models[[task.key]], calculate_pfi, task = task_test,
                    seed = 1, permutation.repetitions = perm.reps)
  vic[[task.key]] = data.frame(feature = list.pfi[[1]]$results$feature)
  for(i in 1:(no.models*length(learner.keys))){
    vic[[task.key]] =
      merge(vic[[task.key]],
            list.pfi[[i]]$results[,c("feature","importance")],
            by = "feature")
  }
  colnames(vic[[task.key]]) = c("feature",
                                paste0("pfi_",
                                       rep(learner.keys, each = no.models),
                                       "_m",
                                       rep(1:no.models, length(learner.keys))))
  timestamp()
  print(paste0("Status: ", task.key, " (", which(task.keys == task.key),
               " / ", length(task.keys), ") done"))
}

# max importance = 1
vic_normalized = list()
for(task.key in task.keys){
  vic_normalized[[task.key]] = vic[[task.key]]
  names = names(vic[[task.key]])
  max_per_model = apply(vic_normalized[[task.key]][,-1], 2, max)
  for(i in 1:length(max_per_model)){
    vic_normalized[[task.key]][,i+1] = vic_normalized[[task.key]][,i+1]/max_per_model[i]
  }
}

####### LOCO #######

# vic_loco = list()
# for(task.key in task.keys){
#   task = list.tasks[[task.key]]
#   task_test = generateCanonicalDataSplits(task, ratio = 2 / 3, seed = 1)$validation
#   list.loco = lapply(models[[task.key]], calculate_loco, task = task_test,
#                     seed = 1, permutation.repetitions = perm.reps)
#   vic_loco[[task.key]] = cbind(list.loco[[1]], list.loco[[2]])
#   for(i in 3:(no.models*length(learner.keys))){
#     vic_loco[[task.key]] = cbind(vic_loco[[task.key]], list.loco[[i]])
#   }
#   vic_loco[[task.key]] = data.frame(feature = rownames(vic_loco[[task.key]]), vic_loco[[task.key]])
#   rownames(vic_loco[[task.key]]) = NULL
#   colnames(vic_loco[[task.key]]) = c("feature", paste0("loco", 1:(no.models*length(learner.keys))))
#   print(timestamp())
#   print(paste0("Status: ", task.key, " (", which(task.keys == task.key),
#                " / ", length(task.keys), ") done"))
# }


## plots #######################################################################
vic_long = list()
vic_wide = list()
plots = list()
for(task.key in task.keys){
  vic_long[[task.key]] = vic[[task.key]] %>%
    pivot_longer(cols = starts_with("pfi"), names_to = "PFI", values_to = "Value")
  vic_long[[task.key]]$learner = rep(rep(learner.keys, each = no.models),
                                   times = length(vic_long[[task.key]]$feature)/(no.models*length(learner.keys)))
  vic_long[[task.key]]$performance = rep(performance[[task.key]],
                                         times = length(vic_long[[task.key]]$feature)/(no.models*length(learner.keys)))
  vic_wide[[task.key]] = vic_long[[task.key]] %>%
    pivot_wider(names_from = feature, values_from = Value)


  # Plot 1: Scatter-plot using jitter colored acc. to model class
  plot1 = ggplot(vic_long[[task.key]], aes(x = Value, y = feature,
                                           color = performance)) +
    geom_jitter(width = 0, height = 0.35, alpha = 0.7, size = 1) +
    scale_color_gradient(low = "blue", high = "red") +
    labs(x = "Importance", y = "Feature",
         title = paste0("PFI values (", task.key, ") colored by performance")) +
    theme_minimal()

  # Plot 2: Scatter-plot using jitter colored acc. to model class
  plot2 = ggplot(vic_long[[task.key]], aes(x = Value, y = feature,
                                           color = learner)) +
    geom_jitter(width = 0, height = 0.35, alpha = 0.3, size = 1) +
    labs(x = "Importance", y = "Feature",
         title = paste0("PFI values (", task.key, ") colored by learner")) +
    theme_minimal()

  # Plot 3: Box plot
  plot3 = ggplot(vic_long[[task.key]], aes(x = feature, y = Value)) +
    geom_boxplot() + coord_flip() +
    labs(x = "Importance", y = "Feature", title = paste("PFI values:", task.key)) +
    theme_minimal()

  # Plot 4: Pairwise Plots
  plot4 = ggpairs(vic_wide[[task.key]][, -c(1,2)],
                  title = paste("Pairwise Comparison:", task.key),
                  ggplot2::aes(colour = model, alpha = 0.3))


  plots[[task.key]] = list()
  plots[[task.key]][["performance_scatter_plot"]] = plot1
  plots[[task.key]][["scatter_plot"]] = plot2
  plots[[task.key]][["box_plot"]] = plot3
  plots[[task.key]][["pairwise_comparison"]] = plot4
}

## Plot 5: Pairwise Plots reproducing results of Dong & Rudin (2020) for cs
# extract four most important features (in D&R: age, race, prior, gender)
tmp_df = data.frame(feature = vic_normalized$cs$feature,
                    mean_pfi = apply(vic_normalized$cs[,-1], 1, mean))
top4_features = tmp_df[order(-tmp_df$mean_pfi), ][1:4, ]
plot5 = ggpairs(vic_wide[[task.key]][c(top4_features$feature, "learner")],
                title = paste("Pairwise Comparison:", task.key),
                ggplot2::aes(colour = learner, alpha = 0.3))
plots$cs[["pairwise_comparison_top4_features"]] = plot5
rm(tmp_df, top4_features)

# vic_normalized
vic_scaled_long = list()
vic_scaled_wide = list()
plots_scaled = list()
for(task.key in task.keys){
  vic_scaled_long[[task.key]] = vic_normalized[[task.key]] %>%
    pivot_longer(cols = starts_with("pfi"), names_to = "PFI", values_to = "Value")
  vic_scaled_long[[task.key]]$learner = rep(rep(learner.keys, each = no.models),
                                   times = length(vic_scaled_long[[task.key]]$feature)/(no.models*length(learner.keys)))
  vic_scaled_long[[task.key]]$performance = rep(performance[[task.key]],
                                                times = length(vic_scaled_long[[task.key]]$feature)/(no.models*length(learner.keys)))
  vic_scaled_wide[[task.key]] = vic_scaled_long[[task.key]] %>%
    pivot_wider(names_from = feature, values_from = Value)


  # Plot 1: Scatter-plot using jitter colored acc. to performance
  plot1 = ggplot(vic_scaled_long[[task.key]], aes(x = Value, y = feature,
                                           color = performance)) +
    geom_jitter(width = 0, height = 0.35, alpha = 0.3, size = 1) +
    scale_color_gradient(low = "blue", high = "red") +
    labs(x = "Importance", y = "Feature",
         title = paste0("PFI values (", task.key, ", max importance = 1) colored by performance")) +
    theme_minimal()

  # Plot 2: Scatter-plot using jitter colored acc. to model class
  plot2 = ggplot(vic_scaled_long[[task.key]], aes(x = Value, y = feature,
                                           color = learner)) +
    geom_jitter(width = 0, height = 0.35, alpha = 0.3, size = 1) +
    labs(x = "Importance", y = "Feature",
         title = paste0("PFI values (", task.key, ", max importance = 1) colored by learner")) +
    theme_minimal()

  # Plot 3: Box plot
  plot3 = ggplot(vic_scaled_long[[task.key]]) +
    geom_boxplot(aes(x = feature, y = Value), fill = "gray") +
    geom_boxplot(aes(x = feature, y = Value, fill = learner, alpha = 0.5)) +
    coord_flip() +
    labs(x = "Importance", y = "Feature", title = paste("PFI values (max importance = 1):", task.key)) +
    theme_minimal()

  # Plot 4: Pairwise Plots
  plot4 = ggpairs(vic_scaled_wide[[task.key]][, -c(1,2)],
                  title = paste("Pairwise Comparison (max importance = 1):", task.key),
                  ggplot2::aes(colour = model, alpha = 0.3))

  plots_scaled[[task.key]] = list()
  plots_scaled[[task.key]][["performance_scatter_plot"]] = plot1
  plots_scaled[[task.key]][["scatter_plot"]] = plot2
  plots_scaled[[task.key]][["box_plot"]] = plot3
  plots_scaled[[task.key]][["pairwise_comparison"]] = plot4
}

## Plot 5: Pairwise Plots reproducing results of Dong & Rudin (2020) for cs
# extract four most important features (in D&R: age, race, prior, gender)
tmp_df = data.frame(feature = vic_normalized$cs$feature,
                    mean_pfi = apply(vic_normalized$cs[,-1], 1, mean))
top4_features = tmp_df[order(-tmp_df$mean_pfi), ][1:4, ]
plot5 = ggpairs(vic_wide[[task.key]][c(top4_features$feature, "learner")],
                title = paste("Pairwise Comparison:", task.key),
                ggplot2::aes(colour = learner, alpha = 0.3))
plots_scaled$cs[["pairwise_comparison_top4_features"]] = plot5
rm(tmp_df, top4_features)


## Save  #######################################################################
# save R
# name = paste0("results/models_top_", no.models, ".RData")
# save(models, file = name)
# save(vic, file = "results/vic_pfi.RData")
# save(vic_normalized, file = "results/vic_pfi_scaled.RData")
# # save(vic_loco, file = "results/vic_loco.RData")
# save(plots, file = "results/plots.RData")
# save(plots_scaled, file = "results/plots_scaled.RData")

# save plots
for(task.key in task.keys){
  # scatter performance
  name = paste0("figures/pfi_values_", task.key, "_scatter_performance.pdf")
  ggsave(name, plots[[task.key]][[1]], width = 10, height = 5)
  name = paste0("figures/pfi_scaled_values_", task.key, "_scatter_performance.pdf")
  ggsave(name, plots_scaled[[task.key]][[1]], width = 10, height = 5)
  # scatter model
  name = paste0("figures/pfi_values_", task.key, "_scatter.pdf")
  ggsave(name, plots[[task.key]][[2]], width = 10, height = 5)
  name = paste0("figures/pfi_scaled_values_", task.key, "_scatter.pdf")
  ggsave(name, plots_scaled[[task.key]][[2]], width = 10, height = 5)
  # boxplot
  name = paste0("figures/pfi_values_", task.key, "_boxPlot.pdf")
  ggsave(name, plots[[task.key]][[3]], width = 10, height = 5)
  name = paste0("figures/pfi_scaled_values_", task.key, "_boxPlot.pdf")
  ggsave(name, plots_scaled[[task.key]][[3]], width = 10, height = 5)
  # pairwise
  # name = paste0("figures/pfi_values_", task.key, "_pairwise.pdf")
  # ggsave(name, plots[[task.key]][[4]], width = 25, height = 12.5)
  # name = paste0("figures/pfi_scaled_values_", task.key, "_pairwise.pdf")
  # ggsave(name, plots_scaled[[task.key]][[4]], width = 25, height = 12.5)
}
# pairwise compare compas with Dong & Rudin
name = paste0("figures/pfi_values_", task.key, "_pairwise_dong.pdf")
ggsave(name, plots$cs[["pairwise_comparison_top4_features"]], width = 25, height = 12.5)
name = paste0("figures/pfi_scaled_values_", task.key, "_pairwise_dong.pdf")
ggsave(name, plots_scaled$cs[["pairwise_comparison_top4_features"]], width = 25, height = 12.5)
