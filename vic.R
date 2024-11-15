source("init.R")

library(batchtools)
library(ggplot2)
library(tidyr)
library(data.table)


# Registry
regr = makeExperimentRegistry(file.dir = NA,
                              source = "init.R",
                              packages = c("iml", "GGally", "patchwork")
)

# Define Cluster-Configurations
regr$cluster.functions = makeClusterFunctionsSocket(ncpus = 18)

# Define Problem
addProblem("fromlist", fun = function(data, job, taskname) {
  task = list.tasks[[taskname]]

  # Fix logical features (for FeatureImp)
  if(taskname == "bs"){
    task_data = as.data.frame(task$data())
    task_id = task$id
    task_target = task$target_names
    for(i in seq_along(task_data)) {
      if (is.logical(task_data[[i]])) task_data[[i]] = as.factor(task_data[[i]])
    }
    task = as_task_regr(task_data, target = task_target, id = task_id)
  }

  # Return of the validation split
  generateCanonicalDataSplits(task, ratio = 2 / 3, seed = 1)$validation
})

# Define Algorithm for VIC calculation based on PFI
addAlgorithm("calculate_vic_pfi", fun = function(data, instance, job, learnername, model.no) {
  # browser()
  name = sprintf("/media/external/rashomon/datafiles/%s/%s/samplemodel_%s_%s_%04d.rds",
                 job$pars$prob.pars$taskname, learnername, learnername, job$pars$prob.pars$taskname, model.no)
  model = readRDS(name)

  # Fix models in case of task bs (logical features)
  if(job$pars$prob.pars$taskname == "bs"){
    # fix model
    holiday.special = ppl("convert_types", "factor", "logical", selector_name(c("holiday", "working_day")), id = "holiday.special")
    invisible(holiday.special$train(instance)) # list.tasks$bs))
    xstate = model$state
    gr = holiday.special$clone(deep = TRUE) %>>% model$clone(deep = TRUE)
    lr = as_learner(gr$clone(deep = TRUE))
    lr$state = xstate
    lr$state$train_task = instance$clone(deep = TRUE)$filter(0)
    lr$model = gr$state
    lr$model[[gr$ids()[[2]]]] = xstate
    model = lr
    rm(gr, lr, xstate, holiday.special)
  }

  # Function calculating PFI
  calculate_pfi = function(task, model, seed, perm.reps){
    set.seed(seed)
    X = task$data(cols = task$feature_names)
    if(model$task_type == "regr"){
      y = task$data(cols = task$target_names)
      predictor = Predictor$new(model = model, data = X, y = y)
      # PFI via ratio (default). Alternative: compare = "difference"
      FeatureImp$new(predictor, loss = "rmse", compare = "difference",
                     n.repetitions = perm.reps)
    } else if (model$task_type == "classif") {  # only binary targets
      y = task$data(cols = task$target_names)[[1]]
      y = ifelse(y == task$positive, 1, 0)
      # Define a custom prediction function that returns class labels (not probabilities)
      predict_function_class = function(model, newdata) {
        model$predict_newdata(newdata)$prob[,1]
      }
      predictor = Predictor$new(model = model, data = X, y = y, predict.function = predict_function_class)
      FeatureImp$new(predictor, loss = "mse", compare = "difference",
                     n.repetitions = perm.reps)
    } else {
      stop("Unsupported task type")
    }
  }

  # Calculate VIC
  calculate_pfi(task = instance, model = model, seed = 1, perm.reps = 10)
  # vic = data.frame(feature = list.pfi[[1]]$results$feature)
  # for(i in 1:no.models) {
  #   vic = merge(vic,
  #                list.pfi[[i]]$results[,c("feature", "importance")],
  #                by = "feature")
  # }
  # colnames(vic) = c("feature",
  #                    paste0("pfi_", rep(learnername, no.models),
  #                           "_m", 1:no.models))
  #
  # # Normalize VIC
  # vic_normalized = vic
  # max_per_model = apply(vic_normalized[,-1], 2, max)
  # for(i in 1:length(max_per_model)){
  #   vic_normalized[,i+1] = vic_normalized[,i+1]/max_per_model[i]
  # }
  #
  # output = list(vic, vic_normalized)
  # names(output) = c(paste0("vic_", job$pars$prob.pars$taskname, "_", learnername),
  #                   paste0("vic_scaled_", job$pars$prob.pars$taskname, "_", learnername))
  # output
})

# (glmnet,tree)x(bs,gc)
run_models = readRDS("/media/external/rashomon/datafiles/model_info/run_models.rds")
# (glmnet,tree)x(cs,st)
run_models_2 = readRDS("/media/external/rashomon/datafiles/model_info/run_models_2.rds")
# (xgb) x (bs,gc, cs,st)
run_models_3 = readRDS("/media/external/rashomon/datafiles/model_info/run_models_3.rds")

run_models_no = data.table(sapply(run_models$torun.samples, function(x) table(x$taskname))[c("gc", "bs"), c("tree", "glmnet")], keep.rownames = TRUE)
run_models_no = merge(run_models_no, data.table(sapply(run_models_2$torun.samples, function(x) table(x$taskname)), keep.rownames = TRUE), all = TRUE)
run_models_no = merge(run_models_no, data.table(sapply(run_models_3$torun.samples, function(x) table(x$taskname)), keep.rownames = TRUE), by = "rn")
pre_design = data.table(pivot_longer(run_models_no[-1,], !rn, names_to = "learnername", values_to = "count"))
design = pre_design[, .(rn = rep(rn, each = count),
                        learnername = rep(learnername, each = count),
                        model.no = sequence(count)), by = .(rn, learnername)]
design = design[,-(1:2)]
rm(run_models, run_models_2, run_models_3, run_models_no)

addExperiments(
  prob.designs = list(fromlist = data.table(taskname = design$rn)),
  algo.designs = list(calculate_vic_pfi = design[,-"rn"]),
  repls = 1,
  combine = "bind"
)

testJob(1)

submitJobs(findErrors())
submitJobs()
waitForJobs()



#### Extract results ###########################################################
library(batchtools)
library(ggplot2)
library(tidyr)
library(data.table)

# run_models = readRDS("/media/external/rashomon/datafiles/model_info/run_models.rds")
# run_models_2 = readRDS("/media/external/rashomon/datafiles/model_info/run_models_2.rds")
# run_models_3 = readRDS("/media/external/rashomon/datafiles/model_info/run_models_3.rds")
# run_models_no = data.table(sapply(run_models$torun.samples, function(x) table(x$taskname))[c("gc", "bs"), c("tree", "glmnet")], keep.rownames = TRUE)
# run_models_no = merge(run_models_no, data.table(sapply(run_models_2$torun.samples, function(x) table(x$taskname)), keep.rownames = TRUE), all = TRUE)
# run_models_no = merge(run_models_no, data.table(sapply(run_models_3$torun.samples, function(x) table(x$taskname)), keep.rownames = TRUE), by = "rn")
# pre_design = data.table(pivot_longer(run_models_no[-1,], !rn, names_to = "learnername", values_to = "count"))
# rm(run_models, run_models_2, run_models_3, run_models_no)

# registry for cs, gc and st
regr = loadRegistry("/tmp/RtmpVBVWJ1/registryb84c531360d80")

## save results per data set and learner
# save_results = function(job_table, ids, learnername){
#   list.pfi_tmp = list()
#   # save median importance and feature from batchtools results
  # list.pfi_tmp[[learnername]] = reduceResultsList(ids = ids, function(x) {
  #   tab = x$results
  #   subset(tab, select = c(feature, importance))
  # })
#   # merge in one data.frame per data set > VIC
#   vic_tmp = data.frame(feature = list.pfi_tmp[[learnername]][[1]]$feature)
#   for(j in 1:length(ids)){
#     vic_tmp = merge(vic_tmp,
#                 list.pfi_tmp[[learnername]][[j]][,c("feature","importance")],
#                 by = "feature")
#     colnames(vic_tmp)[j+1] = paste0("pfi_", learnername, "_m", j)
#   }
#
#   res.list = list()
#   res.list$list.pfi = list.pfi_tmp
#   res.list$vic = vic_tmp
#   return(res.list)
# }

# job_table = getJobTable()
# list.pfi = list()
# vic = list()
# names(list.pfi)

for(i in 1:length(pre_design$count)){
  num = cumsum(pre_design$count)[i]
  learnername = pre_design$learnername[i]
  if(i == 1){
    ids = job_table$job.id[1:num]
  } else {
    ids = job_table$job.id[(num-(pre_design$count[i]-1)):num]
  }
  rm(num)

  res = save_results(job_table, ids, learnername)
  if(!(pre_design$rn[i] %in% names(list.pfi))) list.pfi[[pre_design$rn[i]]] = list()
  list.pfi[[pre_design$rn[i]]][[learnername]] = res$list.pfi[[learnername]]
  if(!(pre_design$rn[i] %in% names(vic))){
    vic[[pre_design$rn[i]]] = res$vic
  } else {
    vic[[pre_design$rn[i]]] = merge(vic[[pre_design$rn[i]]],
                                    res$vic,
                                    by = "feature")
  }
  rm(res)
}

# normalize VIC: max importance = 1
vic_normalized = list()
for(i in 1:length(pre_design$count)){
  vic_normalized[[pre_design$rn[i]]] = vic[[pre_design$rn[i]]]
  names = names(vic[[pre_design$rn[i]]])
  max_per_model = apply(vic_normalized[[pre_design$rn[i]]][,-1], 2, max)
  for(j in 1:length(max_per_model)){
    vic_normalized[[pre_design$rn[i]]][,j+1] = vic_normalized[[pre_design$rn[i]]][,j+1]/max_per_model[j]
  }
}

save(vic, vic_normalized, file = "results_vic.RData")

## plots #######################################################################
vic_long = list()
vic_wide = list()
plots = list()
task.keys = c("cs", "gc")
learner.keys = c("tree", "xgb", "glmnet")
no.models = 1000
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
