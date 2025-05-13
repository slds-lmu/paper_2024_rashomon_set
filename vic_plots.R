source("init.R")

library(data.table)
library(ggplot2)
library(tidyr)
library(dplyr)
library(GGally)
library(ggbeeswarm)
library(rlang)
library(iml)
library(corrplot)
library(xtable)


## General settings ############################################################
load("data/design.RData")

## merge vics
# final_vic = list()
# final_vic_normalized = list()
# for(i in unique(pre_design$rn)){
#   load(paste0("data/results_vic_", i, ".RData"))
#   # if(length(names(vic)) == 1) {
#   #   final_vic = vic
#   #   final_vic_normalized = vic_normalized
#   # } else {
#     final_vic[[i]] = vic[[i]]
#     final_vic_normalized[[i]] = vic_normalized[[i]]
#   # }
# }
# vic = final_vic
# vic_normalized = final_vic_normalized
# save(vic, vic_normalized, file = paste0("data/results_vic.RData"))

##

load("data/results_vic.RData")

task.keys = names(vic) # german credit, compas, bike sharing, synthetic
learner.keys = c("tree", "glmnet", "xgb", "nnet", "svm")



# read in AutoML information on models
run_models = readRDS("data/run_models_merged.rds")
# save performance for considered models
performance = list()
kernel = list()
for(task.key in task.keys){
  performance[[task.key]] = list()
  kernel[[task.key]] = c()
  for(learner.key in learner.keys){
    dt = run_models$torun.samples[[learner.key]]
    performance[[task.key]][[learner.key]] = dt[taskname == task.key]$score
    if(learner.key == "svm"){
      kernel[[task.key]] = c(kernel[[task.key]],paste0("svm_",dt[taskname == task.key]$svm.kernel))
    }
  }
}
# svm per task
svm_per_task = pre_design[pre_design$learnername == "svm",c("rn", "count")]

# create model-agnostic Rashomon set
best_performance = apply(sapply(performance, sapply, min),2,min)
perf_index_RS = list()
vic_RS = list()
vic_normalized_RS = list()
vic_learner_table = list()
for(task.key in task.keys){
  vic_names = colnames(vic[[task.key]])
  perf_index_RS[[task.key]] = lapply(performance[[task.key]], function(x) which(x < best_performance[task.key]*1.05))
  vic_learner = sub(".*_(.*?)_.*", "\\1", vic_names[-1])
  vic_learner_table[[task.key]] = list()
  vic_learner_table[[task.key]]$all = table(vic_learner)[unique(vic_learner)] # also in pre_design so this is a bit useless
  cum_sum_learner = cumsum(vic_learner_table[[task.key]]$all)
  index = 1 # feature column in vic
  for(learner.key in learner.keys){
    if(!is_empty(perf_index_RS[[task.key]][[learner.key]])){
      index_adopt = cum_sum_learner[which(learner.key == learner.keys)-1]
      if(is_empty(index_adopt)){
        model_index = perf_index_RS[[task.key]][[learner.key]]
      } else {
        model_index = perf_index_RS[[task.key]][[learner.key]]+index_adopt
      }
      index = c(index, model_index+1)
    }
  }
  vic_RS[[task.key]] = vic[[task.key]][,index]
  vic_normalized_RS[[task.key]] = vic_normalized[[task.key]][,index]
  perf_index_RS[[task.key]] = index

  vic_RS_names = colnames(vic_RS[[task.key]])
  vic_RS_learner = sub(".*_(.*?)_.*", "\\1", vic_RS_names[-1])
  vic_learner_table[[task.key]]$RS = table(vic_RS_learner)[unique(vic_RS_learner)]
}
rm(vic_names, vic_learner, cum_sum_learner, index)
model_index_RS = lapply(perf_index_RS, function(x) x[-1]-1)

save(vic_RS, vic_normalized_RS, file = paste0("data/results_vic_RS_", paste0(unique(design$rn), collapse = "_"), ".RData"))

# For st, we can easily calculate the "true PFI values"
if("st" %in% task.keys){
  dgp = function(x) x[4]+x[5]+x[4]*x[5]
  task = list.tasks[["st"]]
  task_train = generateCanonicalDataSplits(task, ratio = 2 / 3, seed = 1)$train
  task_test = generateCanonicalDataSplits(task, ratio = 2 / 3, seed = 1)$validation
  # learner / model
  # generate custom learner
  LearnerCustom <- R6::R6Class(
    "LearnerCustom",
    inherit = LearnerRegr, # Für Regression
    public = list(
      initialize = function() {
        # Initialisierung des Learners
        super$initialize(
          id = "regr.custom",               # ID des Learners
          feature_types = c("numeric"),    # Typ der Features
          predict_types = c("response")    # Vorhersagetypen
        )
      }
    ),
    private = list(
      .train = function(task) {
        # Speichere die Funktion als Modell
        self$model <- function(x) {
          x[[4]] + x[[5]] + x[[4]] * x[[5]]
        }
      },
      .predict = function(task) {
        # Daten extrahieren
        newdata <- task$data(cols = task$feature_names)
        # Berechnung: x[4] + x[5] + x[4] * x[5]
        response <- newdata[[4]] + newdata[[5]] + newdata[[4]] * newdata[[5]]
        # Rückgabe der Vorhersage
        list(response = response)
      }
    )
  )
  mlr_learners$add("regr.custom", LearnerCustom)
  learner = lrn("regr.custom")
  learner$train(task_train)

  # Calculate PFI
  set.seed(1)
  X = task_test$data(cols = task_test$feature_names)
  y = task_test$data(cols = task_test$target_names)
  predictor = Predictor$new(model = learner, data = X, y = y)
  # PFI via ratio (default). Alternative: compare = "difference"
  true_pfi = subset(FeatureImp$new(predictor, loss = "rmse", compare = "difference",
                                   n.repetitions = 10)$results,
                    select = c(feature, importance))
  true_pfi_scaled = true_pfi
  true_pfi_scaled$importance = true_pfi$importance/max(true_pfi$importance)
}

## Create plots
# Function needed for plots
gpairs_lower <- function(g){
  g$plots <- g$plots[-(1:g$nrow)]
  g$yAxisLabels <- g$yAxisLabels[-1]
  g$nrow <- g$nrow -1

  g$plots <- g$plots[-(seq(g$ncol, length(g$plots), by = g$ncol))]
  g$xAxisLabels <- g$xAxisLabels[-g$ncol]
  g$ncol <- g$ncol - 1

  g
}

alpha_value = 0.3

vic_long = list()
vic_wide = list()
plots = list()
for(task.key in task.keys){
  vic_long[[task.key]] = vic[[task.key]] %>%
    pivot_longer(cols = starts_with("pfi"), names_to = "PFI", values_to = "Value")
  vic_long[[task.key]]$learner = sub(".*_(.*?)_.*", "\\1", vic_long[[task.key]]$PFI)
  model_no = as.numeric(sub(".*_.*_m(.?)", "\\1", colnames(vic[[task.key]])[-1]))
  vic_long[[task.key]]$learner[vic_long[[task.key]]$learner == "svm"] =
    rep(kernel[[task.key]], times = length(vic[[task.key]]$feature))
  vic_long[[task.key]]$performance = rep(unlist(performance[[task.key]])[model_no], #[perf_index_RS[[task.key]]-1],
                                         times = length(vic[[task.key]]$feature))
  vic_long[[task.key]] = vic_long[[task.key]] %>%
    mutate(alpha_value = ifelse(performance < (best_performance[task.key] * 1.05), 0.4, 0.01))
  vic_wide[[task.key]] = vic_long[[task.key]] %>%
    pivot_wider(names_from = feature, values_from = Value)

  if(task.key != "st"){
    plot1 = ggplot() +
      geom_quasirandom(data = vic_long[[task.key]], aes(x = Value, y = feature,
                                                        color = performance, alpha = alpha_value),
                       cex = 1, shape = 16, stroke = 0) +
      scale_color_gradient(low = "blue", high = "red") +
      scale_alpha_identity() +
      labs(x = "Importance", y = "Feature", color = "Performance",
           title = paste0("PFI values (", task.key, ") colored by performance")) +
      theme_minimal(base_size = 15)+
      theme(legend.text = element_text(size=13))
  } else {
    plot1 = ggplot() +
      geom_quasirandom(data = vic_long[[task.key]], aes(x = Value, y = feature,
                                                        color = performance, alpha = alpha_value),
                       cex = 1, shape = 16, stroke = 0) +
      geom_point(data = true_pfi, aes(x = importance, y = feature,
                                      alpha = 1), cex = 3) +
      scale_color_gradient(low = "blue", high = "red") +
      scale_alpha_identity() +
      labs(x = "Importance", y = "Feature", color = "Performance",
           title = paste0("PFI values (", task.key, ") colored by performance")) +
      theme_minimal(base_size = 15)+
      theme(legend.text = element_text(size=13))
  }


  # Plot 2: Scatter-plot using jitter colored acc. to model class
  plot2 = ggplot(vic_long[[task.key]], aes(x = Value, y = feature,
                                           color = learner, alpha = alpha_value)) +
    geom_quasirandom(cex = 1, shape = 16, stroke = 0) +
    # scale_color_gradient(low = "blue", high = "red") +
    scale_alpha_identity() +
    labs(x = "Importance", y = "Feature", color = "Model Class",
         title = paste0("PFI values (", task.key, ") colored by learner")) +
    theme_minimal(base_size = 15) +
    theme(legend.text = element_text(size=13)) +
    guides(color = guide_legend(override.aes = list(size=8)))

  plots[[task.key]] = list()
  plots[[task.key]][["performance_scatter_plot"]] = plot1
  plots[[task.key]][["scatter_plot"]] = plot2
}

vic_RS_long = list()
vic_RS_wide = list()
for(task.key in task.keys){
  vic_RS_long[[task.key]] = vic_RS[[task.key]] %>%
    pivot_longer(cols = starts_with("pfi"), names_to = "PFI", values_to = "Value")
  vic_RS_long[[task.key]]$learner = sub(".*_(.*?)_.*", "\\1", vic_RS_long[[task.key]]$PFI)
  tmp = vic_long[[task.key]]$PFI[startsWith(vic_long[[task.key]]$learner, "svm")] %in% vic_RS_long[[task.key]]$PFI[vic_RS_long[[task.key]]$learner == "svm"]
  tmp = tmp[1:(length(tmp)/length(vic[[task.key]]$feature))]
  if(sum(tmp) > 0){
    vic_RS_long[[task.key]]$learner[vic_RS_long[[task.key]]$learner == "svm"] =
      rep(kernel[[task.key]][tmp], times = length(vic[[task.key]]$feature))
  }
  vic_RS_long[[task.key]]$performance = rep(unlist(performance[[task.key]])[perf_index_RS[[task.key]]-1],
                                         times = length(vic_RS[[task.key]]$feature))
  vic_RS_wide[[task.key]] = vic_RS_long[[task.key]] %>%
    pivot_wider(names_from = feature, values_from = Value)

  if(task.key != "st"){
    plot1 = ggplot() +
      geom_quasirandom(data = vic_RS_long[[task.key]], aes(x = Value, y = feature,
                                                        color = performance, alpha = alpha_value),
                       cex = 1, shape = 16, stroke = 0) +
      scale_color_gradient(low = "blue", high = "red") +
      labs(x = "Importance", y = "Feature", color = "Performance",
           title = paste0("PFI values (", task.key, ") colored by performance")) +
      theme_minimal(base_size = 15)+
      theme(legend.text = element_text(size=13))
  } else {
    plot1 = ggplot() +
      geom_quasirandom(data = vic_RS_long[[task.key]], aes(x = Value, y = feature,
                                                        color = performance, alpha = alpha_value),
                       cex = 1, shape = 16, stroke = 0) +
      geom_point(data = true_pfi, aes(x = importance, y = feature,
                                      alpha = 1), cex = 3) +
      scale_color_gradient(low = "blue", high = "red") +
      scale_alpha_identity() +
      labs(x = "Importance", y = "Feature", color = "Performance",
           title = paste0("PFI values (", task.key, ") colored by performance")) +
      theme_minimal(base_size = 15) +
      theme(legend.text = element_text(size=13))
  }


  # Plot 2: Scatter-plot using jitter colored acc. to model class
  plot2 = ggplot(vic_RS_long[[task.key]], aes(x = Value, y = feature,
                                           color = learner, alpha = alpha_value)) +
    geom_quasirandom(cex = 1, shape = 16, stroke = 0) +
    # scale_color_gradient(low = "blue", high = "red") +
    labs(x = "Importance", y = "Feature", color = "Model Class",
         title = paste0("PFI values (", task.key, ") colored by learner")) +
    theme_minimal(base_size = 15) +
    theme(legend.text = element_text(size=13)) +
    guides(alpha = FALSE, color = guide_legend(override.aes = list(size=8)))

  # Plot 3: Box plot
  plot3 = ggplot(vic_RS_long[[task.key]]) +
    geom_boxplot(aes(x = feature, y = Value), fill = "gray") +
    geom_boxplot(aes(x = feature, y = Value, fill = learner, alpha = alpha_value)) +
    coord_flip() +
    labs(y = "Importance", x = "Feature", fill = "Model Class",
         title = paste("PFI values (max importance = 1):", task.key)) +
    theme_minimal(base_size = 15)  +
    theme(legend.text = element_text(size=13)) +
    guides(alpha = FALSE, color = guide_legend(override.aes = list(size=8)))

  # Plot 4: Pairwise Plots
  lowerfun <- function(data,mapping){
    ggplot(data = data, mapping = mapping)+
      geom_point()+
      scale_x_continuous(limits = c(min(vic_RS_wide[[task.key]][, -c(1,2,3,dim(vic_RS_wide[[task.key]])[2])]),
                                    max(vic_RS_wide[[task.key]][, -c(1,2,3,dim(vic_RS_wide[[task.key]])[2])])))+
      scale_y_continuous(limits = c(min(vic_RS_wide[[task.key]][, -c(1,2,3,4)]),
                                    max(vic_RS_wide[[task.key]][, -c(1,2,3,4)])))
  }
  plot4 = ggpairs(vic_RS_wide[[task.key]][, -c(1,2,3)],
                  lower = list(continuous = wrap(lowerfun)),
                  upper  = list(continuous = "blank"),
                  diag  = list(continuous = "blankDiag"),
                  legend = c(2,1),
                  title = paste("Pairwise Comparison:", task.key),
                  aes(colour = vic_RS_wide[[task.key]]$learner, alpha = alpha_value)) +
    theme_minimal(base_size = 15) +
    theme(legend.position = "bottom", legend.text = element_text(size=13)) +
    labs(colour = "Model Class")  +
    guides(alpha = FALSE, color = guide_legend(override.aes = list(size=8)))

  ## Plot 5: Pairwise Plots of the four most important features
  tmp_df = data.frame(feature = vic_RS[[task.key]]$feature,
                      mean_pfi = apply(vic_RS[[task.key]][,-1], 1, mean))
  top4_features = tmp_df[order(-tmp_df$mean_pfi), ][1:4, ]
  lowerfun <- function(data,mapping){
    ggplot(data = data, mapping = mapping)+
      geom_point()+
      scale_x_continuous(limits = c(min(vic_RS_wide[[task.key]][, -c(1,2,3,dim(vic_RS_wide[[task.key]])[2])]),
                                    max(vic_RS_wide[[task.key]][, -c(1,2,3,dim(vic_RS_wide[[task.key]])[2])])))+
      scale_y_continuous(limits = c(min(vic_RS_wide[[task.key]][, -c(1,2,3,4)]),
                                    max(vic_RS_wide[[task.key]][, -c(1,2,3,4)])))
  }
  plot5 = ggpairs(vic_RS_wide[[task.key]][c(top4_features$feature)],
                  lower = list(continuous = wrap(lowerfun)),
                  upper  = list(continuous = "blank"),
                  diag  = list(continuous = "blankDiag"),
                  legend = c(2,1),
                  title = paste("Pairwise Comparison of top 4 features:", task.key),
                  ggplot2::aes(colour = vic_RS_wide[[task.key]]$learner, alpha = alpha_value)) +
    theme_minimal(base_size = 15) +
    theme(legend.position = "bottom", legend.text = element_text(size=13)) +
    labs(colour = "Model Class")  +
    guides(alpha = FALSE, color = guide_legend(override.aes = list(size=8)))
  rm(tmp_df, top4_features)

  plots[[task.key]][["RS_performance_scatter_plot"]] = plot1
  plots[[task.key]][["RS_scatter_plot"]] = plot2
  plots[[task.key]][["RS_box_plot"]] = plot3
  plots[[task.key]][["RS_pairwise_comparison"]] = gpairs_lower(plot4)
  plots[[task.key]][["RS_pairwise_comparison_top4_features"]] = gpairs_lower(plot5)
}

# vic_normalized
vic_scaled_long = list()
vic_scaled_wide = list()
plots_scaled = list()
for(task.key in task.keys){
  vic_scaled_long[[task.key]] = vic_normalized[[task.key]] %>%
    pivot_longer(cols = starts_with("pfi"), names_to = "PFI", values_to = "Value")
  vic_scaled_long[[task.key]]$learner = sub(".*_(.*?)_.*", "\\1", vic_scaled_long[[task.key]]$PFI)
  model_no = as.numeric(sub(".*_.*_m(.?)", "\\1", colnames(vic_normalized[[task.key]])[-1]))
  vic_scaled_long[[task.key]]$learner[vic_scaled_long[[task.key]]$learner == "svm"] =
    rep(kernel[[task.key]], times = length(vic_normalized[[task.key]]$feature))
  vic_scaled_long[[task.key]]$performance = rep(unlist(performance[[task.key]])[model_no],
                                                times = length(vic_normalized[[task.key]]$feature))
  vic_scaled_long[[task.key]] = vic_scaled_long[[task.key]] %>%
    mutate(alpha_value = ifelse(performance < (best_performance[task.key] * 1.05), 0.4, 0.01))


  # Plot 1: Scatter-plot using jitter colored acc. to performance
  if(task.key != "st"){
    plot1 = ggplot(vic_scaled_long[[task.key]], aes(x = Value, y = feature,
                                                    color = performance,
                                                    alpha = alpha_value)) +
      geom_quasirandom(cex = 1, shape = 16, stroke = 0) +
      scale_color_gradient(low = "blue", high = "red") +
      scale_alpha_identity() +
      labs(x = "Importance", y = "Feature", color = "Performance",
           title = paste0("PFI values (", task.key, ", max importance = 1) colored by performance")) +
      theme_minimal(base_size = 15)  +
      theme(legend.text = element_text(size=13))
  } else {
    plot1 = ggplot() +
      geom_quasirandom(data = vic_scaled_long[[task.key]],
                       aes(x = Value, y = feature, color = performance,
                           alpha = alpha_value),
                       cex = 1, shape = 16, stroke = 0) +
      geom_point(data = true_pfi_scaled, aes(x = importance, y = feature,
                                             alpha = 1), cex = 3) +
      scale_color_gradient(low = "blue", high = "red") +
      scale_alpha_identity() +
      labs(x = "Importance", y = "Feature", color = "Performance",
           title = paste0("PFI values (", task.key, ", max importance = 1) colored by performance")) +
      theme_minimal(base_size = 15) +
      theme(legend.text = element_text(size=13))
  }


  # Plot 2: Scatter-plot using jitter colored acc. to model class
  plot2 = ggplot(vic_scaled_long[[task.key]], aes(x = Value, y = feature,
                                                  color = learner,
                                                  alpha = alpha_value)) +
    geom_quasirandom(cex = 1, shape = 16, stroke = 0) +
    scale_alpha_identity() +
    labs(x = "Importance", y = "Feature", color = "Model Class",
         title = paste0("PFI values (", task.key, ", max importance = 1) colored by learner")) +
    theme_minimal(base_size = 15) +
    theme(legend.text = element_text(size=13)) +
    guides(color = guide_legend(override.aes = list(size=8)))

  plots_scaled[[task.key]] = list()
  plots_scaled[[task.key]][["performance_scatter_plot"]] = plot1
  plots_scaled[[task.key]][["scatter_plot"]] = plot2
}

vic_RS_scaled_long = list()
vic_RS_scaled_wide = list()
for(task.key in task.keys){
  vic_RS_scaled_long[[task.key]] = vic_normalized_RS[[task.key]] %>%
    pivot_longer(cols = starts_with("pfi"), names_to = "PFI", values_to = "Value")
  vic_RS_scaled_long[[task.key]]$learner = sub(".*_(.*?)_.*", "\\1", vic_RS_scaled_long[[task.key]]$PFI)
  tmp = vic_scaled_long[[task.key]]$PFI[startsWith(vic_long[[task.key]]$learner, "svm")] %in% vic_RS_long[[task.key]]$PFI[startsWith(vic_RS_long[[task.key]]$learner, "svm")]
  tmp = tmp[1:(length(tmp)/length(vic[[task.key]]$feature))]
  if(sum(tmp) > 0){
    vic_RS_scaled_long[[task.key]]$learner[vic_RS_scaled_long[[task.key]]$learner == "svm"] =
      rep(kernel[[task.key]][tmp], times = length(vic[[task.key]]$feature))
  }
  vic_RS_scaled_long[[task.key]]$performance = rep(unlist(performance[[task.key]])[perf_index_RS[[task.key]]-1],
                                                times = length(vic_normalized_RS[[task.key]]$feature))
  vic_RS_scaled_wide[[task.key]] = vic_RS_scaled_long[[task.key]] %>%
    pivot_wider(names_from = feature, values_from = Value)

  # Plot 1: Scatter-plot using jitter colored acc. to performance
  if(task.key != "st"){
    plot1 = ggplot(vic_RS_scaled_long[[task.key]], aes(x = Value, y = feature,
                                                    color = performance,
                                                    alpha = alpha_value)) +
      geom_quasirandom(cex = 1, shape = 16, stroke = 0) +
      scale_color_gradient(low = "blue", high = "red") +
      scale_alpha_identity() +
      labs(x = "Importance", y = "Feature", color = "Performance",
           title = paste0("PFI values (", task.key, ", max importance = 1) colored by performance")) +
      theme_minimal(base_size = 15) +
      theme(legend.text = element_text(size=13))
  } else {
    plot1 = ggplot() +
      geom_quasirandom(data = vic_RS_scaled_long[[task.key]],
                       aes(x = Value, y = feature, color = performance,
                           alpha = alpha_value),
                       cex = 1, shape = 16, stroke = 0) +
      geom_point(data = true_pfi_scaled, aes(x = importance, y = feature,
                                             alpha = 1), cex = 3) +
      scale_color_gradient(low = "blue", high = "red") +
      scale_alpha_identity() +
      labs(x = "Importance", y = "Feature", color = "Performance",
           title = paste0("PFI values (", task.key, ", max importance = 1) colored by performance")) +
      theme_minimal(base_size = 15) +
      theme(legend.text = element_text(size=13))
  }


  # Plot 2: Scatter-plot using jitter colored acc. to model class
  plot2 = ggplot(vic_RS_scaled_long[[task.key]], aes(x = Value, y = feature,
                                                  color = learner,
                                                  alpha = alpha_value)) +
    geom_quasirandom(cex = 1, shape = 16, stroke = 0) +
    scale_alpha_identity() +
    labs(x = "Importance", y = "Feature", color = "Model Class",
         title = paste0("PFI values (", task.key, ", max importance = 1) colored by learner")) +
    theme_minimal(base_size = 15) +
    theme(legend.text = element_text(size=13)) +
    guides(color = guide_legend(override.aes = list(size=8)))

  # Plot 3: Box plot
  plot3 = ggplot(vic_RS_scaled_long[[task.key]]) +
    geom_boxplot(aes(x = feature, y = Value), fill = "gray") +
    geom_boxplot(aes(x = feature, y = Value, fill = learner, alpha = alpha_value)) +
    coord_flip() +
    labs(y = "Importance", x = "Feature", fill = "Model Class",
         title = paste("PFI values (max importance = 1):", task.key)) +
    theme_minimal(base_size = 15) +
    theme(legend.text = element_text(size=13)) +
    guides(alpha = FALSE, color = guide_legend(override.aes = list(size=8)))

  plots_scaled[[task.key]][["RS_performance_scatter_plot"]] = plot1
  plots_scaled[[task.key]][["RS_scatter_plot"]] = plot2
  plots_scaled[[task.key]][["RS_box_plot"]] = plot3
}


# save plots
for(task.key in task.keys){
  # scatter performance
  name = paste0("figures/", task.key, "_pfi_scatter_performance.png")
  ggsave(name, plots[[task.key]][["performance_scatter_plot"]], width = 10, height = 5)
  name = paste0("figures/", task.key, "_pfi_scatter_performance_scaled.png")
  ggsave(name, plots_scaled[[task.key]][["performance_scatter_plot"]], width = 10, height = 5)
  name = paste0("figures/", task.key, "_pfi_RS_scatter_performance.png")
  ggsave(name, plots[[task.key]][["RS_performance_scatter_plot"]], width = 10, height = 5)
  name = paste0("figures/", task.key, "_pfi_RS_scatter_performance_scaled.png")
  ggsave(name, plots_scaled[[task.key]][["RS_performance_scatter_plot"]], width = 10, height = 5)
  # scatter learner
  name = paste0("figures/", task.key, "_pfi_scatter_learner.png")
  ggsave(name, plots[[task.key]][["scatter_plot"]], width = 10, height = 5)
  name = paste0("figures/", task.key, "_pfi_scatter_learner_scaled.png")
  ggsave(name, plots_scaled[[task.key]][["scatter_plot"]], width = 10, height = 5)
  name = paste0("figures/", task.key, "_pfi_RS_scatter_learner.png")
  ggsave(name, plots[[task.key]][["RS_scatter_plot"]], width = 10, height = 5)
  name = paste0("figures/", task.key, "_pfi_RS_scatter_learner_scaled.png")
  ggsave(name, plots_scaled[[task.key]][["RS_scatter_plot"]], width = 10, height = 5)
  # boxplot
  name = paste0("figures/", task.key, "_pfi_RS_boxPlot.png")
  ggsave(name, plots[[task.key]][["RS_box_plot"]], width = 10, height = 5)
  name = paste0("figures/", task.key, "_pfi_RS_boxPlot_scaled.png")
  ggsave(name, plots_scaled[[task.key]][["RS_box_plot"]], width = 10, height = 5)
  # pairwise
  name = paste0("figures/", task.key, "_pfi_RS_pairwise.png")
  ggsave(name, plots[[task.key]][["RS_pairwise_comparison"]], width = 12.5, height = 6.25)
  # pairwise Top 4
  name = paste0("figures/", task.key, "_pfi_RS_pairwise_top4.png")
  ggsave(name, plots[[task.key]][["RS_pairwise_comparison_top4_features"]],
         width = 12.5, height = 6.25)
  print(paste(task.key, "done"))
}


# overview of model classes per task (number of models)
RS_models_count =lapply(vic_RS_long, function (x) table(x$learner)/length(unique(x$feature)))
model_names <- c("glmnet", "nnet", "svm_linear", "svm_polynomial", "svm_radial", "tree", "xgb")
category_names <- c("st", "cs", "bs", "gc")
df <- data.frame(matrix(0, nrow = length(category_names), ncol = length(model_names)))
colnames(df) <- model_names
rownames(df) <- category_names
for (cat in names(RS_models_count)) {
  for (model in names(RS_models_count[[cat]])) {
    df[cat, model] <- RS_models_count[[cat]][model]
  }
}
df
# LaTeX-Code
print(xtable(df, caption = "Übersicht der Modelle"), include.rownames = TRUE)


#### Correlation analysis st data ####

data = generateCanonicalDataSplits(list.tasks$st, ratio = 2 / 3, seed = 1)$validation$data()

# Spearman's Rho
data_spearman_corr <- cor(data, method = "spearman")

# plot
name = paste0("figures/st_data_cor_spearman.pdf")
pdf(file = name, width = 7, height = 7)
corrplot(data_spearman_corr, method="circle", type="lower", tl.col = "black")
dev.off()


#### Correlation analysis FI values ####

vic_t = lapply(vic_RS, function(x){
  tmp = t(x[,-1])
  colnames(tmp) = x[,1]
  tmp
})

# Spearman's Rho
vic_spearman_corr <- lapply(vic_t, function(x) cor(x, method = "spearman"))

# Kendall's Tau
vic_kendall_corr <- lapply(vic_t, function(x) cor(x, method = "kendall"))

# plot
for(task.key in task.keys){
  name = paste0("figures/", task.key, "_pfi_cor_spearman.pdf")
  pdf(file = name, width = 7, height = 7)
  corrplot(vic_spearman_corr[[task.key]], method="circle", type="lower", tl.col = "black")
  dev.off()

  name = paste0("figures/", task.key, "_pfi_cor_kendall.pdf")
  pdf(file = name, width = 7, height = 7)
  corrplot(vic_kendall_corr[[task.key]], method="circle", type="lower", tl.col = "black")
  dev.off()
}


## Create ranks
f_ranks = list()
f_ranks = lapply(vic_RS, function(x){
  tmp = as.data.frame(lapply(x[-1], function(y) rank(-y)))
  tmp = cbind(x[1], tmp)
})


f_ranks_long = list()
for(task.key in task.keys){
  f_ranks_long[[task.key]] = f_ranks[[task.key]] %>%
    pivot_longer(cols = starts_with("pfi"), names_to = "PFI", values_to = "Value")
  f_ranks_long[[task.key]]$learner = sub(".*_(.*?)_.*", "\\1", f_ranks_long[[task.key]]$PFI)
  model_no = as.numeric(sub(".*_.*_m(.?)", "\\1", colnames(f_ranks[[task.key]])[-1]))
  tmp = table(f_ranks_long[[task.key]]$learner)/length(f_ranks[[task.key]]$feature)
  unique_order <- unique(f_ranks_long[[task.key]]$learner)
  threshold = cumsum(tmp[unique_order])
  if(length(threshold) > 1){
    for(i in seq(threshold)[-1]){
      model_no[(threshold[i-1]+1):threshold[i]] = model_no[(threshold[i-1]+1):threshold[i]] + threshold[i-1]
    }
  }
  f_ranks_long[[task.key]]$performance = rep(unlist(performance[[task.key]])[model_no], #[perf_index_RS[[task.key]]-1],
                                         times = length(f_ranks[[task.key]]$feature))

  plot1 = ggplot(f_ranks_long[[task.key]], aes(x = Value, y = feature,
                                           color = performance, alpha = alpha_value)) +
    geom_quasirandom(cex = 1, shape = 16, stroke = 0) +
    scale_color_gradient(low = "blue", high = "red") +
    labs(x = "Rank", y = "Feature", color = "Performance",
         title = paste0("PFI values (", task.key, ") colored by performance")) +
    theme_minimal(base_size = 15)


  # Plot 2: Scatter-plot using jitter colored acc. to model class
  plot2 = ggplot(f_ranks_long[[task.key]], aes(x = Value, y = feature,
                                           color = learner, alpha = alpha_value)) +
    geom_quasirandom(cex = 1, shape = 16, stroke = 0) +
    labs(x = "Rank", y = "Feature", color = "Model Class",
         title = paste0("PFI values (", task.key, ") colored by learner")) +
    theme_minimal(base_size = 15)

  if(is_empty(plots[[task.key]])) plots[[task.key]] = list()
  plots[[task.key]][["performance_scatter_plot_ranks"]] = plot1
  plots[[task.key]][["scatter_plot_ranks"]] = plot2
}

# save plots
for(task.key in task.keys){
  # scatter performance
  name = paste0("figures/pfi_ranks_", task.key, "_scatter_performance.pdf")
  ggsave(name, plots[[task.key]][["performance_scatter_plot_ranks"]], width = 10, height = 5)
  # scatter model
  name = paste0("figures/pfi_ranks_", task.key, "_scatter.pdf")
  ggsave(name, plots[[task.key]][["scatter_plot_ranks"]], width = 10, height = 5)
}
