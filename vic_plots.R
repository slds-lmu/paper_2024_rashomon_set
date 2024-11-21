source("init.R")

library(data.table)
library(ggplot2)
library(tidyr)
library(GGally)
library(ggbeeswarm)
library(rlang)


## General settings ############################################################
load("data/design.RData") # load("data/design_bs.RData")
load("data/results_vic.RData") # load("data/results_vic_bs.RData")
task.keys = names(vic) # german credit, compas, bike sharing, synthetic
learner.keys = c("tree", "glmnet", "xgb")


# read in AutoML information on models
run_models = readRDS("data/run_models_merged.rds")
# save performance for considered models
performance = list()
for(task.key in task.keys){
  performance[[task.key]] = list()
  for(learner.key in learner.keys){
    dt = run_models$torun.samples[[learner.key]]
    performance[[task.key]][[learner.key]] = dt[taskname == task.key]$score[1:pre_design[rn == task.key & learnername == learner.key]$count]
  }
}

best_performance = apply(sapply(performance, sapply, min),2,min)
perf_index_RS = list()
vic_RS = list()
vic_normalized_RS = list()
for(task.key in task.keys){
  perf_index_RS[[task.key]] = lapply(performance[[task.key]], function(x) which(x < best_performance[task.key]*1.1))
  vic_names = colnames(vic[[task.key]])
  vic_learner = sub(".*_(.*?)_.*", "\\1", vic_names[-1])
  vic_learner_table = table(vic_learner)[unique(vic_learner)]
  cum_sum_learner = cumsum(vic_learner_table)
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
}
rm(vic_names, vic_learner, vic_learner_table, cum_sum_learner, index)

save(vic_RS, vic_normalized_RS, file = paste0("data/results_vic_RS_", paste0(unique(design$rn), collapse = "_"), ".RData"))

gpairs_lower <- function(g){
  g$plots <- g$plots[-(1:g$nrow)]
  g$yAxisLabels <- g$yAxisLabels[-1]
  g$nrow <- g$nrow -1
  
  g$plots <- g$plots[-(seq(g$ncol, length(g$plots), by = g$ncol))]
  g$xAxisLabels <- g$xAxisLabels[-g$ncol]
  g$ncol <- g$ncol - 1
  
  g
}

vic_long = list()
vic_wide = list()
plots = list()
for(task.key in task.keys){
  vic_long[[task.key]] = vic_RS[[task.key]] %>%
    pivot_longer(cols = starts_with("pfi"), names_to = "PFI", values_to = "Value")
  vic_long[[task.key]]$learner = sub(".*_(.*?)_.*", "\\1", vic_long[[task.key]]$PFI)
  vic_long[[task.key]]$performance = rep(unlist(performance[[task.key]])[perf_index_RS[[task.key]]-1],
                                         times = length(vic_RS[[task.key]]$feature))
  vic_wide[[task.key]] = vic_long[[task.key]] %>%
    pivot_wider(names_from = feature, values_from = Value)
  
  
  # Plot 1: Scatter-plot using jitter colored acc. to performance
  # plot1 = ggplot(vic_long[[task.key]], aes(x = Value, y = feature,
  #                                          color = performance)) +
  #   geom_jitter(width = 0, height = 0.35, alpha = 0.7, size = 1) +
  #   scale_color_gradient(low = "blue", high = "red") +
  #   labs(x = "Importance", y = "Feature",
  #        title = paste0("PFI values (", task.key, ") colored by performance")) +
  #   theme_minimal()
  
  plot1 = ggplot(vic_long[[task.key]], aes(x = Value, y = feature, 
                                           color = performance)) +
    geom_quasirandom(cex = 1, alpha = 0.3) +  
    scale_color_gradient(low = "blue", high = "red") +  
    labs(x = "Importance", y = "Feature", color = "Performance", 
         title = paste0("PFI values (", task.key, ") colored by performance")) +
    theme_minimal()
  
  # Plot 2: Scatter-plot using jitter colored acc. to model class
  plot2 = ggplot(vic_long[[task.key]], aes(x = Value, y = feature,
                                           color = learner)) +
    geom_quasirandom(cex = 1, alpha = 0.3) +  
    # geom_jitter(width = 0, height = 0.35, alpha = 0.3, size = 1) +
    labs(x = "Importance", y = "Feature", color = "Learner",
         title = paste0("PFI values (", task.key, ") colored by learner")) +
    theme_minimal()
  
  # Plot 3: Box plot
  plot3 = ggplot(vic_long[[task.key]], aes(x = feature, y = Value)) +
    geom_boxplot() + coord_flip() +
    labs(x = "Importance", y = "Feature", title = paste("PFI values:", task.key)) +
    theme_minimal()
  
  # Plot 4: Pairwise Plots
  lowerfun <- function(data,mapping){
    ggplot(data = data, mapping = mapping)+
      geom_point()+
      scale_x_continuous(limits = c(min(vic_wide[[task.key]][, -c(1,2,3,dim(vic_wide[[task.key]])[2])]),
                                    max(vic_wide[[task.key]][, -c(1,2,3,dim(vic_wide[[task.key]])[2])])))+
      scale_y_continuous(limits = c(min(vic_wide[[task.key]][, -c(1,2,3,4)]),
                                    max(vic_wide[[task.key]][, -c(1,2,3,4)])))
  }  
  plot4 = ggpairs(vic_wide[[task.key]][, -c(1,2,3)],
                  lower = list(continuous = wrap(lowerfun)),
                  upper  = list(continuous = "blank"),
                  diag  = list(continuous = "blankDiag"),
                  title = paste("Pairwise Comparison:", task.key),
                  ggplot2::aes(colour = vic_wide[[task.key]]$learner, alpha = 0.3))
  
  
  plots[[task.key]] = list()
  plots[[task.key]][["performance_scatter_plot"]] = plot1
  plots[[task.key]][["scatter_plot"]] = plot2
  plots[[task.key]][["box_plot"]] = plot3
  plots[[task.key]][["pairwise_comparison"]] = gpairs_lower(plot4)
}

# vic_normalized
vic_scaled_long = list()
vic_scaled_wide = list()
plots_scaled = list()
for(task.key in task.keys){
  vic_scaled_long[[task.key]] = vic_normalized_RS[[task.key]] %>%
    pivot_longer(cols = starts_with("pfi"), names_to = "PFI", values_to = "Value")
  vic_scaled_long[[task.key]]$learner = sub(".*_(.*?)_.*", "\\1", vic_scaled_long[[task.key]]$PFI)
  vic_scaled_long[[task.key]]$performance = rep(unlist(performance[[task.key]])[perf_index_RS[[task.key]]-1],
                                                times = length(vic_normalized_RS[[task.key]]$feature))
  vic_scaled_wide[[task.key]] = vic_scaled_long[[task.key]] %>%
    pivot_wider(names_from = feature, values_from = Value)
  
  
  # Plot 1: Scatter-plot using jitter colored acc. to performance
  plot1 = ggplot(vic_scaled_long[[task.key]], aes(x = Value, y = feature,
                                                  color = performance)) +
    geom_quasirandom(cex = 1, alpha = 0.2) +  
    # geom_jitter(width = 0, height = 0.35, alpha = 0.3, size = 1) +
    scale_color_gradient(low = "blue", high = "red") +
    labs(x = "Importance", y = "Feature", color = "Performance",
         title = paste0("PFI values (", task.key, ", max importance = 1) colored by performance")) +
    theme_minimal()
  
  # Plot 2: Scatter-plot using jitter colored acc. to model class
  plot2 = ggplot(vic_scaled_long[[task.key]], aes(x = Value, y = feature,
                                                  color = learner)) +
    geom_quasirandom(cex = 1, alpha = 0.2) +  
    # geom_jitter(width = 0, height = 0.35, alpha = 0.3, size = 1) +
    labs(x = "Importance", y = "Feature", color = "Learner",
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
  # plot4 = ggpairs(vic_scaled_wide[[task.key]][, -c(1,2)],
  #                 title = paste("Pairwise Comparison (max importance = 1):", task.key),
  #                 ggplot2::aes(colour = vic_scaled_wide[[task.key]]$learner, alpha = 0.3))
  
  plots_scaled[[task.key]] = list()
  plots_scaled[[task.key]][["performance_scatter_plot"]] = plot1
  plots_scaled[[task.key]][["scatter_plot"]] = plot2
  plots_scaled[[task.key]][["box_plot"]] = plot3
  # plots_scaled[[task.key]][["pairwise_comparison"]] = plot4
}

if("cs" %in% task.keys){
  ## Plot 5: Pairwise Plots reproducing results of Dong & Rudin (2020) for cs
  # extract four most important features (in D&R: age, race, prior, gender)
  tmp_df = data.frame(feature = vic_RS$cs$feature,
                      mean_pfi = apply(vic_RS$cs[,-1], 1, mean))
  top4_features = tmp_df[order(-tmp_df$mean_pfi), ][1:4, ]
  lowerfun <- function(data,mapping){
    ggplot(data = data, mapping = mapping)+
      geom_point()+
      scale_x_continuous(limits = c(min(vic_wide$cs[, -c(1,2,3,dim(vic_wide$cs)[2])]),
                                    max(vic_wide$cs[, -c(1,2,3,dim(vic_wide$cs)[2])])))+
      scale_y_continuous(limits = c(min(vic_wide$cs[, -c(1,2,3,4)]),
                                    max(vic_wide$cs[, -c(1,2,3,4)])))
  }
  plot5 = ggpairs(vic_wide$cs[c(top4_features$feature)],
                  lower = list(continuous = wrap(lowerfun)),
                  upper  = list(continuous = "blank"),
                  diag  = list(continuous = "blankDiag"),
                  title = "Pairwise Comparison: cs",
                  ggplot2::aes(colour = vic_wide$cs$learner, alpha = 0.3))
  plots$cs[["pairwise_comparison_top4_features"]] = gpairs_lower(plot5)
  rm(tmp_df, top4_features)
  # same features as Dong & Rudin
  tmp = vic_wide$cs[vic_wide$cs$learner == "tree", c("age", "race", "priors_count", "sex")]
  plot6 = ggpairs(tmp,
                  lower = list(continuous = wrap(lowerfun)),
                  upper  = list(continuous = "blank"),
                  diag  = list(continuous = "blankDiag"),
                  title = "Pairwise Comparison: cs (Top 4 features of Dong & Rudin)",
                  ggplot2::aes(alpha = 0.3))
  plots$cs[["pairwise_comparison_top4_features_dong"]] = gpairs_lower(plot6)
  rm(tmp)
}


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
  name = paste0("figures/pfi_values_", task.key, "_pairwise.pdf")
  ggsave(name, plots[[task.key]][[4]], width = 12.5, height = 6.25)
  # name = paste0("figures/pfi_scaled_values_", task.key, "_pairwise.pdf")
  # ggsave(name, plots_scaled[[task.key]][[4]], width = 12.5, height = 6.25)
}
if("cs" %in% task.keys){
  # pairwise compare compas with Dong & Rudin
  name = paste0("figures/pfi_values_cs_pairwise_dong.pdf")
  ggsave(name, plots$cs[["pairwise_comparison_top4_features"]], width = 12.5, height = 6.25)
  name = paste0("figures/pfi_values_cs_pairwise_dong2.pdf")
  ggsave(name, plots$cs[["pairwise_comparison_top4_features_dong"]], width = 10, height = 10)
}
