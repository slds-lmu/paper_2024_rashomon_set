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
load("data/results_vic_TreeFARMS.RData")
load("data/design_TreeFARMS.RData")

task.keys = names(vic) # german credit, compas, bike sharing, synthetic
learner.keys = "TreeFARMS"

vic_TF_RS = vic
vic_TF_normalized_RS = vic_normalized

load("data/design_all_but_TreeFARMS.RData")
load("data/results_vic_all_but_TreeFARMS.RData")

vic_gosdt <- lapply(vic, function(dt) {
  # Identify columns that start with "pfi_gosdt_" or are exactly "features"
  keep_cols <- names(dt)[names(dt) == "feature" | grepl("^pfi_gosdt_", names(dt))]
  dt[, keep_cols]  
})
vic_gosdt_s <- vic_gosdt[names(vic_gosdt) %in% task.keys]
vic_normalized_gosdt <- lapply(vic_normalized, function(dt) {
  # Identify columns that start with "pfi_gosdt_" or are exactly "features"
  keep_cols <- names(dt)[names(dt) == "feature" | grepl("^pfi_gosdt_", names(dt))]
  dt[, keep_cols]  
})
vic_normalized_gosdt_s <- vic_normalized_gosdt[names(vic_normalized_gosdt) %in% task.keys]

# Merge
vic_merge = list()
vic_normalized_merge = list()
for(task.key in task.keys){
  if("feature" %in% names(vic_gosdt_s[[task.key]])){
    vic_merge[[task.key]] = merge(vic_TF_RS[[task.key]], vic_gosdt_s[[task.key]], by = "feature", all = TRUE)
  } else {
    vic_merge[[task.key]] = vic_TF_RS[[task.key]]
  }
  if("feature" %in% names(vic_gosdt_s[[task.key]])){
    vic_normalized_merge[[task.key]] = merge(vic_TF_normalized_RS[[task.key]], vic_normalized_gosdt_s[[task.key]], by = "feature", all = TRUE)
  } else {
    vic_normalized_merge[[task.key]] = vic_TF_normalized_RS[[task.key]]
  }
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

vic_TFm_RS_long = list()
vic_TFm_RS_wide = list()
plots = list()
for(task.key in task.keys){
  vic_TFm_RS_long[[task.key]] = vic_merge[[task.key]] %>%
    pivot_longer(cols = starts_with("pfi"), names_to = "PFI", values_to = "Value")
  vic_TFm_RS_long[[task.key]]$learner = sub(".*_(.*?)_.*", "\\1", vic_TFm_RS_long[[task.key]]$PFI)
  vic_TFm_RS_wide[[task.key]] = vic_TFm_RS_long[[task.key]] %>%
    pivot_wider(names_from = feature, values_from = Value)
  
  # Plot 2: Scatter-plot using jitter colored acc. to model class
  plot2 = ggplot(vic_TFm_RS_long[[task.key]], aes(x = Value, y = feature,
                                              color = learner, alpha = alpha_value)) +
    geom_quasirandom(cex = 1, shape = 16, stroke = 0) +
    # scale_color_gradient(low = "blue", high = "red") +
    labs(x = "Importance", y = "Feature", color = "Model Class",
         title = paste0("PFI values (", task.key, ") colored by learner")) +
    theme_minimal(base_size = 15) +
    theme(legend.text = element_text(size=13)) +
    guides(alpha = FALSE, color = guide_legend(override.aes = list(size=8)))
  
  # Plot 3: Box plot
  plot3 = ggplot(vic_TFm_RS_long[[task.key]]) +
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
      scale_x_continuous(limits = c(min(vic_TFm_RS_wide[[task.key]][, -c(1,2,3,dim(vic_TFm_RS_wide[[task.key]])[2])]),
                                    max(vic_TFm_RS_wide[[task.key]][, -c(1,2,3,dim(vic_TFm_RS_wide[[task.key]])[2])])))+
      scale_y_continuous(limits = c(min(vic_TFm_RS_wide[[task.key]][, -c(1,2,3,4)]),
                                    max(vic_TFm_RS_wide[[task.key]][, -c(1,2,3,4)])))
  }
  plot4 = ggpairs(vic_TFm_RS_wide[[task.key]][, -c(1,2,3)],
                  lower = list(continuous = wrap(lowerfun)),
                  upper  = list(continuous = "blank"),
                  diag  = list(continuous = "blankDiag"),
                  legend = c(2,1),
                  title = paste("Pairwise Comparison:", task.key),
                  aes(colour = vic_TFm_RS_wide[[task.key]]$learner, alpha = alpha_value)) +
    theme_minimal(base_size = 15) +
    theme(legend.position = "bottom", legend.text = element_text(size=13)) +
    labs(colour = "Model Class")  +
    guides(alpha = FALSE, color = guide_legend(override.aes = list(size=8)))
  
  ## Plot 5: Pairwise Plots of the four most important features
  tmp_df = data.frame(feature = vic_merge[[task.key]]$feature,
                      mean_pfi = apply(vic_merge[[task.key]][,-1], 1, mean))
  top4_features = tmp_df[order(-tmp_df$mean_pfi), ][1:4, ]
  lowerfun <- function(data,mapping){
    ggplot(data = data, mapping = mapping)+
      geom_point()+
      scale_x_continuous(limits = c(min(vic_TFm_RS_wide[[task.key]][, -c(1,2,3,dim(vic_TFm_RS_wide[[task.key]])[2])]),
                                    max(vic_TFm_RS_wide[[task.key]][, -c(1,2,3,dim(vic_TFm_RS_wide[[task.key]])[2])])))+
      scale_y_continuous(limits = c(min(vic_TFm_RS_wide[[task.key]][, -c(1,2,3,4)]),
                                    max(vic_TFm_RS_wide[[task.key]][, -c(1,2,3,4)])))
  }
  plot5 = ggpairs(vic_TFm_RS_wide[[task.key]][c(top4_features$feature)],
                  lower = list(continuous = wrap(lowerfun)),
                  upper  = list(continuous = "blank"),
                  diag  = list(continuous = "blankDiag"),
                  legend = c(2,1),
                  title = paste("Pairwise Comparison of top 4 features:", task.key),
                  ggplot2::aes(colour = vic_TFm_RS_wide[[task.key]]$learner, alpha = alpha_value)) +
    theme_minimal(base_size = 15) +
    theme(legend.position = "bottom", legend.text = element_text(size=13)) +
    labs(colour = "Model Class")  +
    guides(alpha = FALSE, color = guide_legend(override.aes = list(size=8)))
  rm(tmp_df, top4_features)
  
  plots[[task.key]] = list()
  plots[[task.key]][["RS_scatter_plot"]] = plot2
  plots[[task.key]][["RS_box_plot"]] = plot3
  plots[[task.key]][["RS_pairwise_comparison"]] = gpairs_lower(plot4)
  plots[[task.key]][["RS_pairwise_comparison_top4_features"]] = gpairs_lower(plot5)
}


vic_TFm_RS_scaled_long = list()
vic_TFm_RS_scaled_wide = list()
plots_scaled = list()
for(task.key in task.keys){
  vic_TFm_RS_scaled_long[[task.key]] = vic_normalized_merge[[task.key]] %>%
    pivot_longer(cols = starts_with("pfi"), names_to = "PFI", values_to = "Value")
  vic_TFm_RS_scaled_long[[task.key]]$learner = sub(".*_(.*?)_.*", "\\1", vic_TFm_RS_scaled_long[[task.key]]$PFI)
  vic_TFm_RS_scaled_wide[[task.key]] = vic_TFm_RS_scaled_long[[task.key]] %>%
    pivot_wider(names_from = feature, values_from = Value)
  
  # Plot 2: Scatter-plot using jitter colored acc. to model class
  plot2 = ggplot(vic_TFm_RS_scaled_long[[task.key]], aes(x = Value, y = feature,
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
  plot3 = ggplot(vic_TFm_RS_scaled_long[[task.key]]) +
    geom_boxplot(aes(x = feature, y = Value), fill = "gray") +
    geom_boxplot(aes(x = feature, y = Value, fill = learner, alpha = alpha_value)) +
    coord_flip() +
    labs(y = "Importance", x = "Feature", fill = "Model Class",
         title = paste("PFI values (max importance = 1):", task.key)) +
    theme_minimal(base_size = 15) +
    theme(legend.text = element_text(size=13)) +
    guides(alpha = FALSE, color = guide_legend(override.aes = list(size=8)))
  
  plots_scaled[[task.key]] = list()
  plots_scaled[[task.key]][["RS_scatter_plot"]] = plot2
  plots_scaled[[task.key]][["RS_box_plot"]] = plot3
}


# save plots
for(task.key in task.keys){
  # scatter learner
  name = paste0("figures/TreeFARMS_", task.key, "_pfi_RS_scatter_learner.png")
  ggsave(name, plots[[task.key]][["RS_scatter_plot"]], width = 10, height = 5)
  name = paste0("figures/TreeFARMS_", task.key, "_pfi_RS_scatter_learner_scaled.png")
  ggsave(name, plots_scaled[[task.key]][["RS_scatter_plot"]], width = 10, height = 5)
  # boxplot
  name = paste0("figures/TreeFARMS_", task.key, "_pfi_RS_boxPlot.png")
  ggsave(name, plots[[task.key]][["RS_box_plot"]], width = 10, height = 5)
  name = paste0("figures/TreeFARMS_", task.key, "_pfi_RS_boxPlot_scaled.png")
  ggsave(name, plots_scaled[[task.key]][["RS_box_plot"]], width = 10, height = 5)
  # pairwise
  name = paste0("figures/TreeFARMS_", task.key, "_pfi_RS_pairwise.png")
  ggsave(name, plots[[task.key]][["RS_pairwise_comparison"]], width = 12.5, height = 6.25)
  # pairwise Top 4
  name = paste0("figures/TreeFARMS_", task.key, "_pfi_RS_pairwise_top4.png")
  ggsave(name, plots[[task.key]][["RS_pairwise_comparison_top4_features"]],
         width = 12.5, height = 6.25)
  print(paste(task.key, "done"))
}


# overview of model classes per task (number of models)
RS_models_count =lapply(vic_TFm_RS_long, function (x) table(x$learner)/length(unique(x$feature)))
model_names <- unique(pre_design$learnername)
category_names <- names(vic)
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
print(xtable(df, caption = "Übersicht der Modelle", digits = 0), include.rownames = TRUE)



