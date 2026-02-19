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
load("data/design_all_but_TreeFARMS.RData")
load("data/results_vic_all_but_TreeFARMS.RData")

task.keys = names(vic) # german credit, compas, bike sharing, synthetic
alpha_value = 0.3


## Create plots ################################################################
# Function needed for pairwise plots
gpairs_lower <- function(g){
  g$plots <- g$plots[-(1:g$nrow)]
  g$yAxisLabels <- g$yAxisLabels[-1]
  g$nrow <- g$nrow - 1

  g$plots <- g$plots[-(seq(g$ncol, length(g$plots), by = g$ncol))]
  g$xAxisLabels <- g$xAxisLabels[-g$ncol]
  g$ncol <- g$ncol - 1

  g
}


#### vic standard ####
vic_long = list()
vic_wide = list()
plots = list()

for(task.key in task.keys){
  vic_long[[task.key]] = vic[[task.key]] %>%
    pivot_longer(cols = starts_with("pfi"), names_to = "PFI", values_to = "Value")
  vic_long[[task.key]]$learner = sub(".*_(.*?)_.*", "\\1", vic_long[[task.key]]$PFI)
  vic_wide[[task.key]] = vic_long[[task.key]] %>%
    pivot_wider(names_from = feature, values_from = Value)

  # Scatter-plot colored according to model class
  plot_scatter = ggplot(vic_long[[task.key]], aes(x = Value, y = feature, color = learner)) +
    geom_quasirandom(alpha = alpha_value, cex = 1, shape = 16, stroke = 0) +
    labs(x = "Importance", y = "Feature", color = "Model Class",
         title = paste0("PFI values (", task.key, ") colored by learner")) +
    theme_minimal(base_size = 15) +
    theme(legend.text = element_text(size = 13)) +
    guides(color = guide_legend(override.aes = list(size = 8)))

  # Box plot
  plot_box = ggplot(vic_long[[task.key]]) +
    geom_boxplot(aes(x = feature, y = Value), fill = "gray") +
    coord_flip() +
    labs(y = "Importance", x = "Feature", fill = "Model Class",
         title = paste("PFI values:", task.key)) +
    theme_minimal(base_size = 15) +
    theme(legend.text = element_text(size = 13))

  # Violin plot
  plot_violin = ggplot(vic_long[[task.key]]) +
    geom_violin(aes(x = feature, y = Value), fill = "gray", color = "gray30", trim = FALSE) +
    coord_flip() +
    labs(y = "Importance", x = "Feature", fill = "Model Class",
         title = paste("PFI values, violin:", task.key)) +
    theme_minimal(base_size = 15) +
    theme(legend.text = element_text(size = 13))

  # Pairwise plot
  lowerfun <- function(data, mapping){
    ggplot(data = data, mapping = mapping) + geom_point()
  }
  feature_cols = setdiff(colnames(vic_wide[[task.key]]), c("PFI", "learner"))
  plot_pairwise = ggpairs(vic_wide[[task.key]][, feature_cols],
                          lower = list(continuous = wrap(lowerfun)),
                          upper = list(continuous = "blank"),
                          diag = list(continuous = "blankDiag"),
                          legend = c(2, 1),
                          title = paste("Pairwise Comparison:", task.key),
                          aes(colour = vic_wide[[task.key]]$learner, alpha = alpha_value)) +
    theme_minimal(base_size = 15) +
    theme(legend.position = "bottom", legend.text = element_text(size = 13)) +
    labs(colour = "Model Class") +
    guides(alpha = FALSE, color = guide_legend(override.aes = list(size = 8)))

  # Pairwise plot of top 4 features
  tmp_df = data.frame(feature = vic[[task.key]]$feature,
                      mean_pfi = apply(vic[[task.key]][, -1], 1, mean))
  top_n = min(4, nrow(tmp_df))
  top4_features = tmp_df[order(-tmp_df$mean_pfi), ][1:top_n, ]
  plot_pairwise_top4 = ggpairs(vic_wide[[task.key]][c(top4_features$feature)],
                               lower = list(continuous = wrap(lowerfun)),
                               upper = list(continuous = "blank"),
                               diag = list(continuous = "blankDiag"),
                               legend = c(2, 1),
                               title = paste("Pairwise Comparison of top features:", task.key),
                               ggplot2::aes(colour = vic_wide[[task.key]]$learner,
                                            alpha = alpha_value)) +
    theme_minimal(base_size = 15) +
    theme(legend.position = "bottom", legend.text = element_text(size = 13)) +
    labs(colour = "Model Class") +
    guides(alpha = FALSE, color = guide_legend(override.aes = list(size = 8)))

  rm(tmp_df, top4_features, feature_cols)

  plots[[task.key]] = list()
  plots[[task.key]][["scatter_plot"]] = plot_scatter
  plots[[task.key]][["box_plot"]] = plot_box
  plots[[task.key]][["violin_plot"]] = plot_violin
  plots[[task.key]][["pairwise_comparison"]] = gpairs_lower(plot_pairwise)
  plots[[task.key]][["pairwise_comparison_top4_features"]] = gpairs_lower(plot_pairwise_top4)
}


#### vic normalized ####
vic_scaled_long = list()
plots_scaled = list()

for(task.key in task.keys){
  vic_scaled_long[[task.key]] = vic_normalized[[task.key]] %>%
    pivot_longer(cols = starts_with("pfi"), names_to = "PFI", values_to = "Value")
  vic_scaled_long[[task.key]]$learner = sub(".*_(.*?)_.*", "\\1", vic_scaled_long[[task.key]]$PFI)

  # Scatter-plot colored according to model class
  plot_scatter = ggplot(vic_scaled_long[[task.key]], aes(x = Value, y = feature, color = learner)) +
    geom_quasirandom(alpha = alpha_value, cex = 1, shape = 16, stroke = 0) +
    labs(x = "Importance", y = "Feature", color = "Model Class",
         title = paste0("PFI values (", task.key, ", max importance = 1) colored by learner")) +
    theme_minimal(base_size = 15) +
    theme(legend.text = element_text(size = 13)) +
    guides(color = guide_legend(override.aes = list(size = 8)))

  # Box plot
  plot_box = ggplot(vic_scaled_long[[task.key]]) +
    geom_boxplot(aes(x = feature, y = Value), fill = "gray") +
    coord_flip() +
    labs(y = "Importance", x = "Feature", fill = "Model Class",
         title = paste("PFI values (max importance = 1):", task.key)) +
    theme_minimal(base_size = 15) +
    theme(legend.text = element_text(size = 13))

  # Violin plot
  plot_violin = ggplot(vic_scaled_long[[task.key]]) +
    geom_violin(aes(x = feature, y = Value), fill = "gray", color = "gray30", trim = FALSE) +
    coord_flip() +
    labs(y = "Importance", x = "Feature", fill = "Model Class",
         title = paste("PFI values (max importance = 1), violin:", task.key)) +
    theme_minimal(base_size = 15) +
    theme(legend.text = element_text(size = 13))

  plots_scaled[[task.key]] = list()
  plots_scaled[[task.key]][["scatter_plot"]] = plot_scatter
  plots_scaled[[task.key]][["box_plot"]] = plot_box
  plots_scaled[[task.key]][["violin_plot"]] = plot_violin
}


## Save plots ##################################################################
for(task.key in task.keys){
  # Scatter learner
  name = paste0("figures/", task.key, "_pfi_scatter_learner.png")
  ggsave(name, plots[[task.key]][["scatter_plot"]], width = 10, height = 5)
  name = paste0("figures/", task.key, "_pfi_scatter_learner_scaled.png")
  ggsave(name, plots_scaled[[task.key]][["scatter_plot"]], width = 10, height = 5)

  # Boxplot
  name = paste0("figures/", task.key, "_pfi_boxPlot.png")
  ggsave(name, plots[[task.key]][["box_plot"]], width = 10, height = 5)
  name = paste0("figures/", task.key, "_pfi_boxPlot_scaled.png")
  ggsave(name, plots_scaled[[task.key]][["box_plot"]], width = 10, height = 5)

  # Violin plot
  name = paste0("figures/", task.key, "_pfi_violinPlot.png")
  ggsave(name, plots[[task.key]][["violin_plot"]], width = 10, height = 5)
  name = paste0("figures/", task.key, "_pfi_violinPlot_scaled.png")
  ggsave(name, plots_scaled[[task.key]][["violin_plot"]], width = 10, height = 5)

  # Pairwise
  name = paste0("figures/", task.key, "_pfi_pairwise.png")
  ggsave(name, plots[[task.key]][["pairwise_comparison"]], width = 12.5, height = 6.25)

  # Pairwise Top 4
  name = paste0("figures/", task.key, "_pfi_pairwise_top4.png")
  ggsave(name, plots[[task.key]][["pairwise_comparison_top4_features"]],
         width = 12.5, height = 6.25)
  print(paste(task.key, "done"))
}


# overview of model classes per task (number of models)
models_count = lapply(vic_long, function(x) table(x$learner) / length(unique(x$feature)))
model_names <- unique(pre_design$learnername)
category_names <- names(vic)
df <- data.frame(matrix(0, nrow = length(category_names), ncol = length(model_names)))
colnames(df) <- model_names
rownames(df) <- category_names
for(cat in names(models_count)) {
  for(model in names(models_count[[cat]])) {
    df[cat, model] <- models_count[[cat]][model]
  }
}
df
print(xtable(df, caption = "Uebersicht der Modelle", digits = 0), include.rownames = TRUE)


#### Correlation analysis st data ####
data = generateCanonicalDataSplits(list.tasks$st, ratio = 2 / 3, seed = 1)$validation$data()

# Spearman's Rho
data_spearman_corr <- cor(data, method = "spearman")

# plot
name = paste0("figures/st_data_cor_spearman.pdf")
pdf(file = name, width = 7, height = 7)
corrplot(data_spearman_corr, method = "circle", type = "lower", tl.col = "black")
dev.off()


#### Correlation analysis FI values ####
vic_t = lapply(vic, function(x){
  tmp = t(x[, -1])
  colnames(tmp) = x[, 1]
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
  corrplot(vic_spearman_corr[[task.key]], method = "circle", type = "lower", tl.col = "black")
  dev.off()

  name = paste0("figures/", task.key, "_pfi_cor_kendall.pdf")
  pdf(file = name, width = 7, height = 7)
  corrplot(vic_kendall_corr[[task.key]], method = "circle", type = "lower", tl.col = "black")
  dev.off()
}


## Create ranks
f_ranks = lapply(vic, function(x){
  tmp = as.data.frame(lapply(x[-1], function(y) rank(-y)))
  cbind(x[1], tmp)
})

f_ranks_long = list()
for(task.key in task.keys){
  f_ranks_long[[task.key]] = f_ranks[[task.key]] %>%
    pivot_longer(cols = starts_with("pfi"), names_to = "PFI", values_to = "Value")
  f_ranks_long[[task.key]]$learner = sub(".*_(.*?)_.*", "\\1", f_ranks_long[[task.key]]$PFI)

  plot_rank_scatter = ggplot(f_ranks_long[[task.key]], aes(x = Value, y = feature, color = learner)) +
    geom_quasirandom(alpha = alpha_value, cex = 1, shape = 16, stroke = 0) +
    labs(x = "Rank", y = "Feature", color = "Model Class",
         title = paste0("PFI ranks (", task.key, ") colored by learner")) +
    theme_minimal(base_size = 15)

  if(is_empty(plots[[task.key]])) plots[[task.key]] = list()
  plots[[task.key]][["scatter_plot_ranks"]] = plot_rank_scatter
}

# save plots
for(task.key in task.keys){
  name = paste0("figures/pfi_ranks_", task.key, "_scatter.pdf")
  ggsave(name, plots[[task.key]][["scatter_plot_ranks"]], width = 10, height = 5)
}
