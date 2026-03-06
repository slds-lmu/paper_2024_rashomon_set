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
library(patchwork)


## General settings ############################################################
load("data/design_all_but_TreeFARMS.RData")
load("data/results_vic_TreeFARMS.RData")

# Keep separate names to avoid accidental overwrites
vic_treefarms = vic
vic_treefarms_normalized = vic_normalized

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

  # Add learner_global: NA except for global models, where it holds the underlying learner
  design_global = design[design$rn == task.key & design$learnername == "global", ]
  design_global$learner_global = sub(paste0(".*global_(.+)_", task.key, ".*"), "\\1", design_global$rds)
  design_global$model_key = paste0("m", design_global$model.no)
  vic_long[[task.key]]$model_key = sub(".+_(m\\d+)$", "\\1", vic_long[[task.key]]$PFI)
  vic_long[[task.key]]$learner_global = ifelse(
    vic_long[[task.key]]$learner == "global",
    design_global$learner_global[match(vic_long[[task.key]]$model_key, design_global$model_key)],
    NA
  )
  vic_long[[task.key]]$model_key = NULL

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
vic_scaled_long_gosdt_compare = list()
plots_scaled = list()

for(task.key in task.keys){
  vic_scaled_long[[task.key]] = vic_normalized[[task.key]] %>%
    pivot_longer(cols = starts_with("pfi"), names_to = "PFI", values_to = "Value")
  vic_scaled_long[[task.key]]$learner = sub(".*_(.*?)_.*", "\\1", vic_scaled_long[[task.key]]$PFI)
  vic_scaled_long[[task.key]]$learner_global = ifelse(
    !is.na(vic_long[[task.key]]$learner_global),
    paste0("TruVarImp (", vic_long[[task.key]]$learner_global, ")"),
    NA
  )
  vic_scaled_long[[task.key]]$learner_label = ifelse(
    vic_scaled_long[[task.key]]$learner != "global",
    paste0("TruVarImp (", vic_scaled_long[[task.key]]$learner, ")"),
    NA
  )

  # Build gosdt comparison data for the right panel:
  # - TruVarImp/CASHomon is stored as pfi_gosdt_*
  # - TreeFARMS gosdt models are stored as pfi_TreeFARMS_*
  has_gosdt  = any(grepl("^pfi_gosdt_",     names(vic_normalized[[task.key]])))
  has_treefarms = any(grepl("^pfi_TreeFARMS_", names(vic_treefarms_normalized[[task.key]])))

  if (has_gosdt || has_treefarms) {
    gosdt_cols  = names(vic_normalized[[task.key]])[
      names(vic_normalized[[task.key]]) == "feature" |
        grepl("^pfi_gosdt_", names(vic_normalized[[task.key]]))
    ]
    treefarms_cols = names(vic_treefarms_normalized[[task.key]])[
      names(vic_treefarms_normalized[[task.key]]) == "feature" |
        grepl("^pfi_TreeFARMS_", names(vic_treefarms_normalized[[task.key]]))
    ]
    gosdt_truevarimp  = vic_normalized[[task.key]][, gosdt_cols,  drop = FALSE]
    gosdt_treefarms = vic_treefarms_normalized[[task.key]][, treefarms_cols, drop = FALSE]

    if (has_gosdt && has_treefarms) {
      gosdt_compare = merge(gosdt_truevarimp, gosdt_treefarms, by = "feature", all = TRUE)
    } else if (has_gosdt) {
      gosdt_compare = gosdt_truevarimp
    } else {
      gosdt_compare = gosdt_treefarms
    }

    # Lookup model class for TruVarImp (gosdt) models from design (same pattern as learner_global)
    design_gosdt = design[design$rn == task.key & design$learnername == "gosdt", ]
    design_gosdt$model_class = sub(paste0(".*gosdt_(.+)_", task.key, ".*"), "\\1", design_gosdt$rds)
    design_gosdt$model_key   = paste0("m", design_gosdt$model.no)

    vic_scaled_long_gosdt_compare[[task.key]] = gosdt_compare %>%
      pivot_longer(cols = starts_with("pfi"), names_to = "PFI", values_to = "Value") %>%
      mutate(
        model_key = sub(".+_(m\\d+)$", "\\1", PFI),
        learner = case_when(
          grepl("^pfi_TreeFARMS_", PFI) ~ "TreeFARMS (gosdt)",
          grepl("^pfi_gosdt_",     PFI) ~ paste0("TruVarImp (",
            design_gosdt$model_class[match(model_key, design_gosdt$model_key)], ")"),
          TRUE ~ "gosdt"
        )
      ) %>%
      select(-model_key)
  } else {
    vic_scaled_long_gosdt_compare[[task.key]] = NULL
  }

  # Shared color scale: built from TruVarImp-formatted labels used in both panels
  all_learners = sort(unique(c(
    na.omit(vic_scaled_long[[task.key]]$learner_label),
    na.omit(vic_scaled_long[[task.key]]$learner_global)
  )))
  okabe_ito = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
  if (length(all_learners) > length(okabe_ito)) {
    extra_cols = grDevices::hcl.colors(length(all_learners) - length(okabe_ito), palette = "Dark 3")
    palette_cols = c(okabe_ito, extra_cols)
  } else {
    palette_cols = okabe_ito
  }
  learner_colors = setNames(palette_cols[seq_along(all_learners)], all_learners)
  shared_color_scale = scale_color_manual(name = "Model Class", values = learner_colors)

  # Scatter-plot colored according to model class (w/o global), right panel of combined
  plot_scatter = ggplot(subset(vic_scaled_long[[task.key]], learner != "global"),
                        aes(x = Value, y = feature, color = learner_label)) +
    geom_quasirandom(alpha = alpha_value, cex = 1, shape = 16, stroke = 0) +
    labs(x = "Importance", y = "Feature") +
    theme_minimal(base_size = 24) +
    theme(legend.text = element_text(size = 20)) +
    guides(color = guide_legend(override.aes = list(size = 7, alpha = 1))) +
    shared_color_scale

  # Scatter-plot global, left panel of combined
  plot_scatter_global = ggplot(subset(vic_scaled_long[[task.key]], learner == "global"),
                               aes(x = Value, y = feature, color = learner_global)) +
    geom_quasirandom(alpha = alpha_value, cex = 1, shape = 16, stroke = 0) +
    labs(x = "Importance", y = "Feature") +
    theme_minimal(base_size = 24) +
    theme(legend.text = element_text(size = 20)) +
    guides(color = "none") +
    shared_color_scale

  # Combined: global (left) + non-global (right, y-axis stripped)
  p_right_scatter = plot_scatter +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())

  combined = plot_scatter_global + p_right_scatter +
    plot_layout(widths = c(1, 1), guides = "collect") +
    plot_annotation(
      title = paste0("PFI values, task ", task.key, " (scaled)"),
      theme = theme(plot.title = element_text(size = 27, hjust = 0.5))
    ) &
    theme(legend.position = "bottom")

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
  plots_scaled[[task.key]][["scatter_plot_global"]] = plot_scatter_global
  plots_scaled[[task.key]][["scatter_plot_combined"]] = combined
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
  name = paste0("figures/", task.key, "_pfi_scatter_global_scaled.png")
  ggsave(name, plots_scaled[[task.key]][["scatter_plot_global"]], width = 10, height = 5)
  name = paste0("figures/", task.key, "_pfi_scatter_combined_scaled.png")
  ggsave(name, plots_scaled[[task.key]][["scatter_plot_combined"]], width = 14, height = 8)
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
  # name = paste0("figures/", task.key, "_pfi_pairwise.png")
  # ggsave(name, plots[[task.key]][["pairwise_comparison"]], width = 12.5, height = 6.25)

  # Pairwise Top 4
  # name = paste0("figures/", task.key, "_pfi_pairwise_top4.png")
  # ggsave(name, plots[[task.key]][["pairwise_comparison_top4_features"]],
  #        width = 12.5, height = 6.25)
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
