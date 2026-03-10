source("init.R")

library(data.table)
library(ggplot2)
library(tidyr)
library(dplyr)
library(GGally)
library(ggbeeswarm)
library(gghalves)
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
alpha_value = 1

# Find reference model learner class and model key per task (for triangle markers in plots)
scores_path = "/media/external/rashomon/rashomon_perfs/foundmodel_scores_global.csv"
ref_model_key = setNames(rep(NA_character_, length(task.keys)), task.keys)
if (file.exists(scores_path)) {
  global_scores = data.table::fread(scores_path)
  ref_models = global_scores[, .SD[which.min(score)], by = task]
  ref_learner = setNames(
    sub(".*global_(.+)_[^_/]+_\\d+\\.rds.*", "\\1", basename(ref_models$filepath)),
    ref_models$task
  )
  ref_learner[ref_learner == "tree"] = "cart"
  for (tk in task.keys) {
    ref_row = ref_models[ref_models$task == tk, ]
    if (nrow(ref_row) == 0) next
    design_tk = design[design$rn == tk & design$learnername == "global", ]
    if (nrow(design_tk) == 0) next
    design_tk$model_key = paste0("m", design_tk$model.no)
    ref_bn = basename(ref_row$filepath[1])
    design_bn = basename(design_tk$rds)
    idx = match(ref_bn, design_bn)
    if (!is.na(idx))
      ref_model_key[tk] = design_tk$model_key[idx]
  }
} else {
  warning("foundmodel_scores_global.csv not found — reference model markers omitted")
  ref_learner = setNames(rep(NA_character_, length(task.keys)), task.keys)
}

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
  vic_long[[task.key]]$learner[vic_long[[task.key]]$learner == "tree"] = "cart"

  # Add learner_global: NA except for global models, where it holds the underlying learner
  design_global = design[design$rn == task.key & design$learnername == "global", ]
  design_global$learner_global = sub(paste0(".*global_(.+)_", task.key, ".*"), "\\1", design_global$rds)
  design_global$learner_global[design_global$learner_global == "tree"] = "cart"
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
  plot_scatter = ggplot(vic_long[[task.key]], aes(x = feature, y = Value, color = learner)) +
    geom_half_boxplot(aes(color = NULL), side = "r", nudge = 0.05,
                      fill = "gray90", color = "gray50",
                      outlier.shape = NA, width = 0.45) +
    geom_quasirandom(aes(x = as.integer(factor(feature)) - .2), alpha = alpha_value, cex = 1, shape = 16, stroke = 0, width = 0.2) +
    labs(x = "Feature", y = "Importance", color = "Model Class",
         title = paste0("PFI values (", task.key, ") colored by learner")) +
    coord_flip() +
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

# Build a global learner color mapping once across all tasks so that the same
# learner always gets the same color regardless of which tasks it appears in.
{
  orange = "#E69F00"
  fixed_colors = c(
    "gosdt"      = orange,
    "cart"       = "#D55E00",
    "svm.linear" = "#0072B2",
    "svm.radial" = "#56B4E9"
  )
  fallback_okabe = c("#009E73", "#F0E442", "#CC79A7")
  all_global_learners = sort(unique(unlist(lapply(task.keys, function(tk) {
    na.omit(vic_long[[tk]]$learner_global)
  }))))
  unassigned_learners = setdiff(all_global_learners, names(fixed_colors))
  if (length(unassigned_learners) > length(fallback_okabe)) {
    extra_cols = grDevices::hcl.colors(
      length(unassigned_learners) - length(fallback_okabe), palette = "Dark 3")
    fallback_cols = c(fallback_okabe, extra_cols)
  } else {
    fallback_cols = fallback_okabe
  }
  global_learner_colors = c(
    fixed_colors[intersect(names(fixed_colors), all_global_learners)],
    setNames(fallback_cols[seq_along(unassigned_learners)], unassigned_learners)
  )
}

vic_scaled_long = list()
vic_scaled_long_gosdt_compare = list()
plots_scaled = list()

for(task.key in task.keys){
  vic_scaled_long[[task.key]] = vic_normalized[[task.key]] %>%
    pivot_longer(cols = starts_with("pfi"), names_to = "PFI", values_to = "Value")
  vic_scaled_long[[task.key]]$learner = sub(".*_(.*?)_.*", "\\1", vic_scaled_long[[task.key]]$PFI)
  vic_scaled_long[[task.key]]$learner[vic_scaled_long[[task.key]]$learner == "tree"] = "cart"
  vic_scaled_long[[task.key]]$learner_global = vic_long[[task.key]]$learner_global
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

  if (has_treefarms) {
    treefarms_cols = names(vic_treefarms_normalized[[task.key]])[
      names(vic_treefarms_normalized[[task.key]]) == "feature" |
        grepl("^pfi_TreeFARMS_", names(vic_treefarms_normalized[[task.key]]))
    ]
    vic_scaled_long_gosdt_compare[[task.key]] = vic_treefarms_normalized[[task.key]][, treefarms_cols, drop = FALSE] %>%
      pivot_longer(cols = starts_with("pfi"), names_to = "PFI", values_to = "Value") %>%
      mutate(learner = "TreeFARMS (gosdt)")
  } else {
    vic_scaled_long_gosdt_compare[[task.key]] = NULL
  }

  # Use the global color mapping so colors are consistent across tasks.
  # Only force gosdt into the legend when a TreeFARMS comparison panel is shown.
  left_learners = sort(unique(c(
    na.omit(vic_scaled_long[[task.key]]$learner_global),
    if (has_treefarms) "gosdt" else character(0)
  )))

  scale_left = scale_color_manual(name = "Learner", values = global_learner_colors,
                                  breaks = left_learners, limits = left_learners)

  # Scatter-plot colored according to model class (w/o global), standalone plot
  plot_scatter = ggplot(subset(vic_scaled_long[[task.key]], learner != "global"),
                        aes(x = feature, y = Value, color = learner_label)) +
    geom_half_boxplot(aes(color = NULL), side = "r", nudge = 0.05,
                      fill = "gray90", color = "gray50",
                      outlier.shape = NA, width = 0.45) +
    geom_quasirandom(aes(x = as.integer(factor(feature)) - .3), alpha = alpha_value, size = 0.8, cex = 1, varwidth = TRUE, shape = 16, stroke = 0, width = 0.3) +
    labs(x = "Feature", y = "Importance") +
    coord_flip() +
    theme_minimal(base_size = 24) +
    theme(legend.text = element_text(size = 20)) +
    guides(color = guide_legend(override.aes = list(size = 5, alpha = 1))) +
    scale_left

  # Scatter-plot global — colored by underlying learner_global
  global_data = subset(vic_scaled_long[[task.key]], learner == "global")
  global_data = global_data[sample(nrow(global_data)), ]  # shuffle to avoid systematic overlap

  plot_scatter_global = ggplot(global_data,
                               aes(x = feature, y = Value, color = learner_global)) +
    geom_half_boxplot(aes(color = NULL), side = "r", nudge = 0.05,
                      fill = "gray90", color = "gray50",
                      outlier.shape = NA, width = 0.45) +
    geom_quasirandom(aes(x = as.integer(factor(feature)) - .3), alpha = alpha_value, size = 0.8, cex = 1, varwidth = TRUE, shape = 16, stroke = 0, width = 0.3) +
    # Invisible anchor to hold gosdt in the legend when a TreeFARMS panel is shown
    {if (has_treefarms) {
      gosdt_anchor = data.frame(Value = 0, feature = global_data$feature[1],
                                learner_global = "gosdt")
      geom_point(data = gosdt_anchor,
                 aes(x = feature, y = Value, color = learner_global),
                 alpha = 0, size = 7)
    }} +
    labs(x = "Feature", y = "Importance") +
    coord_flip() +
    theme_minimal(base_size = 24) +
    theme(legend.text = element_text(size = 20)) +
    guides(color = guide_legend(override.aes = list(size = 5, alpha = 1))) +
    scale_left

  # Combined plot: two panels when TreeFARMS data exists, single panel otherwise
  if (has_treefarms) {
    p_right = ggplot(vic_scaled_long_gosdt_compare[[task.key]],
                     aes(x = feature, y = Value)) +
      geom_half_boxplot(side = "r", nudge = 0.05,
                        fill = "gray90", color = "gray50",
                        outlier.shape = NA, width = 0.45) +
      geom_quasirandom(aes(x = as.integer(factor(feature)) - .3), alpha = alpha_value, size = 0.8, cex = 1, varwidth = TRUE, shape = 16, stroke = 0, color = orange, width = 0.3) +
      labs(x = "Feature", y = "Importance") +
      coord_flip() +
      theme_minimal(base_size = 24) +
      theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())

    combined = plot_scatter_global + p_right +
      plot_layout(widths = c(1, 1), guides = "collect") +
      plot_annotation(
        title = paste0("PFI values, task ", task.key, " (scaled)"),
        theme = theme(plot.title = element_text(size = 27, hjust = 0.5))
      ) &
      theme(legend.position = "bottom")
  } else {
    # Single panel: add reference model PFI as triangles (when available)
    p = plot_scatter_global
    mk = ref_model_key[task.key]
    if (!is.na(mk)) {
      pfi_col = grep(paste0("_", mk, "$"), names(vic_normalized[[task.key]]), value = TRUE)
      pfi_col = grep("^pfi_global_", pfi_col, value = TRUE)
      if (length(pfi_col) > 0) {
        ref_pfi = data.frame(
          feature = vic_normalized[[task.key]]$feature,
          Value = vic_normalized[[task.key]][[pfi_col[1]]],
          learner_global = ref_learner[task.key]
        )
        p = p + geom_point(data = ref_pfi,
                          aes(x = as.integer(factor(feature)) - .3, y = Value, fill = learner_global),
                          shape = 24, size = 2.5, colour = "black", stroke = 0.5, show.legend = FALSE) +
          scale_fill_manual(values = global_learner_colors, breaks = left_learners, limits = left_learners)
      }
    }
    combined = p +
      plot_annotation(
        title = paste0("PFI values, task ", task.key, " (scaled)"),
        theme = theme(plot.title = element_text(size = 27, hjust = 0.5))
      ) &
      theme(legend.position = "bottom")
  }

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
  n_features = nrow(vic_normalized[[task.key]])
  combined_height = max(7, n_features * 0.55 + 1.5)
  ggsave(name, plots_scaled[[task.key]][["scatter_plot_combined"]], width = 14, height = combined_height)
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


# Overview of TruVarImp (CASHomon) model classes per task
# Count unique models (PFI columns) per learner_global per task
cashomon_counts = lapply(task.keys, function(tk) {
  global_rows = subset(vic_long[[tk]], learner == "global" & !is.na(learner_global))
  if (nrow(global_rows) == 0) return(integer(0))
  tapply(global_rows$PFI, global_rows$learner_global, function(x) length(unique(x)))
})
names(cashomon_counts) = task.keys

cashomon_learners = sort(unique(unlist(lapply(cashomon_counts, names))))
df_cashomon = data.frame(
  matrix(0L, nrow = length(task.keys), ncol = length(cashomon_learners)),
  row.names = task.keys
)
colnames(df_cashomon) = cashomon_learners
for(tk in task.keys) {
  for(lrn in names(cashomon_counts[[tk]])) {
    df_cashomon[tk, lrn] = cashomon_counts[[tk]][lrn]
  }
}

# Build character version of the table with "*" marking the reference model class
df_cashomon_tex = as.data.frame(lapply(cashomon_learners, function(lrn) {
  sapply(task.keys, function(tk) {
    val = df_cashomon[tk, lrn]
    ref = ref_learner[tk]
    if (!is.na(ref) && ref == lrn && val > 0)
      paste0(val, "*")
    else
      as.character(val)
  })
}), stringsAsFactors = FALSE)
colnames(df_cashomon_tex) = cashomon_learners
rownames(df_cashomon_tex) = task.keys

df_cashomon_tex
print(xtable(df_cashomon_tex,
             caption = "Number of models of each model class within a \\CashomonSet{} found by \\TruVarImp per task. * marks the reference model class.",
             label = "tab_exp_model_CS"),
      include.rownames = TRUE,
      caption.placement = "top")


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
  f_ranks_long[[task.key]]$learner[f_ranks_long[[task.key]]$learner == "tree"] = "cart"

  plot_rank_scatter = ggplot(f_ranks_long[[task.key]], aes(x = feature, y = Value, color = learner)) +
    geom_half_boxplot(aes(color = NULL), side = "r", nudge = 0.05,
                      fill = "gray90", color = "gray50",
                      outlier.shape = NA, width = 0.45) +
    geom_quasirandom(aes(x = as.integer(factor(feature)) - .2), alpha = alpha_value, cex = 1, shape = 16, stroke = 0, width = 0.2) +
    labs(x = "Feature", y = "Rank", color = "Model Class",
         title = paste0("PFI ranks (", task.key, ") colored by learner")) +
    coord_flip() +
    theme_minimal(base_size = 15)

  if(is_empty(plots[[task.key]])) plots[[task.key]] = list()
  plots[[task.key]][["scatter_plot_ranks"]] = plot_rank_scatter
}

# save plots
for(task.key in task.keys){
  name = paste0("figures/pfi_ranks_", task.key, "_scatter.pdf")
  ggsave(name, plots[[task.key]][["scatter_plot_ranks"]], width = 10, height = 5)
}
