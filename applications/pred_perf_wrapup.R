## Predictive performance wrap-up ----------------------------------------------

source("../init.R")
library(xtable)
library(tidyr)
library(ggplot2)
library(dplyr)
library(ggbeeswarm)

## Load model performance results ----------------------------------------------

## TreeFARMS
load("../data/results_modelperformances_TreeFARMS.RData")  # object: res_perf_TreeFARMS

## other learners
load("../data/results_modelperformances.RData")            # object: res_dt

## Merge into one data.table
MP = rbind(res_perf_TreeFARMS, res_dt, fill = TRUE)


## Basic checks ----------------------------------------------------------------

str(MP)
head(MP)


## Violin plots of predictive performance per task ------------------------------

# Create consistent color palette for all learners,
# matching the colors used in Rashomon_capacity_wrapup.R:
#   global    = "darkgreen"  (mirrors "CASHomon (global)")
#   gosdt /
#   TreeFARMS = "orange"     (mirrors "TreeFARMS (gosdt)")
all_learners_unique = sort(unique(MP$learner))
learner_colors = setNames(scales::hue_pal()(length(all_learners_unique)), all_learners_unique)
learner_colors[names(learner_colors) == "global"]    = "darkgreen"
learner_colors[names(learner_colors) == "gosdt"]     = "orange"
learner_colors[names(learner_colors) == "TreeFARMS"] = "orange"

tasks = sort(unique(MP$task))

for (t in tasks) {
  MP_sub = MP[task == t]

  # Keep only the selected learners
  all_learners = unique(MP_sub$learner)
  global = all_learners[grepl("global", all_learners, ignore.case = TRUE)]
  treefarms = all_learners[grepl("TreeFARMS", all_learners, ignore.case = TRUE)]
  gosdt = all_learners[grepl("gosdt", all_learners, ignore.case = TRUE)]
  tree = all_learners[grepl("^tree$", all_learners, ignore.case = TRUE)]
  selected_learners = unique(c(global, treefarms)) # selected_learners = unique(c(global, gosdt, tree, treefarms))
  MP_sub = MP_sub[learner %in% selected_learners]

  if (nrow(MP_sub) == 0) next

  learner_order = c(global, gosdt, tree, treefarms)

  MP_sub$learner = factor(MP_sub$learner, levels = learner_order)

  metric_name = unique(MP_sub$score)[1]

  p_violin = ggplot(MP_sub, aes(x = learner, y = test.score, fill = learner)) +
    geom_violin(
      alpha = 0.8,
      trim = TRUE,
      scale = "count",
      adjust = 0.7
    ) +
    coord_flip() +
    scale_fill_manual(values = learner_colors, drop = FALSE) +
    scale_x_discrete(labels = c("global" = "TruVarImp (CASH)", "TreeFARMS" = "TreeFARMS (gosdt)")) +
    labs(
      x = "Method",
      y = ifelse(metric_name == "rmse", "RMSE", "Brier score (lower is better)"),
      fill = "Method"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none"
    )

  ggsave(
    filename = sprintf("figures/pred_perf_%s.png", t),
    plot = p_violin,
    width = 6,
    height = 4
  )

  p_beeswarm = ggplot(MP_sub, aes(x = test.score, y = learner, color = learner))
  if ("geom_quasirandom" %in% getNamespaceExports("ggbeeswarm")) {
    p_beeswarm = p_beeswarm +
      ggbeeswarm::geom_quasirandom(
        alpha = 0.7,
        size = 1.6,
        groupOnX = FALSE,
        varwidth = TRUE
      )
  } else {
    # Robust fallback for older ggbeeswarm versions.
    p_beeswarm = p_beeswarm +
      geom_jitter(
        alpha = 0.7,
        size = 1.6,
        width = 0,
        height = 0.15
      )
  }

  p_beeswarm = p_beeswarm +
    scale_color_manual(values = learner_colors, drop = FALSE) +
    scale_y_discrete(labels = c("global" = "TruVarImp (CASH)", "TreeFARMS" = "TreeFARMS (gosdt)")) +
    labs(
      x = ifelse(metric_name == "rmse", "RMSE", "Brier score (lower is better)"),
      y = "Method",
      color = "Method"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none"
    )

  ggsave(
    filename = sprintf("../figures/pred_perf_beeswarm_%s.png", t),
    plot = p_beeswarm,
    width = 6,
    height = 4
  )
}

