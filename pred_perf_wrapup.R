## Predictive performance wrap-up ----------------------------------------------

source("init.R")
library(xtable)
library(tidyr)
library(ggplot2)
library(dplyr)

## Load model performance results ----------------------------------------------

## TreeFARMS
load("data/results_modelperformances_TreeFARMS.RData")  # object: res_perf_TreeFARMS

## other learners
load("data/results_modelperformances.RData")            # object: res_dt

## Merge into one data.table
MP = rbind(res_perf_TreeFARMS, res_dt, fill = TRUE)


## Basic checks ----------------------------------------------------------------

str(MP)
head(MP)


## Boxplots of predictive performance per task ---------------------------------

# Create consistent color palette for all learners
all_learners_unique = sort(unique(MP$learner))
n_learners = length(all_learners_unique)
# Use a color palette that can handle many learners
learner_colors = scales::hue_pal()(n_learners)
names(learner_colors) = all_learners_unique

tasks = sort(unique(MP$task))

for (t in tasks) {
  MP_sub = MP[task == t]
  
  # Order learners: global at top, rest alphabetically, then TreeFARMS, gosdt, tree at bottom
  all_learners = unique(MP_sub$learner)
  global = all_learners[grepl("global", all_learners, ignore.case = TRUE)]
  treefarms = all_learners[grepl("TreeFARMS", all_learners, ignore.case = TRUE)]
  gosdt = all_learners[grepl("gosdt", all_learners, ignore.case = TRUE)]
  tree = all_learners[grepl("^tree$", all_learners, ignore.case = TRUE)]
  rest = setdiff(all_learners, c(global, treefarms, gosdt, tree))
  rest = sort(rest)
  
  learner_order = c(global, rest, gosdt, tree, treefarms)
  
  MP_sub$learner = factor(MP_sub$learner, levels = learner_order)
  
  p = ggplot(MP_sub, aes(x = learner, y = test.score, fill = learner)) +
    geom_boxplot(alpha = 0.8, outlier.size = 0.8) +
    coord_flip() +
    scale_fill_manual(values = learner_colors, drop = FALSE) +
    labs(
      title = paste("Predictive performance per learner for task", t),
      x = "Learner",
      y = ifelse(unique(MP_sub$score)[1] == "rmse", "RMSE", "Brier score"),
      fill = "Learner"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    )
  
  ggsave(
    filename = sprintf("figures/pred_perf_%s.png", t),
    plot = p,
    width = 6,
    height = 4
  )
}



# TRAIN DATA -------------------------------------------------------------------

## Load model performance results ----------------------------------------------

load("data/results_train_modelperformances.RData")        # object: res_dt_train

## Basic checks ----------------------------------------------------------------

str(res_dt_train)
head(res_dt_train)


## Boxplots of predictive performance per task ---------------------------------

# Create consistent color palette for all learners
all_learners_unique = sort(unique(res_dt_train$learner))
n_learners = length(all_learners_unique)
# Use a color palette that can handle many learners
learner_colors = scales::hue_pal()(n_learners)
names(learner_colors) = all_learners_unique

tasks = sort(unique(res_dt_train$task))

for (t in tasks) {
  MP_sub = res_dt_train[task == t]
  
  # Order learners: global at top, rest alphabetically, then TreeFARMS, gosdt, tree at bottom
  all_learners = unique(MP_sub$learner)
  global = all_learners[grepl("global", all_learners, ignore.case = TRUE)]
  treefarms = all_learners[grepl("TreeFARMS", all_learners, ignore.case = TRUE)]
  gosdt = all_learners[grepl("gosdt", all_learners, ignore.case = TRUE)]
  tree = all_learners[grepl("^tree$", all_learners, ignore.case = TRUE)]
  rest = setdiff(all_learners, c(global, treefarms, gosdt, tree))
  rest = sort(rest)
  
  learner_order = c(global, rest, gosdt, tree, treefarms)
  
  MP_sub$learner = factor(MP_sub$learner, levels = learner_order)
  
  p = ggplot(MP_sub, aes(x = learner, y = test.score, fill = learner)) +
    geom_boxplot(alpha = 0.8, outlier.size = 0.8) +
    coord_flip() +
    scale_fill_manual(values = learner_colors, drop = FALSE) +
    labs(
      title = paste("Predictive performance per learner for task", t, "(training data)"),
      x = "Learner",
      y = ifelse(unique(MP_sub$score)[1] == "rmse", "RMSE", "Brier score"),
      fill = "Learner"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    )
  
  ggsave(
    filename = sprintf("figures/pred_perf_train_%s.png", t),
    plot = p,
    width = 6,
    height = 4
  )
}

