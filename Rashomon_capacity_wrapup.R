## Rashomon Capacity ----------------------------------------------------------
source("init.R")
library(xtable)
library(tidyr)
library(ggplot2)
library(dplyr)

## TreeFARMS
load("data/results_pred.mult_TreeFARMS.RData")
RC = result_pred.mult

## other
load("data/results_pred.mult_all_but_TreeFARMS.RData")
RC = rbind(RC, result_pred.mult)



## analyze
# RC[is.na(RC$learnername), "learnername"] = "several"
head(RC)

### compare TreeFRAMS with Cashomon on "gosdt" and on "several" model classes ----------------------------------------------------------
RC_gosdt = RC[RC$learnername == "gosdt" & RC$RS.algo == "TreeFARMS", ]
RC_gosdt = subset(RC_gosdt, select = -c(RS.algo))
colnames(RC_gosdt)[3]  = "TreeFARMS"
RC_gosdt = merge(RC_gosdt, RC[RC$learnername == "gosdt" & RC$RS.algo == "CASHomon", c("taskname", "pred.mult")], by = "taskname", all.x = TRUE)
names(RC_gosdt)[names(RC_gosdt) == "pred.mult"] <- "CASHomon"
RC_gosdt = merge(RC_gosdt, RC[RC$learnername == "global" & RC$RS.algo == "CASHomon", c("taskname", "pred.mult")], by = "taskname", all.x = TRUE)
names(RC_gosdt)[names(RC_gosdt) == "pred.mult"] <- "Full CASHomon"

print(xtable(RC_gosdt, caption = "XX", digits = 9), include.rownames = FALSE)

# plot
small_value <- 1e-12

# Reshape and Clamp the data
RC_gosdt_clamped_long <- RC_gosdt %>%
  # Reshape from wide to long format
  pivot_longer(
    cols = c(TreeFARMS, CASHomon, `Full CASHomon`), 
    names_to = "Method",                            
    values_to = "Value"                             
  ) %>%
  # Apply the clamping transformation for log safety
  mutate(
    # Create the clamped value column
    Value_Clamped = ifelse(Value <= 0, small_value, Value)
  )

plot1 = ggplot(RC_gosdt_long, aes(x = Method, y = Value, fill = Method)) +
  geom_col() +
  facet_wrap(~ taskname, scales = "free_y", ncol = 3) +
  scale_y_continuous() +
  scale_fill_manual(values = c("CASHomon" = "blue", 
                               "Full CASHomon" = "darkgreen", 
                               "TreeFARMS" = "orange")) +
  labs(
    title = "RC Values by Method, Faceted by Task",
    x = "Optimization Method",
    y = "RC Value",
    fill = "Optimization Method"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    # Remove the redundant fill legend
    legend.position = "none",
    # Rotate X-axis labels (Method names) for better readability
    axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
    strip.text = element_text(face = "bold")
  )

ggsave("figures/RC_values_TreeFARMS_comparison.png", plot1, width = 10, height = 5)

#### compare Cashomon based on single learners with Cashomon on several learners ----------------------------------------------------------
RC_CS = RC[RC$RS.algo == "CASHomon", ]
RC_CS = subset(RC_CS, select = -c(RS.algo))
RC_CS_pw = pivot_wider(RC_CS, names_from = learnername, values_from = pred.mult)
RC_CS_pw = RC_CS_pw[, c(setdiff(names(RC_CS_pw), "global"), "global")]

print(xtable(RC_CS_pw, caption = "XX", digits = 4), include.rownames = FALSE)

# plot 
RC_CS_pw_long <- RC_CS_pw %>%
  pivot_longer(
    cols = -taskname,
    names_to = "Learner", 
    values_to = "RC_Value"
  ) %>%
  # Filter out NA values
  filter(!is.na(RC_Value))

plot2 = ggplot(RC_CS_pw_long, aes(x = Learner, y = RC_Value, fill = Learner)) +
  
  geom_col() +
  
  # Ensure the x-axis order is maintained, which keeps 'global' on the right
  # Use fct_inorder to preserve the original column order for the x-axis within facets
  # This makes 'global' appear on the right side of the other learners.
  facet_wrap(~ taskname, scales = "free_y", ncol = 3) +
  
  scale_y_continuous() +
  
  labs(
    title = "RC Values Comparison, Faceted by Task",
    x = "Learner (Algorithm)",
    y = "RC Value",
    fill = "Learner"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    # Rotate X-axis labels (Learner names) for readability
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    strip.text = element_text(face = "bold")
  )

ggsave("figures/RC_values_CSlearner_comparison.png", plot2, width = 10, height = 10)


## RC vs best performance per Rashomon set --------------------------------------
# Naming semantics:
# - RS.algo = set-construction algorithm (TreeFARMS or CASHomon)
# - TreeFARMS applies to learner "gosdt" only
# - CASHomon applies to several learners; learner "global" is the pooled setting

load("data/results_modelperformances_TreeFARMS.RData")
load("data/results_modelperformances.RData")

# Standardize performance data BEFORE merging:
# - non-TreeFARMS results come from CASHomon and keep their learner labels
# - TreeFARMS results are assigned learner = "gosdt"
MP_cashomon = res_dt %>%
  transmute(
    task,
    score,
    test.score,
    learner = trimws(as.character(learner)),
    RS_method = "CASHomon"
  )

MP_treefarms = res_perf_TreeFARMS %>%
  transmute(
    task,
    score,
    test.score,
    learner = "gosdt",
    RS_method = "TreeFARMS"
  )

MP = rbind(MP_cashomon, MP_treefarms, fill = TRUE) %>%
  mutate(
    learner_key = tolower(learner),
    RS_method = trimws(as.character(RS_method))
  )

# Best test performance in each method-specific Rashomon set
MP_best = MP %>%
  group_by(task, learner_key, score, RS_method) %>%
  summarize(best_test_score = min(test.score, na.rm = TRUE), .groups = "drop")

RC_sets = RC %>%
  filter(RS.algo %in% c("CASHomon", "TreeFARMS")) %>%
  transmute(
    task = taskname,
    learner = trimws(as.character(ifelse(RS.algo == "TreeFARMS", "gosdt", learnername))),
    learner_key = tolower(learner),
    RS_method = trimws(as.character(RS.algo)),
    RC_value = pred.mult
  )

# Join on task + learner + set-construction method
RC_perf = RC_sets %>%
  inner_join(MP_best, by = c("task", "learner_key", "RS_method")) %>%
  filter(is.finite(RC_value), is.finite(best_test_score))

# Quick diagnostic: which RC sets have no matching performance entry
RC_perf_unmatched = RC_sets %>%
  anti_join(MP_best, by = c("task", "learner_key", "RS_method"))
if (nrow(RC_perf_unmatched) > 0) {
  message("Unmatched RC sets (task, learner, RS_method):")
  print(unique(RC_perf_unmatched[, c("task", "learner", "RS_method")]))
}

eps = 1e-12

tasks_rc = sort(unique(RC_perf$task))

for (t in tasks_rc) {
  RC_perf_sub = RC_perf %>% filter(task == t)
  score_label = paste(sort(unique(RC_perf_sub$score)), collapse = ", ")
  
  plot_rc_vs_perf_linear = ggplot(
    RC_perf_sub,
    aes(x = best_test_score, y = RC_value, color = learner)
  ) +
    geom_point(aes(shape = RS_method), size = 2.9, alpha = 0.9) +
    scale_shape_manual(values = c("CASHomon" = 16, "TreeFARMS" = 17)) +
    labs(
      title = paste0("Task: ", t, " (linear)"),
      subtitle = paste0("Score: ", score_label),
      x = "Best score (lower is better)",
      y = "RC",
      color = "Learner",
      shape = "Method"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "none"
    )
  
  ggsave(
    filename = sprintf("figures/RC_vs_bestperf_linear_%s.png", t),
    plot = plot_rc_vs_perf_linear,
    width = 6,
    height = 4
  )
  
  RC_perf_sub_log = RC_perf_sub %>%
    mutate(RC_value_plot = ifelse(RC_value <= 0, eps, RC_value))
  
  plot_rc_vs_perf_log = ggplot(
    RC_perf_sub_log,
    aes(x = best_test_score, y = RC_value_plot, color = learner)
  ) +
    geom_point(aes(shape = RS_method), size = 2.9, alpha = 0.9) +
    scale_shape_manual(values = c("CASHomon" = 16, "TreeFARMS" = 17)) +
    scale_y_log10(labels = scales::label_number()) +
    labs(
      title = paste0("Task: ", t, " (log)"),
      subtitle = paste0("Score: ", score_label),
      x = "Best score (lower is better)",
      y = "RC (log axis)",
      color = "Learner",
      shape = "Method"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "none"
    )
  
  ggsave(
    filename = sprintf("figures/RC_vs_bestperf_log_%s.png", t),
    plot = plot_rc_vs_perf_log,
    width = 6,
    height = 4
  )
}

# Shared key figure for all RC-vs-performance plots
learners = sort(unique(RC_perf$learner))
methods = c("CASHomon", "TreeFARMS")
n_learner_rows = ceiling(length(learners) / 2)
n_rows = max(n_learner_rows, length(methods))

learner_key = data.frame(label = learners) %>%
  mutate(
    idx = row_number(),
    col = ((idx - 1) %/% n_learner_rows) + 1,
    row = ((idx - 1) %% n_learner_rows) + 1,
    x = 1 + (col - 1) * 1.5,
    y = n_learner_rows - row + 1
  )

method_key = data.frame(
  label = methods,
  x = 4.4,
  y = n_rows:(n_rows - length(methods) + 1)
)

plot_rc_vs_perf_legend = ggplot() +
  annotate("text", x = 1, y = n_rows + 1, label = "Learner", hjust = 0, fontface = "bold", size = 5) +
  annotate("text", x = 4.4, y = n_rows + 1, label = "Method", hjust = 0, fontface = "bold", size = 5) +
  geom_point(data = learner_key, aes(x = x, y = y, color = label), size = 3.4, show.legend = FALSE) +
  geom_text(data = learner_key, aes(x = x + 0.18, y = y, label = label, color = label), hjust = 0, size = 4.5, show.legend = FALSE) +
  geom_point(data = method_key, aes(x = x, y = y, shape = label), size = 3.6, color = "black", show.legend = FALSE) +
  geom_text(data = method_key, aes(x = x + 0.18, y = y, label = label), hjust = 0, size = 4.5, color = "black") +
  scale_shape_manual(values = c("CASHomon" = 16, "TreeFARMS" = 17), guide = "none") +
  guides(color = "none", shape = "none") +
  coord_cartesian(
    xlim = c(0.7, 5.8),
    ylim = c(0.5, n_rows + 1.3),
    clip = "off"
  ) +
  theme_void(base_size = 16) +
  theme(
    plot.margin = margin(t = 10, r = 30, b = 10, l = 10)
  )

ggsave(
  filename = "figures/RC_vs_bestperf_legend.png",
  plot = plot_rc_vs_perf_legend,
  width = 6,
  height = 4
)

RC_perf_corr = RC_perf %>%
  group_by(score) %>%
  summarize(
    n = n(),
    spearman_rho = cor(best_test_score, RC_value, method = "spearman", use = "complete.obs"),
    .groups = "drop"
  )

print(RC_perf_corr)
