## Rashomon Capacity ----------------------------------------------------------
source("../init.R")
library(xtable)
library(tidyr)
library(ggplot2)
library(dplyr)

## TreeFARMS
load("../data/results_pred.mult_TreeFARMS.RData")
RC = result_pred.mult

## other
load("../data/results_pred.mult_all_but_TreeFARMS.RData")
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

ggsave("../figures/RC_values_TreeFARMS_comparison.png", plot1, width = 10, height = 5)

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

ggsave("../figures/RC_values_CSlearner_comparison.png", plot2, width = 10, height = 10)


## RC vs best performance per Rashomon set --------------------------------------
# Naming semantics:
# - RS.algo = set-construction algorithm (TreeFARMS or CASHomon)
# - TreeFARMS applies to learner "gosdt" only
# - CASHomon applies to several learners; learner "global" is the pooled setting

load("../data/results_modelperformances_TreeFARMS.RData")
load("../data/results_modelperformances.RData")

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

# Filter to only TreeFARMS/gosdt and CASHomon/global
RC_sets_filtered = RC_sets %>%
  filter(
    (RS_method == "TreeFARMS" & learner_key == "gosdt") |
    (RS_method == "CASHomon"  & learner_key == "global")
  ) %>%
  mutate(
    group_label = case_when(
      RS_method == "TreeFARMS" ~ "TreeFARMS (gosdt)",
      RS_method == "CASHomon"  ~ "TruVarImp (CASH)"
    )
  )

# Join ALL individual model performances (not just best) for boxplot visualisation
RC_perf_all = RC_sets_filtered %>%
  inner_join(MP, by = c("task", "learner_key", "RS_method")) %>%
  filter(is.finite(RC_value), is.finite(test.score))

## Reference model test performance -------------------------------------------
# The reference model defines the epsilon-ball for each Rashomon set:
#   CASHomon (global): the model with the best TRAINING CV score across all
#                      model classes for that task (from run_models_merged.rds).
#   TreeFARMS (gosdt): the tree with the lowest training metric inside the
#                      TreeFARMS modelcontainer (found via reticulate).
# We evaluate these once on the held-out test set and cache the result.

if (!file.exists("data/results_ref_model_performances.RData")) {

  library(mlr3measures)

  # `model`: loaded mlr3 learner OR path string (loaded via readRDS).
  eval_ref_model <- function(model, taskname) {
    task <- list.tasks[[taskname]]
    if (taskname == "bs") {                        # replicate pipeline fix for bs
      task_data <- as.data.frame(task$data())
      for (i in seq_along(task_data))
        if (is.logical(task_data[[i]])) task_data[[i]] <- as.factor(task_data[[i]])
      task <- as_task_regr(task_data, target = task$target_names, id = task$id)
    }
    test_task <- generateCanonicalDataSplits(task, ratio = 2 / 3, seed = 1)$validation
    pred  <- model$predict_newdata(test_task$data(cols = test_task$feature_names))
    truth <- test_task$data(cols = test_task$target_names)[[1]]
    if (inherits(test_task, "TaskClassif"))
      list(test.score = bbrier(truth, pred$prob[, 1], positive = test_task$positive),
           score = "brier")
    else
      list(test.score = rmse(truth, pred$response), score = "rmse")
  }

  # ── CASHomon (global) ────────────────────────────────────────────────────────
  global_ref <- data.table::fread(
    "/media/external/rashomon/rashomon_perfs/foundmodel_scores_global.csv"
  )[, .SD[which.min(score)], by = task]

  ref_cashomon <- rbindlist(lapply(seq_len(nrow(global_ref)), function(i) {
    res <- tryCatch({
      if (!file.exists(global_ref$filepath[i]))
        stop("file not found: ", global_ref$filepath[i])
      eval_ref_model(readRDS(global_ref$filepath[i]), global_ref$task[i])
    }, error = function(e) {
      message("CASHomon reference failed: task=", global_ref$task[i], " — ", e$message)
      list(test.score = NA_real_, score = NA_character_)
    })
    data.table(task = global_ref$task[i], ref_test_score = res$test.score,
               score = res$score, group_label = "TruVarImp (CASH)")
  }))

  # ── TreeFARMS (gosdt) ────────────────────────────────────────────────────────
  treefarms_job_ids <- data.table::fread(
    "/media/external/rashomon/datafiles/treefarms/treefarms_info.csv"
  )[offset == 0.05 & balance == FALSE & successful == TRUE & use.adder == FALSE,
    .(job.id, taskname)]

  ref_treefarms <- rbindlist(fill = TRUE, lapply(names(list.tasks.binarized), function(tname) {
    jid <- treefarms_job_ids[taskname == tname, job.id[1L]]
    if (is.na(jid)) {
      message("No TreeFARMS model found for task: ", tname)
      return(data.table(task = tname, ref_test_score = NA_real_, score = NA_character_,
                        group_label = "TreeFARMS (gosdt)"))
    }
    tryCatch({
      treefarms.model <- readRDS(sprintf(
        "/media/external/rashomon/datafiles/treefarms/treefarms_%s.rds", jid
      ))
      try(treefarms.model$modelcontainer)        # first access can fail via reticulate
      modelcontainer    <- treefarms.model$modelcontainer
      metric_values_vec <- sapply(modelcontainer$available_metrics$metric_values, `[[`, 1)
      best.metric.idx   <- which.min(metric_values_vec)
      min_metric        <- metric_values_vec[[best.metric.idx]]
      n_tied_ref        <- sum(metric_values_vec == min_metric)
      if (n_tied_ref > 1L)
        message(sprintf("Task %s: %d trees tied at min training metric (%.6f); picking first.",
                        tname, n_tied_ref, min_metric))

      # metric_pointers index into msc.storage; selected_tree = cumulative count + 1.
      py          <- reticulate::py
      py$msc      <- modelcontainer
      py$best_idx <- as.integer(best.metric.idx - 1L)
      treefarms.model$param_set$values$selected_tree <- reticulate::py_eval(
        "str(sum(msc.storage[msc.available_metrics['metric_pointers'][i]]['count'] for i in range(best_idx)) + 1)"
      )
      perf <- eval_ref_model(treefarms.model, tname)
      data.table(task = tname, ref_test_score = perf$test.score, score = perf$score,
                 group_label = "TreeFARMS (gosdt)",
                 n_tied_ref = n_tied_ref, min_train_metric = min_metric)
    }, error = function(e) {
      message(sprintf("TreeFARMS reference failed for task %s: %s", tname, e$message))
      data.table(task = tname, ref_test_score = NA_real_, score = NA_character_,
                 group_label = "TreeFARMS (gosdt)")
    })
  }))

  ref_perf_all <- rbind(ref_cashomon, ref_treefarms, fill = TRUE)
  save(ref_perf_all, file = "../data/results_ref_model_performances.RData")
  message("Reference model performances saved to data/results_ref_model_performances.RData")

  cat("\nTreeFARMS tied reference-model counts (exact minimum, not epsilon-ball):\n")
  print(ref_treefarms[, .(task, min_train_metric, n_tied_ref)])

} else {
  load("../data/results_ref_model_performances.RData")  # object: ref_perf_all
}

# Harmonize label in case the cache was saved with an older name.
ref_perf_all[group_label == "CASHomon (global)", group_label := "TruVarImp (CASH)"]

tasks_rc = sort(unique(RC_perf_all$task))

group_colours = c("TreeFARMS (gosdt)" = "orange", "TruVarImp (CASH)" = "darkgreen")

for (t in tasks_rc) {
  RC_sub = RC_perf_all %>% filter(task == t)
  score_label = paste(sort(unique(RC_sub$score)), collapse = ", ")

  # ── Tukey boxplot statistics per group ───────────────────────────────────────
  RC_sub_stats = RC_sub %>%
    group_by(group_label, RC_value) %>%
    summarize(
      q1  = quantile(test.score, 0.25, na.rm = TRUE),
      med = median(test.score,         na.rm = TRUE),
      q3  = quantile(test.score, 0.75, na.rm = TRUE),
      iqr = IQR(test.score,            na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(fence_lo = q1 - 1.5 * iqr, fence_hi = q3 + 1.5 * iqr)

  RC_sub_whiskers = RC_sub %>%
    left_join(RC_sub_stats %>% select(group_label, fence_lo, fence_hi),
              by = "group_label") %>%
    filter(test.score >= fence_lo, test.score <= fence_hi) %>%
    group_by(group_label) %>%
    summarize(wlo = min(test.score, na.rm = TRUE),
              whi = max(test.score, na.rm = TRUE),
              .groups = "drop")

  RC_sub_stats  = RC_sub_stats %>% left_join(RC_sub_whiskers, by = "group_label")
  RC_sub_outliers = RC_sub %>%
    left_join(RC_sub_stats %>% select(group_label, fence_lo, fence_hi),
              by = "group_label") %>%
    filter(test.score < fence_lo | test.score > fence_hi)

  # Box half-height in absolute RC units (additive for linear, multiplicative for log)
  rc_range = diff(range(RC_sub_stats$RC_value))
  hh = if (rc_range > 0) rc_range * 0.06 else RC_sub_stats$RC_value[1] * 0.08

  df_linear = RC_sub_stats %>%
    mutate(ylo = RC_value - hh, yhi = RC_value + hh,
           ycap_lo = RC_value - hh * 0.5, ycap_hi = RC_value + hh * 0.5)

  df_log = RC_sub_stats %>%
    mutate(across(c(wlo, q1, med, q3, whi, RC_value), ~ ifelse(. <= 0, eps, .))) %>%
    mutate(           ylo = RC_value / 1.07, yhi = RC_value * 1.07,
           ycap_lo = RC_value / 1.035, ycap_hi = RC_value * 1.035)

  RC_sub_outliers_log = RC_sub_outliers %>%
    mutate(test.score = ifelse(test.score <= 0, eps, test.score),
           RC_value   = ifelse(RC_value   <= 0, eps, RC_value))

  # ── Shared drawing function ──────────────────────────────────────────────────
  # ref_df: optional data.frame with columns ref_test_score and group_label.
  #         A filled triangle (▲) is placed at (ref_test_score, RC_value),
  #         marking the test performance of the model that defined the set.
  #         The y position is obtained by joining ref_df with df.
  draw_hboxplot = function(df, outliers, ref_df = NULL) {
    p <- ggplot(df, aes(color = group_label)) +
      geom_segment(aes(x = wlo, xend = q1,  y = RC_value, yend = RC_value), linewidth = 0.8) +
      geom_segment(aes(x = q3,  xend = whi, y = RC_value, yend = RC_value), linewidth = 0.8) +
      geom_segment(aes(x = wlo, xend = wlo, y = ycap_lo,  yend = ycap_hi),  linewidth = 0.8) +
      geom_segment(aes(x = whi, xend = whi, y = ycap_lo,  yend = ycap_hi),  linewidth = 0.8) +
      geom_segment(aes(x = q1,  xend = q3,  y = yhi,      yend = yhi),      linewidth = 0.8) +
      geom_segment(aes(x = q1,  xend = q3,  y = ylo,      yend = ylo),      linewidth = 0.8) +
      geom_segment(aes(x = q1,  xend = q1,  y = ylo,      yend = yhi),      linewidth = 0.8) +
      geom_segment(aes(x = q3,  xend = q3,  y = ylo,      yend = yhi),      linewidth = 0.8) +
      geom_segment(aes(x = med, xend = med, y = ylo,      yend = yhi),      linewidth = 1.6) +
      geom_point(data = outliers,
                 aes(x = test.score, y = RC_value, color = group_label),
                 shape = 21, size = 2, fill = "white", stroke = 1) +
      scale_color_manual(values = group_colours) +
      guides(color = guide_legend(override.aes = list(
        shape = 16,
        linewidth = 0,
        fill = NA,
        size = 3
      ))) +
      labs(color = "Method") +
      theme_minimal(base_size = 16) +
      theme(legend.position = "bottom")

    if (!is.null(ref_df) && nrow(ref_df) > 0) {
      ref_plot <- ref_df %>%
        left_join(df %>% distinct(group_label, RC_value), by = "group_label") %>%
        filter(!is.na(RC_value))
      if (nrow(ref_plot) > 0)
        p <- p +
          geom_point(data = ref_plot,
                     aes(x = ref_test_score, y = RC_value, color = group_label),
                     shape = 17, size = 4, show.legend = FALSE)
    }
    p
  }

  # Reference model: triangle on each boxplot row; 0–2 rows depending on groups present.
  ref_sub     <- ref_perf_all %>% filter(task == t, !is.na(ref_test_score))
  ref_sub_log <- ref_sub %>%
    mutate(ref_test_score = ifelse(ref_test_score <= 0, eps, ref_test_score))

  # ── Linear plot ──────────────────────────────────────────────────────────────
  plot_rc_vs_perf_linear = draw_hboxplot(df_linear, RC_sub_outliers,
                                         ref_df = ref_sub) +
    labs(title    = paste0("Task: ", t, " (linear)"),
         subtitle = paste0("Score: ", score_label),
         x = "Brier score (lower is better)", y = "RC", color = "Method")

  ggsave(sprintf("../figures/RC_vs_bestperf_linear_%s.png", t),
         plot_rc_vs_perf_linear, width = 6, height = 4)

  # ── Log plot ─────────────────────────────────────────────────────────────────
  plot_rc_vs_perf_log = draw_hboxplot(df_log, RC_sub_outliers_log,
                                      ref_df = ref_sub_log) +
    scale_x_log10(labels = scales::label_number()) +
    scale_y_log10(labels = scales::label_number()) +
    labs(title    = paste0("Task: ", t, " (log)"),
         subtitle = paste0("Score: ", score_label),
         x = "Brier score, log axis (lower is better)", y = "RC (log axis)", color = "Method")

  ggsave(sprintf("../figures/RC_vs_bestperf_log_%s.png", t),
         plot_rc_vs_perf_log, width = 6, height = 4)
}

