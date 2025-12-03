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
