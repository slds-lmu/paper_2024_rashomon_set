## Rashomon Capacity ----------------------------------------------------------
source("init.R")
library(xtable)
library(tidyr)

## TreeFARMS
load("data/results_pred.mult_TreeFARMS.RData")
RC = result_pred.mult

## other
load("data/results_pred.mult_all_but_TreeFARMS.RData")
RC = rbind(RC, result_pred.mult)



## analyze
RC[is.na(RC$learnername), "learnername"] = "several"
head(RC)

# compare TreeFRAMS with Cashomon on "gosdt" and on "several" model classes 
RC_gosdt = RC[RC$learnername == "gosdt" & RC$RS.algo == "TreeFARMS", ]
RC_gosdt = subset(RC_gosdt, select = -c(RS.algo))
colnames(RC_gosdt)[3]  = "TreeFARMS"
RC_gosdt = merge(RC_gosdt, RC[RC$learnername == "gosdt" & RC$RS.algo == "CASHomon", c("taskname", "pred.mult")], by = "taskname", all.x = TRUE)
names(RC_gosdt)[names(RC_gosdt) == "pred.mult"] <- "CASHomon"
RC_gosdt = merge(RC_gosdt, RC[RC$learnername == "several" & RC$RS.algo == "CASHomon", c("taskname", "pred.mult")], by = "taskname", all.x = TRUE)
names(RC_gosdt)[names(RC_gosdt) == "pred.mult"] <- "Full CASHomon"

print(xtable(RC_gosdt, caption = "XX", digits = 9), include.rownames = FALSE)

# compare Cashomon based on single learners with Cashomon on several learners
RC_CS = RC[RC$RS.algo == "CASHomon", ]
RC_CS = subset(RC_CS, select = -c(RS.algo))
RC_CS_pw = pivot_wider(RC_CS, names_from = learnername, values_from = pred.mult)

print(xtable(RC_CS_pw, caption = "XX", digits = 4), include.rownames = FALSE)

