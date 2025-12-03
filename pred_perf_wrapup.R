## Rashomon Capacity ----------------------------------------------------------
source("init.R")
library(xtable)
library(tidyr)
library(ggplot2)
library(dplyr)

## TreeFARMS
load("data/results_modelperformances_TreeFARMS.RData")

## other
load("data/results_modelperformances.RData")
MP = rbind(res_perf_TreeFARMS, res_dt)



## analyze
# RC[is.na(RC$learnername), "learnername"] = "several"
head(MP)





ggsave("figuresXXX.png", plot1, width = 5, height = 10)
