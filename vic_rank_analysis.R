source("init.R")

library(xtable)
library(corrplot)

load("data/design.RData") # load("data/design_bs.RData")
load(paste0("data/results_vic_RS_", paste0(unique(design$rn), collapse = "_"), ".RData"))
task.keys = names(vic_RS) # german credit, compas, bike sharing, synthetic

#### Create ranks ####
f_ranks = list()
f_ranks = lapply(vic_RS, function(x){
  tmp = as.data.frame(lapply(x[-1], function(y) rank(-y)))
  tmp = cbind(x[1], tmp)
})

# check
# vic_RS$cs[1:5]
# f_ranks[[1]][1:5]

# transpose
f_ranks_t = lapply(f_ranks, function(x){
  tmp = t(x[,-1])
  colnames(tmp) = x[,1]
  tmp
})
vic_t = lapply(vic_RS, function(x){
  tmp = t(x[,-1])
  colnames(tmp) = x[,1]
  tmp
})

# Spearman's Rho 
spearman_corr <- lapply(f_ranks_t, function(x) cor(x, method = "spearman"))
vic_spearman_corr <- lapply(vic_t, function(x) cor(x, method = "spearman"))

# Kendall's Tau 
kendall_corr <- lapply(f_ranks_t, function(x) cor(x, method = "kendall"))
vic_kendall_corr <- lapply(vic_t, function(x) cor(x, method = "kendall"))

# plot
for(task.key in task.keys){
  name = paste0("figures/pfi_", task.key, "_rank_cor_spearman.pdf")
  pdf(file = name, width = 7, height = 7)
  corrplot(spearman_corr[[task.key]], method="circle", type="lower", tl.col = "black")
  dev.off()
  
  name = paste0("figures/pfi_", task.key, "_cor_spearman.pdf")
  pdf(file = name, width = 7, height = 7)
  corrplot(vic_spearman_corr[[task.key]], method="circle", type="lower", tl.col = "black")
  dev.off()
  
  name = paste0("figures/pfi_", task.key, "_rank_cor_kendall.pdf")
  pdf(file = name, width = 7, height = 7)
  corrplot(kendall_corr[[task.key]], method="circle", type="lower", tl.col = "black")
  dev.off()
  
  name = paste0("figures/pfi_", task.key, "_cor_kendall.pdf")
  pdf(file = name, width = 7, height = 7)
  corrplot(vic_kendall_corr[[task.key]], method="circle", type="lower", tl.col = "black")
  dev.off()
}


