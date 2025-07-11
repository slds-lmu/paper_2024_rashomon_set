rm(list = ls(all.names = TRUE))
source("init.R")

reg.lse <- getRegistry("../registry_paper_2024_rashomon_set_lseruns/registry8", make.default = TRUE)


# TODO:
#  - run others with different kernels as well
#  - run with test functions


deeper <- grep("truvar.imp.beta|truvar.imp.delta|truvar.imp.eta|truvar.imp.r5", optimizer.scenarios, value = TRUE, invert = TRUE)


addProblem("osr", fun = osGenerator, reg = reg.lse)

addAlgorithm("runOptimize", fun = runOptimize, reg = reg.lse)

addExperiments(reg = reg.lse,
  algo.designs = list(runOptimize = CJ(
    optimizer = optimizer.scenarios
  )),
  prob.design = list(osr = CJ(
    dataset = c("gc", "cs", "bs", "st", "cr", "mk", "bc", "cs.bin", "fc.bin"),
    learners = c("all", names(getLearnersMeta())),
    logscale = c(TRUE, FALSE)
  )),
  repls = 30
)

addExperiments(reg = reg.lse,
  algo.designs = list(runOptimize = CJ(
    optimizer = deeper,
    initsize = c(10, 30, 100),
    nrounds = 600
  )),
  prob.design = list(osr = CJ(
    dataset = c("gc", "cs", "bs", "st", "cr", "mk", "bc", "cs.bin", "fc.bin"),
    learners = c("all", names(getLearnersMeta())),
    logscale = c(TRUE, FALSE)
  )),
  repls = 30
)

### ------

addExperiments(reg = reg.lse,
  algo.designs = list(runOptimize = CJ(
    optimizer = deeper,
    kerneltype = c("matern5_2", "matern3_2", "gauss", "exp", "powexp"),
    epsilon = c(0.05, 0.2, 0.8)
  )),
  prob.design = list(osr = rbind(
      CJ(
        dataset = c("gc", "cs", "bs", "st", "cr", "mk", "bc", "cs.bin", "fc.bin"),
        learners = c("all", names(getLearnersMeta())),
        logscale = c(TRUE, FALSE)
      ),
      CJ(
        dataset = "DUMMY",
        learners = c("gp2", "gp3", "quadratic2", "quadratic3", "branin", "hartmann6"),
        logscale = FALSE
      )
    )
  ),
  repls = 10
)

testJob(364)

findNotDone() -> fj
fj <- fj[sample.int(nrow(fj))]
submitJobs(fj, resources = list(walltime = 360000, memory = 4000, ncpus = 1), reg = reg.lse)


submitJobs(tail(fj, -1000), resources = list(walltime = 360000, memory = 4000, ncpus = 1), reg = reg.lse)


testJob(1981)


library("data.table")

allfilenames <- list.files("data/lseruns", pattern = '\\.csv$', full.names = TRUE)

allfiles <- parallel::mclapply(allfilenames, function(x) {
  filename <- x
  x <- sub("(matern.)_", "\\1", x)
  x <- sub("\\.csv$", "", x)
  fields <- strsplit(x, "_")[[1]][2:10]
  fields <- gsub(".*:", "", fields)
  names(fields) <- c("model", "searchspace", "task", "logperf", "seed", "initsize", "nrounds", "kernel", "epsilon")
  fields <- as.data.table(as.list(fields))
  if (is.na(fields$epsilon)) fields$epsilon <- 0.05 else fields$epsilon <- as.numeric(fields$epsilon)
  if (is.na(fields$nrounds)) fields$nrounds <- 401 else fields$nrounds <- as.numeric(fields$nrounds)
  if (is.na(fields$initsize)) fields$initsize <- 10 else fields$initsize <- as.numeric(fields$initsize)
  if (is.na(fields$kernel)) fields$kernel <- "matern52"  # matern5_2, but we removed the underscore with the sub() earlier
  fields$seed <- as.numeric(fields$seed)
  cbind(fread(filename, integer64 = "numeric"), fields)
}, mc.cores = 90)

#allfiles <- parallel::mclapply(list.files("data/lseruns", pattern = 'epsilon.*\\.csv$', full.names = TRUE), function(x) {
#  fread(x, integer64 = "numeric")[, filename := x]
#}, mc.cores = 90)


allruns <- rbindlist(allfiles)

#allruns <- cbind(allruns, t(gsub(".*:", "", sapply(strsplit(allruns$filename, "_"), function(x) {
#  if (length(x) == 8) x[1:8] else c(x[1:6], "10", "400")
#}))[2:8, ]))


#allruns <- cbind(allruns, t(gsub(".*:|\\.csv$", "",
#  sapply(strsplit(sub("(matern.)_", "\\1", allruns$filename), "_"), `[`, 2:10)))
#)

#setnames(allruns, paste0("V", 1:9), c("model", "searchspace", "task", "logperf", "seed", "initsize", "nrounds", "kernel", "epsilon"))

#allruns[, seed := as.numeric(seed)]
#allruns[, nrounds := as.numeric(nrounds)]
#allruns[, epsilon := as.numeric(epsilon)]
#allruns[, filename := NULL]
allruns[, modeled.optimum := as.numeric(modeled.optimum)]
allruns[, max.relative.model.error := as.numeric(max.relative.model.error)]
#allruns[, initsize := as.numeric(initsize)]

allruns[, idx := seq_len(.N), by = .(model, searchspace, task, logperf, seed, initsize, nrounds, kernel, epsilon)]

saveRDS(allruns, "data/allseruns_extras.rds")
# ------------------------------------------------------------------------------

library("data.table")
deeper <- grep("truvar.imp.beta|truvar.imp.delta|truvar.imp.eta|truvar.imp.r5", optimizer.scenarios, value = TRUE, invert = TRUE)
allruns <- readRDS("data/allseruns_extras.rds")

allruns <- allruns[nrounds == 400 | seed <= 10]

allruns <- allruns[seed <= 10 & idx <= 400 & model %in% deeper]

runconfigs <- allruns[, by = c("model", "searchspace", "task", "logperf", "seed", "initsize", "nrounds", "kernel", "epsilon"), .(idx = 1:400)]

# skel <- CJ(
#   model = unique(allruns$model),
#   searchspace = unique(allruns$searchspace),
#   task = unique(allruns$task),
#   logperf = unique(allruns$logperf),
#   seed = unique(allruns$seed),
#   initsize = unique(allruns$initsize),
#   nrounds = unique(allruns$nrounds),
#   kernel = unique(allruns$kernel),
#   epsilon = unique(allruns$epsilon),
#   idx = seq_len(max(allruns$idx)),
#   sorted = TRUE
# )

allruns.full <- allruns[runconfigs, on = .(model, searchspace, task, logperf, seed, initsize, nrounds, kernel, epsilon, idx)]
allruns.full[, `:=`(
  accuracy = (model.TP + model.TN) /
             (model.TP + model.TN + model.FP + model.FN),

  F1       = 2 * model.TP /
             (2 * model.TP + model.FP + model.FN),

  # avoid integer overflow
  MCC      = (as.numeric(model.TP) * as.numeric(model.TN) - as.numeric(model.FP) * as.numeric(model.FN)) /
             sqrt((as.numeric(model.TP) + as.numeric(model.FP)) *
                  (as.numeric(model.TP) + as.numeric(model.FN)) *
                  (as.numeric(model.TN) + as.numeric(model.FP)) *
                  (as.numeric(model.TN) + as.numeric(model.FN)))
)]

startedruns <- allruns[idx == 1, .(model, searchspace, task, logperf, initsize, nrounds, kernel, epsilon, seed)]
allruns.full <- allruns.full[startedruns, on = c("model", "searchspace", "task", "logperf", "initsize", "nrounds", "kernel", "epsilon", "seed")]

measurement.cols <- setdiff(names(allruns.full), c("model", "searchspace", "task", "logperf", "seed", "initsize", "nrounds", "kernel", "epsilon", "idx"))
setnafill(allruns.full, fill = 1, cols = "last.step.cost")
setnafill(allruns.full, type = "locf" , cols = setdiff(measurement.cols, "time"))

# example unfinished run:
allruns.full[model == "lse" & searchspace == "all" & task == "bs" & logperf == "FALSE" & seed == 1]
# TODO improvements for next time:
# - make sure the last iteration also gets logged
# - go to round 500, not 499?

allruns.split <- split(allruns.full, by = c("model", "searchspace", "task", "logperf", "initsize", "nrounds", "kernel", "epsilon"))
rm(allruns)
rm(startedruns)
rm(allruns.full) ; gc() ; gc() ; gc() ; gc()

statvec <- function(x, na.rm = TRUE) {
  x <- x[is.finite(x)]                    # drop Inf, -Inf, NA
  c(min  = min(x, na.rm = na.rm),
    max  = max(x[is.finite(x)]),
    q10  = quantile(x, 0.10, na.rm = na.rm, names = FALSE),
    q25  = quantile(x, 0.25, na.rm = na.rm, names = FALSE),
    q50  = quantile(x, 0.50, na.rm = na.rm, names = FALSE),
    q75  = quantile(x, 0.75, na.rm = na.rm, names = FALSE),
    q90  = quantile(x, 0.90, na.rm = na.rm, names = FALSE),
    mean = mean(x[is.finite(x)], na.rm = na.rm),
    sd   = sd(x[is.finite(x)], na.rm = na.rm))
}


bycols <- c("model", "searchspace", "task", "logperf", "initsize", "nrounds", "kernel", "epsilon", "idx", "total.points.in.rs",
  "total.points.modeled", "last.step.cost", "true.optimum"
)

allruns.stats <- parallel::mclapply(allruns.split, function(dt) {
  dt[, {
    tmp <- lapply(.SD, function(col) {
      as.list(statvec(col))
    })
    as.list(unlist(tmp, recursive = FALSE))
  }, by = bycols,
  .SDcols = setdiff(measurement.cols, bycols)]
}, mc.cores = 30)

allruns.allinfo <- rbindlist(allruns.stats)

saveRDS(allruns.allinfo, "data/allruns.info.extras.rds")

# ------------------------------------------------------------------------------

allruns.allinfo <- readRDS("data/allruns.info.rds")

allruns.allinfo <- readRDS("data/allruns.info_newest.rds")

library("data.table")
allruns.allinfo <- readRDS("data/allruns.info.extras.rds")

aao <- allruns.allinfo
# these are boring and clutter the plots
# models <- grep("truvar.imp.beta|truvar.imp.delta|truvar.imp.eta|truvar.imp.r5", unique(aao$model), value = TRUE, invert = TRUE)
# models <- c("maxsd", "random", "straddle", grep("truvar", unique(aao$model), value = TRUE))  # data for other models is broken
# allruns.allinfo <- aao[model %in% models]

offsetting <- grep("\\.optimum\\.(?!sd)", colnames(allruns.allinfo), value = TRUE, perl = TRUE)

allruns.allinfo[, (offsetting) := lapply(.SD, function(x) x - true.optimum),
  .SDcols = offsetting]

allruns.allinfo[, model.ext := sprintf("%s_initsize:%s_nrounds:%s_kernel:%s_epsilon:%s", model, initsize, nrounds, kernel, epsilon)]

library(ggrepel)

bycols <- c("model.ext", "searchspace", "task", "logperf", "initsize", "nrounds", "kernel", "epsilon", "idx", "total.points.in.rs",
  "total.points.modeled", "last.step.cost", "true.optimum"
)


plotcols <- setdiff(colnames(allruns.allinfo), c(bycols, "evaluated"))
plotcols <- unique(sub("\\.mean$", "", grep("\\.mean$", plotcols, value = TRUE)))

# Create a single PDF file for all plots
for (sspace in unique(allruns.allinfo$searchspace)) {
  pdf(sprintf("data/plots/all_plots_%s.pdf", sspace), width = 20, height = 20)

  for (pc in plotcols) {
    # Create the plot with end labels
    plt <- ggplot(allruns.allinfo[searchspace == sspace],
      aes_string(x = "idx", color = "model.ext", fill = "model.ext",
          y = paste0(pc, ".mean"), ymin = paste0(pc, ".q25"), ymax = paste0(pc, ".q75"))) +
      geom_line() +
    # geom_ribbon(alpha = 0.2) +
      facet_grid(cols = vars(logperf), rows = vars(task), scales = "free") +
      theme_bw() +
      # Get the last point of each line to place labels
      geom_text_repel(
        data = allruns.allinfo[searchspace == sspace][, .SD[.N], by = .(model.ext, task, logperf)],
        aes(label = model.ext),
        nudge_x = 10,
        hjust = 0,
        segment.size = 0.2,
        direction = "y",
        point.padding = 0.2,
        box.padding = 0.5
      ) +
      theme(legend.position = "bottom") +
      ggtitle(pc)
    print(plt)
  }

  dev.off()
}






# Create a single PDF file for all plots
for (sspace in unique(allruns.allinfo$searchspace)) {
  pdf(sprintf("data/plots/all_boxplots_%s.pdf", sspace), width = 20, height = 20)
  for (pc in plotcols) {
    plt <- ggplot(allruns.allinfo[searchspace == sspace & idx == max(idx)],
      aes_string(x = "model.ext",
          #ymin = paste0(pc, ".min"),ymax = paste0(pc, ".max"),
          ymin = paste0(pc, ".q25"), ymax = paste0(pc, ".q75"),
          lower = paste0(pc, ".q25"), middle = paste0(pc, ".q50"),
          upper = paste0(pc, ".q75"),
          )) +
      geom_boxplot(stat = "identity", width = 0.5) +
      facet_grid(cols = vars(logperf), rows = vars(task), scales = "free") +
      theme_bw() +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      ggtitle(pc)
    print(plt)
  }

  dev.off()
}

######### aggregate plots

target.pc <- "F1"

for (target.pc in c("MCC", "F1")) {
    pdf(sprintf("data/plots/aggregate_%s.pdf", target.pc), width = 20, height = 20)

# Columns in allruns.allinfo corresponding to the target plotcol's mean and quantiles
y.col.name <- paste0(target.pc, ".mean")
ymin.col.name <- paste0(target.pc, ".q25")
ymax.col.name <- paste0(target.pc, ".q75")

# Aggregate data: Calculate average MCC metrics over all 'searchspace'
# Group by idx, model, task, logperf
plot.data.avg.mcc.over.sspace <- allruns.allinfo[logperf == FALSE, .(
  avg.y = mean(get(y.col.name), na.rm = TRUE),
  avg.ymin = mean(get(ymin.col.name), na.rm = TRUE), # Averaging pre-calculated quantiles
  avg.ymax = mean(get(ymax.col.name), na.rm = TRUE)  # Averaging pre-calculated quantiles
), by = .(idx, model.ext, task, logperf)]

# Create the plot
avg.mcc.plot <- ggplot(plot.data.avg.mcc.over.sspace,
  aes(x = idx, color = model.ext, fill = model.ext,
      y = avg.y, ymin = avg.ymin, ymax = avg.ymax)) +
  geom_line() +
  # geom_ribbon(alpha = 0.2) + # Uncomment if you want to show the averaged quantile range as a ribbon
  facet_grid(rows = vars(task), scales = "free") +
  theme_bw() +
  # Add labels for the last point of each line
  geom_text_repel(
    data = plot.data.avg.mcc.over.sspace[, .SD[.N], by = .(model.ext, task, logperf)],
    aes(label = model.ext),
    nudge_x = 10,
    hjust = 0,
    segment.size = 0.2,
    direction = "y",
    point.padding = 0.2,
    box.padding = 0.5,
    max.overlaps = Inf # Allow more labels to overlap if necessary
  ) +
  theme(legend.position = "bottom") +
  labs(
    title = paste("Average", target.pc, "across all searchspaces"),
    y = paste0("Average ", target.pc, ".mean (with avg .q25-.q75 band)"),
    x = "idx (Iteration)"
  )

# Print the plot to the current device
print(avg.mcc.plot)
dev.off()
}


for (target.pc in c("MCC", "F1")) {
    pdf(sprintf("data/plots/aggregate_all_%s.pdf", target.pc), width = 20, height = 20)

# Columns in allruns.allinfo corresponding to the target plotcol's mean and quantiles
y.col.name <- paste0(target.pc, ".mean")
ymin.col.name <- paste0(target.pc, ".q25")
ymax.col.name <- paste0(target.pc, ".q75")

# Aggregate data: Calculate average MCC metrics over all 'searchspace'
# Group by idx, model, task, logperf
plot.data.avg.mcc.over.sspace <- allruns.allinfo[logperf == FALSE, .(
  avg.y = mean(get(y.col.name), na.rm = TRUE),
  avg.ymin = mean(get(ymin.col.name), na.rm = TRUE), # Averaging pre-calculated quantiles
  avg.ymax = mean(get(ymax.col.name), na.rm = TRUE)  # Averaging pre-calculated quantiles
), by = .(idx, model.ext, logperf)]

# Create the plot
avg.mcc.plot <- ggplot(plot.data.avg.mcc.over.sspace,
  aes(x = idx, color = model.ext, fill = model.ext,
      y = avg.y, ymin = avg.ymin, ymax = avg.ymax)) +
  geom_line() +
  # geom_ribbon(alpha = 0.2) + # Uncomment if you want to show the averaged quantile range as a ribbon
  theme_bw() +
  # Add labels for the last point of each line
  geom_text_repel(
    data = plot.data.avg.mcc.over.sspace[, .SD[.N], by = .(model.ext, logperf)],
    aes(label = model.ext),
    nudge_x = 10,
    hjust = 0,
    segment.size = 0.2,
    direction = "y",
    point.padding = 0.2,
    box.padding = 0.5,
    max.overlaps = Inf # Allow more labels to overlap if necessary
  ) +
  theme(legend.position = "bottom") +
  labs(
    title = paste("Average", target.pc, "across all searchspaces"),
    y = paste0("Average ", target.pc, ".mean (with avg .q25-.q75 band)"),
    x = "idx (Iteration)"
  )

# Print the plot to the current device
print(avg.mcc.plot)
dev.off()
}



target.pc <- "F1"
y.col.name <- paste0(target.pc, ".mean")
ymin.col.name <- paste0(target.pc, ".q25")
ymax.col.name <- paste0(target.pc, ".q75")

# Aggregate data: Calculate average MCC metrics over all 'searchspace'
# Group by idx, model, task, logperf
plot.data.avg.mcc.over.sspace <- allruns.allinfo[, .(
  avg.y = mean(get(y.col.name), na.rm = TRUE),
  avg.ymin = mean(get(ymin.col.name), na.rm = TRUE), # Averaging pre-calculated quantiles
  avg.ymax = mean(get(ymax.col.name), na.rm = TRUE)  # Averaging pre-calculated quantiles
), by = .(idx, model, logperf, initsize, nrounds, model.ext, isallss = searchspace == "all")]


x11()

pdf(sprintf("data/plots/aggregate_all_%s_byss.pdf", target.pc), width = 20, height = 20)
for (do.isallss in c(TRUE, FALSE)) {
for (do.logperf in c(TRUE, FALSE)) {
plt <- ggplot(plot.data.avg.mcc.over.sspace[isallss == do.isallss & logperf == do.logperf & nrounds == 600 & initsize == 10],
  aes(x = idx, color = model, fill = model, group = model.ext,
      y = avg.y, ymin = avg.ymin, ymax = avg.ymax)) +
  geom_line() +
  # geom_ribbon(alpha = 0.2) + # Uncomment if you want to show the averaged quantile range as a ribbon
  theme_bw() +
  # Add labels for the last point of each line
  geom_text_repel(
    data = plot.data.avg.mcc.over.sspace[isallss == do.isallss & logperf == do.logperf & nrounds == 600 & initsize == 10][, .SD[.N], by = .(model, logperf)],
    aes(label = model),
    nudge_x = 10,
    hjust = 0,
    segment.size = 0.2,
    direction = "y",
    point.padding = 0.2,
    box.padding = 0.5,
    max.overlaps = Inf # Allow more labels to overlap if necessary
  ) +
  theme(legend.position = "bottom") +
  labs(
    title = paste("Average", target.pc, "across all searchspaces", if (do.isallss) " (combined)" else " (separate)", if (do.logperf) " (logperf)" else " (linear)"),
    y = paste0("Average ", target.pc, ".mean (with avg .q25-.q75 band)"),
    x = "idx (Iteration)"
    )

  print(plt)
  }
}
dev.off()


target.pc <- "observed.optimum"
y.col.name <- paste0(target.pc, ".mean")
ymin.col.name <- paste0(target.pc, ".q25")
ymax.col.name <- paste0(target.pc, ".q75")
# Aggregate data: Calculate average MCC metrics over all 'searchspace'
# Group by idx, model, task, logperf
plot.data.avg.mcc.over.sspace <- allruns.allinfo[, .(
  avg.y = mean(get(y.col.name), na.rm = TRUE),
  avg.ymin = mean(get(ymin.col.name), na.rm = TRUE), # Averaging pre-calculated quantiles
  avg.ymax = mean(get(ymax.col.name), na.rm = TRUE)  # Averaging pre-calculated quantiles
), by = .(idx, model, logperf, initsize, nrounds, model.ext, isallss = searchspace == "all")]
pdf(sprintf("data/plots/aggregate_regret_%s_byss.pdf", target.pc), width = 20, height = 20)
for (do.isallss in c(TRUE, FALSE)) {
for (do.logperf in c(TRUE, FALSE)) {
plt <- ggplot(plot.data.avg.mcc.over.sspace[isallss == do.isallss & logperf == do.logperf & nrounds == 600 & initsize == 10],
  aes(x = idx, color = model, fill = model, group = model.ext,
      y = avg.y, ymin = avg.ymin, ymax = avg.ymax)) +
  geom_line() +
  # geom_ribbon(alpha = 0.2) + # Uncomment if you want to show the averaged quantile range as a ribbon
  theme_bw() +
  # Add labels for the last point of each line
  geom_text_repel(
    data = plot.data.avg.mcc.over.sspace[isallss == do.isallss & logperf == do.logperf & nrounds == 600 & initsize == 10][, .SD[.N], by = .(model, logperf)],
    aes(label = model),
    nudge_x = 10,
    hjust = 0,
    segment.size = 0.2,
    direction = "y",
    point.padding = 0.2,
    box.padding = 0.5,
    max.overlaps = Inf # Allow more labels to overlap if necessary
  ) +
  theme(legend.position = "bottom") +
  labs(
    title = paste("Average", target.pc, "across all searchspaces", if (do.isallss) " (combined)" else " (separate)", if (do.logperf) " (logperf)" else " (linear)"),
    y = paste0("Average ", target.pc, ".mean (with avg .q25-.q75 band)"),
    x = "idx (Iteration)"
    )

  print(plt)
  }
}
dev.off()


#--------------------------------------------------


### facet logperf (left, right), searchspace (all, rest), pages for epsilon (.05, .2, .8)
## set initsize to 10, nrounds to 401
## need:
#  - model, kernel

only.truvars <- grep("truvar", unique(allruns.allinfo$model), value = TRUE, fixed = TRUE)
non.truvars <- c(setdiff(unique(allruns.allinfo$model), only.truvars), "truvar", "truvar.imp")

target.pc <- "observed.optimum"
target.pc <- "F1"
y.col.name <- paste0(target.pc, ".mean")
# Aggregate data: Calculate average MCC metrics over all 'searchspace'
# Group by idx, model, task, logperf
plot.over.sspace.all <- allruns.allinfo[initsize == 10 & nrounds == 401, .(
  avg.y = mean(get(y.col.name), na.rm = TRUE)
), by = .(idx, model, logperf, model.ext, isallss = searchspace == "all", kernel, epsilon)]

pdf(sprintf("data/plots/aggregate_%s_kernel_epsilons.pdf", target.pc), width = 20, height = 20)
for (epsilon.plot in c(0.05, 0.2, 0.8)) {
  for (subset in list(only.truvars, non.truvars)) {
    plot.over.sspace <- plot.over.sspace.all[epsilon == epsilon.plot & model %in% subset]
    plt <- ggplot(plot.over.sspace,
      aes(x = idx, color = sprintf("%s.%s", model, kernel), group = sprintf("%s.%s", model, kernel), y = avg.y)) +
      geom_line() +
      theme_bw() +
      facet_grid(rows = vars(isallss), cols = vars(logperf),
        labeller = labeller(logperf = c("FALSE" = "linear", "TRUE" = "logperf"),
        isallss = c("FALSE" = "separate", "TRUE" = "combined (CASH)"))
      ) +
      geom_text_repel(
        data = plot.over.sspace[, .SD[.N], by = .(model, kernel, isallss, logperf)],
        aes(label = sprintf("%s.%s", model, kernel)),
        size = 1,
        nudge_x = 1,
        hjust = 0,
        segment.size = 0.1,
        direction = "y",
        point.padding = 0.05,
        box.padding = 0.05,
        max.overlaps = Inf # Allow more labels to overlap if necessary
      ) +
      theme(legend.position = "bottom") +
      labs(
        title = paste("Average", target.pc, " with epsilon", epsilon.plot),
        y = paste0("Average ", target.pc, ".mean"),
        x = "idx (Iteration)"
        )

      print(plt)
  }
}
dev.off()

# ---------------------


target.pc <- "observed.optimum"
target.pc <- "F1"
y.col.name <- paste0(target.pc, ".mean")
ymin.col.name <- paste0(target.pc, ".q25")
ymax.col.name <- paste0(target.pc, ".q75")

# Aggregate data: Calculate average MCC metrics over all 'searchspace'
# Group by idx, model, task, logperf
plot.over.sspace.all <- allruns.allinfo[initsize == 10 & nrounds == 401, .(
  avg.y = mean(get(y.col.name), na.rm = TRUE),
  avg.ymin = mean(get(ymin.col.name), na.rm = TRUE), # Averaging pre-calculated quantiles
  avg.ymax = mean(get(ymax.col.name), na.rm = TRUE)  # Averaging pre-calculated quantiles
), by = .(idx, model, logperf, model.ext, isallss = searchspace == "all", kernel, epsilon)]

pdf(sprintf("data/plots/boxplot_%s_kernel_epsilons.pdf", target.pc), width = 20, height = 20)
for (epsilon.plot in c(0.05, 0.2, 0.8)) {
  for (subset in list(only.truvars, non.truvars)) {
    for (do.isallss in c(TRUE, FALSE)) {
      plot.over.sspace <- plot.over.sspace.all[epsilon == epsilon.plot & model %in% subset & isallss == do.isallss & idx %in% c(50, 100, 200, 400)]
      plt <- ggplot(plot.over.sspace,
        aes(x = model, fill = kernel, ymin = avg.ymin, lower = avg.ymin, upper = avg.ymax, middle = avg.y, ymax = avg.ymax)) +
        geom_boxplot(stat = "identity") +
        theme_bw() +
        facet_grid(rows = vars(idx), cols = vars(logperf),
          labeller = labeller(logperf = c("FALSE" = "linear", "TRUE" = "logperf"))
        ) +
        theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(
          title = paste("Average", target.pc, " with epsilon", epsilon.plot, if (do.isallss) " (combined)" else " (separate)"),
          y = paste0("Average ", target.pc, ".mean"),
          x = "idx (Iteration)"
          )

        print(plt)
    }
  }
}
dev.off()

## --------------------------------

subset <- c("truvar.imp", "truvar", "straddle", "truvar.imp.global", "lse", "lse.imp", "optimize", "random")
epsilon.plot <- 0.05
target.pc <- "observed.optimum"
target.pc <- "F1"
y.col.name <- paste0(target.pc, ".mean")
# Aggregate data: Calculate average MCC metrics over all 'searchspace'
# Group by idx, model, task, logperf
plot.over.sspace.all <- allruns.allinfo[initsize == 10 & nrounds == 401, .(
  avg.y = mean(get(y.col.name), na.rm = TRUE)
), by = .(idx, model, logperf, model.ext, isallss = searchspace == "all", kernel, epsilon)]
pdf(sprintf("data/plots/present_%s_kernel.pdf", target.pc), width = 20, height = 20)
plot.over.sspace <- plot.over.sspace.all[epsilon == epsilon.plot & model %in% subset & logperf == TRUE]
plt <- ggplot(plot.over.sspace,
  aes(x = idx, color = model, group = sprintf("%s.%s", model, kernel), y = avg.y)) +
  geom_line() +
  theme_bw() +
  facet_grid(rows = vars(isallss),
    labeller = labeller(isallss = c("FALSE" = "separate", "TRUE" = "combined (CASH)"))
  ) +
  geom_text_repel(
    data = plot.over.sspace[, .SD[.N], by = .(model, kernel, isallss, logperf)],
    aes(label = sprintf("%s.%s", model, kernel)),
    size = 1,
    nudge_x = 2,
    hjust = 0,
    segment.size = 0.1,
    direction = "y",
    point.padding = 0.05,
    box.padding = 0.05,
    max.overlaps = Inf # Allow more labels to overlap if necessary
  ) +
  theme(legend.position = "bottom") +
  labs(
    title = paste("Average", target.pc, " with epsilon", epsilon.plot),
    y = paste0("Average ", target.pc, ".mean"),
    x = "idx (Iteration)"
    )

print(plt)
dev.off()





allruns.allinfo[searchspace == sspace & task == "bc" & logperf == FALSE & model == "truvar.opt"][, c(bycols, "evaluated.points.in.rs.mean"), with = FALSE][order(idx)]



ggplot(allruns.allinfo[searchspace == sspace & task == "bc" & logperf == FALSE & model == "truvar.opt"],
      aes_string(x = "idx", color = "model", fill = "model",
          y = paste0(pc, ".mean"), ymin = paste0(pc, ".q25"), ymax = paste0(pc, ".q75"))) +
      geom_point() +
    # geom_ribbon(alpha = 0.2) +
      theme_bw()

## from which point in time is the ranking stable?

ttbl <- dcast(
  allruns.allinfo[total.points.in.rs >= 100, .(model, task, logperf, searchspace, idx, evaluated.points.in.assumed.rs.q50, evaluated.points.in.assumed.rs.q75)],
  idx + task + logperf + searchspace ~ model,
  value.var = c("evaluated.points.in.assumed.rs.q50", "evaluated.points.in.assumed.rs.q75")
)

ranktbl <- ttbl[, {
  prefix <- unique(sub("\\.q[0-9]+_.*", "", colnames(.SD)))
  stopifnot(length(prefix) == 1)
  suffixes <- unique(sub(".*_", "", colnames(.SD)))

  q50cols <- unlist(.SD[, paste0(prefix, ".q50_", suffixes), with = FALSE])
  q75cols <- unlist(.SD[, paste0(prefix, ".q75_", suffixes), with = FALSE])

  ranks <- numeric(length(suffixes))
  lastq75 <- -Inf
  lastrank <- 0
  for (idx in order(q50cols)) {
    if (q50cols[[idx]] > lastq75) {
      lastrank <- lastrank + 1
      lastq75 <- q75cols[[idx]]
    }
    ranks[[idx]] <- lastrank
  }
  names(ranks) <- suffixes
  as.list(ranks)
}, by = c("idx", "task", "logperf", "searchspace")]


restbl <- ranktbl[, .(
    idx,
    lastcor = {
      lastrow <- unlist(.SD[nrow(.SD)])
      apply(.SD, 1, function(x) {
        if (length(unique(x)) == 1 || length(unique(lastrow)) == 1) {
          return(0)
        }
        cor(x, lastrow, method = "kendall")
      })
    }
  ), .SDcols = setdiff(colnames(ranktbl), c("idx", "task", "logperf", "searchspace")),
  by = c("task", "logperf", "searchspace")
]

restbl[, .(m = mean(lastcor[is.finite(lastcor)])), by = "idx"][, plot(idx, m)]

.sd <- ttbl[task == "bc" & logperf == FALSE & searchspace == "xgb"]
.sdd <- .sd[, -c("idx", "task", "logperf", "searchspace")]
sapply(1:10, function(i) apply(.sdd, 1, function(x) {
      cor(x, unlist(.sdd[nrow(.sdd) - i + 1]), method = "kendall")
    }))






