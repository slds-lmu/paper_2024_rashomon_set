rm(list = ls(all.names = TRUE))
source("init.R")

reg.lse <- getRegistry("../registry_paper_2024_rashomon_set_lseruns/registry7", make.default = TRUE)



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


testJob(364)

findNotDone() -> fj
fj <- fj[sample.int(nrow(fj))]
submitJobs(fj, resources = list(walltime = 360000, memory = 4000, ncpus = 1), reg = reg.lse)


submitJobs(tail(fj, -1000), resources = list(walltime = 360000, memory = 4000, ncpus = 1), reg = reg.lse)


testJob(1981)



allfiles <- parallel::mclapply(list.files("data/lseruns", pattern = '\\.csv$', full.names = TRUE), function(x) {
  fread(x, integer64 = "numeric")[, filename := x]
}, mc.cores = 90)

allruns <- rbindlist(allfiles)


allruns <- cbind(allruns, t(gsub(".*:", "", sapply(strsplit(allruns$filename, "_"), identity))[2:6, ]))

setnames(allruns, paste0("V", 1:5), c("model", "searchspace", "task", "logperf", "seed"))

allruns[, seed := as.numeric(sub("\\..*", "", seed))]
allruns[, filename := NULL]
allruns[, modeled.optimum := as.numeric(modeled.optimum)]
allruns[, max.relative.model.error := as.numeric(max.relative.model.error)]

allruns[, idx := seq_len(.N), by = .(model, searchspace, task, logperf, seed)]

saveRDS(allruns, "data/allseruns.rds")

allruns <- readRDS("data/allseruns.rds")


skel <- CJ(
  model = unique(allruns$model),
  searchspace = unique(allruns$searchspace),
  task = unique(allruns$task),
  logperf = unique(allruns$logperf),
  seed = unique(allruns$seed),
  idx = seq_len(max(allruns$idx)),
  sorted = TRUE
)

allruns.full <- allruns[skel, on = .(model, searchspace, task, logperf, seed, idx)]
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

startedruns <- allruns[idx == 1, .(model, searchspace, task, logperf, seed)]
allruns.full <- allruns.full[startedruns, on = c("model", "searchspace", "task", "logperf", "seed")]

measurement.cols <- setdiff(names(allruns.full), c("model", "searchspace", "task", "logperf", "seed", "idx"))
setnafill(allruns.full, fill = 1, cols = "last.step.cost")
setnafill(allruns.full, type = "locf" , cols = setdiff(measurement.cols, "time"))

# example unfinished run:
allruns.full[model == "lse" & searchspace == "all" & task == "bs" & logperf == "FALSE" & seed == 1]
# TODO improvements for next time:
# - make sure the last iteration also gets logged
# - go to round 500, not 499?

allruns.split <- split(allruns.full, by = c("model", "searchspace", "task", "logperf"))

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


bycols <- c("model", "searchspace", "task", "logperf", "idx", "total.points.in.rs",
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
}, mc.cores = 20)

allruns.allinfo <- rbindlist(allruns.stats)

saveRDS(allruns.allinfo, "data/allruns.info.rds")

# ------------------------------------------------------------------------------

allruns.allinfo <- readRDS("data/allruns.info_3.rds")

aao <- allruns.allinfo
models <- c("maxsd", "random", "straddle", grep("truvar", unique(aao$model), value = TRUE))  # data for other models is broken
allruns.allinfo <- aao[model %in% models]

offsetting <- grep("\\.optimum\\.(?!sd)", colnames(allruns.allinfo), value = TRUE, perl = TRUE)

allruns.allinfo[, (offsetting) := lapply(.SD, function(x) x - true.optimum),
  .SDcols = offsetting]


library(ggrepel)

bycols <- c("model", "searchspace", "task", "logperf", "idx", "total.points.in.rs",
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
      aes_string(x = "idx", color = "model", fill = "model",
          y = paste0(pc, ".mean"), ymin = paste0(pc, ".q25"), ymax = paste0(pc, ".q75"))) +
      geom_line() +
    # geom_ribbon(alpha = 0.2) +
      facet_grid(cols = vars(logperf), rows = vars(task), scales = "free") +
      theme_bw() +
      # Get the last point of each line to place labels
      geom_text_repel(
        data = allruns.allinfo[searchspace == sspace][, .SD[.N], by = .(model, task, logperf)],
        aes(label = model),
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
      aes_string(x = "model",
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

target.pc <- "MCC"

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
), by = .(idx, model, task, logperf)]

# Create the plot
avg.mcc.plot <- ggplot(plot.data.avg.mcc.over.sspace,
  aes(x = idx, color = model, fill = model,
      y = avg.y, ymin = avg.ymin, ymax = avg.ymax)) +
  geom_line() +
  # geom_ribbon(alpha = 0.2) + # Uncomment if you want to show the averaged quantile range as a ribbon
  facet_grid(rows = vars(task), scales = "free") +
  theme_bw() +
  # Add labels for the last point of each line
  geom_text_repel(
    data = plot.data.avg.mcc.over.sspace[, .SD[.N], by = .(model, task, logperf)],
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
), by = .(idx, model,logperf)]

# Create the plot
avg.mcc.plot <- ggplot(plot.data.avg.mcc.over.sspace,
  aes(x = idx, color = model, fill = model,
      y = avg.y, ymin = avg.ymin, ymax = avg.ymax)) +
  geom_line() +
  # geom_ribbon(alpha = 0.2) + # Uncomment if you want to show the averaged quantile range as a ribbon
  theme_bw() +
  # Add labels for the last point of each line
  geom_text_repel(
    data = plot.data.avg.mcc.over.sspace[, .SD[.N], by = .(model, logperf)],
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
    title = paste("Average", target.pc, "across all searchspaces"),
    y = paste0("Average ", target.pc, ".mean (with avg .q25-.q75 band)"),
    x = "idx (Iteration)"
  )

# Print the plot to the current device
print(avg.mcc.plot)
dev.off()
}









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






