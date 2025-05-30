allds.pivot <- readRDS("data/allds.pivot.rds")

list.learners.meta <- lapply(list.learners.regr, function(x) x$clone(deep = TRUE))
# list.learners.meta$svm$param_set$search_space()$levels$svm.kernel
list.learners.meta$svm <- NULL
list.learners.meta$svmlinear <- list.learners.regr$svm$clone(deep = TRUE)
list.learners.meta$svmlinear$param_set$set_values(svm.kernel = "linear", svm.degree = NULL, svm.gamma = NULL)
list.learners.meta$svmradial <- list.learners.regr$svm$clone(deep = TRUE)
list.learners.meta$svmradial$param_set$set_values(svm.kernel = "radial", svm.degree = NULL)
#list.learners.meta$svmpolynomial <- list.learners.regr$svm$clone(deep = TRUE)
#list.learners.meta$svmpolynomial$param_set$set_values(svm.kernel = "polynomial")


allds <- sapply(c("st", "bs", "cs", "gc"), function(datasetid) {

  considered.dataset <- copy(allds.pivot[[datasetid]])
  scoreby <- c(gc = "bbrier", cs = "bbrier", bs = "rmse", st = "rmse")[[datasetid]]

  lapply(names(list.learners.meta), function(x) {
    ss <- list.learners.meta[[x]]$param_set$search_space()$ids()
    if (x %like% "^svm") {
      ds <- copy(considered.dataset[["svm"]])
      ds <- ds[svm.kernel == sub("^svm", "", x), ]
      ds <- ds[, c(ss, scoreby), with = FALSE]
    } else  {
      ds <- copy(considered.dataset[[x]][, c(ss, scoreby), with = FALSE])
    }
    colnames(ds) <- paste0(x, ".", colnames(ds))

    ds[, learner := x]
    ds[, score := get(paste0(x, ".", scoreby))]
    ds[, -paste0(x, ".", scoreby), with = FALSE]
  }) |> rbindlist(use.names = TRUE, fill = TRUE) -> considered.dataset.merged

  considered.dataset.merged
}, simplify = FALSE)


proportions <- sapply(allds, function(x) x[, mean(score < 1.05 * min(score))])
rsize <- sapply(allds, function(x) x[, sum(score < 1.05 * min(score))])
allsize <- sapply(allds, nrow)
rcutoff <- sapply(allds, function(x) x[, (1.05 * min(score))])
absmin <- sapply(allds, function(x) x[, min(score)])

info <- data.table(props = names(proportions), proportions, rsize, allsize, absmin)


alpha <- (1 - pnorm(1)) * 2


qbinom(proportions[[1]], c(1:100), alpha / 2)

qbinom(proportions[[1]], c(1:100), alpha / 2, lower.tail = FALSE)

info


curds <- "st"


lapply(c("cs", "gc", "st", "bs"), function(curds) {
  data.table(
    dataset = curds,
    opter = "random",
    step = 1:600,
    meanfr = rsize[[curds]] / allsize[[curds]] * 1:600,
    medianfr = qhyper(0.5, rsize[[curds]], allsize[[curds]] - rsize[[curds]], 1:600),
    lqfr = qhyper(0.25, rsize[[curds]], allsize[[curds]] - rsize[[curds]], 1:600),
    uqfr = qhyper(0.25, rsize[[curds]], allsize[[curds]] - rsize[[curds]], 1:600, lower.tail = FALSE),
    meanfr.prop = rsize[[curds]] / allsize[[curds]] * 1:600 / rsize[[curds]],
    medianfr.prop = qhyper(0.5, rsize[[curds]], allsize[[curds]] - rsize[[curds]], 1:600) / rsize[[curds]],
    lqfr.prop = qhyper(0.25, rsize[[curds]], allsize[[curds]] - rsize[[curds]], 1:600) / rsize[[curds]],
    uqfr.prop = qhyper(0.25, rsize[[curds]], allsize[[curds]] - rsize[[curds]], 1:600, lower.tail = FALSE) /
      rsize[[curds]]
  )
}) |> rbindlist(use.names = TRUE, fill = TRUE) -> random.fr





getqs <- function(x, n) {
  x <- sort(x)           # Ensure x is sorted
  N <- length(x)

  F_min <- function(k) {
    1 - choose(N - k, n) / choose(N, n)
  }

  # Find 25% quantile index
  k25 <- which(F_min(seq_len(N)) >= 0.25)[1]
  Q25 <- x[k25]

  # Find 75% quantile index
  k75 <- which(F_min(seq_len(N)) >= 0.75)[1]
  Q75 <- x[k75]
  c(Q25 = Q25, Q75 = Q75)
}

lapply(c("cs", "gc", "st", "bs"), function(curds) {
  samples <- replicate(1000, sample(allds[[curds]]$score, 600, replace = FALSE) |> cummin())
  data.table(
    dataset = curds,
    opter = "random",
    step = 1:600,
    meanfm = rowMeans(samples),
    medianfm = apply(samples, 1, median),
    lqfm = apply(samples, 1, quantile, 0.25),
    uqfm = apply(samples, 1, quantile, 0.75),
    meanfm.prop = rowMeans(samples) - absmin[[curds]],
    medianfm.prop = apply(samples, 1, median) - absmin[[curds]],
    lqfm.prop = apply(samples, 1, quantile, 0.25) - absmin[[curds]],
    uqfm.prop = apply(samples, 1, quantile, 0.75, lower.tail = FALSE) - absmin[[curds]]
  )
}) |> rbindlist(use.names = TRUE, fill = TRUE) -> random.fm








lapply(files, function(file) {
  lines <- readLines(file) ; list(info= strsplit(grep("datasetid", lines, value = TRUE), "[:,]")[[1]],
  table = suppressWarnings(data.table::as.data.table(tail(t(sapply(strsplit(lines, ","),
  function(x) c(x[22], x[20])) ), -10))[, V1 := as.numeric(V1)][])) }) -> result

res <- readRDS("data/result.rds")
res2 <- readRDS("data/result2.rds")

avgruntimes <- c()

f1 <- function(values.found, true.cutoff) {
  found.min <- cummin(values.found)
  assumed.cutoff <- (1.05 * found.min)
  vapply(seq_along(values.found), function(i) {
    ac <- assumed.cutoff[[i]]
    values.so.far <- values.found[1:i]
    TP <- sum(values.so.far < ac & values.so.far < true.cutoff)
    FP <- sum(values.so.far < ac & values.so.far >= true.cutoff)
    FN <- sum(values.so.far >= ac & values.so.far < true.cutoff)
    2 * TP / (2 * TP + FP + FN)
  }, numeric(1))
}

maketbl <- function(resel) {
  table <- resel$table
  table <- table[!is.na(V1) & !is.na(V2)]
  setnames(table, c("V1", "V2"), c("score", "learner"))
  table[, score := exp(score)]
  datasetid <- trimws(resel$info[2])
  table[, dataset := datasetid]
  table[, seed := as.numeric(resel$info[4])]
  table[, opter := trimws(resel$info[6])]
  table[, is.rashomon := score < rcutoff[[datasetid]]]
  table[, found.rashomon := cumsum(is.rashomon)]
  table[, found.rashomon.prop := found.rashomon / rsize[[datasetid]]]
  table[, found.min := cummin(score)]
  table[, found.min.prop := found.min - absmin[[datasetid]]]
  table[, f1 := f1(score, rcutoff[[datasetid]])]
  table[, real := TRUE]
  if (nrow(table) < 600) {
    table <- rbind(table, copy(table[rep(nrow(table), 600 - nrow(table))])[, real := FALSE])
  }
  table[, step := seq_len(nrow(table))]
  table
}

allinfo. <- rbindlist(lapply(res, maketbl))[]
allinfo2 <- rbindlist(lapply(res2, maketbl))[]

simres <- lapply(names(allds), function(adx) lapply(1:1000, function(genseed) {
  dataset <- allds[[adx]]
  list(
    info = c("datasetid", adx, " genseed", genseed, " optimizer", "random_search"),
    table = dataset[sample.int(nrow(dataset), 600), .(V1 = log(score), V2 = learner)]
  )
})) |> unlist(recursive = FALSE, use.names = FALSE)

allinfo3 <- rbindlist(lapply(simres, maketbl))[]


allinfo <- rbind(allinfo., allinfo2, allinfo3)

infotable <- allinfo[, .(
  meanfr = mean(found.rashomon),
  sdfr = sd(found.rashomon),
  medianfr = quantile(found.rashomon, 0.5),
  lqfr = quantile(found.rashomon, 0.25),
  uqfr = quantile(found.rashomon, 0.75),
  meanfr.prop = mean(found.rashomon.prop),
  sdfr.prop = sd(found.rashomon.prop),
  medianfr.prop = quantile(found.rashomon.prop, 0.5),
  lqfr.prop = quantile(found.rashomon.prop, 0.25),
  uqfr.prop = quantile(found.rashomon.prop, 0.75),
  f1 = mean(f1),
  medianf1 = quantile(f1, 0.5),
  lqf1 = quantile(f1, 0.25),
  uqf1 = quantile(f1, 0.75),
  finished = mean(real)
), by = .(dataset, opter, step)]



mininfotable <- allinfo[, .(
  meanfm = mean(found.min, na.rm = TRUE),
  medianfm = quantile(found.min, 0.5, na.rm = TRUE),
  lqfm = quantile(found.min, 0.25, na.rm = TRUE),
  uqfm = quantile(found.min, 0.75, na.rm = TRUE),
  meanfm.prop = mean(found.min.prop, na.rm = TRUE),
  medianfm.prop = quantile(found.min.prop, 0.5, na.rm = TRUE),
  lqfm.prop = quantile(found.min.prop, 0.25, na.rm = TRUE),
  uqfm.prop = quantile(found.min.prop, 0.75, na.rm = TRUE),
  finished = mean(real, na.rm = TRUE)
), by = .(dataset, opter, step)]




library(ggplot2)
ggplot(
  rbind(random.fr, infotable, use.names = TRUE, fill = TRUE)[step <= 600],
  aes(x = step, y = meanfr, ymin = lqfr, ymax = uqfr, color = opter)
) +
  geom_line() +
  facet_wrap(~dataset)

ggplot(
  infotable[step <= 600],
  aes(x = step, y = finished, color = opter)
) +
  geom_line() +
  facet_wrap(~dataset)



####### figure  main
pdf("figures/main.pdf", width = 10, height = 5)
ggplot(
  rbind(random.fr[dataset %in% c("bs", "st")], infotable[dataset %in% c("bs", "st")],
    use.names = TRUE, fill = TRUE)[step <= 600],
  aes(x = step, y = medianfr.prop, ymin = lqfr.prop, ymax = uqfr.prop, color = opter, fill = opter)
) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  facet_wrap(~dataset) +
  theme_minimal()


ggplot(
  rbind(mininfotable[dataset %in% c("bs", "st")], use.names = TRUE, fill = TRUE)[step <= 600],
  aes(x = step, y = medianfm.prop, ymin = lqfm.prop, ymax = uqfm.prop, color = opter, fill = opter)
) +
  geom_line() +
  scale_y_log10() +
  geom_ribbon(alpha = 0.2) +
  facet_wrap(~dataset) +
  theme_minimal()


dev.off()

####### figure appendix

pdf("figures/appendix.pdf", width = 10, height = 5)
ggplot(
  rbind(random.fr[!dataset %in% c("bs", "st")], infotable[!dataset %in% c("bs", "st")],
    use.names = TRUE, fill = TRUE)[step <= 600],
  aes(x = step, y = meanfr.prop, ymin = lqfr.prop, ymax = uqfr.prop, color = opter, fill = opter)
) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  facet_wrap(~dataset) +
  theme_minimal()
dev.off()



####### figure appendix

pdf("figures/appendix.pdf", width = 10, height = 5)
ggplot(
  rbind(random.fr[!dataset %in% c("bs", "st")], infotable[!dataset %in% c("bs", "st")],
    use.names = TRUE, fill = TRUE)[step <= 600],
  aes(x = step, y = medianfr.prop, ymin = lqfr.prop, ymax = uqfr.prop, color = opter, fill = opter)
) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  facet_wrap(~dataset) +
  theme_minimal()
dev.off()





infotable2 <- allinfo[, .(
  meanfr.prop = mean(found.rashomon.prop),
  medianfr.prop = quantile(found.rashomon.prop, 0.5),
  lqfr.prop = quantile(found.rashomon.prop, 0.25),
  uqfr.prop = quantile(found.rashomon.prop, 0.75),
  finished = mean(real)
), by = .(opter, step)]



ggplot(
  rbind(infotable2, use.names = TRUE, fill = TRUE)[step <= 600],
  aes(x = step, y = medianfr.prop, ymin = lqfr.prop, ymax = uqfr.prop, color = opter, fill = opter)
) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  theme_minimal()




library(patchwork)


pdf("figures/appendix_opt.pdf", width = 14, height = 6)
# Create first plot (linear scale)
p1 <- ggplot(random.fr[dataset %in% c("cs", "gc", "bs", "st")],
  aes(x = step, y = medianfr.prop, ymin = lqfr.prop, ymax = uqfr.prop,
      color = opter, fill = opter)
) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  facet_wrap(~dataset, labeller = as_labeller(c(cs = "COMPAS (CS)", gc = "German Credit (GC)",
    bs = "Bike Sharing (BS)", st = "Synthetic (ST)"))) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
    #panel.grid.major.x = element_blank(),
    #panel.grid.minor.x = element_blank()
  ) +
  ylab("Proportion of CASHomon samples found")

# Create second plot (log scale)
p2 <- ggplot(
  rbind(mininfotable[dataset %in% c("cs", "gc")],
        random.fm[dataset %in% c("cs", "gc")],
        use.names = TRUE, fill = TRUE)[step <= 600],
  aes(x = step, y = medianfm.prop, ymin = lqfm.prop, ymax = uqfm.prop,
      color = opter, fill = opter)
) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  scale_y_log10() +
  facet_wrap(~dataset, strip.position = "bottom") +
  theme_minimal() + theme(strip.background = element_blank(), strip.text.x = element_blank()) +
  ylab("Distance to minimum") + xlab("Number of samples")

opter_labels <- c(
  random = "RANDOM",
  straddle = "STRADDLE",
  tvi = "TruVarImp",
  maxsd = "VAR"
)








# Combine them with patchwork
# - Stack them vertically (p1 / p2)
# - Collect guides (legend) into one
# - Add a shared plot title/description at the top
combined_plot <- p1 / p2 +
  plot_layout(guides = "collect") +
  plot_annotation(

  ) &
  scale_color_discrete(name = "Optimizer", labels = opter_labels) &
  scale_fill_discrete(name = "Optimizer", labels = opter_labels)

# Display the combined plot
combined_plot
dev.off()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pdf("figures/truvar_main_narrow.pdf", width = 7, height = 6)
# Create first plot (linear scale)
p1 <- ggplot(
  rbind(random.fr[dataset %in% c("bs", "st")], infotable[dataset %in% c("bs", "st")],
        use.names = TRUE, fill = TRUE)[step <= 600],
  aes(x = step, y = medianfr.prop, ymin = lqfr.prop, ymax = uqfr.prop,
      color = opter, fill = opter)
) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  facet_wrap(~dataset, labeller = as_labeller(c(bs = "Bike Sharing (BS)", st = "Synthetic (ST)"))) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
    #panel.grid.major.x = element_blank(),
    #panel.grid.minor.x = element_blank()
  ) +
  ylab("Proportion of CASHomon samples found")

# Create second plot (log scale)
p2 <- ggplot(
  rbind(mininfotable[dataset %in% c("bs", "st")],
        random.fm[dataset %in% c("bs", "st")],
        use.names = TRUE, fill = TRUE)[step <= 600],
  aes(x = step, y = medianfm.prop, ymin = lqfm.prop, ymax = uqfm.prop,
      color = opter, fill = opter)
) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  scale_y_log10() +
  facet_wrap(~dataset, strip.position = "bottom") +
  theme_minimal() + theme(strip.background = element_blank(), strip.text.x = element_blank()) +
  ylab("Distance to minimum") + xlab("Number of samples")

opter_labels <- c(
  random = "RANDOM",
  straddle = "STRADDLE",
  tvi = "TruVarImp",
  maxsd = "VAR"
)

# Combine them with patchwork
# - Stack them vertically (p1 / p2)
# - Collect guides (legend) into one
# - Add a shared plot title/description at the top
combined_plot <- p1 / p2 +
  plot_layout(guides = "collect") +
  plot_annotation(

  ) &
  scale_color_discrete(name = "Optimizer", labels = opter_labels) &
  scale_fill_discrete(name = "Optimizer", labels = opter_labels) &
  theme(
    legend.position = "bottom", legend.box = "horizontal"
  )

# Display the combined plot
combined_plot
dev.off()


#########################

opter_labels2 <- c(
  random = "RANDOM",
  straddle = "STRADDLE",
  tvi = "TruVarImp",
  maxsd = "VAR",
  random_search = "RANDOM"
)


pdf("figures/f1_main.pdf", width = 7, height = 6)
 ggplot(
  infotable[dataset %in% c("cs", "gc", "bs", "st")][step <= 600],
  aes(x = step, y = medianf1, ymin = lqf1, ymax = uqf1,
      color = opter, fill = opter)
) +
  geom_line() +
  geom_ribbon(alpha = 0.2) +
  facet_wrap(~dataset, labeller = as_labeller(c(cs = "COMPAS (CS)", gc = "German Credit (GC)",
    bs = "Bike Sharing (BS)", st = "Synthetic (ST)")), scales = "free_y") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank()
    #panel.grid.major.x = element_blank(),
    #panel.grid.minor.x = element_blank()
  ) +
  ylab("F1 score") +
  scale_color_discrete(name = "Optimizer", labels = opter_labels2) +
  scale_fill_discrete(name = "Optimizer", labels = opter_labels2)
dev.off()
