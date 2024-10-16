
library("data.table")
library("ggplot2")
readRDS("/tmp/rashomon.table.glmnet.rds") -> tt
readRDS("/tmp/rashomon.table.tree.rds") -> ttree
readRDS("/tmp/rashomon.table.xgb.rds") -> txgb


tt[, is.grid := TRUE]
tt[seq_len(nrow(tt)/2), is.grid := FALSE]

reduced <- tt[, .(bbrier = mean(result.classif.bbrier), bbrier.se = sd(result.classif.bbrier), rmse = mean(result.regr.rmse), rmse.se = sd(result.regr.rmse), is.grid = all(is.grid)),
  by = c("taskname", "config.glmnet.alpha", "config.glmnet.lambda")]

sliced.by.trial <- lapply(1:10, function(i) tt[seq(i, nrow(tt), by = i), .(bbrier = result.classif.bbrier, rmse = result.regr.rmse, taskname, config.glmnet.alpha, config.glmnet.lambda)])


ttree[, is.grid := TRUE]
ttree[seq_len(nrow(ttree)/2), is.grid := FALSE]

ttree[is.grid == TRUE, table(config.cp) |> length()]
ttree[is.grid == FALSE, table(config.cp) |> length()]

reduced.tree <- ttree[, .(bbrier = mean(result.classif.bbrier), bbrier.se = sd(result.classif.bbrier), rmse = mean(result.regr.rmse), rmse.se = sd(result.regr.rmse), is.grid = all(is.grid)),
  by = c("taskname", "config.cp", "config.minbucket", "config.minsplit")]

bycols <- c("taskname", grep("^(config\\.)?xgb\\.", colnames(txgb), value = TRUE))
reduced.xgb <- txgb[, .(bbrier = mean(result.classif.bbrier), bbrier.se = sd(result.classif.bbrier), rmse = mean(result.regr.rmse), rmse.se = sd(result.regr.rmse), .N, is.grid = FALSE), by = bycols]
#plot(sort(reduced.xgb$N |> table()))
reduced.xgb[, table(N, taskname)]



reduced.tree[is.grid == TRUE, table(config.cp) |> length()]
reduced.tree[is.grid == FALSE, table(config.cp) |> length()]
#reduced.xgb[is.grid == TRUE, table(config.xgb.subsample) |> length()]
reduced[is.grid == FALSE, table(config.glmnet.lambda) |> length()]
reduced[is.grid == TRUE, table(config.glmnet.lambda) |> length()]






###
## # Load necessary libraries
## # Assuming 'allresults' is your data.table
## # Set the number of top scores to display
## n.scores <- 10  # You can adjust this number as needed

## # Step 1: Select the top n.scores within each 'taskname' and 'algoname'
## top_results <- allresults[, .SD[order(score)][1:n.scores], by = .(taskname, algoname)]

## # Step 2: Add a rank column within each group
## top_results[, rank := 1:.N, by = .(taskname, algoname)]

## # Step 3: Create a combined dataset where all 'algoname's are combined
## combined_results <- copy(top_results)
## combined_results[, orig_algoname := algoname]  # Preserve original algoname
## combined_results[, algoname := 'All']          # Set algoname to 'All' for combined plot

## # Step 4: Combine the original and combined datasets
## all_plot_data <- rbind(top_results, combined_results, fill = TRUE)

## # Step 5: Create the plot
## ggplot(all_plot_data, aes(x = rank, y = score)) +
##   # Add confidence bands using geom_crossbar
##   geom_crossbar(
##     aes(
##       ymin = score - score.se,
##       ymax = score + score.se,
##       fill = ifelse(algoname == 'All', orig_algoname, NA)
##     ),
##     width = 0.2,
##     alpha = 0.3,
##     color = NA
##   ) +
##   # Add points
##   geom_point(
##     aes(color = ifelse(algoname == 'All', orig_algoname, 'black'))
##   ) +
##   # Log-scale the x-axis
##   scale_x_log10() +
##   # Facet the plot by 'taskname' and 'algoname'
##   facet_grid(taskname ~ algoname, scale = "free_y") +
##   # Set theme and labels
##   theme_bw() +
##   labs(color = 'Algorithm', fill = 'Algorithm', x = 'Rank (log scale)', y = 'Score')


########

scores.used = c(gc = "bbrier", cs = "bbrier", bs = "rmse")

allred <- list(
  xgb = reduced.xgb,
  tree = reduced.tree,
  glmnet = reduced
)

alltoeval <- sapply(names(allred), function(ln) {
  ut <- allred[[ln]]
  lapply(names(scores.used), function(su) {
    outcome <- scores.used[[su]]
    ut[taskname == su, `:=`(score = get(outcome), score.se = get(paste0(outcome, ".se")))]
  }) |> rbindlist()
}, simplify = FALSE)

torun.minima <- lapply(alltoeval, function(x) x[, .SD[which.min(score)], by = "taskname"])


set.seed(1)
torun.samples <- lapply(alltoeval, function(x) x[is.grid == FALSE, .SD[score <= min(score) * 1.05][sample(.N, 1000)], by = "taskname"])

saveRDS(list(torun.minima = torun.minima, torun.samples = torun.samples), "toeval.rds")



##########

allresults <- lapply(names(allred), function(ln) {
  ut <- allred[[ln]]
  lapply(names(scores.used), function(su) {
    outcome <- scores.used[[su]]
    ut[taskname == su, .(taskname, algoname = ln, score = get(outcome), score.se = get(paste0(outcome, ".se")))]
  })
}) |> unlist(recursive = FALSE) |> rbindlist()

# Set the value for top n scores
usetask <- "bs"

n.scores <- 1e4


# Step 1: Extract the top n.scores for each combination of taskname and algoname
allresults_top <- allresults[, .SD[order(score)][1:min(.N, n.scores)], by = .(taskname)]
allresults_allalgs_top <- allresults[, .SD[order(score)][1:min(.N, n.scores)], by = .(taskname)]

allresults_top[, underlying := algoname]
allresults_allalgs_top[, underlying := algoname]
allresults_allalgs_top[, algoname := "combined"]

# Step 3: Combine the original and combined datasets
allresults_final <- rbind(allresults_top, allresults_allalgs_top, use.names = TRUE)

# Step 4: Assign rank within each taskname and algoname, ensuring proper ordering in combined plot
allresults_final[, rank := seq_len(.N), by = .(taskname, algoname)]
allresults_final[rank > 10, score.se := NA]

# Step 5: Plotting using ggplot2
ggplot(allresults_final[taskname == usetask], aes(x = rank, y = score,
    color = underlying, fill = underlying)) +
  geom_point() +
  geom_ribbon(aes(ymin = score - score.se, ymax = score + score.se), alpha = 0.2) +
#  scale_x_log10() +
  facet_grid(taskname ~ algoname, scales = "free_y") +
  labs(
    title = "Top Scores within Each Task and Algorithm",
    x = "Rank (log scale)",
    y = "Score",
    color = "Algorithm",
    fill = "Algorithm"
  ) +
  theme_minimal()
###


allresults[, .(score_min = min(score), sd_min = score.se[which.min(score)]), by = c("taskname", "algoname")]


library("mlr3")
library("mlr3learners")
demotask <- as_task_regr(data.table(x = rnorm(80) + c(0, 1), y = 1:2), target = "x")
# plot(demotask$data())
ll2 <- lrn("regr.ranger", num.trees = 2000, splitrule = "extratrees")$train(demotask)
#plot(ll2$predict_newdata(data.table(y = seq(1, 2, length.out = 100)))$response)

confcols <- grep("^config\\.xgb\\.", colnames(txgb), value = TRUE)

linear.cols <- names(which(sapply(confcols, function(x) { sample(reduced.xgb[[x]], 1000) -> y ; y <- y - min(y) ; mean(y) / median(y) }) < 1.5))
tolog <- setdiff(confcols, linear.cols)

# task <- "gc" works well
# task <- "cs"  # 1. the optimum is very flat here; also the model is broken for some reason:
# outcome <- "bbrier"
task <- "bs"
outcome <- "rmse"

# allpred <- ll$predict_newdata(ttask)
# ttask2 <- cbind(ttask, response = allpred$response)
# ttask2[, plot(bbrier, response, pch = ".")]
# ttask2[, plot(rmse, response, pch = ".")]

ttask <- reduced.xgb[taskname == task, c(confcols, outcome), with = FALSE]
ttask[, (tolog) := lapply(.SD, log), .SDcols = tolog]
ttask[, (outcome) := log(get(outcome))]

# ttask[sample(nrow(ttask), 1000), lapply(.SD, function(x) { plot(sort(x)) ; readline() })]

ll <- lrn("regr.ranger", num.trees = 500, num.threads = 16, splitrule = "extratrees", oob.error = FALSE)$train(as_task_regr(ttask, target = outcome))
#ll <- lrn("regr.ranger", num.trees = 500, num.threads = 16, oob.error = FALSE)$train(as_task_regr(ttask, target = outcome))

# optpoint <- ttask[which.min(get(outcome))][, -"bbrier"]
optpoint <- as.data.table(lapply(ttask[order(get(outcome))[1:10]][, -"bbrier"], median))

optpoint.sd <- reduced.xgb[task == taskname, .(bbrier, bbrier.se, rmse, rmse.se, N)][which.min(get(outcome)), get(paste0(outcome, ".se"))]
# optpoint.pred <- ll$predict_newdata(optpoint)$data$response
#predtask <- ttask[which.min(get(outcome))][, -"bbrier"]
#predtask <- ttask[which.min(get(outcome))][, -"rmse"]
# ll$predict_newdata(predtask)$data$response
ranges <- lapply(ttask[, confcols, with = FALSE], range)


#library(ggforce)

# Assuming 'll', 'confcols', 'ranges', and 'optpoint' are already defined.

# Create an empty list to store data frames for each pair
data_list <- list()
# Get all pairs of confcols where i < j
confcols_indices <- seq_along(confcols)
for (i_idx in confcols_indices) {
  for (j_idx in confcols_indices) {
    # Only consider upper triangle (i < j)
    if (i_idx >= j_idx) next
#
    i <- confcols[i_idx]
    j <- confcols[j_idx]
#
    # Create a grid over the ranges of i and j
    xi <- seq(ranges[[i]][1], ranges[[i]][2], length.out = 100)
    xj <- seq(ranges[[j]][1], ranges[[j]][2], length.out = 100)
    grid_dt <- as.data.table(expand.grid(xi, xj))
    setnames(grid_dt, c(i, j))
#
    # Set other columns to values in optpoint
    other_cols <- setdiff(confcols, c(i, j))
    for (col in other_cols) {
      grid_dt[, (col) := optpoint[[col]]]
    }
#
    # Predict using the trained model
    preds <- ll$predict_newdata(grid_dt)
    grid_dt[, prediction := preds$data$response]
#
    # Add columns for faceting and plotting
    grid_dt[, Var1 := i]
    grid_dt[, Var2 := j]
    grid_dt[, x := get(i)]
    grid_dt[, y := get(j)]
#
    # Store the data frame
    data_list[[length(data_list) + 1]] <- grid_dt[, .(x, y, Var1, Var2, prediction)]
  }
}

## ggplot(data_list[[1]], aes(x = x, y = y, z = prediction)) +
##   stat_contour_filled(breaks = seq(optpoint.pred, by = 10 * optpoint.sd, length.out = 20)) +
##   scale_colour_gradient(low = "blue", high = "red") +
## #  geom_point(data = min_point, aes(x = config.glmnet.alpha, y = log10_lambda), color = "red", shape = "x", size = 4, stroke = 2) +
##   theme_minimal()


# Combine all data frames
plot_data <- rbindlist(data_list)

# Ensure proper ordering of facets
plot_data[, Var1 := factor(Var1, levels = confcols)]
plot_data[, Var2 := factor(Var2, levels = rev(confcols))]

# TODO: tic marks ~ average point distance
opx <- optpoint
opx <- ttask[order(get(outcome)) |> head(10)]

bestpoint <- list()
for (i_idx in confcols_indices) {
  for (j_idx in confcols_indices) {
    # Only consider upper triangle (i < j)
    if (i_idx >= j_idx) next
    i <- confcols[i_idx]
    j <- confcols[j_idx]
    bestpoint[[length(bestpoint) + 1]] <- data.table(Var1 = i, Var2 = j, x = opx[[i]], y = opx[[j]])
  }
}
bestpoint <- rbindlist(bestpoint)
bestpoint[, Var1 := factor(Var1, levels = confcols)]
bestpoint[, Var2 := factor(Var2, levels = rev(confcols))]
#
#
# Create the plot
p <- ggplot(plot_data, aes(x = x, y = y, z = (prediction))) +
  geom_contour_filled(breaks = seq((min(plot_data$prediction)), by = 0.01, length.out = 20)) +
  geom_point(data = bestpoint, aes(x = x, y = y), color = "red", shape = 4, size = 2, stroke = 1.4, inherit.aes = FALSE) +
  facet_grid(Var2 ~ Var1, scales = "free", switch = "y") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(
    strip.background = element_blank(),
#    strip.text.x = element_text(angle = 0, hjust = 0),
#    strip.text.y = element_text(angle = 90, hjust = 0),
#    axis.text.x = element_text(angle = 0, hjust = 0),
#    axis.text.y = element_text(angle = 90, hjust = 0),
    panel.spacing = unit(0.1, "lines"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  )
# Adjust the axes to display only on the outer plots
# Get the levels of Var1 and Var2
var1_levels <- levels(plot_data$Var1)
var2_levels <- levels(plot_data$Var2)
# Function to set axis labels only on the outer plots
p <- p + theme(
  panel.border = element_rect(color = "gray80", fill = NA),
  axis.title.x = element_blank(),
  axis.title.y = element_blank()
)
# Print the plot
print(p)

optpoint.sd

reduced.xgb[task == taskname, .(bbrier, bbrier.se, rmse, rmse.se, N)][which.min(get(outcome)), get(paste0(outcome, ".se"))]

1+1






































task <- "cs"
outcome <- "bbrier"
working <- rbind(reduced[, .(taskname, bbrier, bbrier.se, rmse, rmse.se, learner = 'glmnet')], reduced.tree[, .(taskname, bbrier, bbrier.se, rmse, rmse.se, learner = 'tree')])[taskname == task][order(get(outcome))]
working[, plot((get(outcome))[1:1000], col = ifelse(learner == "glmnet", "red", "blue"))]
working[, lines((get(outcome) + get(paste0(outcome, ".se")))[1:1000])]
working[, lines((get(outcome) - get(paste0(outcome, ".se")))[1:1000])]

task <- "gc"
outcome <- "bbrier"
working <- rbind(reduced[, .(taskname, bbrier, bbrier.se, rmse, rmse.se, learner = 'glmnet')], reduced.tree[, .(taskname, bbrier, bbrier.se, rmse, rmse.se, learner = 'tree')])[taskname == task][order(get(outcome))]
working[, plot((get(outcome)), col = ifelse(learner == "glmnet", "red", "blue"))]
working[, lines((get(outcome) + get(paste0(outcome, ".se"))))]
working[, lines((get(outcome) - get(paste0(outcome, ".se"))))]



ggplot(reduced[taskname == "gc"], aes(x = config.glmnet.alpha, y = config.glmnet.lambda, color = bbrier)) + scale_y_log10() + geom_point()

reduced$taskname |> table()  # bs, cs, gc

task <- "bs"
outcome <- "rmse"
min_point <- reduced[taskname == task][10001:20000][which.min(get(sub("\\..*", "", outcome))),
                .(config.glmnet.alpha, log10_lambda = log10(config.glmnet.lambda), bbrier, rmse, bbrier.se, rmse.se)]
ggplot(reduced[taskname == task][10001:20000], aes(x = config.glmnet.alpha, y = log10(config.glmnet.lambda), z = get(outcome))) +
  stat_contour_filled(breaks = seq(min(reduced[taskname == task, get(outcome)]), max(reduced[taskname == task, get(outcome)]), length.out = 20)) +
  scale_colour_gradient(low = "blue", high = "red") +
  geom_point(data = min_point, aes(x = config.glmnet.alpha, y = log10_lambda), color = "red", shape = "x", size = 4, stroke = 2) +
  labs(
    x = "Alpha",
    y = "Log10(Lambda)",
    colour = outcome,
    title = "Contour plot of score across Alpha and Lambda"
  ) +
  theme_minimal()


min_point <- reduced[taskname == task][10001:20000][which.min(get(outcome)),
                .(config.glmnet.alpha, log10_lambda = log10(config.glmnet.lambda), bbrier, rmse, bbrier.se, rmse.se)]
ggplot(reduced[taskname == task][10001:20000], aes(x = config.glmnet.alpha, y = log10(config.glmnet.lambda), z = rank(get(outcome)))) +
  stat_contour_filled(breaks = seq(1, 10000, length.out = 20)) +
  scale_colour_gradient(low = "blue", high = "red") +
  geom_point(data = min_point, aes(x = config.glmnet.alpha, y = log10_lambda), color = "red", shape = "x", size = 4, stroke = 2) +
  labs(
    x = "Alpha",
    y = "Log10(Lambda)",
    colour = outcome,
    title = "Contour plot of score across Alpha and Lambda"
  ) +
  theme_minimal()

min_point <- reduced[taskname == task][10001:20000][which.min(get(outcome)),
                .(config.glmnet.alpha, log10_lambda = log10(config.glmnet.lambda), bbrier, rmse, bbrier.se, rmse.se)]
ggplot(reduced[taskname == task][10001:20000], aes(x = config.glmnet.alpha, y = log10(config.glmnet.lambda), z = log(get(outcome) - min(get(outcome))))) +
  stat_contour_filled(breaks = seq(-11, log(max(reduced[taskname == task, get(outcome)]) - min(reduced[taskname == task, get(outcome)])), length.out = 20)) +
  scale_colour_gradient(low = "blue", high = "red") +
  geom_point(data = min_point, aes(x = config.glmnet.alpha, y = log10_lambda), color = "red", shape = "x", size = 4, stroke = 2) +
  labs(
    x = "Alpha",
    y = "Log10(Lambda)",
    colour = outcome,
    title = "Contour plot of score across Alpha and Lambda"
  ) +
  theme_minimal()




subset_data <- reduced[taskname == task][10001:20000]

library("plotly")
# Plot the 3D surface
plot_ly(
  data = subset_data,
  x = ~config.glmnet.alpha,
  y = ~log10(config.glmnet.lambda),
  z = ~get(outcome),
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 3, color = ~get(outcome), colorscale = "Viridis")
) |>
  layout(
    scene = list(
      xaxis = list(title = "Alpha"),
      yaxis = list(title = "Log10(Lambda)"),
      zaxis = list(title = outcome),
      title = "3D Contour Plot of BBrier across Alpha and Lambda"
    )
  )

# Plot the 3D surface
plot_ly(
  data = subset_data,
  x = ~config.glmnet.alpha,
  y = ~log10(config.glmnet.lambda),
  z = ~get(outcome),
  type = "mesh3d",
  intensity = ~get(outcome),
  colorscale = "Viridis"
) |>
  layout(
    scene = list(
      xaxis = list(title = "Alpha"),
      yaxis = list(title = "Log10(Lambda)"),
      zaxis = list(title = outcome),
      title = "3D Contour Plot of BBrier across Alpha and Lambda"
    )
  )



#############




task <- "gc"
outcome <- "bbrier"
slicing <- "config.cp"
others <- setdiff(c("config.cp", "config.minbucket", "config.minsplit"), slicing)
tgrid <- lapply(reduced.tree[, .(config.cp, config.minbucket, config.minsplit)], function(x) { t <- table(x) ; as.numeric(names(t))[t > 300]})
working <- reduced.tree[taskname == task]
working <- working[apply(sapply(names(tgrid), function(n) get(n) %in% tgrid[[n]]), 1, all)]
min_point <- working[which.min(get(outcome)),
                .(config.cp, config.minbucket, config.minsplit, bbrier, rmse, bbrier.se, rmse.se)]
wslice <- working[get(slicing) == min_point[, get(slicing)]]
ggplot(wslice, aes(x = get(others[[1]]), y = get(others[[2]]), z = get(outcome))) +
  stat_contour_filled(breaks = head(seq(min(wslice[, get(outcome)]), max(wslice[, get(outcome)]), by = min_point[[paste0(outcome, ".se")]]), 40)) +
  scale_colour_gradient(low = "blue", high = "red") +
  scale_x_log10() + scale_y_log10() +
  geom_point(data = min_point, aes(x = get(others[[1]]), y = get(others[[2]])), color = "red", shape = "x", size = 4, stroke = 2) +
  labs(
    x = others[[1]],
    y = others[[2]],
    colour = outcome,
    title = paste0("Contour plot for minimal ", slicing)
  ) +
  theme_minimal()


task <- "cs"
outcome <- "bbrier"
working <- reduced[taskname == task][10001:20000]
min_point <- working[which.min(get(sub("\\..*", "", outcome))),
                .(config.glmnet.alpha, config.glmnet.lambda, bbrier, rmse, bbrier.se, rmse.se)]
ggplot(working, aes(x = config.glmnet.alpha, y = config.glmnet.lambda, z = get(outcome))) +
  stat_contour_filled(breaks = head(seq(min(working[, get(outcome)]), max(working[, get(outcome)]), by = min_point[[paste0(outcome, ".se")]]), 40)) +
  scale_colour_gradient(low = "blue", high = "red") +
  scale_y_log10() +
  geom_point(data = min_point, aes(x = config.glmnet.alpha, y = config.glmnet.lambda), color = "red", shape = "x", size = 4, stroke = 2) +
  labs(
    x = "Alpha",
    y = "Lambda",
    colour = outcome,
    title = "Contour plot of score across Alpha and Lambda"
  ) +
  theme_minimal()






task <- "gc"
outcome <- "bbrier"
working <- reduced[taskname == task]
min_point <- working[which.min(get(sub("\\..*", "", outcome))),
                .(config.glmnet.alpha, config.glmnet.lambda, bbrier, rmse, bbrier.se, rmse.se)]
breaks.scale <- head(seq(min(working[, get(outcome)]), max(working[, get(outcome)]), by = 2 * min_point[[paste0(outcome, ".se")]]), 5)
ggplot(working, aes(x = config.glmnet.alpha, y = config.glmnet.lambda, color = cut(get(outcome), breaks = breaks.scale, include.lowest = TRUE))) +
  geom_point() +
  scale_y_log10() +
  geom_point(data = min_point, aes(x = config.glmnet.alpha, y = config.glmnet.lambda), color = "red", shape = "x", size = 4, stroke = 2) +
  labs(
    x = "Alpha",
    y = "Lambda",
    colour = outcome,
    title = "Contour plot of score across Alpha and Lambda"
  ) +
  theme_minimal()


task <- "gc"
outcome <- "bbrier"
working <- sliced.by.trial[[3]][taskname == task]
min_point <- reduced[taskname == task][which.min(get(sub("\\..*", "", outcome))),
                .(config.glmnet.alpha, config.glmnet.lambda, bbrier, rmse, bbrier.se, rmse.se)]
breaks.scale <- head(seq(min(working[, get(outcome)]), max(working[, get(outcome)]), by = 4 * min_point[[paste0(outcome, ".se")]]), 5)
ggplot(working, aes(x = config.glmnet.alpha, y = config.glmnet.lambda, color = cut(get(outcome), breaks = breaks.scale, include.lowest = TRUE))) +
  geom_point() +
  scale_y_log10() +
  geom_point(data = min_point[, .(config.glmnet.alpha, config.glmnet.lambda, bbrier, rmse)], aes(x = config.glmnet.alpha, y = config.glmnet.lambda), color = "red", shape = "x", size = 4, stroke = 2) +
  labs(
    x = "Alpha",
    y = "Lambda",
    colour = outcome,
    title = "Contour plot of score across Alpha and Lambda"
  ) +
  theme_minimal()
