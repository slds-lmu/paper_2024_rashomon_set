rm(list = ls(all.names = TRUE))
source("init.R")
library("ggplot2")
library("patchwork")



plotInitialModel <- function(n) {

  osr <- makeObjectiveStreamRecorded("st", "svm.radial", 1, FALSE, debug = FALSE)
  xes <- osr$sample(n)
  yes <- osr$eval(xes)
  task <- as_task_regr(data.frame(xes[, -".id"], y = yes), target = "y")

  learner <- LearnerRegrKMExtra$new()
  learner$predict_type <- "se"
  learner$train(task)

  predictgrid <- paradox::generate_design_grid(osr$domain, 100)$data

  predictions <- learner$predict_newdata(predictgrid)

  predictions

  plotdata <- rbind(
    cbind(predictgrid, y = predictions$response, type = "response"),
    cbind(predictgrid, y = predictions$se, type = "se")
  )


  plot.response <- ggplot(plotdata[type == "response"], aes(x = get(colnames(predictgrid)[1]), y = get(colnames(predictgrid)[2]), z = y)) +
    geom_contour_filled(alpha = 0.7, bins = 15) +
    geom_contour(alpha = 0.5, bins = 15, size = 0.2) +
    geom_point(data = cbind(xes, y = yes), color = "red", size = 2) +
    theme(aspect.ratio = 1) +
    labs(title = "Response")

  plot.se <- ggplot(plotdata[type == "se"], aes(x = get(colnames(predictgrid)[1]), y = get(colnames(predictgrid)[2]), z = y)) +
    geom_contour_filled(alpha = 0.7, bins = 15) +
    geom_contour(alpha = 0.5, bins = 15, size = 0.2) +
    geom_point(data = cbind(xes, y = yes), color = "red", size = 2) +
    theme(aspect.ratio = 1) +
    labs(title = "SE")

  plot.response + plot.se

}


plotInitialModel(100)
plotInitialModel(30)
plotInitialModel(20)
plotInitialModel(10)
plotInitialModel(5)


plotCompleteModel <- function(problem, learner, n) {

  fulldata <- unique(getAllStreams(problem)[[learner]])
  fulltask <- as_task_regr(fulldata[, grep("^config|^score\\.se", colnames(fulldata), value = TRUE, invert = TRUE), with = FALSE], target = "score")

  osr <- makeObjectiveStreamRecorded(problem, learner, 1, FALSE, debug = FALSE)
  xes <- osr$sample(n)
  yes <- osr$eval(xes)
  task <- as_task_regr(data.frame(xes[, -".id"], y = yes), target = "y")

  learner <- LearnerRegrKMExtra$new()
  learner$predict_type <- "se"
  learner$param_set$set_values(.values = list(nugget.estim = TRUE, jitter = 0.0001, covtype = "exp"))
  learner$train(task)

  predictions <- learner$predict(fulltask)

  predictions

  plotdata <- rbind(
    cbind(fulldata, y = predictions$response, type = "response"),
    cbind(fulldata, y = predictions$se, type = "se")
  )

  makePlotGrid <- function(data, var1, var2, y) {
    plotgrid <- interp::interp(data[[var1]], data[[var2]], data[[y]], method = "linear", duplicate = "strip", nx = 200, ny = 200)
    cbind(CJ(var2 = plotgrid$x, var1 = plotgrid$y), z = as.vector(plotgrid$z))
  }

  plotgrid.groundtruth <- makePlotGrid(fulldata, "svm.cost", "svm.gamma", "score")
  plotgrid.response <- makePlotGrid(plotdata[type == "response"], "svm.cost", "svm.gamma", "y")
  plotgrid.se <- makePlotGrid(plotdata[type == "se"], "svm.cost", "svm.gamma", "y")

  plot.groundtruth <- ggplot(plotgrid.groundtruth, aes(x = var1, y = var2, z = z)) +
    geom_contour_filled(alpha = 0.7, bins = 15) +
    geom_contour(alpha = 0.5, bins = 15, size = 0.2) +
    theme(aspect.ratio = 1) +
    # geom_point(data = fulldata, aes(x = svm.cost, y = svm.gamma, z = score), color = "black", size = 0.1) +
    labs(title = "Ground Truth")

  plot.response <- ggplot(plotgrid.response, aes(x = var1, y = var2, z = z)) +
    geom_contour_filled(alpha = 0.7, bins = 15) +
    geom_contour(alpha = 0.5, bins = 15, size = 0.2) +
    theme(aspect.ratio = 1) +
    geom_point(data = task$data()[, z := 1], aes(x = svm.cost, y = svm.gamma), color = "red", size = 1) +
    labs(title = "Response")

  plot.se <- ggplot(plotgrid.se, aes(x = var1, y = var2, z = z)) +
    geom_contour_filled(alpha = 0.7, bins = 15) +
    geom_contour(alpha = 0.5, bins = 15, size = 0.2) +
    theme(aspect.ratio = 1) +
    geom_point(data = task$data()[, z := 1], aes(x = svm.cost, y = svm.gamma), color = "red", size = 1) +
    labs(title = "SE")

  plot.groundtruth + plot.response + plot.se
}


plotCompleteModel("st", "svm.radial", 100)
plotCompleteModel("st", "svm.radial", 30)
plotCompleteModel("st", "svm.radial", 5)
plotCompleteModel("cs", "svm.radial", 100)
plotCompleteModel("cs", "svm.radial", 30)
plotCompleteModel("cs", "svm.radial", 5)



plotSEDeviation <- function(n, psv, qqplot = FALSE, problem = "st") {
  learner <- "svm.radial"

  fulldata <- unique(getAllStreams(problem)[[learner]])
  fulltask <- as_task_regr(fulldata[, grep("^config|^score\\.se", colnames(fulldata), value = TRUE, invert = TRUE), with = FALSE], target = "score")

  osr <- makeObjectiveStreamRecorded(problem, learner, 1, FALSE, debug = FALSE)
  xes <- osr$sample(n)
  yes <- osr$eval(xes)
  task <- as_task_regr(data.frame(xes[, -".id"], y = yes), target = "y")

  learner <- LearnerRegrKMExtra$new()
  learner$predict_type <- "se"
  learner$param_set$set_values(.values = psv)
  learner$train(task)

  predictions <- learner$predict(fulltask)

  ootpreds <- data.table(truth = predictions$truth, response = predictions$response, se = predictions$se)[
    !fulltask$data()$svm.cost %in% xes$svm.cost | !fulltask$data()$svm.gamma %in% xes$svm.gamma
  ]

  if (qqplot) {
    plot(qnorm(0.5 + seq_len(nrow(ootpreds)) / (2 * nrow(ootpreds))), sort(ootpreds[, abs(truth - response) / se]), main = paste("n =", n))
    abline(0, 1, col = "red")
  } else {
    plot(sort(ootpreds[, abs(truth - response) / se]), ylim = c(0, 10), main = paste("n =", n))
    points(sort(abs(rnorm(nrow(ootpreds), 0, 1))), col = "red", pch = ".")
  }

}

plotSEDeviation(150, list(nugget.estim = TRUE, jitter = 0.0001))
plotSEDeviation(100, list(nugget.estim = TRUE, jitter = 0.0001))
plotSEDeviation(30, list(nugget.estim = TRUE, jitter = 0.0001))
plotSEDeviation(5, list(nugget.estim = TRUE, jitter = 0.0001))

plotSEDeviation(150, list(nugget.estim = TRUE, jitter = 0.0001, covtype = "matern3_2"))
plotSEDeviation(150, list(nugget.estim = TRUE, jitter = 0.0001, covtype = "matern5_2"))
plotSEDeviation(150, list(nugget.estim = TRUE, jitter = 0.0001, covtype = "powexp"))
plotSEDeviation(300, list(nugget.estim = TRUE, jitter = 0.0001, covtype = "powexp"))
plotSEDeviation(300, list(nugget.estim = TRUE, jitter = 0.0001, covtype = "exp"))
plotSEDeviation(330, list(nugget.estim = TRUE, jitter = 0.0001, covtype = "exp"))

plotSEDeviation(100, list(nugget.estim = TRUE, jitter = 0.0001, covtype = "matern3_2"))
plotSEDeviation(100, list(nugget.estim = TRUE, jitter = 0.0001, covtype = "powexp"))
plotSEDeviation(100, list(nugget.estim = TRUE, jitter = 0.0001, covtype = "exp"))
plotSEDeviation(100, list(nugget.estim = TRUE, jitter = 0.0001, covtype = "gauss"))
plotSEDeviation(5, list(nugget.estim = TRUE, jitter = 0.0001, covtype = "exp"))
plotSEDeviation(5, list(nugget.estim = TRUE, jitter = 0.0001, covtype = "exp"), qqplot = TRUE)
plotSEDeviation(5, list(nugget.estim = TRUE, jitter = 0.0001, covtype = "exp"), qqplot = TRUE, problem = "cs")
plotSEDeviation(5, list(nugget.estim = TRUE, jitter = 0.0001, covtype = "exp"), problem = "cs")
plotSEDeviation(100, list(nugget.estim = TRUE, jitter = 0.0001, covtype = "powexp"))
plotSEDeviation(30, list(nugget.estim = TRUE, jitter = 0.0001, covtype = "powexp"))
plotSEDeviation(5, list(nugget.estim = TRUE, jitter = 0.0001, covtype = "powexp"))

plotSEDeviation(150, list(nugget.estim = TRUE, jitter = 0.0001))
plotSEDeviation(100, list(nugget.estim = TRUE, jitter = 0.0001))
plotSEDeviation(30, list(nugget.estim = TRUE, jitter = 0.0001))
plotSEDeviation(5, list(nugget.estim = TRUE, jitter = 0.0001))




datarows <- unique(getAllStreams("st")$svm.radial)

dataplot <- interp::interp(datarows$svm.cost, datarows$svm.gamma, datarows$score, method = "linear", duplicate = "strip", nx = 200, ny = 200)

ctr <- cbind(CJ(var2 = dataplot$x, var1 = dataplot$y), z = as.vector(dataplot$z))

range(ctr$z, na.rm = TRUE)

plot(sort(ctr$z))

ggplot(ctr, aes(x = var1, y = var2, z = z)) +
  geom_contour_filled(alpha = 0.7, bins = 100) +
  geom_contour(alpha = 0.5, bins = 100, size = 0.2) +
  geom_point(data = datarows, aes(x = svm.cost, y = svm.gamma, z = score), color = "black", size = 0.1) +
  theme(aspect.ratio = 1) +
  labs(title = "Response")

ggplot(ctr, aes(x = var1, y = var2, z = z)) +







# things to do
# - HPO what is the most robust gp w/r/t se deviance?
#   - truvarimp metaoptimization
# - which points are wrong, small SE or big SE ones?
# - how does truvarimp work on GP data?
# - model updating instead of refitting
# - change model parameter optimization