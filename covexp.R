

in.x <- runif(5)
data.train <- data.table(
  x = in.x,
  y = sin(2 * pi * in.x)
)
data.test <- data.table(
  x = seq(0, 1, length.out = 200),
  y = sin(2 * pi * seq(0, 1, length.out = 200))
)
task.train <- TaskRegr$new(
  "sinwave",
  backend = data.train,
  target = "y"
)
task.test <- TaskRegr$new(
  "sinwave_test",
  backend = data.test,
  target = "y"
)


plottask <- function(task) {
  plot(task$data()[, .(x, y)], pch = 19, ylim = c(-1.4, 1.4), xlim = c(0, 1))
  lines(data.test$x, data.test$y, col = "red")
}

modelselines <- function(model, col = "blue") {
  preds <- DiceKriging::predict(model, data.test[, .(x)], type = "SK", se.compute = TRUE)
  lines(data.test$x, preds$mean, col = col)
  lines(data.test$x, preds$mean + preds$sd, col = col, lty = 2)
  lines(data.test$x, preds$mean - preds$sd, col = col, lty = 2)
}

plotSEs <- function(model, newplot = TRUE, col = "red") {
  preds <- DiceKriging::predict(model, data.test[, .(x)], type = "SK", se.compute = TRUE)
  if (newplot) {
    plot(data.test$x, preds$sd, col = col, type = "l")
  } else {
    lines(data.test$x, preds$sd, col = col)
  }
}

newpoint.x <- runif(3)
newpoint <- data.table(x = newpoint.x, y = sin(2 * pi * newpoint.x))

learner.base <- LearnerRegrKM$new()
learner.base$train(task.train)


model <- learner.base$model
model2 <- lapply(seq_len(nrow(newpoint)), function(i) DiceKriging::update(model, newX = as.matrix(newpoint[i, .(x)]), newy = as.matrix(newpoint[i, .(y)]),
  cov.reestim = FALSE, trend.reestim = FALSE, nugget.reestim = FALSE
))

plottask(task.train)
modelselines(model)
modelselines(model2[[1]], col = "green")
modelselines(model2[[2]], col = "green")
modelselines(model2[[3]], col = "green")
points(newpoint$x, newpoint$y, col = "red", pch = 19)

plotSEs(model)
plotSEs(model2[[1]], newplot = FALSE, col = "green")
plotSEs(model2[[2]], newplot = FALSE, col = "blue")
plotSEs(model2[[3]], newplot = FALSE, col = "violet")

model2


source("R/algorithms/LearnerRegrKMExtra.R")

newmat <- posteriorVarGivenNewPoints(model, as.matrix(newpoint[, .(x)]), as.matrix(data.test[, .(x)]), 0)

points(data.test$x, newmat[, 1]^.5, col = ifelse(newmat > 0, "red", "blue"))
points(data.test$x, newmat[, 2]^.5, col = ifelse(newmat > 0, "red", "blue"))
points(data.test$x, newmat[, 3]^.5, col = ifelse(newmat > 0, "red", "blue"))


learner.extra <- LearnerRegrKMExtra$new()
# learner.extra$param_set$set_values(nugget = 1e-8)
learner.extra$encapsulate("evaluate", lrn("regr.featureless"))
learner.extra$train(task.train)

learner.base$model
learner.extra$model$model

learner.extra$predict(task.test)

lemat <- learner.extra$predictConditionalSE(as_task_regr(newpoint, target = "y"), task.test)

all.equal(lemat, newmat^.5, tol = 1e-6)

lemat - newmat






