
# MARSIN
# model-agnostic rashomon set inference

readRDS("data/run_models.rds")$torun.samples |> names()
readRDS("data/run_models.rds")$torun.samples |> names()
readRDS("data/run_models_3.rds")$torun.samples |> names()

source("init.R")

tl <- list.learners.regr$xgb

ts <- tl$param_set$search_space()

tt <- readRDS("data/run_models_3.rds")$torun.samples$xgb[taskname == "st" & is.in.grid == FALSE]



os <- ObjectiveStreamRecorded$new(id = "xgb_st", domain = ts, minimize = TRUE, table = tt, scorecol = "score")

os$eval(os$sample(3))

os$eval(os$getRow(1:3))


oa <- ObjectiveStreamActual$new(
  list.learners.regr$tree,
  list.tasks$st,
  resampling.inner$instantiate(list.tasks$st),
  measure.regr,
  seed = c(1, 2)
)


oa$sample(1)
oa$sample(3)


rr <- rbind(oa$getRow(1), oa$getRow(3))
rr
oa$eval(rr)

plot(1:10, 1:10)





library("manipulateWidget")
library("ggplot2")
library("plotly")

plotf <- function(hp) {
  plot <- ggplot(mtcars[mtcars$hp > hp, ], aes(x = hp, y = mpg)) + geom_point() +
    facet_wrap(~cyl) +
    theme_bw()
  ggplotly(plot)
}

manipulateWidget(
  plotf(hp),
  hp = mwSlider(0, 400, value = 100)
)




func <- function(x) sin(1.5 * sin(x) / x)

x.range <- c(-15, 15)

y.range <- c(-0.5, 2)

# Add plotting code
x.values <- seq(x.range[1], x.range[2], length.out = 200)
plot.data <- data.table(
  x = x.values,
  y = sapply(x.values, func)
)

ggplot(plot.data, aes(x = x, y = y)) +
  geom_line(linetype = "dotted", linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Plot of sin(x)/x",
    x = "x",
    y = "f(x)"
  ) +
  coord_cartesian(ylim = y.range)


library("DiceKriging")

init.size <- 5

set.seed(1)
xvals <- runif(init.size, x.range[1], x.range[2])
yvals <- sapply(xvals, func)

ggplot(plot.data, aes(x = x, y = y)) +
  geom_line(linetype = "dotted", linewidth = 1) +
  geom_point(data = data.table(x = xvals, y = yvals),
    size = 3, color = "black") +
  theme_minimal() +
  labs(
    title = "Plot of sin(x)/x",
    x = "x",
    y = "f(x)"
  ) +
  coord_cartesian(ylim = y.range)

# Fit Kriging model
km.model <- km(
  design = data.frame(x = xvals),
  response = yvals,
  covtype = "gauss",
  control = list(trace = FALSE)
)

# Predict across the range
pred.data <- predict(
  km.model,
  newdata = data.frame(x = plot.data$x),
  type = "UK",
  se.compute = TRUE
)

# Create plot with all elements
ggplot(plot.data, aes(x = x, y = y)) +
  # Original function
  geom_line(linetype = "dotted", linewidth = 1) +
  # Uncertainty ribbon (± 2 standard deviations)
  geom_ribbon(
    data = data.table(
      x = plot.data$x,
      ymin = pred.data$lower95,
      ymax = pred.data$upper95
    ),
    aes(x = x, y = NULL, ymin = ymin, ymax = ymax),
    fill = "blue",
    alpha = 0.2
  ) +
  # Kriging prediction
  geom_line(
    data = data.table(x = plot.data$x, y = pred.data$mean),
    color = "blue",
    linewidth = 1
  ) +
  # Maximum horizontal lines
  geom_hline(yintercept = max(yvals), color = "red", linewidth = 1) +
  geom_hline(yintercept = max(yvals) * 0.85, color = "red",
             linetype = "dotted", linewidth = 1) +
  # Sample points
  geom_point(
    data = data.table(x = xvals, y = yvals),
    size = 3,
    color = "black"
  ) +
  theme_minimal() +
  labs(
    title = "Plot of sin(x)/x with Kriging prediction",
    x = "x",
    y = "f(x)"
  ) +
  coord_cartesian(ylim = y.range)

set.seed(2)
testpoints <- runif(100, x.range[1], x.range[2])
testvals <- predict(km.model, newdata = data.frame(x = testpoints), type = "UK", se.compute = TRUE)

testvals

testdata <- data.table(x = testpoints, y = testvals$mean, ymin = testvals$lower95, ymax = testvals$upper95, sd = testvals$sd)

# Create plot with all elements
ggplot(plot.data, aes(x = x, y = y)) +
  # Original function
  geom_line(linetype = "dotted", linewidth = 1) +
  # Uncertainty ribbon (± 2 standard deviations)
  geom_ribbon(
    data = data.table(
      x = plot.data$x,
      ymin = pred.data$lower95,
      ymax = pred.data$upper95
    ),
    aes(x = x, y = NULL, ymin = ymin, ymax = ymax),
    fill = "blue",
    alpha = 0.2
  ) +
  # Kriging prediction
  geom_line(
    data = data.table(x = plot.data$x, y = pred.data$mean),
    color = "blue",
    linewidth = 1
  ) +
  # Maximum horizontal lines
  geom_hline(yintercept = max(yvals), color = "red", linewidth = 1) +
  geom_hline(yintercept = max(yvals) * 0.85, color = "red",
             linetype = "dotted", linewidth = 1) +
  # Test points with error bars
  geom_errorbar(
    data = data.table(
      x = testpoints,
      y = testvals$mean,
      ymin = testvals$lower95,
      ymax = testvals$upper95
    ),
    aes(x = x, ymin = ymin, ymax = ymax),
    color = "orange",
    width = 0.5
  ) +
  geom_point(
    data = testdata,
    color = "orange",
    size = 2
  ) +
  # Sample points
  geom_point(
    data = data.table(x = xvals, y = yvals),
    size = 3,
    color = "black"
  ) +
  theme_minimal() +
  labs(
    title = "Plot of sin(x)/x with Kriging prediction",
    x = "x",
    y = "f(x)"
  ) +
  coord_cartesian(ylim = y.range)

curmax <- max(yvals)
currash <- max(yvals) * 0.85

testdata[, out := ymax < currash]

testdata

ggplot(plot.data, aes(x = x, y = y)) +
  # Original function
  geom_line(linetype = "dotted", linewidth = 1) +
  # Uncertainty ribbon (± 2 standard deviations)
  geom_ribbon(
    data = data.table(
      x = plot.data$x,
      ymin = pred.data$lower95,
      ymax = pred.data$upper95
    ),
    aes(x = x, y = NULL, ymin = ymin, ymax = ymax),
    fill = "blue",
    alpha = 0.2
  ) +
  # Kriging prediction
  geom_line(
    data = data.table(x = plot.data$x, y = pred.data$mean),
    color = "blue",
    linewidth = 1
  ) +
  # Maximum horizontal lines
  geom_hline(yintercept = max(yvals), color = "red", linewidth = 1) +
  geom_hline(yintercept = max(yvals) * 0.85, color = "red",
             linetype = "dotted", linewidth = 1) +
  # Test points with error bars
  geom_errorbar(
    data = testdata,
    aes(ymin = ymin, ymax = ymax, alpha = !out),
    color = "orange",
    width = 0.5
  ) +
  geom_point(
    data = testdata,
    color = "orange",
    aes(alpha = !out),
    size = 2
  ) +
  # Sample points
  geom_point(
    data = data.table(x = xvals, y = yvals),
    size = 3,
    color = "black"
  ) +
  theme_minimal() +
  labs(
    title = "Plot of sin(x)/x with Kriging prediction",
    x = "x",
    y = "f(x)"
  ) +
  coord_cartesian(ylim = y.range)

{
samplepoint <- sample(testdata[, which(!out)], 1)
samplerow <- testdata[samplepoint]
# Fit Kriging model
km.model.2 <- km(
  design = data.frame(x = c(xvals, samplerow$x)),
  response = c(yvals, samplerow$y),
  covtype = "gauss",
  control = list(trace = FALSE)
)
# Predict across the range
pred.data.2 <- predict(
  km.model.2,
  newdata = data.frame(x = plot.data$x),
  type = "UK",
  se.compute = TRUE
)
testvals.2 <- predict(
  km.model.2,
  newdata = data.frame(x = testpoints),
  type = "UK",
  se.compute = TRUE
)
testdata.2 <- data.table(x = testpoints, y = testvals.2$mean, ymin = testvals.2$lower95, ymax = testvals.2$upper95, sd = testvals.2$sd)
testdata.2[, out := ymax < currash]
# Create plot with all elements
ggplot(plot.data, aes(x = x, y = y)) +
  # Original function
  geom_line(linetype = "dotted", linewidth = 1) +
  # Uncertainty ribbon (± 2 standard deviations)
  geom_ribbon(
    data = data.table(
      x = plot.data$x,
      ymin = pred.data.2$lower95,
      ymax = pred.data.2$upper95
    ),
    aes(x = x, y = NULL, ymin = ymin, ymax = ymax),
    fill = "blue",
    alpha = 0.2
  ) +
  # Kriging prediction
  geom_line(
    data = data.table(x = plot.data$x, y = pred.data.2$mean),
    color = "blue",
    linewidth = 1
  ) +
  geom_errorbar(
    data = testdata.2,
    aes(x = x, ymin = ymin, ymax = ymax, alpha = !out),
    color = "orange",
    width = 0.5
  ) +
  geom_point(
    data = testdata.2,
    color = "orange",
    size = 2
  ) +
    # Maximum horizontal lines
  geom_hline(yintercept = max(yvals), color = "red", linewidth = 1) +
  geom_hline(yintercept = max(yvals) * 0.85, color = "red",
             linetype = "dotted", linewidth = 1) +
  # Sample points
  geom_point(
    data = data.table(x = xvals, y = yvals),
    size = 3,
    color = "black"
  ) +
  geom_point(
    data = data.table(x = samplerow$x, y = samplerow$y),
    size = 3,
    color = "orange"
  ) +
  theme_minimal() +
  labs(
    title = "Plot of sin(x)/x with Kriging prediction",
    x = "x",
    y = "f(x)"
  ) +
  coord_cartesian(ylim = y.range)
}

