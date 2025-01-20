
source("init.R")

lr <- lrn("regr.km", predict_type = "se")
lr$encapsulate("try", lrn("regr.featureless", predict_type = "se"))

initsize <- 10

func <- function(x) sin(1.5 * sin(x) / x)
otest <- ObjectiveStreamSynthetic$new(
  function(x) func(x$x),
  id = "test",
  domain = ps(x = p_dbl(lower = -15, upper = 15)),
  minimize = FALSE,
  seed = c(1, 2)
)

evaluator <- RashomonSamplerOptimize$new(
  id = "test",
  domain = otest$domain,
  minimize = otest$minimize,
  learner = lr,
  aqf = AqfLcb(1),
  search.grid.size = 30,
  seed = 1
)

ays <- evaluator$askXSamples()

smp <- otest$sample(min(ays, initsize))

smp$.score <- otest$eval(smp)

evaluator$tellXSamples(smp, scorecol = ".score")

ays <- evaluator$askXSamples()

smp <- otest$sample(ays)
evaluator$tellXSamples(smp)

# -- repeat the following until the search grid is exhausted

req <- evaluator$askYValues()

req$.score <- otest$eval(req)

evaluator$tellYValues(req)

grid <- seq(otest$domain$lower['x'], otest$domain$upper['x'], length.out = 1000)


# -- plot progress
plot(grid, sapply(grid, func), type = "l")
ds <- evaluator$.__enclos_env__$private$.search.grid[!is.na(.score), ]
points(.score ~ x, ds, col = ifelse(ds$.id == req$.id, "red", "black"))
abline(v = ds[.id == req$.id, x])
points(evaluator$.__enclos_env__$private$.search.grid[, .(x, -.2)])
