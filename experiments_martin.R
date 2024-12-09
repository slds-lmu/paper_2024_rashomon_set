
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
