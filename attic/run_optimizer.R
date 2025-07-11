
rm(list = ls(all.names = TRUE))
source("init.R")

getLearnersMeta()


getOptimSpace("svm.radial")

makeObjectiveStreamRecorded("st", "svm.radial", 1, FALSE, debug = TRUE)


debug.streams <- list(
  svm.radial = ObjectiveStreamRecorded$new(
    id = "test1",
    domain = getOptimSpace("svm.radial"),
    minimize = TRUE,
    table = getAllStreams("st")$svm.radial[1:10],
    scorecol = "score"
  ),
  svm.linear = ObjectiveStreamRecorded$new(
    id = "test2",
    domain = getOptimSpace("svm.linear"),
    minimize = TRUE,
    table = getAllStreams("st")$svm.linear[1:20],
    scorecol = "score"
  )
)

debugosc <- ObjectiveStreamConjoined$new(
  id = "test3",
  objective.streams = debug.streams,
  sampling.strategy = "roundrobin",
  choice.param.name = "learner"
)




debugosc <- makeOSRConjoined("st", learners = c("svm.radial", "svm.linear"), logscale = FALSE, genseed = 1, debug = TRUE)

plotOS(debugosc, "svm.radial.svm.cost", 50)

names(debugosc)

sampled <- debugosc$sample(10)
sampled2 <- debugosc$sample(4)

debugonce(debugosc$.__enclos_env__$private$.sample)


osrmodel <- getOSModel(debugosc)


osrmodel




rsr <- RashomonSamplerRandom$new(
  id = "optim",
  domain = debugosc$domain,
  minimize = debugosc$minimize,
  0.05,
  TRUE,
  1,
  1000
)

names(rsr)

## -----------------------------------------

ossquare <- ObjectiveStreamSynthetic$new(
  objective = function(x) {
    0.02 + x$x ^ 2  # add some offset to 0
  },
  id = "xsquared",
  domain = ps(x = p_dbl(lower = -10, upper = 10)),
  minimize = TRUE,
  seed = c(1, 2)
)

osgauss <- ObjectiveStreamSynthetic$new(
  objective = function(x) {
    exp(- x$x ^ 2 / 2)
  },
  id = "gauss",
  domain = ps(x = p_dbl(lower = -5, upper = 5)),
  minimize = FALSE,
  seed = c(1, 2)
)


library("ggplot2")

plotOS(osgauss, "x", 100)

{
pointslimit <- 800
# problem <- osgauss$clone(deep = TRUE)
# optim <- makeScenarioRS("truvar", problem, 5, 0.3, TRUE, 1, pointslimit, optimize.length = 4)

problem <- ossquare$clone(deep = TRUE)
# optim <- makeScenarioRS("truvar.imp", problem, 5, 20, FALSE, 1, pointslimit, optimize.length = 4)

# problem <- makeOSRConjoined("st", logscale = FALSE, genseed = 1)
#problem <- makeObjectiveStreamRecorded("st", "svm.radial", 1, FALSE, debug = FALSE)
# optim <- makeScenarioRS("truvar.imp", problem, 100, 0.05, TRUE, 1, problem$remaining.rows, optimize.length = 4)
optim <- makeScenarioRS("truvar.imp", problem, 5, 0.05, TRUE, 1, pointslimit, optimize.length = 4)

### optimization loop
optimizer <- optim$clone(deep = TRUE)
# rsampler <- ossquare$clone(deep = TRUE)
rsampler <- problem$clone(deep = TRUE)

# TODO need jitter = TRUE

tracker <- RashomonTracker$new(rsampler, optimizer, pointslimit, filename = "DEBUGRUN")


# repeat until out of points
x.asked <- optimizer$askXSamples()
cat(sprintf("Asked %s points\n", x.asked))
x.answered <- rsampler$sample(x.asked)
cat("Answered:\n")
print(x.answered, topn = 1, trunc.cols = TRUE)
optimizer$tellXSamples(x.answered)
y.asked <- optimizer$askYValues()
## here we have the model fitted for the last told result
lastmodel <- optimizer$samplers[[optimizer$sampler.index]]$lastmodel
tracker$recordYAsked(y.asked, lastmodel, optimizer$samplers[[optimizer$sampler.index]]$metainfo)
}

### --------- repeat from here
repeat {
cat(sprintf("Asked %s points\n", nrow(y.asked)))
y.answered <- rsampler$eval(y.asked)
cat("Answered:\n")
str(y.answered)

fullresult <- cbind(y.asked, score = y.answered)
optimizer$tellYValues(fullresult, scorecol = "score")
## -- this part is duplicated
x.asked <- optimizer$askXSamples()
cat(sprintf("Asked %s points\n", x.asked))
x.answered <- rsampler$sample(x.asked)
cat("Answered:\n")
print(x.answered, topn = 1, trunc.cols = TRUE)
optimizer$tellXSamples(x.answered)
y.asked <- optimizer$askYValues()
## here we have the model fitted for the last told result
lastmodel <- optimizer$samplers[[optimizer$sampler.index]]$lastmodel
tracker$recordYAsked(y.asked, lastmodel, optimizer$samplers[[optimizer$sampler.index]]$metainfo)
# ggsave("data/debugplot.pdf", tracker$plot2D("svm.cost", "svm.gamma"))
ggsave("data/debugplot.pdf", tracker$plot1D("x"))
readline("Press Enter to continue")
}

tracker$plot1D("x")

.trace.table

lastmetainfo <- optimizer$samplers[[optimizer$sampler.index]]$metainfo

optimizer$samplers[[optimizer$sampler.index]]$epoch

optimizer$samplers[[optimizer$sampler.index]]$.__enclos_env__$private$.metainfo
optimizer$samplers[[optimizer$sampler.index]]$.__enclos_env__$private$.search.grid

debugonce(optimizer$samplers[[optimizer$sampler.index]]$.__enclos_env__$private$.askYValuesWithLearner)
debugonce(optimizer$samplers[[optimizer$sampler.index + 1]]$.__enclos_env__$private$.askYValuesWithLearner)

optimizer$samplers[[optimizer$sampler.index]]$.__enclos_env__$private$.M


optimizer$samplers[[optimizer$sampler.index]]$.__enclos_env__$private$.beta



