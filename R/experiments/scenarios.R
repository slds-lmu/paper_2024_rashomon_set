
.all.precomputed.streams <- NULL

getAllStreams <- function(datasetid) {
  if (is.null(.all.precomputed.streams)) {
    dataset <- readRDS("data/allds.pivot.8k.rds")  # nolint
    ## can go when we have the new data
    dataset <- lapply(dataset, function(subds) {
      subds <- lapply(subds, function(learnerds) {
        learnerds[,
          c("bbrier", "bbrier.se", "rmse", "rmse.se", "is.in.grid", "is.grid",
            "N", "taskname") := NULL
        ]
        learnerds
      })
      subds$svm.radial[, c("svm.kernel", "config.svm.kernel") := NULL]
      subds$svm.linear[, c("svm.kernel", "config.svm.kernel") := NULL]
      subds
    })
    .all.precomputed.streams <<- dataset
  }
  assertChoice(datasetid, names(.all.precomputed.streams))
  .all.precomputed.streams[[datasetid]]
}

.list.learners.meta <- NULL

getLearnersMeta <- function() {
  if (is.null(.list.learners.meta)) {
    llm <- lapply(list.learners.regr, function(x) x$clone(deep = TRUE))
    llm$svm.linear <- llm$svm$clone(deep = TRUE)
    llm$svm.linear$param_set$set_values(svm.kernel = "linear", svm.degree = NULL, svm.gamma = NULL)
    llm$svm.radial <- llm$svm$clone(deep = TRUE)
    llm$svm.radial$param_set$set_values(svm.kernel = "radial", svm.degree = NULL)
    llm$svm <- NULL
    .list.learners.meta <<- llm  # nolint
  }
  .list.learners.meta
}

getOptimSpace <- function(learner.id) {
  llm <- getLearnersMeta()
  assertChoice(learner.id, names(llm))
  llm[[learner.id]]$param_set$search_space()
}


makeObjectiveStreamRecorded <- function(datasetid, learnerid, genseed, debug = FALSE) {
  assertString(datasetid)
  assertString(learnerid)
  assertInt(genseed, tol = 0)
  assertLogical(debug)
  ObjectiveStreamRecorded$new(
    id = sprintf("recorded_%s_%s_%s", datasetid, learnerid, genseed),
    domain = getOptimSpace(learnerid),
    minimize = TRUE,
    table = {
      set.seed(genseed)
      if (debug) {
        getAllStreams(datasetid)[[learnerid]][1:100]
      } else {
        getAllStreams(datasetid)[[learnerid]][sample.int(nrow(getAllStreams(datasetid)[[learnerid]]))]
      }
    },
    scorecol = "score"
  )
}

makeOSRConjoined <- function(datasetid, learners = names(getLearnersMeta()), ...) {
  osr <- sapply(learners, function(x) makeObjectiveStreamRecorded(datasetid, x, ...), simplify = FALSE)
  ObjectiveStreamConjoined$new(
    id = sprintf("conjoined_%s", datasetid),
    objective.streams = osr,
    sampling.strategy = "roundrobin",
    choice.param.name = "learner"
  )
}

getOSModel <- function(osr) {
  assertClass(osr, "ObjectiveStream")
  learner <- if (inherits(osr, "ObjectiveStreamConjoined")) {
    LearnerRegrKMExtraConjoined$new(osr$domain)
  } else {
    LearnerRegrKMExtra$new()
  }
  learner$param_set$values$nugget.estim <- TRUE
  learner$param_set$values$jitter <- 0.0001
  learner
}


optimizer.scenarios <- c(
  "random",  # random search
  "straddle",  # straddle heuristic
  "lse",  # naive level set estimation
  "truvar",  # naive TruVaR
  "maxsd",  # max SD acquisition function
  "lse.imp",  # level set est with implicit threshold
  "truvar.imp",  # TruVaRImp (ours, uwu)
  "optimize",  # bayesian optimization
  "lse.opt",  # bayesian opt to estimate threshold, followed by lse
  "truvar.opt",  # bayesian opt to estimate threshold, followed by truvar
  "maxsd.opt",  # bayesian opt to estimate threshold, followed by maxsd
  "random.opt"
)

makeScenarioRS <- function(scenario, stream, initial.sample.size, rashomon.epsilon, rashomon.is.relative, seed, pointslimit, optimize.length) {
  assertChoice(scenario, optimizer.scenarios)
  assertClass(stream, "ObjectiveStream")
  assertCount(initial.sample.size, tol = 0)
  assertNumber(rashomon.epsilon, lower = 0)
  assertFlag(rashomon.is.relative)
  assertInt(seed, tol = 0)
  assertCount(pointslimit, positive = TRUE, tol = 0)

  sampler.init <- RashomonSamplerRandom$new(
    id = sprintf("init_%s", stream$id),
    domain = stream$domain,
    minimize = stream$minimize,
    rashomon.epsilon = rashomon.epsilon,
    rashomon.is.relative = rashomon.is.relative,
    seed = seed,
    n.rashomon.samples = 999999,  # not yet used for anything useful
    initial.sample.size = initial.sample.size,
    batchsize = 1
  )

  sampler.main <- switch(scenario,
    random.opt = ,
    random = {
      rsr <- RashomonSamplerRandom$new(
        id = sprintf("rs_random_%s", stream$id),
        domain = stream$domain,
        minimize = stream$minimize,
        rashomon.epsilon = rashomon.epsilon,
        rashomon.is.relative = rashomon.is.relative,
        seed = seed,
        n.rashomon.samples = 999999,  # not yet used for anything useful
        initial.sample.size = initial.sample.size,
        batchsize = 1
      )
      RashomonSamplerLearnerWrapper$new(
        wrapped.sampler = rsr,
        learner = getOSModel(stream),
        seed = seed
      )
    },
    straddle = {
      RashomonSamplerStraddle$new(
        id = sprintf("rs_straddle_%s", stream$id),
        domain = stream$domain,
        minimize = stream$minimize,
        rashomon.epsilon = rashomon.epsilon,
        rashomon.is.relative = rashomon.is.relative,
        learner = getOSModel(stream),
        seed = seed,
        search.grid.size = min(pointslimit, stream$remaining.rows)
      )
    },
    lse.opt = ,
    lse = {
      RashomonSamplerLSEImplicit$new(
        id = sprintf("rs_lse_%s", stream$id),
        domain = stream$domain,
        minimize = stream$minimize,
        rashomon.epsilon = rashomon.epsilon,
        rashomon.is.relative = rashomon.is.relative,
        learner = getOSModel(stream),
        search.grid.size = min(pointslimit, stream$remaining.rows),
        seed = seed,
        lse.beta = 9,
        lse.epsilon = 0.0001,
        n.rashomon.samples = 999999,
        implicit.threshold.method = FALSE
      )
    },
    truvar.opt = ,
    truvar = {
      RashomonSamplerTruVaRImp$new(
        id = sprintf("rs_truvar_%s", stream$id),
        domain = stream$domain,
        minimize = stream$minimize,
        rashomon.epsilon = rashomon.epsilon,
        rashomon.is.relative = rashomon.is.relative,
        learner = getOSModel(stream),
        search.grid.size = min(pointslimit, stream$remaining.rows),
        seed = seed,
        beta = function(i, t, D) log(D * t^2),
        delta.bar = 0,
        r = 0.1,
        eta = 1,
        n.rashomon.samples = 999999,
        implicit.threshold.method = FALSE
      )
    },
    maxsd.opt = ,
    maxsd = {
      RashomonSamplerOptimize$new(
        id = sprintf("rs_maxsd_%s", stream$id),
        domain = stream$domain,
        minimize = stream$minimize,
        rashomon.epsilon = rashomon.epsilon,
        rashomon.is.relative = rashomon.is.relative,
        learner = getOSModel(stream),
        aqf = AqfSd(),
        search.grid.size = min(pointslimit, stream$remaining.rows),
        seed = seed
      )
    },
    lse.imp = {
      RashomonSamplerLSEImplicit$new(
        id = sprintf("rs_lse_%s", stream$id),
        domain = stream$domain,
        minimize = stream$minimize,
        rashomon.epsilon = rashomon.epsilon,
        rashomon.is.relative = rashomon.is.relative,
        learner = getOSModel(stream),
        search.grid.size = min(pointslimit, stream$remaining.rows),
        seed = seed,
        lse.beta = 9,
        lse.epsilon = 0.0001,
        n.rashomon.samples = 999999
      )
    },
    truvar.imp = {
      RashomonSamplerTruVaRImp$new(
        id = sprintf("rs_truvarimp_%s", stream$id),
        domain = stream$domain,
        minimize = stream$minimize,
        rashomon.epsilon = rashomon.epsilon,
        rashomon.is.relative = rashomon.is.relative,
        learner = getOSModel(stream),
        search.grid.size = min(pointslimit, stream$remaining.rows),
        seed = seed,
        beta = function(i, t, D) log(D * t^2),
        delta.bar = 0,
        r = 0.1,
        eta = 1,
        n.rashomon.samples = 999999
      )
    },
    optimize = {
      RashomonSamplerOptimize$new(
        id = sprintf("rs_optimize_%s", stream$id),
        domain = stream$domain,
        minimize = stream$minimize,
        rashomon.epsilon = rashomon.epsilon,
        rashomon.is.relative = rashomon.is.relative,
        learner = getOSModel(stream),
        aqf = AqfEi(),
        search.grid.size = min(pointslimit, stream$remaining.rows),
        seed = seed
      )
    },
    stop("Unknown scenario: ", scenario)
  )
  if (scenario %like% "\\.opt$") {
    optimizer <- RashomonSamplerOptimize$new(
      id = sprintf("rs_optimize_%s", stream$id),
      domain = stream$domain,
      minimize = stream$minimize,
      rashomon.epsilon = rashomon.epsilon,
      rashomon.is.relative = rashomon.is.relative,
      learner = getOSModel(stream),
      aqf = AqfEi(),
      search.grid.size = min(pointslimit, stream$remaining.rows),
      seed = seed
    )
    samplers <- list(sampler.init, optimizer, sampler.main)
    ask.y.each <- c(initial.sample.size, optimize.length, Inf)
  } else {
    samplers <- list(sampler.init, sampler.main)
    ask.y.each <- c(initial.sample.size, Inf)
  }
  RashomonSamplerChain$new(
    id = sprintf("chain_%s", stream$id),
    samplers = samplers,
    ask.y.each = ask.y.each,
    n.rashomon.samples = 999999
  )
}