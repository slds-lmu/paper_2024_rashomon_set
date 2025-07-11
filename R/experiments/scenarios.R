
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
    .all.precomputed.streams <<- dataset  # nolint
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


makeObjectiveStreamRecorded <- function(datasetid, learnerid, genseed, logscale, debug = FALSE) {
  assertString(datasetid)
  assertString(learnerid)
  assertInt(genseed, tol = 0)
  assertLogical(debug)
  set.seed(genseed)
  if (debug) {
    table <- getAllStreams(datasetid)[[learnerid]][1:100]
  } else {
    table <- getAllStreams(datasetid)[[learnerid]][sample.int(nrow(getAllStreams(datasetid)[[learnerid]]))]
  }
  if (logscale) {
    table[, score := log(score)]
  }
  ObjectiveStreamRecorded$new(
    id = sprintf("recorded_%s_%s_%s%s", datasetid, learnerid, genseed, if (logscale) "_log" else ""),
    domain = getOptimSpace(learnerid),
    minimize = TRUE,
    table = table,
    scorecol = "score"
  )
}

hartmann6 <- function(xv) {
  assertNumeric(xv, len = 6, any.missing = FALSE, finite = TRUE)
  alpha <- c(1.0, 1.2, 3.0, 3.2)

  A <- matrix(c(
    10,    3, 17,  3.5, 1.7,  8,
     0.05,10, 17,  0.1, 8,   14,
     3,   3.5,1.7,10,   17,   8,
    17,    8, 0.05,10,  0.1, 14
  ), 4, byrow = TRUE)

  P <- 1e-4 * matrix(c(
    1312, 1696, 5569,  124, 8283, 5886,
    2329, 4135, 8307, 3736, 1004, 9991,
    2348, 1451, 3522, 2883, 3047, 6650,
    4047, 8828, 8732, 5743, 1091,  381
  ), 4, byrow = TRUE)

  diff <- sweep(P, 2, xv, FUN = "-")            # 4 x 6 matrix of (xj - Pij)
  -sum(alpha * exp(-rowSums(A * diff^2)))       # objective value (to be minimised)
}

synthetic.objective.types <- c("gp2", "gp3", "quadratic2", "quadratic3", "branin", "hartmann6")

makeObjectiveStreamSynthetic <- function(type, genseed) {
  assertChoice(type, synthetic.objective.types)
  assertInt(genseed, tol = 0)
  if (type %like% "gp") {
    ObjectiveStreamGP$new(
      lengthscales = if (type == "gp2") c(0.2, 0.2) else c(0.2, 0.2, 0.2),
      noise = 0,
      id = sprintf("%s_%s", type, genseed),
      seed = c(genseed, genseed * 1001),
      kernel = "se"
    )
  } else if (type %like% "quadratic") {
    ObjectiveStreamSynthetic$new(
      objective = function(x) {
        sum(unlist(x)^2) + 0.02
      },
      id = sprintf("%s_%s", type, genseed),
      domain = ps_replicate(ps(x = p_dbl(-1, 1)), if (type == "quadratic2") 2 else 3),
      minimize = TRUE,
      seed = c(genseed, genseed * 1001)
    )
  } else if (type == "branin") {
    ObjectiveStreamSynthetic$new(
      objective = function(x) {
        (x$x2 - 5.1 * x$x1^2 / (4 * pi^2) + 5 * x$x1 / pi - 6)^2 +
          10 * (1 - 1 / (8 * pi)) * cos(x$x1) + 10
      },
      id = sprintf("%s_%s", type, genseed),
      domain = ps(x1 = p_dbl(-5, 10), x2 = p_dbl(0, 15)),
      minimize = TRUE,
      seed = c(genseed, genseed * 1001)
    )
  } else if (type == "hartmann6") {
    ObjectiveStreamSynthetic$new(
      objective = function(x) {
        hartmann6(unlist(x)) + 4
      },
      id = sprintf("%s_%s", type, genseed),
      domain = ps_replicate(ps(x = p_dbl(0, 1)), 6),
      minimize = TRUE,
      seed = c(genseed, genseed * 1001)
    )
  } else {
    stop(sprintf("Unknown synthetic objective type: %s", type))
  }
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

getOSModel <- function(osr, kerneltype = "matern5_2") {
  assertClass(osr, "ObjectiveStream")
  learner <- if (inherits(osr, "ObjectiveStreamConjoined")) {
    LearnerRegrKMExtraConjoined$new(osr$domain)
  } else {
    LearnerRegrKMExtra$new()
  }
  learner$param_set$values$nugget.estim <- TRUE
  learner$param_set$values$jitter <- 0.0001
  learner$param_set$values$covtype <- kerneltype
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
  "random.opt",
  "truvar.imp.eta2",
  "truvar.imp.r5",
  "truvar.imp.beta2",
  "truvar.imp.beta5",
  "truvar.imp.beta.const3",
  "truvar.imp.beta.const5",
  "truvar.imp.delta1",
  "truvar.imp.delta3",
  "truvar.imp.global",
  "truvar.imp.nonmonotonic",
  "truvar.imp.global.nonmonotonic",
  "truvar.imp.gauss",
  "truvar.imp.exp",
  "truvar.imp.powexp",
  "truvar.imp.exp.opt"
)

makeScenarioRS <- function(scenario, stream, initial.sample.size, rashomon.epsilon, rashomon.is.relative, seed,
    pointslimit, optimize.length, kerneltype = "matern5_2") {
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
        learner = getOSModel(stream, kerneltype = kerneltype),
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
        learner = getOSModel(stream, kerneltype = kerneltype),
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
        learner = getOSModel(stream, kerneltype = kerneltype),
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
        learner = getOSModel(stream, kerneltype = kerneltype),
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
        learner = getOSModel(stream, kerneltype = kerneltype),
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
        learner = getOSModel(stream, kerneltype = kerneltype),
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
        learner = getOSModel(stream, kerneltype = kerneltype),
        search.grid.size = min(pointslimit, stream$remaining.rows),
        seed = seed,
        beta = function(i, t, D) log(D * t^2),
        delta.bar = 0,
        r = 0.1,
        eta = 1,
        n.rashomon.samples = 999999
      )
    },
    truvar.imp.gauss = {
      RashomonSamplerTruVaRImp$new(
        id = sprintf("rs_truvarimp_gauss_%s", stream$id),
        domain = stream$domain,
        minimize = stream$minimize,
        rashomon.epsilon = rashomon.epsilon,
        rashomon.is.relative = rashomon.is.relative,
        learner = {
          learner <- getOSModel(stream, kerneltype = kerneltype)
          learner$param_set$set_values(covtype = "gauss")
          learner
        },
        search.grid.size = min(pointslimit, stream$remaining.rows),
        seed = seed,
        beta = function(i, t, D) log(D * t^2),
        delta.bar = 0,
        r = 0.1,
        eta = 1,
        n.rashomon.samples = 999999
      )
    },
    truvar.imp.exp = {
      RashomonSamplerTruVaRImp$new(
        id = sprintf("rs_truvarimp_exp_%s", stream$id),
        domain = stream$domain,
        minimize = stream$minimize,
        rashomon.epsilon = rashomon.epsilon,
        rashomon.is.relative = rashomon.is.relative,
        learner = {
          learner <- getOSModel(stream, kerneltype = kerneltype)
          learner$param_set$set_values(covtype = "exp")
          learner
        },
        search.grid.size = min(pointslimit, stream$remaining.rows),
        seed = seed,
        beta = function(i, t, D) log(D * t^2),
        delta.bar = 0,
        r = 0.1,
        eta = 1,
        n.rashomon.samples = 999999
      )
    },
    truvar.imp.powexp = {
      RashomonSamplerTruVaRImp$new(
        id = sprintf("rs_truvarimp_powexp_%s", stream$id),
        domain = stream$domain,
        minimize = stream$minimize,
        rashomon.epsilon = rashomon.epsilon,
        rashomon.is.relative = rashomon.is.relative,
        learner = {
          learner <- getOSModel(stream, kerneltype = kerneltype)
          learner$param_set$set_values(covtype = "powexp")
          learner
        },
        search.grid.size = min(pointslimit, stream$remaining.rows),
        seed = seed,
        beta = function(i, t, D) log(D * t^2),
        delta.bar = 0,
        r = 0.1,
        eta = 1,
        n.rashomon.samples = 999999
      )
    },
    truvar.imp.exp.opt = {
      RashomonSamplerTruVaRImp$new(
        id = sprintf("rs_truvarimp_exp_%s", stream$id),
        domain = stream$domain,
        minimize = stream$minimize,
        rashomon.epsilon = rashomon.epsilon,
        rashomon.is.relative = rashomon.is.relative,
        learner = {
          learner <- getOSModel(stream, kerneltype = kerneltype)
          learner$param_set$set_values(covtype = "exp")
          learner
        },
        search.grid.size = min(pointslimit, stream$remaining.rows),
        seed = seed,
        beta = function(i, t, D) log(D * t^2),
        delta.bar = 0,
        r = 0.1,
        eta = 1,
        n.rashomon.samples = 999999
      )
    },
    truvar.imp.global = {
      RashomonSamplerTruVaRImp$new(
        id = sprintf("rs_truvarimp_global_%s", stream$id),
        domain = stream$domain,
        minimize = stream$minimize,
        rashomon.epsilon = rashomon.epsilon,
        rashomon.is.relative = rashomon.is.relative,
        learner = getOSModel(stream, kerneltype = kerneltype),
        search.grid.size = min(pointslimit, stream$remaining.rows),
        seed = seed,
        beta = function(i, t, D) log(D * t^2),
        delta.bar = 0,
        r = 0.1,
        eta = 1,
        n.rashomon.samples = 999999,
        global.choice = TRUE
      )
    },
    truvar.imp.nonmonotonic = {
      RashomonSamplerTruVaRImp$new(
        id = sprintf("rs_truvarimp_nonmonotonic_%s", stream$id),
        domain = stream$domain,
        minimize = stream$minimize,
        rashomon.epsilon = rashomon.epsilon,
        rashomon.is.relative = rashomon.is.relative,
        learner = getOSModel(stream, kerneltype = kerneltype),
        search.grid.size = min(pointslimit, stream$remaining.rows),
        seed = seed,
        beta = function(i, t, D) log(D * t^2),
        delta.bar = 0,
        r = 0.1,
        eta = 1,
        n.rashomon.samples = 999999,
        monotonic.sets = FALSE
      )
    },
    truvar.imp.global.nonmonotonic = {
      RashomonSamplerTruVaRImp$new(
        id = sprintf("rs_truvarimp_global_nonmonotonic_%s", stream$id),
        domain = stream$domain,
        minimize = stream$minimize,
        rashomon.epsilon = rashomon.epsilon,
        rashomon.is.relative = rashomon.is.relative,
        learner = getOSModel(stream, kerneltype = kerneltype),
        search.grid.size = min(pointslimit, stream$remaining.rows),
        seed = seed,
        beta = function(i, t, D) log(D * t^2),
        delta.bar = 0,
        r = 0.1,
        eta = 1,
        n.rashomon.samples = 999999,
        global.choice = TRUE,
        monotonic.sets = FALSE
      )
    },

    truvar.imp.eta2 = {
      RashomonSamplerTruVaRImp$new(
        id = sprintf("rs_truvarimp_%s", stream$id),
        domain = stream$domain,
        minimize = stream$minimize,
        rashomon.epsilon = rashomon.epsilon,
        rashomon.is.relative = rashomon.is.relative,
        learner = getOSModel(stream, kerneltype = kerneltype),
        search.grid.size = min(pointslimit, stream$remaining.rows),
        seed = seed,
        beta = function(i, t, D) log(D * t^2),
        delta.bar = 0,
        r = 0.1,
        eta = 2,
        n.rashomon.samples = 999999
      )
    },
    truvar.imp.r5 = {
      RashomonSamplerTruVaRImp$new(
        id = sprintf("rs_truvarimp_%s", stream$id),
        domain = stream$domain,
        minimize = stream$minimize,
        rashomon.epsilon = rashomon.epsilon,
        rashomon.is.relative = rashomon.is.relative,
        learner = getOSModel(stream, kerneltype = kerneltype),
        search.grid.size = min(pointslimit, stream$remaining.rows),
        seed = seed,
        beta = function(i, t, D) log(D * t^2),
        delta.bar = 0,
        r = 0.5,
        eta = 1,
        n.rashomon.samples = 999999
      )
    },
    truvar.imp.beta2 = {
      RashomonSamplerTruVaRImp$new(
        id = sprintf("rs_truvarimp_%s", stream$id),
        domain = stream$domain,
        minimize = stream$minimize,
        rashomon.epsilon = rashomon.epsilon,
        rashomon.is.relative = rashomon.is.relative,
        learner = getOSModel(stream, kerneltype = kerneltype),
        search.grid.size = min(pointslimit, stream$remaining.rows),
        seed = seed,
        beta = function(i, t, D) 2 * log(D * t^2),
        delta.bar = 0,
        r = 0.5,
        eta = 1,
        n.rashomon.samples = 999999
      )
    },
    truvar.imp.beta5 = {
      RashomonSamplerTruVaRImp$new(
        id = sprintf("rs_truvarimp_%s", stream$id),
        domain = stream$domain,
        minimize = stream$minimize,
        rashomon.epsilon = rashomon.epsilon,
        rashomon.is.relative = rashomon.is.relative,
        learner = getOSModel(stream, kerneltype = kerneltype),
        search.grid.size = min(pointslimit, stream$remaining.rows),
        seed = seed,
        beta = function(i, t, D) 5 * log(D * t^2),
        delta.bar = 0,
        r = 0.5,
        eta = 1,
        n.rashomon.samples = 999999
      )
    },
    truvar.imp.beta.const3 = {
      RashomonSamplerTruVaRImp$new(
        id = sprintf("rs_truvarimp_%s", stream$id),
        domain = stream$domain,
        minimize = stream$minimize,
        rashomon.epsilon = rashomon.epsilon,
        rashomon.is.relative = rashomon.is.relative,
        learner = getOSModel(stream, kerneltype = kerneltype),
        search.grid.size = min(pointslimit, stream$remaining.rows),
        seed = seed,
        beta = 3,
        delta.bar = 0,
        r = 0.5,
        eta = 1,
        n.rashomon.samples = 999999
      )
    },
    truvar.imp.beta.const5 = {
      RashomonSamplerTruVaRImp$new(
        id = sprintf("rs_truvarimp_%s", stream$id),
        domain = stream$domain,
        minimize = stream$minimize,
        rashomon.epsilon = rashomon.epsilon,
        rashomon.is.relative = rashomon.is.relative,
        learner = getOSModel(stream, kerneltype = kerneltype),
        search.grid.size = min(pointslimit, stream$remaining.rows),
        seed = seed,
        beta = 5,
        delta.bar = 0,
        r = 0.5,
        eta = 1,
        n.rashomon.samples = 999999
      )
    },
    truvar.imp.delta1 = {
      RashomonSamplerTruVaRImp$new(
        id = sprintf("rs_truvarimp_%s", stream$id),
        domain = stream$domain,
        minimize = stream$minimize,
        rashomon.epsilon = rashomon.epsilon,
        rashomon.is.relative = rashomon.is.relative,
        learner = getOSModel(stream, kerneltype = kerneltype),
        search.grid.size = min(pointslimit, stream$remaining.rows),
        seed = seed,
        beta = function(i, t, D) log(D * t^2),
        delta.bar = 0.1,
        r = 0.1,
        eta = 1,
        n.rashomon.samples = 999999
      )
    },
    truvar.imp.delta3 = {
      RashomonSamplerTruVaRImp$new(
        id = sprintf("rs_truvarimp_%s", stream$id),
        domain = stream$domain,
        minimize = stream$minimize,
        rashomon.epsilon = rashomon.epsilon,
        rashomon.is.relative = rashomon.is.relative,
        learner = getOSModel(stream, kerneltype = kerneltype),
        search.grid.size = min(pointslimit, stream$remaining.rows),
        seed = seed,
        beta = function(i, t, D) log(D * t^2),
        delta.bar = 0.3,
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
        learner = getOSModel(stream, kerneltype = kerneltype),
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
      learner = getOSModel(stream, kerneltype = kerneltype),
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