


osGenerator <- function(data, job, dataset, learners, logscale, ...) {
  repl <- assertInt(job$repl, lower = 1, tol = 0)
  assertChoice(learners, c(names(getLearnersMeta()), "all", synthetic.objective.types))
  assertFlag(logscale)
  if (learners == "all") {
    makeOSRConjoined(datasetid = dataset, genseed = repl + 1000, logscale = logscale)
  } else if (learners %in% synthetic.objective.types) {
    makeObjectiveStreamSynthetic(type = learners, genseed = repl + 1000)
  } else {
    makeObjectiveStreamRecorded(datasetid = dataset, genseed = repl + 1000, learnerid = learners, logscale = logscale)
  }
}


runOptimize <- function(data, instance, job, optimizer, ..., initsize = 10, nrounds = 401, kerneltype = "matern5_2", epsilon = 0.05) {
  repl <- assertInt(job$repl, lower = 1, tol = 0)
  assertCount(nrounds, positive = TRUE, tol = 0)
  assertCount(initsize, positive = TRUE, tol = 0)
  ostream <- instance$clone(deep = TRUE)
  logscale <- job$pars$prob.pars$logscale
  assertFlag(logscale)
  pointslimit <- if (is.finite(instance$remaining.rows)) instance$remaining.rows else 8000
  rsampler <- makeScenarioRS(
    scenario = optimizer,
    stream = ostream,
    initial.sample.size = ifelse(job$pars$prob.pars$learners == "all", 6 * initsize, initsize),
    rashomon.epsilon = if (logscale) log(1 + epsilon) else epsilon,
    rashomon.is.relative = !logscale && !(job$pars$prob.pars$learners %like% "^gp"),
    seed = repl,
    pointslimit = pointslimit,
    optimize.length = 20,
    kerneltype = kerneltype
  )

  if (initsize != 10 || nrounds != 401 || kerneltype != "matern5_2" || epsilon != 0.05) {
    addendum <- sprintf("_initsize:%s_nrounds:%s_kernel:%s_epsilon:%s", initsize, nrounds, kerneltype, epsilon)
  } else {
    addendum <- ""
  }

  filename <- sprintf("log_smplr:%s_strm:%s_data:%s_log:%s_seed:%s%s.csv",
    optimizer,
    job$pars$prob.pars$learners,
    job$pars$prob.pars$dataset,
    logscale,
    repl,
    addendum
  )


  tracker <- RashomonTracker$new(ostream, rsampler, pointslimit, filename = filename)
  archive <- data.table()
  for (i in 1:nrounds) {
    x.asked <- rsampler$askXSamples()
    cat(sprintf("Asked %s points\n", x.asked))
    x.answered <- ostream$sample(x.asked)
    cat("Answered:\n")
    print(x.answered, topn = 1, trunc.cols = TRUE)
    rsampler$tellXSamples(x.answered)
    iserror <- tryCatch({

      y.asked <- rsampler$askYValues()
      ## here we have the model fitted for the last told result
      lastmodel <- rsampler$samplers[[rsampler$sampler.index]]$lastmodel
      tracker$recordYAsked(y.asked, lastmodel, rsampler$samplers[[rsampler$sampler.index]]$metainfo)
      FALSE
    }, error = function(e) {
      conditionMessage(e)
    })
    if (!isFALSE(iserror)) {
      break
    }
    cat(sprintf("Asked %s points\n", nrow(y.asked)))
    y.answered <- ostream$eval(y.asked)

    cat("Answered:\n")
    str(y.answered)

    fullresult <- cbind(y.asked, score = y.answered)
    rsampler$tellYValues(fullresult, scorecol = "score")
    archive <- rbind(archive, fullresult)
  }
  list(file = filename, error = if (isFALSE(iserror)) NA_character_ else iserror, archive = archive)
}