


osGenerator <- function(data, job, dataset, learners, logscale, ...) {
  repl <- assertInt(job$repl, lower = 1, tol = 0)
  assertChoice(learners, c(names(getLearnersMeta()), "all"))
  assertFlag(logscale)
  if (learners == "all") {
    makeOSRConjoined(datasetid = dataset, genseed = repl + 1000, logscale = logscale)
  } else {
    makeObjectiveStreamRecorded(datasetid = dataset, genseed = repl + 1000, learnerid = learners, logscale = logscale)
  }
}


runOptimize <- function(data, instance, job, optimizer, ...) {
  repl <- assertInt(job$repl, lower = 1, tol = 0)
  ostream <- instance$clone(deep = TRUE)
  logscale <- job$pars$prob.pars$logscale
  assertFlag(logscale)
  rsampler <- makeScenarioRS(
    scenario = optimizer,
    stream = ostream,
    initial.sample.size = ifelse(job$pars$prob.pars$learners == "all", 60, 10),
    rashomon.epsilon = if (logscale) log(1.05) else 0.05,
    rashomon.is.relative = !logscale,
    seed = repl,
    pointslimit = instance$remaining.rows,
    optimize.length = 20
  )


  filename <- sprintf("log_smplr:%s_strm:%s_data:%s_log:%s_seed:%s.csv",
    optimizer,
    job$pars$prob.pars$learners,
    job$pars$prob.pars$dataset,
    logscale,
    repl
  )


  tracker <- RashomonTracker$new(ostream, rsampler, instance$remaining.rows, filename = filename)

  for (i in 1:401) {
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
  }
  list(file = filename, error = if (isFALSE(iserror)) NA_character_ else iserror)
}