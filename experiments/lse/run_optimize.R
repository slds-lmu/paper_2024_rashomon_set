


osGenerator <- function(data, job, dataset, learners, ...) {
  repl <- assertInt(job$repl, lower = 1, tol = 0)
  assertChoice(learners, c(names(getLearnersMeta()), "all"))
  if (learners == "all") {
    makeOSRConjoined(datasetid = dataset, genseed = repl + 1000)
  } else {
    makeObjectiveStreamRecorded(datasetid = dataset, genseed = repl + 1000, learnerid = learners)
  }
}


runOptimize <- function(data, instance, job, optimizer, logscale, ...) {
  repl <- assertInt(job$repl, lower = 1, tol = 0)
  assertFlag(logscale)
  makeScenarioRS(
    scenario = optimizer,
    stream = instance,
    initial.sample.size = ifelse(job$pars$prob.pars$learners == "all", 100, 30),
    rashomon.epsilon = if (logscale) log(1.05) else 0.05,
    rashomon.is.relative = !logscale,
    seed = repl,
    pointslimit = instance$remaining.rows,
    optimize.length = 20
  )

  filename <- sprintf("log_smplr:%s_strm:%s_data:%s_log:%s_seed:%s.rds",
    optimizer,
    job$pars$prob.pars$learners,
    instance$prob.pars$dataset,
    logscale,
    repl
  )

  tracker <- RashomonTracker$new(rsampler, optimizer, instance$remaining.rows, filename = filename)

  for (i in 1:500) {
    x.asked <- optimizer$askXSamples()
    cat(sprintf("Asked %s points\n", x.asked))
    x.answered <- rsampler$sample(x.asked)
    cat("Answered:\n")
    print(x.answered, topn = 1, trunc.cols = TRUE)
    optimizer$tellXSamples(x.answered)
    y.asked <- optimizer$askYValues()
    ## here we have the model fitted for the last told result
    lastmodel <- optimizer$samplers[[optimizer$sampler.index]]$lastmodel
    iserror <- tryCatch({
      tracker$recordYAsked(y.asked, lastmodel, optimizer$samplers[[optimizer$sampler.index]]$metainfo)
      FALSE
    }, error = function(e) {
      conditionMessage(e)
    })
    if (!isFALSE(iserror)) {
      break
    }
    cat(sprintf("Asked %s points\n", nrow(y.asked)))
    y.answered <- rsampler$eval(y.asked)
    cat("Answered:\n")
    str(y.answered)

    fullresult <- cbind(y.asked, score = y.answered)
    optimizer$tellYValues(fullresult, scorecol = "score")
  }
  data.table(file = filename, error = if (isFALSE(iserror)) NA_character_ else iserror)
}