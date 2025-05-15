

plotOS <- function(os, variable = os$domain$ids()[[1]], samples = 1000) {
  assertClass(os, "ObjectiveStream")
  assertString(variable)
  assertChoice(variable, os$domain$ids())
  assertInt(samples, lower = 1)
  os <- os$clone(deep = TRUE)
  data <- os$sample(samples)
  column <- data[[variable]]
  yvals <- os$eval(data)
  plotdat <- data.table(column, yvals)[!is.na(column)]
  plot(plotdat$column, plotdat$yvals)
}

.trace.table <- "not initialized"
.has.header <- FALSE
.tracing.start <- NULL

resetTabulate <- function(filename) {
  dir.create("data/lseruns", showWarnings = FALSE, recursive = TRUE)
  .trace.table <<- paste0("data/lseruns/", filename)  # nolint
  .has.header <<- FALSE
  unlink(.trace.table)
  .tracing.start <<- Sys.time()  # nolint
}

# run this after each optim step
tabulateResultRow <- function(
  last.step.cost,
  # Number of points in the RS that were evaluated so far
  evaluated.points.in.rs,
  # Number of points that were eval'd that are currently assumed to be in the RS, based on known optimum. This is always
  # a superset of the above.
  evaluated.points.in.assumed.rs,
  # Number of points correctly predicted by the model to be in the RS
  model.TP,
  # Number of points incorrectly predicted by the model to be in the RS
  model.FP,
  # Number of points that are incorrectly predicted to not be in the RS
  model.FN,
  # Number of points that are correctly predicted to not be in the RS
  model.TN,
  # If the method has an 'H' set: how many of them are correctly predicted to be in the RS
  hset.correct,
  # ... and how many are incorrectly predicted to be in the RS
  hset.incorrect,
  # If the method has an 'L' set: how many of them are correctly predicted to not be in the RS
  lset.correct,
  # ... and how many are incorrectly predicted to not be in the RS
  lset.incorrect,
  # number of yet unclassified points
  uset.unclassified,

  # total number of points in candidate set
  total.points.modeled,
  # total number of true positive rashomon points
  total.points.in.rs,
  # number of points more than 3 sd away from the mean
  total.points.outlier,
  # max relative error of the model
  max.relative.model.error,
  # true optimum
  true.optimum,
  # observed optimum
  observed.optimum,
  # optimum according to model
  modeled.optimum
) {
  newrow <- data.table(  # nolint
    last.step.cost = last.step.cost,
    evaluated.points.in.rs = evaluated.points.in.rs,
    evaluated.points.in.assumed.rs = evaluated.points.in.assumed.rs,
    model.TP = model.TP,
    model.FP = model.FP,
    model.FN = model.FN,
    model.TN = model.TN,
    hset.correct = hset.correct,
    hset.incorrect = hset.incorrect,
    lset.correct = lset.correct,
    lset.incorrect = lset.incorrect,
    uset.unclassified = uset.unclassified,
    total.points.modeled = total.points.modeled,
    total.points.in.rs = total.points.in.rs,
    total.points.outlier = total.points.outlier,
    max.relative.model.error = max.relative.model.error,
    true.optimum = true.optimum,
    observed.optimum = observed.optimum,
    modeled.optimum = modeled.optimum,
    time = difftime(Sys.time(), .tracing.start, units = "secs")
  )
  write.table(newrow, .trace.table, append = TRUE, col.names = !.has.header, row.names = FALSE, sep = ",")
  .has.header <<- TRUE  # nolint
}


RashomonTracker <- R6Class("RashomonTracker",
  public = list(
    truedata = NULL,
    already.evaluated = 0,
    minimize = NULL,
    rashomon.epsilon = NULL,
    rashomon.is.relative = NULL,
    true.optimum = NULL,
    last.step.points = 0,
    initialize = function(stream, optimizer, pointslimit) {
      assertClass(stream, "ObjectiveStream")
      assertClass(optimizer, "RashomonSampler")

      self$minimize <- optimizer$minimize
      self$rashomon.epsilon <- optimizer$rashomon.epsilon
      self$rashomon.is.relative <- optimizer$rashomon.is.relative

      gt.stream <- stream$clone(deep = TRUE)
      truedata <- gt.stream$sample(min(pointslimit, gt.stream$remaining.rows))
      truedata[, .score := gt.stream$eval(truedata)]
      self$true.optimum <- if (self$minimize) min(truedata$.score) else max(truedata$.score)
      truedata[, .known := FALSE]
      truedata[, .iteration := NA_integer_]
      truedata[, .true.rashomon := self$inferInRashomon(.score, self$true.optimum)]
      truedata[, .predicted := NA_real_]
      truedata[, .predicted.sd := NA_real_]
      truedata[, .lse.set := "U"]
      truedata[, .is.in.M := FALSE]
      truedata[, .upper := NA_real_]
      truedata[, .lower := NA_real_]
      self$truedata <- truedata

      resetTabulate()
    },
    recordYAsked = function(y.asked, lastmodel, metainfo) {
      if (self$already.evaluated != 0) {
        # this block is skipped in the first iteration, when no points have been eval'd yet
        assumed.in.rs <- logical(nrow(self$truedata))
        assumed.in.rs[self$truedata$.known] <- self$inferInRashomon(self$truedata[.known == TRUE, .score])

        dropcols <- grep("^\\.", colnames(self$truedata), value = TRUE)
        tabdat <- self$truedata[, !dropcols, with = FALSE]

        pr <- lastmodel$predict_newdata(tabdat)
        self$truedata[, .predicted := pr$response]
        self$truedata[, .predicted.sd := pr$se]

        modeled.in.rs <- self$inferInRashomon(self$truedata$.predicted)

        if (!is.null(metainfo)) {
          metainfo <- metainfo[J(self$truedata$.id), on = ".id"]
          if (".is.in.M" %in% colnames(metainfo)) {
            self$truedata[, .is.in.M := metainfo$.is.in.M]
          }
          if (".lse.set" %in% colnames(metainfo)) {
            self$truedata[, .lse.set := metainfo$.lse.set]
          }
          if (".upper" %in% colnames(metainfo)) {
            self$truedata[, .upper := metainfo$.upper]
          }
          if (".lower" %in% colnames(metainfo)) {
            self$truedata[, .lower := metainfo$.lower]
          }
        }

        tabulateResultRow(
          last.step.cost = self$last.step.points,
          evaluated.points.in.rs = self$truedata[, sum(.true.rashomon & .known)],
          evaluated.points.in.assumed.rs = sum(assumed.in.rs),
          model.TP = sum(modeled.in.rs & self$truedata$.true.rashomon),
          model.FP = sum(modeled.in.rs & !self$truedata$.true.rashomon),
          model.FN = sum(!modeled.in.rs & self$truedata$.true.rashomon),
          model.TN = sum(!modeled.in.rs & !self$truedata$.true.rashomon),

          hset.correct = self$truedata[, sum(!.true.rashomon & .lse.set == "H")],
          hset.incorrect = self$truedata[, sum(.true.rashomon & .lse.set == "H")],
          lset.correct = self$truedata[, sum(.true.rashomon & .lse.set == "L")],
          lset.incorrect = self$truedata[, sum(!.true.rashomon & .lse.set == "L")],
          uset.unclassified = self$truedata[, sum(.lse.set == "U")],

          total.points.modeled = nrow(self$truedata),
          total.points.in.rs = sum(self$truedata$.true.rashomon),
          total.points.outlier = self$truedata[, sum(abs(.score - .predicted) > 3 * .predicted.sd)],
          max.relative.model.error = self$truedata[, max(abs(.score - .predicted) / .predicted.sd)],
          true.optimum = self$true.optimum,
          observed.optimum = self$truedata[.known == TRUE, if (self$minimize) min(.score) else max(.score)],
          modeled.optimum = self$truedata[, if (self$minimize) min(.predicted) else max(.predicted)]
        )
      }
      self$truedata[y.asked, .known := TRUE, on = ".id"]
      self$truedata[y.asked, .iteration := self$already.evaluated + 1, on = ".id"]
      self$already.evaluated <- self$already.evaluated + nrow(y.asked)
      self$last.step.points <- nrow(y.asked)
      invisible(NULL)
    },
    inferInRashomon = function(yvals, yopt = if (self$minimize) min(yvals) else max(yvals)) {
      assertNumeric(yvals, any.missing = FALSE, finite = TRUE, min.len = 1L)
      assertNumber(yopt, finite = TRUE)
      epsilon <- self$rashomon.epsilon
      if (!self$minimize) {
        yvals <- -yvals
        yopt <- -yopt
        if (self$rashomon.is.relative) {
          epsilon <- -epsilon
        }
      }
      if (self$rashomon.is.relative) {
        cutoff <- yopt * (1 + epsilon)
        if (cutoff < yopt) {
          # stop("rashomon.is.relative does not work for negative scores")
          cutoff <- 0
        }
      } else {
        cutoff <- yopt + epsilon
      }
      yvals <= cutoff
    },
    plot1D = function(variable, subset) {
      if (!missing(subset)) {
        subset.expr <- substitute(subset)
        subset.expr <- eval(subset.expr, self$truedata)
        assert(
          checkLogical(subset.expr, any.missing = FALSE, len = nrow(self$truedata)),
          checkIntegerish(subset.expr, any.missing = FALSE, lower = 1, upper = nrow(self$truedata), tol = 0)
        )
        if (is.logical(subset.expr)) {
          subset.expr <- which(subset.expr)
        }
        plotting <- self$truedata[subset.expr]
      } else {
        plotting <- self$truedata
      }

      true.opt <- self$true.optimum
      true.cutoff <- calculateCutoff(true.opt, self$rashomon.epsilon, self$minimize, self$rashomon.is.relative)
      assumed.opt <- if (self$minimize) min(self$truedata[.known == TRUE, .score]) else max(self$truedata[.known == TRUE, .score])
      assumed.cutoff <- calculateCutoff(assumed.opt, self$rashomon.epsilon, self$minimize, self$rashomon.is.relative)

      ggplot(plotting, aes_string(x = variable, y = ".score")) +
        geom_point(size = 0.1) +
        geom_point(data = plotting[.is.in.M == TRUE & .lse.set == "L"], color = "green", size = 4, shape = 8) +
        geom_point(data = plotting[.is.in.M == FALSE & .lse.set == "L"], color = "green", size = 4, shape = 7) +
        geom_point(data = plotting[.is.in.M == TRUE & .lse.set == "H"], color = "blue", size = 4, shape = 8) +
        geom_point(data = plotting[.is.in.M == FALSE & .lse.set == "H"], color = "blue", size = 4, shape = 7) +
        geom_point(aes(y = .predicted), color = "red", size = 1) +
        geom_point(data = plotting[.known == TRUE], color = "blue", size = 2) +
        theme_bw() +
        geom_hline(yintercept = true.opt, color = "black") +
        geom_hline(yintercept = assumed.opt, color = "black", linetype = "dashed") +
        geom_hline(yintercept = true.cutoff, color = "blue") +
        geom_hline(yintercept = assumed.cutoff, color = "blue", linetype = "dashed") +
        geom_ribbon(aes(ymin = .predicted - 3 *.predicted.sd, ymax = .predicted + 3 *.predicted.sd), alpha = 0.2) +
        geom_text(data = plotting[.known == TRUE], aes(label = .iteration), color = "red", size = 10, vjust = 1.5) +
        theme(aspect.ratio = 1)
    }
  )
)


calculateCutoff <- function(optimum, epsilon, minimize, relative) {
  if (relative) {
    if (minimize) {
      optimum * (1 + epsilon)
    } else {
      optimum * (1 - epsilon)
    }
  } else {
    if (minimize) {
      optimum + epsilon
    } else {
      optimum - epsilon
    }
  }
}
