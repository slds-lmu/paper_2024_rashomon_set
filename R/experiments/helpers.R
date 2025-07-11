

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
  .has.header <<- FALSE  # nolint
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
    initialize = function(stream, optimizer, pointslimit, filename) {
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

      resetTabulate(filename)
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
    # subset: index or logical selecting rows of truedata
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
      if (self$minimize) {
        assumed.opt <- min(self$truedata[.known == TRUE, .score])
      } else {
        assumed.opt <- max(self$truedata[.known == TRUE, .score])
      }
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
    },
    plot2D = function(variable1, variable2, subset) {
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
      if (self$minimize) {
        assumed.opt <- min(self$truedata[.known == TRUE, .score])
      } else {
        assumed.opt <- max(self$truedata[.known == TRUE, .score])
      }
      assumed.cutoff <- calculateCutoff(assumed.opt, self$rashomon.epsilon, self$minimize, self$rashomon.is.relative)

      # Remove rows with missing values for plotting variables
      plotting <- plotting[!is.na(get(variable1)) & !is.na(get(variable2))]

      # Create data for the four different surfaces
      plot.data.list <- list(
        "True Score" = plotting[, c(variable1, variable2, ".score", ".is.in.M", ".lse.set", ".known", ".iteration"), with = FALSE],
        "Predicted" = plotting[!is.na(.predicted), c(variable1, variable2, ".predicted", ".is.in.M", ".lse.set", ".known", ".iteration"), with = FALSE],
        "Lower Bound" = plotting[!is.na(.predicted) & !is.na(.predicted.sd),
                                c(variable1, variable2, ".is.in.M", ".lse.set", ".known", ".iteration"), with = FALSE],
        "Upper Bound" = plotting[!is.na(.predicted) & !is.na(.predicted.sd),
                                c(variable1, variable2, ".is.in.M", ".lse.set", ".known", ".iteration"), with = FALSE]
      )

      # Add computed z values
      names(plot.data.list[[1]])[3] <- "z"
      names(plot.data.list[[2]])[3] <- "z"
      plot.data.list[[3]][, z := plotting[!is.na(.predicted) & !is.na(.predicted.sd), .predicted - 3 * .predicted.sd]]
      plot.data.list[[4]][, z := plotting[!is.na(.predicted) & !is.na(.predicted.sd), .predicted + 3 * .predicted.sd]]

      # Add panel identifier
      for (i in seq_along(plot.data.list)) {
        plot.data.list[[i]][, panel := names(plot.data.list)[i]]
      }

      # Combine all data
      combined.data <- rbindlist(plot.data.list, fill = TRUE)
      combined.data[, panel := factor(panel, levels = names(plot.data.list))]

      # Define reference levels for each panel
      ref.levels <- list(
        "True Score" = c(true.opt, assumed.opt, true.cutoff, assumed.cutoff),
        "Predicted" = c(true.opt, assumed.opt, true.cutoff, assumed.cutoff),
        "Lower Bound" = c(true.opt, assumed.opt, true.cutoff, assumed.cutoff),
        "Upper Bound" = c(true.opt, assumed.opt, true.cutoff, assumed.cutoff)
      )
      ctr <- combined.data[,
        {
          itp <- interp::interp(svm.cost, svm.gamma, z, method = "linear", duplicate = "strip", nx = 200, ny = 200)
          cbind(CJ(var2 = itp$x, var1 = itp$y), z = as.vector(itp$z))
        },
        by = panel
      ]
      setnames(ctr, c("var1", "var2"), c(variable1, variable2))

      # Create base plot
      p <- ggplot(combined.data, aes_string(x = variable1, y = variable2, z = "z")) +
        geom_contour_filled(data = ctr, alpha = 0.7, bins = 15) +
        geom_contour(data = ctr, color = "white", alpha = 0.5, bins = 15, size = 0.2) +
        facet_wrap(~panel, scales = "free", ncol = 2) +
        theme_bw() +
        theme(aspect.ratio = 1, legend.position = "bottom")

      # Add reference contour lines for each panel
      for (panel.name in names(ref.levels)) {
        panel.data <- ctr[panel == panel.name]
        if (nrow(panel.data) > 0) {
          # Add contour lines for reference values
          p <- p +
            geom_contour(data = panel.data,
                        breaks = c(true.opt),
                        color = "black", linetype = "solid", size = 1) +
            geom_contour(data = panel.data,
                        breaks = c(assumed.opt),
                        color = "black", linetype = "dashed", size = 1) +
            geom_contour(data = panel.data,
                        breaks = c(true.cutoff),
                        color = "blue", linetype = "solid", size = 1) +
            geom_contour(data = panel.data,
                        breaks = c(assumed.cutoff),
                        color = "blue", linetype = "dashed", size = 1)
        }
      }

      # Add point markers for different classifications
      p <- p +
        geom_point(data = combined.data[.is.in.M == TRUE & .lse.set == "L"],
                  color = "green", size = 2, shape = 8) +
        geom_point(data = combined.data[.is.in.M == FALSE & .lse.set == "L"],
                  color = "green", size = 2, shape = 7) +
        geom_point(data = combined.data[.is.in.M == TRUE & .lse.set == "H"],
                  color = "blue", size = 2, shape = 8) +
        geom_point(data = combined.data[.is.in.M == FALSE & .lse.set == "H"],
                  color = "blue", size = 2, shape = 7) +
        geom_point(data = combined.data[.known == TRUE],
                  color = "red", size = 3, shape = 16)

      # Add iteration labels for known points
      if (nrow(combined.data[.known == TRUE & !is.na(.iteration)]) > 0) {
        p <- p + geom_text(data = combined.data[.known == TRUE & !is.na(.iteration)],
                          aes_string(label = ".iteration"),
                          color = "red", size = 5, vjust = -0.5)
      }

      return(p)
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
