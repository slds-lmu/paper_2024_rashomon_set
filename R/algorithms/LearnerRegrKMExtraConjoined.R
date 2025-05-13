#' @title Kriging Regression with Additional Functionality for Conjoined Search Spaces
#'
#' @description
#' Fits a separate Kriging model for each independent subspace in the conjoined search space.
#'
#' Also offers functionality to efficiently compute the posterior variance of the
#' Kriging model conditioned on a set of new points, see [`LearnerRegrKMExtra`].
#'
#' @export
LearnerRegrKMExtraConjoined <- R6Class("LearnerRegrKMExtraConjoined",
  inherit = LearnerRegr,
  public = list(
    baselearner = NULL,
    disjoiner = NULL,
    initialize = function(param.set, also.disjoin.on = character(0), allow.int = FALSE) {
      self$baselearner <- LearnerRegrKMExtra$new()
      self$disjoiner <- SpaceDisjoiner$new(param.set, also.disjoin.on, allow.int)
      super$initialize("regr.km.extra.conjoined", packages = "DiceKriging",
        feature_types = c("numeric", "integer", "logical", "factor"),
        param_set = self$baselearner$param_set$clone(deep = TRUE),
        properties = c(self$baselearner$properties, "missings"),
        predict_types = self$baselearner$predict_types,
        label = "Conjoined-Extra-Kriging"
      )
    },
    #' @description
    #' Predict the conditional standard error of the Kriging model at a set of query points,
    #' given that new points have individually been observed.
    #'
    #' @param new.points.task [TaskRegr]\cr
    #'   Task containing the new points to condition on. These are the "`|x`" in the notation of the paper.
    #' @param query.task [TaskRegr]\cr
    #'   Task containing the query points. This is "`M`" in the notation of the paper.
    #' @return [matrix]\cr
    #'   Matrix of size `nrow(query.task)` x `nrow(new.points.task)`, where entry (i, j) is sigma_{t-1|x_j}^2(M_i).
    predictConditionalSE = function(new.points.task, query.task) {
      assertClass(new.points.task, "TaskRegr")
      assertClass(query.task, "TaskRegr")
      assert_predictable(new.points.task, self)  # checks for column compatibility etc.
      assert_predictable(query.task, self)

      result <- matrix(self$predict(query.task)$se, nrow = query.task$nrow, ncol = new.points.task$nrow)

      querytables <- self$disjoiner$disjoinTable(cbind(query.task$data(cols = self$state$train_task$feature_names), ..row_id = seq_len(query.task$nrow)))
      newpointtables <- self$disjoiner$disjoinTable(cbind(new.points.task$data(cols = self$state$train_task$feature_names), ..row_id = seq_len(new.points.task$nrow)))

      common.pars <- intersect(names(querytables), names(newpointtables))
      for (par in common.pars) {
        model <- self$model[[par]]$model$model
        newpoints <- as.matrix(newpointtables[[par]][, -"..row_id", with = FALSE])
        if (is.logical(newpoints)) {
          storage.mode(newpoints) <- "numeric"
        }
        querypoints <- as.matrix(querytables[[par]][, -"..row_id", with = FALSE])
        if (is.logical(querypoints)) {
          storage.mode(querypoints) <- "numeric"
        }
        nugget <- if (model@covariance@nugget.flag) {
          model@covariance@nugget
        } else {
          0
        }
        se.par <- posteriorVarGivenNewPoints(model, newpoints, querypoints, nugget)
        se.par[se.par < 0] <- 0
        result[querytables[[par]]$..row_id, newpointtables[[par]]$..row_id] <- sqrt(se.par)
      }
      result
    },
    #' @description
    #' Get the total scaled excess variance reduction for a set of query points `x[i]`,
    #' given that new points have individually been observed:
    #'
    #' ```
    #'   sum_{\bar{x} \in M}
    #'     max{(\beta \cdot \sigma^2(\bar{x}) - \eta^2), 0} -
    #'     max{(\beta \cdot \sigma^2(\bar{x} | x[i]) - \eta^2), 0}
    #' ```
    #'
    #' @param new.points.task [TaskRegr]\cr
    #'   Task containing the new points to condition on. These are the "`|x`" in the notation of the paper.
    #' @param query.task [TaskRegr]\cr
    #'   Task containing the query points. This is "`M`" in the notation of the paper.
    #'   This dimension is summed over here (hence "total").
    #' @param beta [`numeric(1)`]\cr
    #'   The "beta" scaling parameter
    #' @param eta [`numeric(1)`]\cr
    #'   The confidence interval cutoff width (squared units, i.e. target scaled variance)
    #' @param chunk.size [`integer(1)`]\cr
    #'   Size of chunks to process at a time when calculating scaled excess variance reduction.
    #'   If `Inf`, the entire grid is processed at once.
    #' @param use.subgrids [`logical(1)`]\cr
    #'   Whether to compute result for each subspace separately.
    #'   This is for debugging only and should always be `TRUE`.
    #' @return [`numeric(1)`]\cr
    #'   The total scaled excess variance reduction.
    totalScaledExcessVarianceReduction = function(new.points.task, query.task, beta, eta, chunk.size = Inf,
        use.subgrids = TRUE) {
      # we make use of the fact that the total scaled excess var reduction is 0 whenever a new point is
      # on a different subspace than a given query point.
      # We thus don't need to construct the entire new.points x query.points matrix, only the
      # submatrices for all pairs of points that are on the same subspace.
      assertClass(new.points.task, "TaskRegr")
      assertClass(query.task, "TaskRegr")
      assertNumber(beta)
      assertNumber(eta)
      assert(
        checkCount(chunk.size, positive = TRUE, tol = 0),
        checkNumber(chunk.size, lower = Inf)
      )
      if (!use.subgrids) {
        old.var <- beta * self$predict(query.task)$se^2 - eta^2
        old.var[old.var < 0] <- 0
        cond.var <- beta * self$predictConditionalSE(new.points.task, query.task)^2 - eta^2
        cond.var[cond.var < 0] <- 0
        return(sum(old.var) - colSums(cond.var))
      }

      old.var <- beta * self$predict(query.task)$se^2 - eta^2
      old.var[old.var < 0] <- 0

      result <- numeric(new.points.task$nrow)

      querytables <- self$disjoiner$disjoinTable(
        cbind(query.task$data(cols = self$state$train_task$feature_names),
          ..row_id = seq_len(query.task$nrow))
      )
      newpointtables <- self$disjoiner$disjoinTable(
        cbind(new.points.task$data(cols = self$state$train_task$feature_names),
          ..row_id = seq_len(new.points.task$nrow))
      )

      common.pars <- intersect(names(querytables), names(newpointtables))
      for (par in common.pars) {
        model <- self$model[[par]]$model$model
        newpoints <- as.matrix(newpointtables[[par]][, -"..row_id", with = FALSE])
        chunks <- split(seq_len(nrow(newpoints)), 1 + floor((seq_len(nrow(newpoints)) - 1) / chunk.size))
        if (is.logical(newpoints)) {
          storage.mode(newpoints) <- "numeric"
        }
        querypoints <- as.matrix(querytables[[par]][, -"..row_id", with = FALSE])
        if (is.logical(querypoints)) {
          storage.mode(querypoints) <- "numeric"
        }
        nugget <- if (model@covariance@nugget.flag) {
          model@covariance@nugget
        } else {
          0
        }
        for (ch in chunks) {
          var.par <- posteriorVarGivenNewPoints(model, newpoints[ch, , drop = FALSE], querypoints, nugget)
          var.par[var.par < 0] <- 0
          cond.var <- beta * var.par - eta^2
          cond.var[cond.var < 0] <- 0
          result[newpointtables[[par]]$..row_id[ch]] <- sum(old.var[querytables[[par]]$..row_id]) - colSums(cond.var)
        }
      }
      result
    }
  ),
  private = list(
    .train = function(task) {
      pars <- self$param_set$values
      tables <- self$disjoiner$disjoinTable(task$data())
      sapply(names(tables), function(x) {
        subtask <- as_task_regr(tables[[x]], target = task$target_names, id = paste0("subspace_", x))
        l <- self$baselearner$clone(deep = TRUE)
        l$param_set$values <- pars
        l$predict_type <- self$predict_type
        l$train(subtask)
        l$state
      }, simplify = FALSE)
    },
    .predict = function(task) {

      pars <- self$param_set$values
      tables <- self$disjoiner$disjoinTable(cbind(task$data(), ..row_id = task$row_ids))
      indiv.preds <- sapply(names(tables), function(x) {
        subtask <- as_task_regr(as_data_backend(tables[[x]], primary_key = "..row_id"), target = task$target_names, id = paste0("subspace_", x))
        l <- self$baselearner$clone(deep = TRUE)
        l$predict_type <- self$predict_type
        l$param_set$values <- pars
        l$state <- self$model[[x]]
        l$predict(subtask)
      }, simplify = FALSE)
      allpreds <- lapply(indiv.preds, as.data.table) |> rbindlist()
      allpreds <- allpreds[J(task$row_ids), on = "row_ids"]
      as.list(allpreds)[c("response", "se")]
    }
  )
)
