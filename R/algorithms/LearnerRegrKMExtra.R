#' @title Kriging Regression with Additional Functionality
#'
#' @description
#' Offers functionality to efficiently compute the posterior variance of the
#' Kriging model conditioned on a set of new points.
#'
#' @details
#' Uses the algorithm from Bogunovic e t al (TruVaR paper).
#'
#' @export
LearnerRegrKMExtra <- R6Class("LearnerRegrKMExtra",
  inherit = LearnerRegr,
  public = list(
    baselearner = NULL,
    #' @description
    #' Initialize the learner.
    initialize = function() {
      self$baselearner <- LearnerRegrKM$new()
      super$initialize("regr.km.extra", packages = "DiceKriging",
        feature_types = self$baselearner$feature_types,
        param_set = self$baselearner$param_set$clone(deep = TRUE),
        properties = self$baselearner$properties,
        predict_types = self$baselearner$predict_types,
        label = "Extra-Kriging"
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

      model <- self$model$model
      newpoints <- as.matrix(new.points.task$data(cols = self$state$train_task$feature_names))
      if (is.logical(newpoints)) {
        storage.mode(newpoints) <- "numeric"
      }
      querypoints <- as.matrix(query.task$data(cols = self$state$train_task$feature_names))
      if (is.logical(querypoints)) {
        storage.mode(querypoints) <- "numeric"
      }
      nugget <- if (model@covariance@nugget.flag) {
        model@covariance@nugget
      } else {
        0
      }
      result <- posteriorVarGivenNewPoints(model, newpoints, querypoints, nugget)
      result[result < 0] <- 0
      sqrt(result)
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
    #' @return [`numeric(1)`]\cr
    #'   The total scaled excess variance reduction.
    totalScaledExcessVarianceReduction = function(new.points.task, query.task, beta, eta) {
      assertClass(new.points.task, "TaskRegr")
      assertClass(query.task, "TaskRegr")
      assertNumber(beta)
      assertNumber(eta)
      old.var <- beta * self$predict(query.task)$se^2 - eta^2
      old.var[old.var < 0] <- 0
      cond.var <- beta * self$predictConditionalSE(new.points.task, query.task)^2 - eta^2
      cond.var[cond.var < 0] <- 0
      sum(old.var) - colSums(cond.var)
    }
  ),
  private = list(
    .train = function(task) {
      lrn <- self$baselearner$clone(deep = TRUE)
      lrn$predict_type <- self$predict_type
      lrn$param_set$values <- self$param_set$values
      lrn$train(task)
      lrn$state
    },
    .predict = function(task) {
      lrn <- self$baselearner$clone(deep = TRUE)
      lrn$predict_type <- self$predict_type
      lrn$param_set$values <- self$param_set$values
      lrn$state <- self$model
      prediction <- lrn$predict(task)
      result <- list(response = prediction$response)
      if (self$predict_type == "se") {
        result$se <- prediction$se
      }
      result
    }
  )
)


posteriorVarGivenNewPoints <- function(model, Xcand, M, new.nugget) {
  assertClass(model, "km")
  assertMatrix(Xcand, mode = "numeric", ncols = model@d)
  assertMatrix(M, mode = "numeric", ncols = model@d)
  assert(checkNumber(new.nugget), checkNumeric(new.nugget, len = nrow(Xcand)))
  # model:    An object of class 'km' from DiceKriging
  # Xcand:    A matrix of candidate points, size [nCand x d]
  # M:        A matrix of query/test points, size [nM x d]
  # new.nugget: Scalar or vector for the noise var at new points x
  #
  # Returns:  A numeric matrix of size [nM x nCand],
  #           where entry (i, j) is sigma_{t-1|x_j}^2(M_i).

  # ----------------------------------------------------------------------
  # 1) Extract basics from the model
  # ----------------------------------------------------------------------
  # T is the Cholesky factor, so that t(T) %*% T = K + (training nugget) * I
  T.chol <- model@T
  X.train <- model@X  # the original training inputs (n x d)

  # 'invKTimesMatrix' multiplies (K + nugget*I)^(-1) by a given matrix
  # without forming the full inverse.
  invKTimesMatrix <- function(T.chol, A) {
    # A is n x m, Tchol is n x n
    # forwardsolve(..., transpose=TRUE, upper.tri=TRUE) solves T^T x = A
    tmp <- forwardsolve(T.chol, A, transpose = TRUE, upper.tri = TRUE)
    # backsolve(..., transpose=FALSE, upper.tri=TRUE) solves T x = tmp
    out <- backsolve(T.chol, tmp, transpose = FALSE, upper.tri = TRUE)
    out
  }

  # ----------------------------------------------------------------------
  # 2) Compute old posterior variance at M, i.e. sigma_{t-1}^2(M).
  #    We only need the diagonal of k(M,M) - k(M,X)*invK*k(X,M).
  # ----------------------------------------------------------------------
  # TODO: this can be optimized when chunking
  # Cross-cov of M vs X
  k.MX <- DiceKriging::covMat1Mat2(model@covariance, M, X.train)  # [nM x n]
  # Multiply by inv(K): zM = inv(K)* t(kMX) => dimension n x nM
  z.M <- invKTimesMatrix(T.chol, t(k.MX))                           # [n x nM]

  # Diagonal( k(M,M) )
  # K.M.diag <- diag(DiceKriging::covMat1Mat2(model@covariance, M, M)) # vector length nM
  if (is(model@covariance, "covUser")) {
    K.M.diag <- vapply(seq_len(nrow(M)), function(i) {
      model@covariance@kernel(M[i, ], M[i, ])
    }, numeric(1))
  } else {
    K.M.diag <- rep(model@covariance@sd2, nrow(M))
  }

  # diag( k(M,X)*inv(K)*k(X,M) ) == colSums( t(kMX) * zM ), since zM = inv(K)* t(kMX)
  old.var.M <- K.M.diag - colSums(t(k.MX) * z.M)  # length nM

  # ----------------------------------------------------------------------
  # 3) Precompute cross-covariances for candidate points, to allow vectorization
  # ----------------------------------------------------------------------
  # k(X, Xcand) => dimension [n x nCand]
  k.X.Xcand <- DiceKriging::covMat1Mat2(model@covariance, X.train, Xcand)

  # Multiply once by inv(K): Z_xcand = inv(K)* kXXcand => dimension [n x nCand]
  Z.xcand <- invKTimesMatrix(T.chol, k.X.Xcand)

  # For each candidate x_j, we want k(x_j, x_j).  That is the diag of covMat1Mat2(..., Xcand, Xcand).
  # We'll subtract k(x_j, Xtrain)*inv(K)*k(Xtrain, x_j) to get sigma_{t-1}(x_j)^2
  # in a vectorized manner.
  # k.Xcand.Xcand.diag <- diag(DiceKriging::covMat1Mat2(model@covariance, Xcand, Xcand))  # length nCand
  if (is(model@covariance, "covUser")) {
    k.Xcand.Xcand.diag <- vapply(seq_len(nrow(Xcand)), function(i) {
      model@covariance@kernel(Xcand[i, ], Xcand[i, ])
    }, numeric(1))
  } else {
    k.Xcand.Xcand.diag <- rep(model@covariance@sd2, nrow(Xcand))
  }

  # sigma_{t-1}(x_j)^2 = k(x_j,x_j) - k(x_j,X)*inv(K)*k(X,x_j) = diag(...) - colSums(...)
  sigma2.x <- k.Xcand.Xcand.diag - colSums(k.X.Xcand * Z.xcand)

  # Similarly, for Cov(M, x_j) = k(M, x_j) - k(M,X)* inv(K)* k(X, x_j),
  # we can do this in matrix form for all x_j:
  k.M.Xcand <- DiceKriging::covMat1Mat2(model@covariance, M, Xcand)  # [nM x nCand]
  # k(M,X)*inv(K)*k(X,x_j) => for all x_j, we do (k(M,X) %*% Z_xcand)
  tmp.Mx  <- k.MX %*% Z.xcand                                     # [nM x nCand]
  cov.M.x <- k.M.Xcand - tmp.Mx                                     # [nM x nCand]

  # Handle the new.nugget: if it's a scalar, it is recycled; if it's length nCand,
  # each candidate x_j can have a different noise level.
  # Denominator for each candidate point is sigma_{t-1}(x_j)^2 + noise at x_j
  denom.x <- sigma2.x + new.nugget  # length nCand

  # ----------------------------------------------------------------------
  # 4) Compute the "subtraction term" for each x_j:  (Cov(M,x_j)^2) / denom_x[j]
  #    Then subtract from oldVarM to get sigma_{t-1| x_j}^2(M).
  # ----------------------------------------------------------------------
  # We'll store result in an [nM x nCand] matrix: row i for M_i, col j for x_j
  n.M     <- nrow(M)
  n.Cand  <- nrow(Xcand)
  out.mat <- matrix(NA_real_, nrow = n.M, ncol = n.Cand)

  # Cov(M,x_j) is in cov.M.x[, j].  We subtract (cov.M.x[,j]^2 / denom.x[j]) from oldVarM.
  # We can do this in a vectorized way by dividing each column of cov.M.x^2 by denom.x,
  # then subtract from oldVarM.
  cov.M.x.sq <- cov.M.x^2  # elementwise square
  # Sweep out each column by dividing by denom.x[j].
  # E.g. for column j, do cov.M.x.sq[, j] / denom.x[j].
  to.subtract <- sweep(cov.M.x.sq, 2, denom.x, FUN = "/")

  # Now outMat = oldVarM - toSubtract, but oldVarM is a single vector of length nM,
  # so replicate it column-wise.
  out.mat <- sweep(-to.subtract, 1, old.var.M, FUN = "+")

  # outMat[i, j] = sigma_{t-1| x_j}^2( M_i )
  colnames(out.mat) <- paste0("cand", seq_len(n.Cand), recycle0 = TRUE)
  rownames(out.mat) <- paste0("M",    seq_len(n.M), recycle0 = TRUE)

  out.mat
}

