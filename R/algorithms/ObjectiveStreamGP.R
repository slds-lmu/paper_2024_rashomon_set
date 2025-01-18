#' @title Objective Based on Sampling from a Gaussian Process
#'
#' @description
#' This objective function samples from a Gaussian process with given parameters.
#'
#' The objective is always on a hypercube ranging from 0 to 1, but with configurable lengthscales.
#' Available kernels are `"matern32"`, `"matern52"`, and `"se"` (squared exponential).
#'
#' @details
#' This is a concrete class, currently not meant to be subclassed.
#' @export
ObjectiveStreamGP <- R6Class("ObjectiveStreamGP",
  inherit = ObjectiveStreamActual,
  public = list(
    #' @description
    #' Initialize the objective function.
    #' @param lengthscales (`numeric`) The lengthscales of the Gaussian process.
    #'   This implicitly defines the number of dimensions.
    #' @param noise (`numeric(1)`) The noise of the Gaussian process.
    #' @param id (`character(1)`) The id of the objective function, used to identify the objective when printing.
    #' @param seed (`integer(2)`) Seed used both to initialize the sample stream (first element) and the evaluation
    #'   function (second element).
    #' @param kernel (`character(1)`) The kernel of the Gaussian process.
    #'   One of `"matern32"`, `"matern52"`, or `"se"` (squared exponential).
    #'   Defaults to `"matern52"`.
    initialize = function(lengthscales, noise, id, seed, kernel = "matern52") {
      private$.lengthscales <- assertNumeric(lengthscales, lower = 0, finite = TRUE, any.missing = FALSE, min.len = 1)
      private$.dimensions <- length(lengthscales)
      private$.noise <- assertNumber(noise, lower = 0, finite = TRUE)
      private$.kernel <- assertChoice(kernel, c("matern32", "matern52", "se"))
      private$.cache <- data.table(matrix(nrow = 0, ncol = private$.dimensions + 1))
      setnames(private$.cache, c(sprintf("x.d%s", seq_len(private$.dimensions)), "y"))

      domain <- ps_replicate(
        ps(x = p_dbl(0, 1)),
        affixes = sprintf("d%s", seq_len(private$.dimensions)),
        postfix = TRUE
      )
      super$initialize(id, domain, minimize = TRUE, seed)
    }
  ),
  active = list(
    #' @field dimensions (`integer(1)`) The number of dimensions of the Gaussian process.
    dimensions = function() private$.dimensions,
    #' @field lengthscales (`numeric`) The lengthscales of the Gaussian process.
    lengthscales = function() private$.lengthscales,
    #' @field noise (`numeric(1)`) The noise of the Gaussian process.
    noise = function() private$.noise,
    #' @field kernel (`character(1)`) The kernel of the Gaussian process.
    kernel = function() private$.kernel
  ),
  private = list(
    .dimensions = NULL,
    .lengthscales = NULL,
    .noise = NULL,
    .kernel = NULL,
    .cache = NULL,
    .cache.kernel = NULL,

    .computeKernel = function(X1, X2 = NULL) {
      # computes the kernel matrix, either between X1 and X2 or between X1 and itself
      if (is.null(X2)) X2 <- X1
      n1 <- nrow(X1)
      n2 <- nrow(X2)
      K <- matrix(0, nrow = n1, ncol = n2)

      for (i in seq_len(n1)) {
        for (j in seq_len(n2)) {
          dist <- sqrt(sum(((X1[i, ] - X2[j, ]) / private$.lengthscales)^2))

          K[i, j] <- switch(private$.kernel,
            se = exp(-0.5 * dist^2),
            matern32 = (1 + sqrt(3) * dist) * exp(-sqrt(3) * dist),
            matern52 = (1 + sqrt(5) * dist + 5/3 * dist^2) * exp(-sqrt(5) * dist)
          )
        }
      }
      K
    },

    .eval = function(x) {
      # Evaluates the GP at the given points.
      # At this point, `x` is a `data.table` with numeric columns, as well as the `.id` column.
      # Caches previous kernel matrix values, as well as previously evaluated points.
      x.mat <- as.matrix(x[, -".id", with = FALSE])
      n.new <- nrow(x.mat)

      if (nrow(private$.cache) == 0) {
        # First evaluation: sample unconditionally
        K.xx <- private$.computeKernel(x.mat) + diag(private$.noise, n.new)
        private$.cache.kernel <- K.xx
        L <- chol(K.xx)
        y <- drop(crossprod(L, rnorm(n.new)))
      } else {
        # Get cached data
        x.old <- as.matrix(private$.cache[, -"y"])
        y.old <- private$.cache$y

        # Compute new kernel matrices
        K.xx <- private$.computeKernel(x.mat) + diag(private$.noise, n.new)
        K.xX <- private$.computeKernel(x.mat, x.old)
        K.XX <- private$.cache.kernel

        # Compute conditional mean and covariance
        K.XX.inv <- solve(K.XX)
        mu <- K.xX %*% K.XX.inv %*% y.old
        Sigma <- K.xx - K.xX %*% K.XX.inv %*% t(K.xX)

        # Sample from conditional distribution
        L <- chol(Sigma + diag(1e-10, n.new))  # Add small constant for numerical stability
        y <- drop(mu + crossprod(L, rnorm(n.new)))

        # Update kernel cache
        private$.cache.kernel <- rbind(
          cbind(K.XX, t(K.xX)),
          cbind(K.xX, K.xx)
        )
      }

      # Update cache
      private$.cache <- rbind(private$.cache,
        data.table(x.mat, y = y))

      y
    }
  )
)
