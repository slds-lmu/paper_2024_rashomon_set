#' @title Objective Based on Sampling from a Gaussian Process
#'
#' @description
#' This objective function samples from a Gaussian process with given parameters.
#'
#' The objective is always on a hypercube ranging from 0 to 1, but with configurable lengthscales.
#' The amplitude of the kernel is set to 1.
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
    #'   Large lengthscales mean that the GP is smooth and does not vary much.
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
    .cache = NULL,  # data.table holding old x and y
    .chol = NULL,  # store the Cholesky factor of the kernel of all cached points

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
      # Generate two samples from N(0, 1) for each new point.
      # The following ensures that normal.sample[1, ] is always the same
      # whether .eval() is called once for a batch, or multiple times for each point.
      # Same for normal.sample[2, ].
      normal.sample <- matrix(rnorm(n.new * 2), nrow = 2)

      if (nrow(private$.cache) == 0) {
        #---------------------------------------------------------
        # FIRST EVALUATION (UNCONDITIONAL)
        #---------------------------------------------------------
        # 1. Compute K_xx + noise
        K.xx <- private$.computeKernel(x.mat) + diag(1e-10, n.new)

        # 2. Cholesky factor (R upper triangular: K.xx = R^T R)
        R <- chol(K.xx)

        # 3. Sample from N(0, K.xx)
        #    We add the noise separately, so that the same seed gives rise to the
        #    same GP sample with scaled noise.

        y <- drop(crossprod(R, normal.sample[1, ]))

        # 4. Cache
        private$.chol <- R  # store the factor for future block updates

      } else {
        #---------------------------------------------------------
        # SUBSEQUENT EVALUATIONS (CONDITIONAL)
        #---------------------------------------------------------
        # 1. Gather old data and old R
        x.old <- as.matrix(private$.cache[, -"y", with = FALSE])
        y.old <- private$.cache$y
        R.old <- private$.chol  # R.old is the upper-triangular chol factor of K_{XX}
        n.old <- nrow(x.old)

        # 2. Compute K.xx for new points + noise
        K.xx <- private$.computeKernel(x.mat) + diag(1e-10, n.new)
        # 3. Compute cross-covariances for new points:
        #     K.xX = cov(new points, old points)
        K.xX <- private$.computeKernel(x.mat, x.old)

        #---------------------------------------------------------
        # 4a. Posterior mean:
        # mu = K.xX K.XX^-1 y_old
        #
        #   But K.XX^-1 y_old can be computed by a two-step solve with R.old:
        #   R.old^T R.old = K.XX
        #   Let alpha = K.XX^-1 y_old => R.old * R.old^T * alpha = y_old
        #   => solve upper then solve lower, or vice versa.
        #   Since R.old is upper triangular, we do:
        v <- backsolve(R.old, y.old, transpose = TRUE)
        alpha <- backsolve(R.old, v, transpose = FALSE)

        # Now we need (K.xX K.XX^-1), which is K.xX * alpha if we want the mean directly,
        # but for the covariance we also need K.xX K.XX^-1 K.Xx. So let's get
        # W = K.xX K.XX^-1 in a matrix sense. That means W = K.xX * solve(K.XX).
        #   => W = K.xX * (K.XX^-1)
        #   => we can do: W = K.xX * alpha for the vector part, but for the matrix part:
        #   => W = K.xX %*% K.XX^-1 = (K.xX * solve(R.old^T)) * solve(R.old)
        #
        # We'll do it in steps:

        # Step A: solve(R.old^T, K.xX^T)
        #  (that is, backsolve(R.old, t(K.xX), transpose=TRUE).)
        K.Xx <- t(K.xX)  # dimension n.old x n.new
        U <- backsolve(R.old, K.Xx, transpose = TRUE)
        # Now solve(R.old, temp, transpose=FALSE) => W^T
        Wt <- backsolve(R.old, U, transpose = FALSE)
        W <- t(Wt)  # dimension: n.new x n.old

        mu <- W %*% y.old  # K.xX K.XX^-1 y_old

        # 4b. Posterior covariance: Sigma = K.xx - K.xX K.XX^-1 K.Xx = K.xx - W K.Xx
        #     But W K.Xx = W %*% t(K.xX). We'll just re-use Wt = t(W).
        #     => Wt is n.old x n.new, so Wt * t(K.xX) is n.old x ?
        # Actually more directly:
        #   W K.Xx = W %*% t(K.xX) if we interpret "K.Xx" = (K.xX)^T
        #   => so W K.Xx = W %*% t(K.xX)

        # K.Xx is n.old x n.new.
        # W is n.new x n.old, so W %*% K.Xx is n.new x n.new.

        crossTerm <- W %*% K.Xx  # n.new x n.new
        Sigma <- K.xx - crossTerm

        # 4c. Sample from N(mu, Sigma)
        # do chol and draw: y = mu + R_Sigma^T * z
        R.Sig <- chol(Sigma)

        y <- drop(mu + crossprod(R.Sig, normal.sample[1, ]))

        #---------------------------------------------------------
        # 5. **Block Cholesky update** for the *full* kernel.
        #
        # We want to produce R.new (upper-triangular) such that
        #    K.new = [ K.XX   K.Xx ] = R.new^T R.new
        #            [ K.xX   K.xx ]
        #
        # We already have R.old s.t. K.XX = R.old^T R.old.
        # The standard block formula is:
        #
        #   U = solve(R.old, K.Xx, transpose = TRUE)  # => dimension n.old x n.new; we solved that above
        #   Then define block S = K.xx - U^T U
        #   Then R.new = block upper-tri:
        #        [ R.old    U     ]
        #        [   0    chol(S) ]
        #
        # We'll store R.new to handle the next iteration.

        S <- K.xx - crossprod(U)  # = K.xx - U^T U

        # Cholesky of S:
        R.S <- chol(S)  # upper triangular, dimension n.new x n.new

        # Now we form the new big R block-matrix. For memory reasons,
        # we can store it all or keep it in some compact form. We will store
        # the entire new factor in a (n.old + n.new) x (n.old + n.new) matrix:

        private$.chol <- rbind(
          cbind(R.old, U),
          cbind(matrix(0, nrow = n.new, ncol = n.old), R.S)
        )
      }

      # Update cache
      private$.cache <- rbind(private$.cache,
        data.table(x.mat, y = y))

      y + private$.noise * normal.sample[2, ]
    }
  )
)
