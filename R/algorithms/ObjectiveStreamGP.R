# Objective based on sampling from a Gaussian process

ObjectiveStreamGP <- R6Class("ObjectiveStreamGP",
  inherits = ObjectiveStreamActual,
  public = list(
    initialize = function(dimensions, lengthscales, noise, id, seed, kernel = "matern52") {
      private$.dimensions <- assertInt(dimensions, lower = 1, tol = 0)
      private$.lengthscales <- assertNumeric(lengthscales, lower = 0, finite = TRUE, any.missing = FALSE, len = dimensions)
      private$.noise <- assertNumber(noise, lower = 0, finite = TRUE)
      private$.kernel <- assertChoice(kernel, c("matern32", "matern52", "se"))
      private$.cache <- data.table(matrix(nrow = 0, ncol = dimensions + 1))
      setnames(private$.cache, c(sprintf("x.d%s", seq_len(dimensions)), "y"))

      domain <- ps_replicate(ps(x = p_dbl(0, 1)), affixes = sprintf("d%s", seq_len(dimensions)), postfix = TRUE)
      super$initialize(id, domain, minimize = TRUE, seed)
    }
  ),
  active = list(
    dimensions = function() private$.dimensions,
    lengthscales = function() private$.lengthscales,
    noise = function() private$.noise,
    kernel = function() private$.kernel
  ),
  private = list(
    .dimensions = NULL,
    .lengthscales = NULL,
    .noise = NULL,
    .kernel = NULL,
    .cache = NULL,
    .cache.kernel = NULL,

    computeKernel = function(X1, X2 = NULL) {
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
      x.mat <- as.matrix(x)
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
