#' @title Base Class for Rashomon Set Sampling
#'
#' @description
#' Abstract base class for sampling configurations from a Rashomon set - the set of model configurations that perform
#' within some epsilon of the optimal performance.
#' The class provides infrastructure for iteratively requesting configuration samples, selectively requesting evaluation
#' of their performance, and identifying those that belong to the Rashomon set.
#'
#' @details
#' A Rashomon set contains all configurations whose performance is within epsilon of the optimal performance.
#' The epsilon can be specified either as an absolute value or as a relative fraction of the optimal performance.
#'
#' This class implements the "ask-tell" pattern, where an "ask" function is called first, which informs the user about
#' the requested information (number of samples to generate, samples to evaluate), followed by a "tell" function, which
#' provides the requested information and leads to an internal state update.
#' This pattern, while more complex to implement, allows for more flexibility on the user side and makes asynchonous
#' algorithms more straightforward.
#'
#' Note that the `RashomonSampler` does not propose configurations to evaluate.
#' Instead it relies on the caller to supply propositions.
#' The point of the `RashomonSampler` is, instead, to decide whether a configuration should be evaluated.
#'
#' The following methods are used for sampling and updating the state:
#' * `askXSamples()`: get the number of configurations (X-values) to sample
#' * `tellXSamples()`: provide the sampled configurations
#' * `askYValues()`: get the configurations for which to evaluate scores (Y-values)
#' * `tellYValues()`: provides those scores
#'
#' Concrete implementations must provide the private methods:
#' * `.askXSamples()`: Determine how many samples to request next.
#'    Should return `integer(1)`: the number of samples that should be given to `tellXSamples()`.
#'    If this returns greater than 0, the next internal call will be to `.tellXSamples()` with precisely the number of
#'    samples requested.
#'    If this returns 0, the next internal call will be to `.askYValues()`.
#' * `.tellXSamples()`: Process a batch of scored configurations.
#'   Called with a `data.table` with columns `.id` and `.score`, as well as the (user-given) domain parameters.
#'   The `.score` column will be `NA_real_` for configurations that have not been scored yet (the common case).
#'   After this is called, the next call will be to `.askXValues()`.
#' * `.askYValues()`: Select specific configurations for scoring.
#'   Should return a `data.table` with column `.id`, as well as the domain parameters.
#'   Rows should be a subset of the rows that were ever given to `.tellXSamples()`.
#'   At least one row must be returned.
#'   After this is called, the next call will be to `.tellYValues()`.
#' * `.tellYValues()`: Process scores for requested configurations.
#'   Called with a `data.table` with columns `.id` and `.score`.
#'   Row IDs will be in the order in which they were requested by `.askYValues()`.
#'   After this is called, the next call will be to `.askXValues()`.
#' * `.rashomonSamplesComplete()`: Count found Rashomon set members.
#' * `.getRashomonSamples()`: Retrieve the current Rashomon set.
#'
#' The base class does not take care of storing values given to `.tellXSamples()`.
#'
#' @section State Machine
#'
#' The class can be in states `INIT`, `ASKING.X(n)`, `ASKING.Y(m)`, and `TRANSITION`.
#'
#' The class starts in the `INIT` state and enters the `TRANSITION` state when `askXSamples()` (recommended) or
#' `askYValues()` is called.
#'
#' The `TRANSITION` state is only conceptual and left before control is returned to the caller:
#' Upon entering the state, `private$.askXSamples()` is called.
#' If it returns a value `n` > 0, the class transitions to `ASKING.X(n)`.
#' If it returns 0, the `ASKING.Y.PRELIMINARY` state is entered.
#'
#' In the `ASKING.Y.PRELIMINARY` state, `askYValues()` must be called by the user, which causes
#' `private$.askYValues()` to be called.
#' It returns a `data.table` with `m` rows, where `m` must be at least 1.
#' The class transitions to `ASKING.Y(m)`.
#' The `ASKING.Y.PRELIMINARY` state exists so that the user has control over when the (potentially costly)
#' `private$.askYValues()` is called.
#'
#' The class transitions from `ASKING.X(n)` to `ASKING.X(n - k)` when `k` rows are provided by `tellXSamples()`.
#' The class transitions from `ASKING.Y(m)` to `ASKING.Y(m - k)` when `k` rows are provided by `tellYValues()`.
#'
#' Whenever `ASKING.X(0)` or `ASKING.Y(0)` are entered, `private$.tellXSamples()` or `private$.tellYValues()` are
#' called, respectively, and the `TRANSITION` state is entered.
#'
#' State overview:
#'
#' * `INIT`: The class is initialized in this state.
#'   `askXSamples()` should be called to transition to `TRANSITION`
#' * `TRANSITION`: The class is in this state when the user first interacts with the class after initialization
#'   (`INIT`), or when `ASKING.X(0)` or `ASKING.Y(0)` were reached.
#'   The state is immediately changed to `ASKING.X(n)` or `ASKING.Y(n)`, where `n` is the value of
#'   `private$.askXSamples()` or `nrow(private$.askYValues())`, respectively (see description above).
#' * `ASKING.X(n)`: `tellXSamples()` should be called with `k` rows, where `k` is at most `n`, to transition to
#'    `ASKING.X(n - k)`.
#'   `askYValues()` will always return an empty `data.table` in this state and `tellYValues()` will throw an error.
#'   `private$.tellXSamples()` is called when `ASKING.X(0)` is entered, and the state transitions to `TRANSITION`.
#' * `ASKING.Y.PRELIMINARY`: `askYValues()` should be called by the user, which causes
#'   `private$.askYValues()` to be called and the state transitions to `ASKING.Y(m)`, where `m` is the number of rows
#'   returned by `private$.askYValues()`.
#'   `askXSamples()` will always return `0` in this state and both `tellXSamples()` and `tellYValues()` will throw an
#'    error.
#' * `ASKING.Y(m)`: `tellYValues()` should be called with `k` rows, where `k` is at most `m`, to transition to
#'   `ASKING.Y(m - k)`.
#'   `askXSamples()` will always return `0` in this state and `tellXSamples()` will throw an error.
#'   `private$.tellYValues()` is called when `ASKING.Y(0)` is entered, and the state transitions to `TRANSITION`.
#'
#' State diagram:
#'
#' ```
#'                       +-----------------------+
#'                       |         INIT          |
#'                       +-----------------------+
#'                                   |
#'           user calls              |
#'  askXSamples() or askYValues() -> |
#'                                   v
#'                       +-----------------------+                       +-----------------------+
#'                       |      TRANSITION       | if .askXSamples() = 0 | ASKING.Y.PRELIMINARY  |
#'  .------------------->|         calls         |---------------------->|                       |
#'  |                    |private$.askXSamples() |                       |                       |
#'  |                    +-----------------------+                       +-----------------------+
#'  |                                |                                               |
#'  |                   if .askXSamples() > 0            user calls askYValues(),    |
#'  |                                |                         which calls           |
#'  |                                |                 private$.askYValues() => m -> |
#'  |                                v                                               v
#'  |                    +-----------------------+                       +-----------------------+
#'  |                    |      ASKING.X(n)      |<--------.             |      ASKING.Y(m)      |<--------.
#'  |                    +-----------------------+         |             +-----------------------+         |
#'  |                                |                     |                         |                     |
#'  |                user calls      |                     |         user calls      |                     |
#'  |             tellXSamples(k) -> |                     |       tellYValues(k) -> |                     |
#'  |                                v                     |                         v                     |
#'  |                                +---------------------+                         +---------------------'
#'  |                                |     if n < k: n <- n - k                      |     if m < k: m <- m - k
#'  |                                |                                               |
#'  |                          if n == k                                       if m == k
#'  |                                |                                               |
#'  |                                v                                               v
#'  |                    +-----------------------+                       +-----------------------+
#'  |                    |      ASKING.X(0)      |                       |      ASKING.Y(0)      |
#'  |                    |         calls         |                       |         calls         |
#'  |                    |private$.tellXSamples()|                       |private$.tellYValues() |
#'  |                    +-----------------------+                       +-----------------------+
#'  |                                |                                               |
#'  |                                v                                               |
#'  '--------------------------------+<----------------------------------------------'
#' ```
#' @export
RashomonSampler <- R6Class("RashomonSampler",
  public = list(
    #' @description
    #' Initialize the Rashomon sampler.
    #' @param id (`character(1)`) Identifier for this sampler instance, used for logging and printing
    #' @param domain (`ParamSet`) Parameter space to search over
    #' @param minimize (`logical(1)`) Whether to minimize (`TRUE`) or maximize (`FALSE`) the objective
    #' @param rashomon.epsilon (`numeric(1)`) Epsilon threshold for Rashomon set membership
    #' @param rashomon.is.relative (`logical(1)`) Whether epsilon is relative to optimal score (`TRUE`) or absolute
    #'   (`FALSE`)
    #' @param seed (`integer(1)`) Random seed for reproducibility
    #' @param n.rashomon.samples (`integer(1)` | `Inf`) Target number of Rashomon set samples.
    #'   RashomonSampler implementations may optimize their behaviour to hit this target specifically.
    initialize = function(id, domain, minimize, rashomon.epsilon, rashomon.is.relative, seed, n.rashomon.samples) {
      private$.id <- assertString(id)
      private$.domain <- assertClass(domain, "ParamSet")
      assertDisjunct(private$.domain$ids(), c(".id", ".score"))
      private$.minimize <- assertLogical(minimize)
      private$.rashomon.is.relative <- assertLogical(rashomon.is.relative)
      private$.rashomon.epsilon <- assertNumber(rashomon.epsilon, lower = 0,
        upper = if (rashomon.is.relative) 1 else Inf, finite = TRUE
      )
      assertIntegerish(seed,
        lower = 0, upper = .Machine$integer.max, any.missing = FALSE, len = 1, tol = 0
      )
      withLocalSeed(private$.seed, set.seed(seed))
      assert(checkCount(n.rashomon.samples, tol = 0), checkNumber(n.rashomon.samples, lower = Inf))
      private$.n.rashomon.samples <- n.rashomon.samples
      private$.told.x.samples <- 0L
    },

    #' @description
    #' Get the number of Rashomon set samples found.
    #' @return (`integer(1)`) Number of Rashomon set samples found
    rashomonSamplesComplete = function() {
      nrow(self$getRashomonSamples())
    },

    #' @description
    #' Get the current set of configurations belonging to the Rashomon set.
    #' The result contains the domain parameters and a `.score` column.
    #' @return (`data.table`) Configurations and their scores.
    getRashomonSamples = function() {
      result <- private$.getRashomonSamples()
      assertDataTable(result)
      assertNames(colnames(result), must.include = c(self$domain$ids(), ".score"))
      ids <- self$domain$ids()
      result[, .(.score = mean(.score, na.rm = TRUE)), by = ids]
    },

    #' @description
    #' Request the next batch of configurations to evaluate.
    #' @return (`integer(1)`) Number of configurations requested
    askXSamples = function() {
      if (DEBUG) cat(sprintf("[%s] askXSamples():\n", private$.id))
      if (!is.null(private$.ask.y.buffer)) return(0L)
      if (!is.null(private$.ask.x.buffer)) {
        if (DEBUG) cat(sprintf("[%s:askXSamples] %s (buffered)\n", private$.id, private$.ask.x.buffer))
        return(private$.ask.x.buffer)
      }
      axb <- withLocalSeed(private$.seed, private$.askXSamples())
      private$.ask.x.buffer <- assertCount(axb, tol = 0, coerce = TRUE)
      if (DEBUG) cat(sprintf("[%s:askXSamples] %s (new)\n", private$.id, private$.ask.x.buffer))
      private$.ask.x.buffer
    },

    #' @description
    #' Provide scores for previously requested configurations.
    #'
    #' At most the number requested by `askXSamples()` should be provided, but it is possible to provide fewer.
    #' `tellXSamples()` will then return the number of additional configurations needed.
    #' Incomplete batches are cached, so that calling `tellXSamples()` with small batches or individual configurations
    #' incrementally results in the same behaviour as when all configurations are provided at once.
    #'
    #' If scores are already known, they can optionally be provided as an additional column.
    #' @param x (`data.frame`) Configurations with their scores.
    #'   May include an `".id"` column, which must be unique across all samples ever given to `tellXSamples()`
    #'   (and should then always be present).
    #' @param scorecol (`character(1)`) Name of the column containing scores, if present.
    #'   If this is provided and the column is not found, an error is thrown.
    #'   Defaults to `NULL`: no scores are provided.
    #' @return (`integer(1)`) Number of additional configurations needed
    tellXSamples = function(x, scorecol = NULL) {
      if (!is.null(private$.ask.y.buffer)) stop("Cannot tell X when Y was asked and not answered.")
      if (DEBUG) cat(sprintf("[%s] tellXSamples(%s):\n", private$.id, nrow(x)))
      ask.x.samples <- self$askXSamples()
      assertDataFrame(x, max.rows = ask.x.samples)
      if (!nrow(x)) {
        if (DEBUG) cat(sprintf("[%s:tellXSamples] %s (empty input)\n", private$.id, ask.x.samples))
        return(ask.x.samples)
      }
      assertString(scorecol, null.ok = TRUE)
      assertDisjunct(scorecol, c(".id", self$domain$ids()))
      required.cols <- c(self$domain$ids(), scorecol)
      assertNames(colnames(x), must.include = required.cols)

      x <- as.data.table(x)  # make a copy so we can modify x in-place
      if (!".id" %in% colnames(x)) {
        # either accept the user's ids or generate new ones
        x$.id <- seq.int(private$.told.x.samples + 1, private$.told.x.samples + nrow(x))
      }
      if (!is.null(scorecol)) {
        # we asserted above that the column exists
        colnames(x)[colnames(x) == scorecol] <- ".score"
      } else {
        x$.score <- NA_real_
      }
      private$.told.x.samples <- private$.told.x.samples + nrow(x)

      x <- x[, c(self$domain$ids(), ".id", ".score"), with = FALSE]

      private$.ask.x.buffer <- ask.x.samples - nrow(x)
      if (is.null(private$.tell.x.buffer)) {
        private$.tell.x.buffer <- copy(x)
      } else {
        private$.tell.x.buffer <- rbind(private$.tell.x.buffer, x, use.names = TRUE)
      }

      if (private$.ask.x.buffer <= 0) {
        assertTRUE(private$.ask.x.buffer == 0)
        x <- private$.tell.x.buffer
        private$.tell.x.buffer <- NULL

        withLocalSeed(private$.seed, private$.tellXSamples(x))
        private$.ask.x.buffer <- NULL
      }
      result <- self$askXSamples()
      if (DEBUG) cat(sprintf("[%s:tellXSamples] %s (new)\n", private$.id, result))
      result
    },

    #' @description
    #' Request specific configurations to evaluate.
    #'
    #' The returned `data.table` contains the domain parameters and a `.id` column.
    #' When `tellYValues()` is called and provides only some of the requested configurations, these results are cached
    #' and only the remaining configurations still need to be provided.
    #' Therefore, if `askYValues()` is called again at this point, it will return the remaining configurations.
    #' @return (`data.table`) Configurations to evaluate
    #' @note
    #' The returned `data.table` "belongs" to the sampler and should not be modified by reference by the caller.
    #' In fact, the `tellYValues()` function may modify the returned `data.table` object in place.
    #' Therefore, if the caller needs the returned `data.table` object after `tellYValues()` has been called,
    #' it must make a deep copy of it using `copy()`.
    askYValues = function() {
      if (DEBUG) cat(sprintf("[%s] askYValues():\n", private$.id))
      stillasking <- self$askXSamples()
      if (stillasking != 0) {
        if (DEBUG) cat(sprintf("[%s:askYValues] (empty; still asking %s xvals)\n", private$.id, stillasking))
        return(getNullTable(self$domain, include.id = TRUE))
      }
      if (!is.null(private$.ask.y.buffer)) {
        if (DEBUG) cat(sprintf("[%s:askYValues] %s (buffered)\n", private$.id, nrow(private$.ask.y.buffer)))
        return(private$.ask.y.buffer)
      }
      rdf <- withLocalSeed(private$.seed, private$.askYValues())
      assertDataFrame(rdf, min.rows = 1)
      if (!is.data.table(rdf)) rdf <- as.data.table(rdf)
      required.cols <- c(self$domain$ids(), ".id")
      assertNames(colnames(rdf), must.include = required.cols)
      rdf <- rdf[, required.cols, with = FALSE]
      for (i in which(self$domain$is_categ)) {
        set(rdf, j = i, value = factor(rdf[[i]], levels = self$domain$levels[[i]]))
      }
      private$.ask.y.id.order <- assertIntegerish(rdf$.id, tol = 0, any.missing = FALSE, unique = TRUE)
      private$.ask.y.buffer <- rdf
      private$.tell.y.buffer <- set(rdf[0], j = ".score", value = numeric(0))
      if (DEBUG) cat(sprintf("[%s:askYValues] %s (new)\n", private$.id, nrow(rdf)))
      rdf
    },

    #' @description
    #' Provide scores for specifically requested configurations.
    #'
    #' At most the number requested by `askYValues()` should be provided, but it is possible to provide fewer.
    #' `tellYValues()` will then return the number of additional configurations needed.
    #' Incomplete batches are cached, so that calling `tellYValues()` with small batches or individual configurations
    #' incrementally results in the same behaviour as when all configurations are provided at once.
    #' @param y (`data.frame`) Configurations with their scores.
    #'   The `".id"` column and the `scorecol` column must be present and match the `".id"` values of the result of
    #'   `askYValues()`.
    #'   Other columns are ignored.
    #' @param scorecol (`character(1)`) Name of the column containing scores.
    #'   Defaults to `".score"`.
    #' @return (`data.table`) Additional configurations to evaluate, if any
    tellYValues = function(y, scorecol = ".score") {
      assertDataFrame(y)
      if (DEBUG) cat(sprintf("[%s] tellYValues(%s):\n", private$.id, nrow(y)))
      if (!nrow(y)) {
        if (DEBUG) {
          cat(sprintf("[%s:tellYValues] still asking %s (empty input)\n",
          private$.id, nrow(private$.ask.y.buffer)))
        }
        return(private$.ask.y.buffer)
      }
      assertChoice(scorecol, colnames(y))
      assertDisjunct(scorecol, c(".id", self$domain$ids()))
      assertNames(colnames(y), must.include = ".id")
      y <- data.table(.id = y$.id, .score = y[[scorecol]])
      tyb <- private$.tell.y.buffer
      ayb <- private$.ask.y.buffer
      if (is.null(tyb)) {
        stop("Cannot tell Y when no Y was asked for.")
      }
      if (nrow(y[!ayb, on = ".id"])) {
        stop("Gave .id values that were not asked for.")
      }

      tyb <- rbind(
        tyb,
        ayb[y, on = ".id"],
        use.names = TRUE
      )
      ayb <- ayb[!y, on = ".id"]
      if (nrow(ayb)) {
        private$.ask.y.buffer <- ayb
        private$.tell.y.buffer <- tyb
        if (DEBUG) cat(sprintf("[%s:tellYValues] %s (remaining)\n", private$.id, nrow(ayb)))
        return(ayb)
      }
      yido <- private$.ask.y.id.order
      private$.ask.y.id.order <- NULL
      private$.ask.y.buffer <- NULL
      private$.tell.y.buffer <- NULL
      private$.ask.x.buffer <- NULL
      private$.tell.x.buffer <- NULL

      withLocalSeed(private$.seed, private$.tellYValues(tyb[J(yido), on = ".id"]))
      self$askXSamples()  # mandatory state transition
      if (DEBUG) cat(sprintf("[%s:tellYValues] 0 (told all)\n", private$.id))
      invisible(getNullTable(self$domain, include.id = TRUE))  # stay in ASKING.Y.PRELIMINARY state if necessary
    }
  ),
  active = list(
    #' @field id (`character(1)`) Unique identifier of this sampler instance
    id = function() private$.id,

    #' @field domain (`ParamSet`) Parameter space being sampled
    domain = function() private$.domain,

    #' @field minimize (`logical(1)`) Whether the objective is being minimized
    minimize = function() private$.minimize,

    #' @field seed (`integer(1)`) Random seed used for sampling
    seed = function() private$.seed,

    #' @field rashomon.epsilon (`numeric(1)`) Epsilon threshold for Rashomon set membership
    rashomon.epsilon = function() private$.rashomon.epsilon,

    #' @field rashomon.is.relative (`logical(1)`) Whether epsilon is relative to optimal score
    rashomon.is.relative = function() private$.rashomon.is.relative,

    #' @field n.rashomon.samples (`integer(1)` | `Inf`) Target number of Rashomon set samples
    n.rashomon.samples = function(rhs) {
      if (!missing(rhs)) {
        assertCount(rhs, tol = 0)
        if (rhs != private$.n.rashomon.samples) {
          if (!is.null(private$.tell.x.buffer) || !is.null(private$.tell.y.buffer)) {
            stop("Cannot change n.rashomon.samples after values have been asked or samples / values have been incompletely told")  # nolint
          }
          private$.ask.x.buffer <- NULL
          private$.n.rashomon.samples <- rhs
        }
      }
      private$.n.rashomon.samples
    }
  ),
  private = list(
    .id = NULL,
    .domain = NULL,
    .minimize = NULL,
    .rashomon.epsilon = NULL,
    .rashomon.is.relative = NULL,
    .seed = NULL,
    .n.rashomon.samples = NULL,
    # Caches the number of configurations requested by .askXSamples.
    # Is updated when tellXSamples is called with an incomplete batch; .askXSamples is only called again when it reaches
    # zero.
    .ask.x.buffer = NULL,  # integer(1) | NULL
    # Collects configurations provided to tellXSamples.
    # Reset to `NULL` when .tellXSamples is called with a complete batch.
    .tell.x.buffer = NULL,  # data.table | NULL
    # Total number of configurations ever provided to tellXSamples.
    .told.x.samples = NULL,  # integer(1)
    # Caches the result of .askYValues.
    .ask.y.buffer = NULL,  # data.table | NULL
    # Collects scores provided to tellYValues.
    # Reset to `NULL` when .tellYValues is called with a complete batch and set when askYValues is called in a state
    # where no X-samples are requested.
    # Has columns of .ask.y.buffer plus .score.
    .tell.y.buffer = NULL,  # data.table | NULL
    # Order of .id values returned by .askYValues.
    # This is used to match the .id values in .tellYValues to the .id values in .askYValues.
    .ask.y.id.order = NULL,  # integer | NULL
    # return number of samples to expect in .tellXSamples
    # Called at the very beginning, and every time .tellYValues() returns.
    .askXSamples = function() {
      stop("Not implemented")
    },
    # get a table with codomain$ids(), .id, and .score (may be NA_real_)
    # Called when the user has provided exactly the number of samples requested by .askXSamples().
    .tellXSamples = function(x) {
      stop("Not implemented")
    },
    # return data.table of configs to evaluate. contains .id, which .tellYValues will match to.
    # If .tellXSamples contained .id, it should match here.
    # Called after .tellXSamples().
    .askYValues = function() {
      stop("Not implemented")
    },
    # given a table with codomain$ids(), .id, and .score.
    # Called when the user has provided exactly the number of samples requested by .askYValues().
    .tellYValues = function(y) {
      stop("Not implemented")
    },
    # Get a table with codomain$ids(), .id, and .score
    # May be called in any state.
    .getRashomonSamples = function() {
      stop("Not implemented")
    },
    # Helper function to get the indices of the Rashomon set from given scores,
    # assuming that the optimum in the scores is the reference performance.
    .getRashomonIndices = function(scorevector) {
      assertNumeric(scorevector, any.missing = FALSE, finite = TRUE)
      if (!length(scorevector)) {
        return(integer(0))
      }
      scores <- scorevector
      epsilon <- self$rashomon.epsilon
      if (!self$minimize) {
        scores <- -scores
        if (self$rashomon.is.relative) {
          epsilon <- -epsilon
        }
      }
      optimum <- min(scores)
      if (self$rashomon.is.relative) {
        cutoff <- optimum * (1 + epsilon)
        if (cutoff < optimum) {
          stop("rashomon.is.relative does not work for negative scores")
        }
      } else {
        cutoff <- optimum + epsilon
      }
      which(scores <= cutoff)
    }
  )
)

