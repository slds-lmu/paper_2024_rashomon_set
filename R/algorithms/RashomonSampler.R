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
#' The following methods are used for sampling and updating the state:
#' * `askXSamples()`: get the number of configurations (X-values) to sample
#' * `tellXSamples()`: provide the sampled configurations
#' * `askYValues()`: get the configurations for which to evaluate scores (Y-values)
#' * `tellYValues()`: provides those scores
#'
#' Concrete implementations must provide the private methods:
#' * `.tellXSamples()`: Process a batch of scored configurations
#' * `.askXSamples()`: Determine how many samples to request next
#' * `.askYValues()`: Select specific configurations for scoring
#' * `.tellYValues()`: Process scores for requested configurations
#' * `.rashomonSamplesComplete()`: Count found Rashomon set members
#' * `.getRashomonSamples()`: Retrieve the current Rashomon set
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
      (assertCount(private$.rashomonSamplesComplete(), tol = 0))  # parens to make result visible
    },

    #' @description
    #' Get the current set of configurations belonging to the Rashomon set.
    #' The result contains the domain parameters and a `.score` column.
    #' @return (`data.table`) Configurations and their scores.
    getRashomonSamples = function() {
      result <- private$.getRashomonSamples()
      assertDataTable(result)
      assertNames(colnames(result), must.include = c(self$domain$ids(), ".score"))
      result
    },

    #' @description
    #' Augment the sampler with pre-evaluated configurations.
    #' @param x (`data.frame`) Configurations with their scores
    #' @param scorecol (`character(1)`) Name of the column containing scores
    #' @return (`data.frame`) Input configurations, invisibly
    augmentXYSamples = function(x, scorecol = "score") {
      # TODO: not sure if this is a good idea
      if (!is.null(private$.tell.x.buffer)) stop("Cannot augment X when X request was already partially answered.")
      if (!is.null(private$.ask.y.buffer)) stop("Cannot augment X when Y was asked and not answered.")
      if (self$askXSamples() == 0) stop("Cannot augment X when no X requested.")

      assertDataFrame(x)
      assertChoice(scorecol, colnames(x))
      assertDisjunct(scorecol, c(".id", self$domain$ids()))
      assertNames(colnames(x), must.include = self$domain$ids())
      if (!nrow(x)) return(invisible(x))

      x <- as.data.table(x)
      if (!".id" %in% colnames(x)) {
        # either accept the user's ids or generate new ones
        x$.id <- seq.int(private$.told.x.samples + 1, private$.told.x.samples + nrow(x))
      }
      private$.told.x.samples <- private$.told.x.samples + nrow(x)

      xy <- cbind(data.table(.id = x$.id, .score = x[[scorecol]]), x[, self$domain$ids(), with = FALSE])
      private$.augmentXYSamples(xy)
      invisible(x)
    },

    #' @description
    #' Request the next batch of configurations to evaluate.
    #' @return (`integer(1)`) Number of configurations requested
    askXSamples = function() {
      if (!is.null(private$.ask.y.buffer)) return(0L)
      if (!is.null(private$.ask.x.buffer)) {
        return(private$.ask.x.buffer)
      }
      axb <- withLocalSeed(private$.seed, private$.askXSamples())
      private$.ask.x.buffer <- assertCount(axb, tol = 0)
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
      ask.x.samples <- self$askXSamples()
      assertDataFrame(x, max.rows = ask.x.samples)
      if (!nrow(x)) {
        return(ask.x.samples)
      }
      assertString(scorecol, null.ok = TRUE)
      assertDisjunct(scorecol, c(".id", self$domain$ids()))
      required.cols <- c(self$domain$ids(), scorecol)
      assertNames(colnames(x), must.include = required.cols)

      if (!is.data.table(x)) x <- as.data.table(x)
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
      self$askXSamples()
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
      if (self$askXSamples() != 0) return(getNullTable(self$domain, include.id = TRUE))
      if (!is.null(private$.ask.y.buffer)) {
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
      private$.ask.y.buffer <- rdf
      private$.tell.y.buffer <- set(rdf[, ".id", with = FALSE], j = ".score", value = NA_real_)
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
      assertDataFrame(y, min.rows = 1)
      assertChoice(scorecol, colnames(y))
      assertDisjunct(scorecol, c(".id", self$domain$ids()))
      assertNames(colnames(y), must.include = ".id")
      y <- data.table(.id = y$.id, score.new = y[[scorecol]])
      tyb <- private$.tell.y.buffer
      if (is.null(tyb)) {
        stop("Cannot tell Y when no Y was asked for.")
      }
      if (nrow(y[!tyb, on = ".id"]) || !all(is.na(tyb[y, .score, on = ".id"]))) {
        stop("Gave .id values that were not asked for.")
      }
      tyb <- private$.tell.y.buffer[y, .score := score.new, on = ".id"]
      still.asking <- tyb[is.na(.score), ".id", with = FALSE]
      if (nrow(still.asking)) {
        private$.ask.y.buffer <- private$.ask.y.buffer[still.asking, on = ".id"]
        return(private$.ask.y.buffer)
      }
      ayb <- private$.ask.y.buffer
      # we are guaranteed that `ayb` does not contain a `.score` column at this point
      assertDisjunct(".score", colnames(ayb))
      ayb[tyb, .score := .score, on = ".id"]
      private$.ask.y.buffer <- NULL
      private$.tell.y.buffer <- NULL
      private$.ask.x.buffer <- NULL
      private$.tell.x.buffer <- NULL

      withLocalSeed(private$.seed, private$.tellYValues(ayb))
      invisible(self$askYValues())
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
    # Has columns `.id` and `.score`.
    .tell.y.buffer = NULL,  # data.table | NULL
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
    # return the number of rashomon samples that have been found
    # May be called in any state.
    .rashomonSamplesComplete = function() {
      stop("Not implemented")
    },
    # Get a table with codomain$ids(), .id, and .score
    # May be called in any state.
    .getRashomonSamples = function() {
      stop("Not implemented")
    },
    # not yet fully implemented / designed
    .augmentXYSamples = function(xy) {
      stop("Not implemented")
    }
  )
)
