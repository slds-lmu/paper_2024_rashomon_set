RashomonSampler <- R6Class("RashomonSampler",
  public = list(
    initialize = function(id, domain, minimize, rashomon.epsilon, rashomon.is.relative, seed, n.rashomon.samples) {
      private$.id <- assertString(id)
      private$.domain <- assertClass(domain, "ParamSet")
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
    rashomonSamplesComplete = function() {
      private$.rashomonSamplesComplete()
    },
    getRashomonSamples = function() {
      private$.getRashomonSamples()
    },
    # TODO: not sure if this is a good idea
    augmentXYSamples = function(x, scorecol = "score") {
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
    askXSamples = function() {
      if (!is.null(private$.ask.y.buffer)) return(0)
      if (!is.null(private$.ask.x.buffer)) {
        return(private$.ask.x.buffer)
      }
      axb <- withLocalSeed(private$.seed, private$.askXSamples())
      private$.ask.x.buffer <- assertCount(axb, tol = 0)
      private$.ask.x.buffer
    },
    tellXSamples = function(x, scorecol = "score") {
      if (!is.null(private$.ask.y.buffer)) stop("Cannot tell X when Y was asked and not answered.")
      ask.x.samples <- self$askXSamples()
      assertDataFrame(x, max.rows = ask.x.samples - NROW(private$.tell.x.buffer))
      if (!nrow(x)) {
        return(ask.x.samples)
      }
      assertNames(colnames(x), must.include = self$domain$ids())
      if (!".id" %in% colnames(x)) {
        # either accept the user's ids or generate new ones
        x$.id <- seq.int(private$.told.x.samples + 1, private$.told.x.samples + nrow(x))
      }
      private$.told.x.samples <- private$.told.x.samples + nrow(x)
      private$.tell.x.buffer <- rbind(private$.tell.x.buffer, x)
      if (nrow(private$.tell.x.buffer) >= ask.x.samples) {
        assertTRUE(nrow(private$.tell.x.buffer) == ask.x.samples)
        x <- private$.tell.x.buffer
        private$.tell.x.buffer <- NULL
        if (scorecol %in% colnames(x)) {
          colnames(x)[colnames(x) == scorecol] <- ".score"
        } else {
          x$.score <- NA_real_
        }
        withLocalSeed(private$.seed, private$.tellXSamples(x))
      }
      private$.ask.x.buffer <- NULL
      self$askXSamples()
    },
    askYValues = function() {
      if (self$askXSamples() != 0) return(getNullTable(self$domain, include.id = TRUE))
      if (!is.null(private$.ask.y.buffer)) {
        still.asked <- private$.ask.y.buffer[!is.na(score), .(.id)]
        return(private$.ask.y.buffer[still.asked, on = ".id"])
      }
      rdf <- withLocalSeed(private$.seed, private$.askYValues())
      assertDataFrame(rdf, min.rows = 1)
      assertNames(colnames(rdf), must.include = c(self$domain$ids(), ".id"))
      for (i in which(self$domain$is_categ)) {
        rdf[[i]] <- factor(rdf[[i]], levels = self$domain$levels[[i]])
      }
      private$.ask.y.buffer <- rdf
      private$.tell.y.buffer <- set(rdf[, ".id", with = FALSE], j = "score", value = NA_real_)
      rdf
    },
    tellYValues = function(y, scorecol = "score") {
      assertDataFrame(y, min.rows = 1)
      assertChoice(scorecol, colnames(y))
      assertDisjunct(scorecol, c(".id", self$domain$ids()))
      assertNames(colnames(y), must.include = ".id")
      y <- data.table(.id = y$.id, score.new = y[[scorecol]])
      tyb <- private$.tell.y.buffer
      if (nrow(y[!tyb, on = ".id"]) || !all(is.na(tyb[y, score, on = ".id"]))) {
        stop("Gave .id values that were not asked for.")
      }
      tyb <- private$.tell.y.buffer[y, score := score.new, on = ".id"]
      still.asking <- private$.ask.y.buffer[!is.na(score), .(.id)]
      if (nrow(still.asking)) {
        return(private$.ask.y.buffer[still.asking, on = ".id"])
      }
      ayb <- private$.ask.y.buffer
      setnames(tyb, "score", scorecol)
      ayb[tyb, .score := get(scorecol), on = ".id"]
      private$.ask.y.buffer <- NULL
      private$.tell.y.buffer <- NULL
      private$.ask.x.buffer <- NULL
      private$.tell.x.buffer <- NULL

      withLocalSeed(private$.seed, private$.tellYValues(ayb, scorecol))
      getNullTable(self$domain, include.id = TRUE)
    }
  ),
  active = list(
    id = function() private$.id,
    domain = function() private$.domain,
    minimize = function() private$.minimize,
    seed = function() private$.seed,
    rashomon.epsilon = function() private$.rashomon.epsilon,
    rashomon.is.relative = function() private$.rashomon.is.relative,
    n.rashomon.samples = function(rhs) {
      if (!missing(rhs)) {
        assertCount(rhs, tol = 0)
        if (rhs != private$.n.rashomon.samples) {
          if (!is.null(private$.tell.x.buffer) || !is.null(private$.tell.y.buffer)) {
            stop("Cannot change n.rashomon.samples after samples have been asked (x) or incompletely told (x, y)")
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
    .ask.x.buffer = NULL,
    .tell.x.buffer = NULL,
    .told.x.samples = NULL,
    .ask.y.buffer = NULL,
    .tell.y.buffer = NULL,
    .tellXSamples = function(x) {
      stop("Not implemented")
    },
    .askXSamples = function() {
      stop("Not implemented")
    },
    .askYValues = function() {
      stop("Not implemented")
    },
    .tellYValues = function(y) {
      stop("Not implemented")
    },
    .rashomonSamplesComplete = function() {
      stop("Not implemented")
    },
    .getRashomonSamples = function() {
      stop("Not implemented")
    },
    .augmentXYSamples = function(xy) {
      stop("Not implemented")
    }
  )
)
