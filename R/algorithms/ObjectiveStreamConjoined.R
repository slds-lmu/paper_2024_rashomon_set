#' @title Conjoined Objective Stream
#'
#' @description
#' Combines multiple `ObjectiveStream`s into a single stream.
#' Samples are interleaved from the input streams, either randomly or in a round-robin fashion.
#'
#' @export
ObjectiveStreamConjoined <- R6Class("ObjectiveStreamConjoined",
  inherit = ObjectiveStream,
  public = list(
    #' @description
    #' Initialize the objective function.
    #' @param id (`character(1)`) The id of the objective function, used to identify the objective when printing.
    #' @param objective.streams (named `list` of `ObjectiveStream`) List of objective streams to conjoin.
    #'   Streams are cloned when stored internally.
    #'   Must be named. Minimize settings must be identical.
    #' @param sampling.strategy (`character(1)`) Sampling strategy to use.
    #'   Must be one of `"roundrobin"` (default) or `"random"`.
    #' @param weights (`numeric` | `NULL`) Optional weights for sampling.
    #'   Must be a numeric vector of the same length as `objective.streams`.
    #'   Defaults to uniform weights.
    #' @param choice.param.name (`character(1)`) The name of the new parameter that
    #'   indicates the choice of the original space.
    #' @param seed (`integer(1)`) Seed used to initialize the sample stream, when `sampling.strategy = "random"`.
    initialize = function(id, objective.streams, sampling.strategy = "roundrobin", weights = NULL,
        choice.param.name = "subspace", seed = NULL) {
      assertString(id)
      assertList(objective.streams, types = "ObjectiveStream", names = "unique", min.len = 1)
      assertChoice(sampling.strategy, c("roundrobin", "random"))
      assertInt(seed, lower = 0, upper = .Machine$integer.max, tol = 0, null.ok = sampling.strategy == "roundrobin")
      assertNumeric(weights, len = length(objective.streams), lower = 0, finite = TRUE, any.missing = FALSE,
        null.ok = TRUE)
      if (!is.null(weights) && all(weights == 0)) {
        stop("There must be at least one stream with non-zero weight.")
      }

      objective.streams <- lapply(objective.streams, function(x) x$clone(deep = TRUE))

      domains <- lapply(objective.streams, function(os) os$domain)
      minimize.settings <- unique(vapply(objective.streams, function(os) os$minimize, logical(1)))

      if (length(minimize.settings) > 1) {
        stop("All objective streams must have the same 'minimize' setting.")
      }

      # Using the domain of the first objective stream as the conjoined domain.
      combined.domain <- conjoinSpaces(lapply(objective.streams, function(os) os$domain))

      super$initialize(id = id, domain = combined.domain, minimize = minimize.settings,
        seed = c(if (sampling.strategy == "roundrobin") 0 else seed, 0L))
        # second seed is eval-seed, which is not used

      private$.objective.streams <- objective.streams
      private$.sampling.remainder <- numeric(length(objective.streams))

      private$.weights <- weights %??% rep(1, length(objective.streams))
      private$.sampling.strategy <- sampling.strategy
      private$.choice.param.name <- choice.param.name
      # translate unique outside names to original param names
      private$.name.translate <- sapply(names(objective.streams), function(osn) {
        os <- objective.streams[[osn]]
        n <- os$domain$ids()
        structure(n, names = paste0(osn, ".", n))
      }, simplify = FALSE)
    }
  ),
  active = list(
    #' @field objective.streams (named `list` of `ObjectiveStream`) List of objective streams that are conjoined.
    objective.streams = function() private$.objective.streams,
    #' @field sampling.strategy (`character(1)`) Sampling strategy used.
    sampling.strategy = function() private$.sampling.strategy,
    #' @field weights (`numeric` | `NULL`) Weights used for random sampling (or `NULL` for roundrobin).
    weights = function() private$.weights,
    #' @field choice.param.name (`character(1)`) The name of the new parameter that
    #'   indicates the choice of the original space.
    choice.param.name = function() private$.choice.param.name
  ),
  private = list(
    .objective.streams = NULL,
    .sampling.strategy = NULL,
    .weights = NULL,
    .choice.param.name = NULL,
    .sampling.remainder = NULL,
    .name.translate = NULL,
    .getSampleIndicesRandom = function(n) {
      sample(
        length(private$.objective.streams),
        n,
        replace = TRUE,
        prob = private$.weights
      )
    },
    .getSampleIndicesRoundRobin = function(n) {
      output <- integer(n)

      sw <- sum(self$weights)
      for (k in seq_len(n)) {
        # Step 1: Add weights to the remainder
        private$.sampling.remainder <- private$.sampling.remainder + self$weights

        # Step 2: Select the stream with the maximum remainder
        idx <- which.max(private$.sampling.remainder)

        # Step 3: Draw one sample from that stream
        output[[k]] <- idx

        # Update counters and remainder
        private$.sampling.remainder[[idx]] <- private$.sampling.remainder[[idx]] - sw
      }
      output
    },
    .sample = function(n) {
      sample.indices <- if (self$sampling.strategy == "roundrobin") {
        private$.getSampleIndicesRoundRobin(n)
      } else {
        private$.getSampleIndicesRandom(n)
      }
      num.streams <- length(private$.objective.streams)
      n.each <- tabulate(sample.indices, num.streams)
      tables <- mapply(function(stream, stream.name, i) {
        sample <- stream$sample(i)
        sample[, .id := .id * num.streams + i - 1L]
        sample
      }, private$.objective.streams, names(private$.objective.streams), n.each, SIMPLIFY = FALSE)
      fulltable <- conjoinSamples(tables, self$choice.param.name, keep.cols = ".id")

      fulltable[rank(sample.indices, ties.method = "first")]
    },
    .eval = function(x) {
      cpm <- self$choice.param.name
      num.streams <- length(private$.objective.streams)
      nt <- private$.name.translate
      x[, result := {
        current.stream <- get(cpm)
        subsamples <- copy(.SD)[, c(names(nt[[current.stream]]), ".id"), with = FALSE]
        subsamples[, .id := .id %/% num.streams]
        setnames(subsamples, names(nt[[current.stream]]), nt[[current.stream]])
        private$.objective.streams[[current.stream]]$eval(subsamples)
      }, by = cpm]
      x$result
    }
  )
)
