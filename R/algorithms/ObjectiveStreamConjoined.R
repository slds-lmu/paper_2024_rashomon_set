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
      combined.domain <- conjoinSpaces(lapply(objective.streams, function(os) os$domain),
        choice.param.name = choice.param.name)  # TODO: test that choice.param.name is respected by OSC

      super$initialize(id = id, domain = combined.domain, minimize = minimize.settings,
        seed = c(if (sampling.strategy == "roundrobin") 0 else seed, 0L))
        # second seed is eval-seed, which is not used

      private$.objective.streams <- objective.streams
      private$.sampling.remainder <- numeric(length(objective.streams))

      private$.weights <- weights %??% rep(1, length(objective.streams))
      private$.weights.internal <- private$.weights
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
    choice.param.name = function() private$.choice.param.name,
    #' @field nrow (`integer(1)`) The number of samples that can be queried in total.
    nrow = function() {
      sum(vapply(private$.objective.streams, function(os) os$nrow, numeric(1)))
    },
    #' @field remaining.rows (`integer(1)`) The number of remaining samples that can still be queried.
    remaining.rows = function() {
      sum(vapply(private$.objective.streams, function(os) os$remaining.rows, numeric(1)))
    }
  ),
  private = list(
    .objective.streams = NULL,
    .sampling.strategy = NULL,
    .weights = NULL,
    .weights.internal = NULL,  # reset to 0 when one stream is exhausted
    .choice.param.name = NULL,
    .sampling.remainder = NULL,
    .name.translate = NULL,
    .sample.translate = NULL,
    .getSampleIndicesRandom = function(n) {
      sample(
        length(private$.objective.streams),
        n,
        replace = TRUE,
        prob = private$.weights.internal
      )
    },
    .getSampleIndicesRoundRobin = function(n) {
      output <- integer(n)

      sw <- sum(private$.weights.internal)
      for (k in seq_len(n)) {
        # Step 1: Add weights to the remainder
        private$.sampling.remainder <- private$.sampling.remainder + private$.weights.internal

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
      num.streams <- length(private$.objective.streams)
      remaining.each <- vapply(private$.objective.streams, function(os) os$remaining.rows, numeric(1))
      if (n > sum(remaining.each)) {
        stop("Not enough samples remaining in the streams to sample n samples.")
      }
      sample.indices <- integer(0)
      n.each <- integer(num.streams)
      n.new <- n
      repeat {
        # maybe we run out  of samples for one or more streams
        # TODO: test this: running out for one stream, for multiple streams, running out exactly at the beginning/end
        # TODO: test that we run out of samples, and in the backtrack run out of samples again
        # In that case, we backtrack, throw away the additional sapmles, and reset the weight to 0
        sample.indices.new <- if (self$sampling.strategy == "roundrobin") {
          private$.getSampleIndicesRoundRobin(n.new)
        } else {
          private$.getSampleIndicesRandom(n.new)
        }
        n.each <- n.each + tabulate(sample.indices.new, num.streams)
        if (any(n.each > remaining.each)) {
          overhang <- pmax(n.each - remaining.each, 0)
          for (i in which(overhang > 0)) {
            to.remove <- tail(which(sample.indices.new == i), overhang[i])
            sample.indices.new <- sample.indices.new[-to.remove]
            n.each[i] <- n.each[i] - overhang[i]
          }
          assertTRUE(all(n.each[overhang > 0] == remaining.each[overhang > 0]))
          private$.weights.internal[overhang > 0] <- 0
          sample.indices <- c(sample.indices, sample.indices.new)
          assertTRUE(length(sample.indices.new) == n.new - sum(overhang))
          n.new <- sum(overhang)
        } else {
          sample.indices <- c(sample.indices, sample.indices.new)
          break
        }
      }
      tables <- mapply(function(stream, stream.name, i, index) {
          sample <- stream$sample(i)  # handles i == 0 correctly
          sample[, .id := .id * num.streams + index - 1L]
          sample
        }, private$.objective.streams, names(private$.objective.streams), n.each, seq_along(private$.objective.streams),
        SIMPLIFY = FALSE
      )
      fulltable <- conjoinSamples(tables, self$choice.param.name, keep.cols = ".id")
      # .id is deleted by calling function and replaced by 1:n, so we need to store the original id mapping
      # the id we get here is .id (1:n_subsample) * num.streams + index-within-streams - 1L, so to get back the
      # original id, we need to do id %/% num.streams + 1L
      fulltable <- fulltable[rank(sample.indices, ties.method = "first")]
      private$.sample.translate <- c(private$.sample.translate, fulltable$.id)
      fulltable
    },
    .eval = function(x) {
      cpm <- self$choice.param.name
      num.streams <- length(private$.objective.streams)
      nt <- private$.name.translate
      x[, result := {
        current.stream <- get(cpm)
        subsamples <- copy(.SD)[, c(names(nt[[current.stream]]), ".id"), with = FALSE]
        subsamples[, .id := private$.sample.translate[.id] %/% num.streams]
        setnames(subsamples, names(nt[[current.stream]]), nt[[current.stream]])
        private$.objective.streams[[current.stream]]$eval(subsamples)
      }, by = cpm]
      x$result
    },
    deep_clone = function(name, value) {
      if (name == ".objective.streams") {
        value <- lapply(value, function(os) os$clone(deep = TRUE))
      }
      value
    }
  )
)

# TODO: need to test various things, in particular that eval() gives the correct results under all circumstances
#  - # streams 1, 2, 3
#  - different weights
#  - eval() in weird order
