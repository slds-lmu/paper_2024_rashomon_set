
#' @title Objective Function with Pre-Recorded Samples
#'
#' @description
#' This objective function is based on a pre-recorded table of samples and their evaluations.
#' The table must be provided to the constructor.
#'
#' @details
#' This is a concrete class, currently not meant to be subclassed.
#' @export
ObjectiveStreamRecorded <- R6Class("ObjectiveStreamRecorded",
  inherit = ObjectiveStream,
  public = list(
    #' @description
    #' Initialize the objective function.
    #' @param id (`character(1)`) The id of the objective function, used to identify the objective when printing.
    #' @param domain (`ParamSet`) The domain of the objective function.
    #' @param minimize (`logical(1)`) Whether the objective function should be minimized.
    #' @param table (`data.frame`) A data.frame with the samples and their evaluations.
    #'   Must contain the columns specified in `$domain` and a column with the objective values.
    #' @param scorecol (`character(1)`) The name of the column containing the objective values.
    initialize = function(id, domain, minimize, table, scorecol = "score") {
      super$initialize(id, domain, minimize, seed = c(0, 0))
      assertDataFrame(table)
      private$.scorecol <- assertChoice(scorecol, colnames(table))
      assertTRUE(!any(self$domain$ids() == scorecol))
      assertNames(colnames(table), must.include = c(self$domain$ids(), scorecol))
      private$.table <- table
    }
  ),
  private = list(
    .next.id = 1,  # position of next sample in `private$.table`
    .scorecol = NULL,
    .table = NULL,
    .augmentTable = function(table) {
      # attach ID column to the table, which are not present in `private$.table`
      private$.next.id <- private$.next.id + nrow(table)
      set(table, j = ".id", value = seq.int(private$.next.id - nrow(table), private$.next.id - 1))
      table
    },
    .eval = function(x) {
      rows <- assertIntegerish(x$.id, lower = 1, upper = nrow(private$.table), any.missing = FALSE, tol = 0,
        coerce = TRUE)
      private$.table[rows, private$.scorecol, with = FALSE][[1]]
    },
    .sample = function(n) {
      if (private$.next.id + n - 1 > nrow(private$.table)) {
        stop("No more samples available")
      }
      rows <- seq.int(private$.next.id, private$.next.id + n - 1)
      # .next.id is updated by `.augmentTable()`. This is in case public `$sample()` gives an error before values are
      # returned -- in that case, we give the same values on the next call to `$sample()`.
      private$.table[rows, self$domain$ids(), with = FALSE]
    }
  )
)
