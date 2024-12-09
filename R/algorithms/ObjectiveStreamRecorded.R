

ObjectiveStreamRecorded <- R6Class("ObjectiveStreamRecorded",
  inherit = ObjectiveStream,
  public = list(
    initialize = function(id, domain, minimize, table, scorecol = "score") {
      super$initialize(id, domain, minimize, seed = c(0, 0))
      assertDataFrame(table)
      assertNames(colnames(table), must.include = self$domain$ids())
      private$.scorecol <- assertChoice(scorecol, colnames(table))
      private$.table <- table
    }
  ),
  private = list(
    .next.id = 1,
    .scorecol = NULL,
    .augmentTable = function(table) {
      private$.next.id <- private$.next.id + nrow(table)
      set(table, j = ".id", value = seq.int(private$.next.id - nrow(table), private$.next.id - 1))
      table
    },
    .eval = function(x) {
      rows <- assertIntegerish(x$.id, lower = 1, upper = nrow(private$.table), tol = 0)
      private$.table[rows, private$.scorecol, with = FALSE][[1]]
    },
    .sample = function(n) {
      if (private$.next.id + n - 1 > nrow(private$.table)) {
        stop("No more samples available")
      }
      rows <- seq.int(private$.next.id, private$.next.id + n - 1)

      private$.table[rows, self$domain$ids(), with = FALSE]
    }
  )
)
