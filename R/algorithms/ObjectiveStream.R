ObjectiveStream <- R6Class("ObjectiveStream",
  public = list(
    initialize = function(id, domain, minimize, seed) {
      private$.id <- assertString(id)
      private$.domain <- assertClass(domain, "ParamSet")
      private$.minimize <- assertLogical(minimize)
      seeds <- assertIntegerish(seed, lower = 0, upper = .Machine$integer.max, any.missing = FALSE, len = 2, tol = 0)
      withLocalSeed(private$.seed.sample, set.seed(seeds[1]))
      withLocalSeed(private$.seed.eval, set.seed(seeds[2]))
    },
    eval = function(x) {
      assertDataFrame(x, min.rows = 1)
      assertNames(colnames(x), must.include = self$domain$ids())
      result <- withLocalSeed(private$.seed.eval, private$.eval(x))
      assertNumeric(result, finite = TRUE, len = nrow(x))
      result
    },
    sample = function(n = 1) {
      assertCount(n)
      if (n == 0) {
        table <- getNullTable(self$domain)
      } else {
        table <- withLocalSeed(private$.seed.sample, private$.sample(n))
        assertDataFrame(table, nrows = n)
        assertNames(colnames(table), must.include = self$domain$ids())
      }
      table <- table[, self$domain$ids(), with = FALSE]
      for (i in which(self$domain$is_categ)) {
        table[[i]] <- factor(table[[i]], levels = self$domain$levels[[i]])
      }
      private$.augmentTable(table)
    },
    getRow = function(id) {
      assertIntegerish(id, lower = 1, upper = nrow(private$.table), tol = 0)
      ret <- private$.table[id, self$domain$ids(), with = FALSE]
      set(ret, j = ".id", value = as.integer(id))
      ret
    }
  ),
  active = list(
    id = function() private$.id,
    domain = function() private$.domain,
    minimize = function() private$.minimize
  ),
  private = list(
    .id = NULL,
    .domain = NULL,
    .minimize = NULL,
    .seed.sample = NULL,
    .seed.eval = NULL,
    .table = NULL,
    .augmentTable = function(table) {
      # add ID column to table. also remember table
      private$.table <- rbind(private$.table, table)
      set(table, j = ".id", value = seq.int(nrow(private$.table) - nrow(table) + 1, nrow(private$.table)))
      table
    },
    .eval = function(x) {
      stop("Not implemented")
    },
    .sample = function(n) {
      stop("Not implemented")
    }
  )
)
