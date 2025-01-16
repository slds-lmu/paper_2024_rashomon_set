
withLocalSeed <- function(local.seed.var, expr) {
  old.seed <- get0(".Random.seed", .GlobalEnv, ifnotfound = NULL)
  on.exit({
    if (is.null(old.seed)) {
      rm(".Random.seed", envir = .GlobalEnv)
    } else {
      assign(".Random.seed", old.seed, envir = .GlobalEnv)
    }
  })
  if (utils::compareVersion(sprintf("%s.%s", R.version$major, R.version$minor), "4.3.0") < 0) {
    stop("R version 4.3.0 or higher is required.")
  }
  RNGversion("4.3.0")
  local.seed.var <- substitute(local.seed.var)
  seed.state <- eval.parent(local.seed.var)
  if (is.null(seed.state)) {
    if (!is.null(.GlobalEnv$.Random.seed)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  } else {
    assign(".Random.seed", seed.state, envir = .GlobalEnv)
  }
  result <- force(expr)
  eval.parent(substitute(local.seed.var <- seed.state,  # nolint
    list(local.seed.var = local.seed.var, seed.state = .GlobalEnv$.Random.seed)
  ))
  result
}

getNullTable <- function(domain, include.id = FALSE, include.score = FALSE) {
  indt <- sapply(domain$ids(), function(x) numeric(0), simplify = FALSE)
  table <- domain$qunif(as.data.frame(indt))
  for (i in which(domain$is_categ)) {
    table[[i]] <- factor(table[[i]], levels = domain$levels[[i]])
  }
  if (include.id) {
    set(table, j = ".id", value = integer(0))
  }
  if (include.score) {
    set(table, j = ".score", value = numeric(0))
  }
  table
}
