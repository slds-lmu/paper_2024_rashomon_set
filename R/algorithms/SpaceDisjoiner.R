

#' @title Separate a Search Space into Disjoint Subspaces
#'
#' @description
#' This class separates a hierarchical search space into disjoint subspaces.
#'
#' @details
#' The search space is separated into subspaces based on the parameter hierarchy, which is inferred from the
#' `ParamSet`'s `$deps` field.
#'
#' @export
SpaceDisjoiner <- R6Class("SpaceDisjoiner",
  public = list(
    #' @description
    #' Initialize the SpaceDisjoiner
    #' @param paramset (`ParamSet`) The search space to initialize with.
    #' @param also.disjoin.on (`character`) Additional parameters to consider for disjoining.
    #'   Defaults to `character(0)` (only use parameters that are depended on by others).
    #' @param allow.int (`logical`) Whether to allow integer parameters to be disjoined on.
    #'   Disjoining on non-factor / logical parameters is typically a mistake, so this has to be explicitly enabled.
    #'   Defaults to `FALSE`.
    initialize = function(paramset, also.disjoin.on = character(0), allow.int = FALSE) {
      private$.paramset <- assertR6(paramset, "ParamSet")$clone(deep = TRUE)
      assertFlag(allow.int)
      assertSubset(also.disjoin.on, private$.paramset$ids())
      disjoin.on <- unique(c(private$.paramset$deps$on, also.disjoin.on))
      params.to.keep <- setdiff(private$.paramset$ids(), disjoin.on)
      # construct topologically (by their own dependencies)sorted list of disjoining parameters
      disjoining.dependees <- character(0)
      deps.tmp <- private$.paramset$deps
      while (nrow(deps.tmp)) {
        free <- setdiff(unique(deps.tmp$on), deps.tmp$id)
        disjoining.dependees <- c(disjoining.dependees, free)
        deps.tmp <- deps.tmp[!on %in% free]
      }
      params.to.keep.always <- setdiff(params.to.keep, private$.paramset$deps$id)
      params.to.keep.checkdep <- intersect(params.to.keep, private$.paramset$deps$id)


      disjoin.space <- paramset$subset(disjoin.on)
      if (!all(disjoin.space$class %in% c("ParamFct", "ParamLgl", if (allow.int) "ParamInt"))) {
        stop("Disjoining on non-factor / logical parameters is typically a mistake. Set allow.int to TRUE to allow disjoining on integer parameters.")  # nolint
      }
      if (!all(disjoin.space$is_bounded)) {
        stop("Disjoining on unbounded parameters is not allowed.")
      }
      levels <- disjoin.space$levels
      for (int in disjoin.space$ids(class = "ParamInt")) {
        levels[[int]] <- seq.int(disjoin.space$lower[[int]], disjoin.space$upper[[int]])
      }
      levels.grid <- expand.grid(levels, stringsAsFactors = FALSE)
      subspacetable <- rbindlist(.mapply(dots = levels.grid, FUN = function(...) {
        x <- list(...)

        # first check if there are dependencies between the disjoining parameters that could be violated
        # we have topologically sorted disjoining.dependees, otherwise this could fail.
        for (par in disjoining.dependees) {
          if (!depsAreMet(x, par, private$.paramset$deps)) {
            # deps not met. Only go on if this is the first time we are called with a name for this parameter,
            if (identical(x[[par]], levels[[par]][[1]])) {
              x[[par]] <- NA_character_
              next
            }
            return(NULL)
          }
        }
        # okay, this disjoiner-config is valid.
        # representation to identify the configuration
        repr <- paste(names(x), x, sep = "=", collapse = ",")

        # filter out params by dependencies. Only check the ones for which we know they have deps
        params.subspace <- Filter(x = params.to.keep.checkdep, f = function(p) depsAreMet(x, p, private$.paramset$deps))
        params.subspace <- c(params.subspace, params.to.keep.always)  # we know we don't need to check deps for these

        subspace <- private$.paramset$subset(params.subspace, allow_dangling_dependencies = TRUE)
        # all 'on' vars are in the disjoining parameters, there are no dependencies left within the subspace
        subspace$deps <- subspace$deps[0]

        data.table(repr = repr, subspace = list(subspace))
      }, NULL))

      private$.subspacelist <- structure(subspacetable$subspace, names = subspacetable$repr)

      private$.levels <- levels
    },
    #' @description
    #' Disjoin a table of parameter configurations.
    #' @param dt (`data.table`) A table of parameter configurations.
    #' @return `list` of `data.table`: A list of tables, named by the subspace they belong to.
    disjoinTable = function(dt) {
      # assuming dt conforms with self$param.set
      assertDataTable(dt)
      assertNames(names(dt), must.include = self$param.set$ids())
      additional.cols <- setdiff(names(dt), self$param.set$ids())
      disjoiners <- dt[, names(self$disjoiner.levels), with = FALSE]
      splitby <- .mapply(dots = disjoiners, FUN = function(...) {
        x <- list(...)
        paste(names(x), vapply(x, as.character, character(1)), sep = "=", collapse = ",")
      }, MoreArgs = NULL)
      results <- split(dt, factor(unlist(splitby), levels = self$subspace.names))
      mapply(function(subspace, result) {
        # subset to subspace
        result[, c(subspace$ids(), additional.cols), with = FALSE]
      }, self$subspaces, results)
    }
  ),
  active = list(
    # input paramset
    param.set = function() private$.paramset,
    # table of subspaces
    subspaces = function() private$.subspacelist,
    # names of subspaces
    subspace.names = function() names(private$.subspacelist),
    # list of levels for disjoining parameters, named by parameter id
    disjoiner.levels = function() private$.levels
  ),
  private = list(
    .paramset = NULL,
    .levels = NULL,
    .subspacelist = NULL
  )
)


# 'configuration' is a named vector that contains all parameters in 'deps',
# if a parameter is NA or not in 'configuration', this implies that
# that parameter's dependency itself is not met
depsAreMet <- function(configuration, id, deps) {
  rows.with.deps <- which(deps$id == id)
  for (row in rows.with.deps) {
    onidx <- match(deps$on[[row]], names(configuration))
    if (is.na(onidx)) {
      return(FALSE)  # dependent parameter is itself not valid
    }
    onval <- configuration[[onidx]]
    if (is.na(onval) || !conditionTest(deps$cond[[row]], onval)) {
      return(FALSE)
    }
  }
  TRUE
}

# Need this because paradox does not export condition_test.
conditionTest <- function(cond, onval) {
  if (inherits(cond, "CondEqual")) {
    onval == cond$rhs
  } else if (inherits(cond, "CondAnyOf")) {
    onval %in% cond$rhs
  } else {
    stop("Unknown condition type: ", last(class(cond)))
  }
}
