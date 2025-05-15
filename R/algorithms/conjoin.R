

#' @title Join Multiple `paradox` Parameter Spaces
#'
#' @description
#' This function joins multiple `ParamSet`s into a single `ParamSet`.
#' Parameters get a prefix indicating the original space.
#' It adds an additional parameter indicating the choice of the original space.
#' Parameters from the original space all get a dependency on this new parameter.
#'
#' @param spaces (named `list` of `ParamSet`) The Parameter Spaces to join.
#' @param choice.param.name (`character(1)`) The name of the new parameter that
#'   indicates the choice of the original space.
#' @return (`ParamSet`) The joined Parameter Space.
#' @export
conjoinSpaces <- function(spaces, choice.param.name = "subspace") {
  assertList(spaces, types = "ParamSet", names = "unique")
  assertString(choice.param.name)
  pids <- unlist(lapply(spaces, function(x) x$ids()))
  params.with.deps <- unique(unlist(lapply(spaces, function(x) x$deps$id)))

  choicespace <- ParamSet$new(structure(list(p_fct(names(spaces))), names = choice.param.name))

  allspaces <- ps_union(c(spaces, list(choicespace)), tag_sets = TRUE)

  for (spacename in names(spaces)) {
    dependers <- allspaces$ids(tags = paste0("set_", spacename))
    for (dep in dependers) {
      allspaces$add_dep(id = dep, on = choice.param.name, cond = CondEqual(spacename))
    }
  }

  allspaces
}


#' @title Join Multiple `data.table` to Fit to a Common Conjoined Parameter Space
#'
#' @description
#' This function joins multiple `data.table`s into a single `data.table`.
#' Columns get a prefix indicating the original space.
#' It adds an additional column indicating the choice of the original space.
#'
#' @param samples (named `list` of `data.table`) The `data.table`s to join.
#' @param choice.param.name (`character(1)`) The name of the new parameter that
#'   indicates the choice of the original space.
#' @param keep.cols (`character`) The columns from the original `data.table`s
#'   that should not be renamed.
#' @return (`data.table`) The joined `data.table`.
#' @export
conjoinSamples <- function(samples, choice.param.name = "subspace", keep.cols = character(0)) {
  assertList(samples, types = "data.table")
  assertString(choice.param.name)
  assertCharacter(keep.cols, any.missing = FALSE)
  samples <- lapply(names(samples), function(n) {
    x <- samples[[n]]
    rename <- !colnames(x) %in% keep.cols
    colnames(x)[rename] <- paste0(n, ".", colnames(x)[rename])
    x[[choice.param.name]] <- if (nrow(x) == 0) character(0) else n
    x
  })
  rbindlist(samples, use.names = TRUE, fill = TRUE)
}
