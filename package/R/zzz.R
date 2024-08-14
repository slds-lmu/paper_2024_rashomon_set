#' @import mlr3misc
#' @import checkmate
#' @import bbotk
#' @import paradox
#' @import R6
#' @import data.table
#' @import mlr3pipelines
#'
#' @description
#'
#' Rashomon things.
"_PACKAGE"

lg = NULL

.onLoad = function(libname, pkgname) {  # nocov start

}  # nocov end

.onUnload = function(libpath) {  # nocov start

}  # nocov end

# static code checks should not complain about commonly used data.table columns
utils::globalVariables(c("dob", "eol"))

if (!Sys.getenv("DEVTOOLS_LOAD") == "true") {
  leanify_package()
}

