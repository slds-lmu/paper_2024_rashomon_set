getRegistry <- function(regpath, make.default = FALSE) {
  if (!file.exists(regpath)) {
    makeExperimentRegistry(
      file.dir = regpath,
      source = "init.R",
      seed = 1,
      make.default = make.default
    )
  } else {
    loadRegistry(file.dir = regpath, writeable = TRUE, make.default = make.default)
  }
}