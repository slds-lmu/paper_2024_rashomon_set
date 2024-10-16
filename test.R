

source("init.R")
library("testthat")

# invisible(lapply(list.files("tests", "\\.r$",
#   ignore.case = TRUE,
#   recursive = FALSE,
#   full.names = TRUE
# ), source))

test_dir("tests/")
