
library("mlr3")
library("mlr3learners")
library("paradox")
library("mlr3tuning")
library("mlr3pipelines")


data.table::setDTthreads(1)
lgr::get_logger("mlr3")$set_threshold("warn")
lgr::get_logger("bbotk")$set_threshold("info")

for (loading in c("assets", "algorithms")) {
  list.files(file.path("R", loading), "\\.r$", ignore.case = TRUE, full.names = TRUE) |>
    lapply(source, verbose = FALSE) |>
    invisible()
}
