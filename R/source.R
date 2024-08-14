data.table::setDTthreads(1)
lgr::get_logger("mlr3")$set_threshold("warn")
lgr::get_logger("bbotk")$set_threshold("info")

for (loading in c("assets")) {
  list.files(file.path("R", loading), "\\.r$", ignore.case = TRUE, full.names = TRUE) |>
    lapply(source, verbose = FALSE) |>
    invisible()
}
