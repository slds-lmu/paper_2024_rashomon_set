

test_that("Everything that starts with 'task.' is in list.tasks", {
  all.tasks <- mget(ls(pattern = "^task\\.", envir = .GlobalEnv), envir = .GlobalEnv)
  all.tasks <- all.tasks[vapply(all.tasks, inherits, logical(1), "Task")]
  all.tasks <- all.tasks[names(all.tasks) != "task.bh"]  # task.bh only used for tests, not in list.tasks
  expect_names(names(list.tasks), permutation.of = sub("^task\\.", "", names(all.tasks)))
  for (task.name in names(list.tasks)) {
    expect_identical(list.tasks[[task.name]], all.tasks[[paste0("task.", task.name)]])
  }
})
