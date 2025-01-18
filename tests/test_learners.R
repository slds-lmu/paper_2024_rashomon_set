


# learners sanity check:
# do regr / classif learners have the same search space?

test_that("Regression and classification learners have the same names", {
  expect_equal(
    sort(names(list.learners.regr)),
    sort(names(list.learners.classif)),
    info = "Names of regression and classification learners should match"
  )
})

test_that("Corresponding regression and classification learners have the same search space", {
  for (learner_name in names(list.learners.regr)) {
    regr.learner <- list.learners.regr[[learner_name]]
    classif.learner <- list.learners.classif[[learner_name]]

    expect_equal(
      regr.learner$param_set$search_space(),
      classif.learner$param_set$search_space(),
      info = paste("Search space mismatch for learner:", learner_name)
    )
  }
})

test_that("All learners have a non-empty search space", {
  all.learners <- c(list.learners.regr, list.learners.classif)

  for (learner.name in names(all.learners)) {
    learner <- all.learners[[learner.name]]
    search.space <- learner$param_set$search_space()

    expect_true(
      length(search.space$ids()) > 0,
      info = paste("Learner", learner.name, "has an empty search space")
    )
  }
})

test_that("All learners can be resampled on all datasets", {
  # Split into regression and classification tasks
  regr.tasks <- list.tasks[vapply(list.tasks, function(t) t$task_type == "regr", logical(1))]
  classif.tasks <- list.tasks[vapply(list.tasks, function(t) t$task_type == "classif", logical(1))]

  # Create holdout resampling
  holdout <- rsmp("holdout", ratio = 0.5)

  # Function to test a single learner on a task
  testLearner <- function(task, learner.name, learner) {
    learner <- learner$clone(deep = TRUE)
    config <- generate_design_grid(learner$param_set$search_space(), 1)$transpose()[[1]]
    learner$param_set$set_values(.values = config)
    rr <- expect_error(
      resample(task, learner, holdout),
      NA,
      info = sprintf("%s learner %s failed on task %s",
        task$task_type, learner.name, task$id)
    )
    measure <- if (task$task_type == "regr") measure.regr else measure.classif
    expect_number(
      rr$aggregate(measure),
      info = sprintf("%s learner %s produced NA/non-scalar score on task %s",
        task$task_type, learner.name, task$id)
    )
  }

  # Test regression learners
  for (task in regr.tasks) {
    for (learner.name in names(list.learners.regr)) {
      testLearner(task, learner.name, list.learners.regr[[learner.name]])
    }
  }

  # Test classification learners
  for (task in classif.tasks) {
    for (learner.name in names(list.learners.classif)) {
      testLearner(task, learner.name, list.learners.classif[[learner.name]])
    }
  }
})
