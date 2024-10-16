


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
    regr_learner <- list.learners.regr[[learner_name]]
    classif_learner <- list.learners.classif[[learner_name]]

    expect_equal(
      regr_learner$param_set$search_space(),
      classif_learner$param_set$search_space(),
      info = paste("Search space mismatch for learner:", learner_name)
    )
  }
})

test_that("All learners have a non-empty search space", {
  all_learners <- c(list.learners.regr, list.learners.classif)

  for (learner_name in names(all_learners)) {
    learner <- all_learners[[learner_name]]
    search_space <- learner$param_set$search_space()

    expect_true(
      length(search_space$ids()) > 0,
      info = paste("Learner", learner_name, "has an empty search space")
    )
  }
})

