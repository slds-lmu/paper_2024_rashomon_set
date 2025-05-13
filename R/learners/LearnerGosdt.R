#' @title Classification GOSDT Learner
#'
#' @description
#' An mlr3 learner for the Generalized Optimal Sparse Decision Tree (GOSDT) algorithm.
#' Wraps the Python 'gosdt' package via reticulate, specifically the `GOSDTClassifier`.
#' GOSDT finds the optimal sparse decision tree for a dataset.
#'
#' @details
#' The learner interfaces with the Python `GOSDTClassifier` class.
#' Key hyperparameters:
#' * `regularization`: Controls the trade-off between accuracy (loss) and tree complexity (number of leaves).
#'   A value of 0 optimizes solely for accuracy. Higher values encourage simpler trees.
#' * `depth_budget`: Maximum depth allowed for the decision tree.
#' * `time_limit`: Maximum time in seconds for the optimization process. 0 means no limit.
#' * `allow_small_reg`: If `FALSE` (default), throws an error if regularization is too small relative to dataset size,
#'   which can lead to very long runtimes. Set to `TRUE` to override this check.
#' * `verbose`: Controls the level of detail printed by the GOSDT algorithm during training.
#'
#' The learner currently expects binary features (encoded as 0/1 numeric or logical) and a two-class factor target variable.
#'
#' @export
LearnerClassifGosdt <- R6::R6Class("LearnerClassifGosdt",
  inherit = LearnerClassif,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps <- ps(
        # Core GOSDT Parameters
        regularization = p_dbl(lower = 0, upper = 0.5, default = 0.05, tags = c("train", "gosdt")), # Complexity penalty. Segfaults may occur when > 0.5  # nolint
        depth_budget = p_int(lower = 0L, tags = c("train", "gosdt")), # Max depth, 0 = just root node
        time_limit = p_dbl(lower = 0, tags = c("train", "gosdt")), # Max time in seconds
        allow_small_reg = p_lgl(default = FALSE, tags = c("train", "gosdt")), # Allow potentially slow runs with regularization < 1 / nrow  # nolint

        balance = p_lgl(default = FALSE, tags = c("train", "gosdt")), # Balance class weights

        cancellation = p_lgl(default = TRUE, tags = c("train", "gosdt")), # Enable task cancellation
        look_ahead = p_lgl(default = TRUE, tags = c("train", "gosdt")), # Use look-ahead bound
        similar_support = p_lgl(default = TRUE, tags = c("train", "gosdt")), # Use similar support bound
        rule_list = p_lgl(default = FALSE, tags = c("train", "gosdt")), # Use rule list constraints

        uncertainty_tolerance = p_dbl(lower = 0, default = 0, tags = c("train", "gosdt")), # Precision limit for objective  # nolint
        upperbound_guess = p_dbl(lower = 0, upper = 1, tags = c("train", "gosdt")), # Initial upper bound on objective
        worker_limit = p_int(lower = 1L, default = 1L, special_vals = list(0L), tags = c("train", "gosdt")), # Max threads, 0 = auto (?)  # nolint

        debug = p_lgl(default = FALSE, tags = c("train", "gosdt")), # Enable debug mode
        diagnostics = p_lgl(default = FALSE, tags = c("train", "gosdt")), # Enable diagnostics
        verbose = p_lgl(default = FALSE, tags = c("train", "gosdt")) # Control verbosity
      )

      super$initialize(
        id = "classif.gosdt",
        param_set = ps,
        predict_types = c("response", "prob"),
        feature_types = c("logical", "integer", "numeric"), # Expecting 0/1 features
        properties = "twoclass",
        packages = c("reticulate", "data.table", "mlr3misc", "jsonlite", "checkmate"),
        label = "GOSDT Optimal Sparse Decision Tree"
      )
    }
  ),

  private = list(
    .train = function(task) {

      reticulate::py_set_seed(sample.int(.Machine$integer.max, 1)) # Set Python seed

      # Import the necessary Python class
      gosdt.mod <- reticulate::import("gosdt", delay_load = TRUE)
      GOSDTClassifier <- gosdt.mod$GOSDTClassifier

      # --- Parameter Handling ---
      pv <- self$param_set$get_values(tags = c("train", "gosdt"))

      feats.dt <- task$data(cols = task$feature_names)
      colnames(feats.dt) <- paste0("V", seq_len(ncol(feats.dt)))
      feats <- as.matrix(feats.dt)

      if (!all(feats %in% c(0, 1))) {
        # TODO: note: gosdt may be able to handle continuous features,
        # although this leads to combinatorial explosion in many cases.
        # May want to support this in the future.
        stop("Learner 'classif.gosdt' expects binary features (0/1). Non-binary features detected. GOSDT might fail or produce unexpected results.")  # nolint
      }

      mode(feats) <- "integer"

      target <- task$data(cols = task$target_names)[[1]]
      y <- as.integer(target) - 1L

      classifier <- do.call(GOSDTClassifier, pv)

      # Fit the model
      classifier$fit(feats, y)

      result <- classifier$get_result()

      result$status <- result$status$name

      result$party.list <- jsonToParty(result$models_string)

      result$party.list$transformtbl <- copy(feats.dt)[, y.col := y][,
        node := partykit::fitted_node(result$party.list$tree, .SD), .SDcols = patterns("^V[0-9]+$")][,
        .(score = mean(y.col == 0)), keyby = node
      ]

      result$target.levels <- levels(target)

      result
    },

    .predict = function(task) {
      # --- Prepare newdata ---
      newdata.dt <- task$data(cols = task$feature_names)

      colnames(newdata.dt) <- paste0("V", seq_len(ncol(newdata.dt)))

      # --- Prediction --- #
      pred.response <- NULL
      pred.prob <- NULL

      pred.nodes <- partykit::fitted_node(self$model$party.list$tree, newdata.dt)
      # Response Prediction
      if ("response" %in% self$predict_types) {
        pred.response <- factor(self$model$party.list$predictions[pred.nodes],
          levels = c(0L, 1L), labels = self$model$target.levels)
      }

      # Probability Prediction
      if (self$predict_type == "prob") {
        prob.lookup <- self$model$party.list$transformtbl[J(pred.nodes), on = .(node), score]

        pred.prob <- cbind(prob.lookup, 1.0 - prob.lookup)
        colnames(pred.prob) <- self$model$target.levels
      }

      return(list(response = pred.response, prob = pred.prob))
    }
  )
)