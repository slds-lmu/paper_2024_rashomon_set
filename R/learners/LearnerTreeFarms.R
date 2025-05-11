#' @title Classification TreeFarms Learner
#'
#' @description
#' An mlr3 learner for the TreeFarms algorithm, which finds optimal sparse
#' decision trees and can enumerate the Rashomon set of such trees.
#' Wraps the Python 'treefarms' package via reticulate.
#'
#' @details
#' The learner uses the `treefarms.libgosdt` Python module.
#' Important parameters:
#' * `selected_tree`: Which tree to use for prediction.
#'   Trees are (afaik) not ordered by performance.
#' * `regularization`: Controls the trade-off between accuracy and tree complexity (number of leaves).
#'   The smaller the regularization, the more trees are found; if the dataset has many features, this can lead to
#'   a combinatorial explosion of trees.
#' * `rashomon`: If `TRUE`, finds the Rashomon set (all trees within a certain objective range of the best).
#'   If `FALSE`, finds only the optimal tree.
#' * `rashomon_bound_multiplier`, `rashomon_bound`, `rashomon_bound_adder`:
#'   Control the size of the Rashomon set (mutually exclusive ways to define the bound).
#' * `time_limit`: Maximum time in seconds for the optimization.
#' * `depth_budget`: Maximum depth of the trees (0 for unlimited).
#'
#' @export
LearnerClassifTreeFarms <- R6::R6Class("LearnerClassifTreeFarms",
  inherit = LearnerClassif,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps <- ps(
        # Selected tree
        selected_tree = p_uty(tags = c("predict", "required"), init = "1", custom_check = mlr3misc::crate(function(x) {
          checkString(x, pattern = "^[1-9][0-9]*$")
        })), # which tree to use for prediction  # nolint
        # Key Parameters
        regularization = p_dbl(lower = 0, upper = 1, default = 0.05, tags = c("train", "treefarms")), # shoud be larger than 1 / #samples  # nolint
        rashomon = p_lgl(default = TRUE, tags = c("train", "treefarms")),  # if false, only fit best tree. `FALSE` DOES NOT APPEAR TO WORK  # nolint

        # Rashomon-specific configs
        rashomon_bound_multiplier = p_dbl(lower = 0, default = 0.05, tags = c("train", "treefarms")), # If set, uses this * (optimal_objective) as bound.  # nolint
        rashomon_bound = p_dbl(lower = 0, tags = c("train", "treefarms")), # Absolute objective bound
        rashomon_bound_adder = p_dbl(lower = 0, tags = c("train", "treefarms")), # Adder to optimal objective for bound

        # File Output (Generally not needed for mlr3 integration, but available)
        output_accuracy_model_set = p_lgl(default = FALSE, tags = c("train", "treefarms")),
        output_covered_sets = p_uty(default = list(), tags = c("train", "treefarms")), # Array of strings e.g., c('f1', 'bacc', 'auc')  # nolint
        covered_sets_thresholds = p_uty(default = list(), tags = c("train", "treefarms")), # Array of doubles
        rashomon_model_set_suffix = p_uty(default = "", tags = c("train", "treefarms")),
        rashomon_ignore_trivial_extensions = p_lgl(default = TRUE, tags = c("train", "treefarms")),
        rashomon_trie = p_uty(default = "", tags = c("train", "treefarms")), # Path to output trie file

        # Limits
        depth_budget = p_int(lower = 1L, default = 0L, special_vals = list(0L), tags = c("train", "treefarms")), # max tree depth, 0 = unlimited  # nolint
        tile_limit = p_int(lower = 1L, default = 0L, special_vals = list(0L), tags = c("train", "treefarms")), # Undocumented in README text, assuming 0 = default/special  # nolint
        precision_limit = p_int(lower = 1L, default = 0L, special_vals = list(0L), tags = c("train", "treefarms")), # 0 = no limit  # nolint
        stack_limit = p_int(lower = 1L, default = 0L, special_vals = list(0L), tags = c("train", "treefarms")), # 0 = use heap  # nolint
        minimum_captured_points = p_int(lower = 0L, default = 0L, tags = c("train", "treefarms")), # Undocumented in README text  # nolint

        # Tuners
        uncertainty_tolerance = p_dbl(lower = 0, upper = 1, default = 0.0, tags = c("train", "treefarms")),  # allow early termination  # nolint
        upperbound = p_dbl(lower = 0, upper = 1, default = 0.0, tags = c("train", "treefarms")), # limit risk of model search space (default 0.0 implies max possible risk)  # nolint

        # Execution Control
        time_limit = p_dbl(lower = 0, default = 0, tags = c("train", "treefarms")), # 0 = no limit
        worker_limit = p_int(lower = 1L, default = 1L, special_vals = list(0L), tags = c("train", "treefarms")), # 0 = #cores  # nolint

        # Flags
        balance = p_lgl(default = FALSE, tags = c("train", "treefarms")),  # sample importance: equal weights to all classes  # nolint
        cancellation = p_lgl(default = TRUE, tags = c("train", "treefarms")),  # propagate up the dependency graph of task cancellations  # nolint
        look_ahead = p_lgl(default = TRUE, tags = c("train", "treefarms")),  # one-step look-ahead bound via scopes  # nolint
        similar_support = p_lgl(default = TRUE, tags = c("train", "treefarms")),  # similar support bound via distance index  # nolint
        feature_exchange = p_lgl(default = FALSE, tags = c("train", "treefarms")),  # prune pairs of features using subset comparison  # nolint
        continuous_feature_exchange = p_lgl(default = FALSE, tags = c("train", "treefarms")),  # prune pairs of continuous features  # nolint
        feature_transform = p_lgl(default = TRUE, tags = c("train", "treefarms")),  # equivalence discovery through simple feature transformations  # nolint
        rule_list = p_lgl(default = FALSE, tags = c("train", "treefarms")),  # rule-list constraints on models  # nolint
        non_binary = p_lgl(default = FALSE, tags = c("train", "treefarms")),  # non-binary encoding (support for non-binary features?)  # nolint

        costs = p_uty(default = "", tags = c("train", "treefarms")), # input: cost matrix; must contain path to cost matrix CSV  # nolint

        # Output & Diagnostics
        diagnostics = p_lgl(default = FALSE, tags = c("train", "treefarms")),
        verbose = p_lgl(default = FALSE, tags = c("train", "treefarms")), # Defaulting to FALSE for less noisy mlr3 runs  # nolint

        # Low-Level File Output (Paths - generally avoid in mlr3 unless needed for specific debugging)
        model = p_uty(default = "", tags = c("train", "treefarms")), # Path for output model file
        profile = p_uty(default = "", tags = c("train", "treefarms")), # Path for analytics log file
        timing = p_uty(default = "", tags = c("train", "treefarms")), # Path for timing log file
        trace = p_uty(default = "", tags = c("train", "treefarms")), # Path for trace visualization dir
        tree = p_uty(default = "", tags = c("train", "treefarms")), # Path for trace-tree visualization dir
        datatset_encoding = p_uty(default = "", tags = c("train", "treefarms")), # Undocumented in README text
        memory_checkpoints = p_uty(default = list(), tags = c("train", "treefarms")), # Undocumented in README text
        model_limit = p_int(lower = 1, default = 10000, special_vals = list(0L), tags = c("train", "treefarms")) # Undocumented  # nolint

      )

      super$initialize(
        id = "classif.treefarms",
        param_set = ps,
        predict_types = c("response", "prob"),
        feature_types = c("logical", "integer", "numeric"), # actually, only 0 / 1 features are supported
        properties = "twoclass", # TODO need to check whether multiclass is supported
        packages = c("reticulate", "jsonlite", "utils", "partykit"),
        label = "TreeFarms Sparse Decision Tree"
      )
    },

    #' @description
    #' Sample a tree from the model container.
    #' Handles the case where the tree count is a Python big integer.
    #'
    #' Not efficient, but (hopefully) correct.
    #' @return A tree from the model container.
    sampleTreeIndex = function() {
      treecount <- self$tree.count
      if (treecount == "0") stop("Model was not fitted.")
      assertString(treecount, pattern = "^[0-9]+$")
      firstdigit <- as.integer(substr(treecount, 1, 1)) + 1L
      zerosample <- strrep("0", nchar(treecount))
      repeat {
        sample.first <- sample.int(firstdigit + 1L, 1) - 1L
        sample.others <- sample.int(10, nchar(treecount) - 1L) - 1L
        sample <- paste(as.character(c(sample.first, sample.others)), collapse = "")
        if (sample <= treecount && sample != zerosample) break
      }
      sub("^0+", "", sample)  # we return the string!
    }
  ),

  active = list(
    modelcontainer = function() {
      # .modelcontainer is NULL after initialization or cloning, and may be a 0-externalptr after deserialization
      # We also check that self$model$result was not changed
      if (is.null(private$.modelcontainer) || reticulate::py_is_null_xptr(private$.modelcontainer) ||
          is.null(private$.modelcontainer.id) || private$.modelcontainer.id != address(self$model$result)) {
        if (is.null(self$model)) return(NULL)  # not trained yet
        # recreate model container from stored JSON

        js <- reticulate::py_call(reticulate::import("json")$loads, self$model$result)
        private$.modelcontainer <- reticulate::import("treefarms.model.model_set")$ModelSetContainer(js)
        private$.modelcontainer.id <- address(self$model$result)
        private$.partycache <- NULL  # reset cached party object
      }
      return(private$.modelcontainer)
    },
    tree.count = function() {
      if (is.null(self$modelcontainer)) return("0")
      # the following could be a python big integer with arbitrary number of digits, so we convert to string
      as.character(reticulate::py_call(self$modelcontainer$get_tree_count))
    }
  ),

  private = list(
    # cached model container that needs to be recreated after cloning / serialization etc.
    .modelcontainer = NULL,
    .modelcontainer.id = NULL,
    .partycache = NULL,
    .partycache.tree = NULL,
    deep_clone = function(name, value) {
      if (name == ".modelcontainer" || name == ".partycache") {
        return(NULL)
      }
      super$deep_clone(name, value)
    },
    .train = function(task) {
      private$.modelcontainer <- NULL  # reset cached model container
      private$.partycache <- NULL

      reticulate::py_set_seed(sample.int(.Machine$integer.max, 1))
      # Import Python modules
      tf.module <- reticulate::import("treefarms.libgosdt")

      # Get parameters
      defaults <- self$param_set$default

      pv <- self$param_set$get_values(tags = c("train", "treefarms"))

      # Handle mutually exclusive Rashomon bounds (error if more than one is set)
      mutex <- c("rashomon_bound_multiplier", "rashomon_bound", "rashomon_bound_adder")

      if (sum(mutex %in% names(pv)) > 1) {
        stop("Only one of 'rashomon_bound_multiplier', 'rashomon_bound', or 'rashomon_bound_adder' can be set.")
      }

      if (any(mutex %in% names(pv))) {
        # if any of the rashomon bounds are given, we don't do default behaviour
        # instead, we need to set this to 0 to overwrite C++ internal value from last invocation
        defaults$rashomon_bound_multiplier <- 0
      }
      defaults$rashomon_bound_adder <- 0
      defaults$rashomon_bound <- 0

      if ("rashomon_bound" %in% names(pv) && pv$rashomon_bound == 0) {
        stop("For an implementation specific reason, rashomon_bound can not be 0.")
      }

      pv <- mlr3misc::insert_named(defaults, pv)  # treefarms CPP backend is stateful, so need to reset to defaults

      # Convert vectorized parameters to lists so they appear as JSON lists
      vector.params <- c("output_covered_sets", "covered_sets_thresholds", "memory_checkpoints")
      for (param in vector.params) {
        if (param %in% names(pv)) {
           pv[[param]] <- as.list(pv[[param]])
        }
      }


      # Create JSON configuration
      pv.json <- jsonlite::toJSON(pv, auto_unbox = TRUE)

      # Prepare data
      # TreeFarms expects features first, then target column
      # We handle numeric/logical but need to convert to numeric since we don't want TRUE/FALSE matrix
      feats <- as.matrix(task$data(cols = task$feature_names))
      mode(feats) <- "numeric"
      colnames(feats) <- paste0("V", seq_len(ncol(feats)))
      if (!all(feats %in% c(0, 1))) {
        stop("TreeFarms only supports binary features (0/1 or FALSE/TRUE).")
      }
      target <- task$data(cols = task$target_names)[[1]]
      data <- as.data.table(cbind(feats, target = as.integer(target) - 1))

      # Generate CSV string - TreeFarms fit() expects a string
      data.csv <- paste(utils::capture.output(fwrite(data)), collapse = "\n")

      # Configure and fit
      tf.module$configure(pv.json)
      timing <- system.time({tf.result <- tf.module$fit(data.csv)}, gcFirst = FALSE)
      tf.status <- tf.module$status() # 0 = success, 2 = timeout, other = error

      info <- list(timing.raw = timing, timing = timing, result = tf.result, timeout = FALSE, config = pv.json,
        target.levels = levels(target),
        data = if (self$predict_type == "prob") {
          data
        }
      )

      if (tf.status == 2) {
        # timeout
        # Store minimal info, maybe indicate timeout?
        info$timeout <- TRUE
        info$timing[] <- -1
      } else if (tf.status != 0) {
         stop(sprintf("TreeFarms training failed with status %d. Result:\n%s", tf.status, tf.result))
      }

      info  # model
    },

    .predict = function(task) {

      pv <- self$param_set$get_values(tags = "predict")

      # Prepare newdata
      newdata <- task$data(cols = task$feature_names)

      # Check if any trees were found
      treecount <- self$tree.count
      selected.tree <- pv$selected_tree
      ndigits <- nchar(treecount)
      selected.tree.padded <- chartr(" ", "0", sprintf("%*s", ndigits, selected.tree))

      if (ndigits < nchar(selected.tree.padded) || selected.tree.padded > treecount) {
        stop(sprintf("Selected tree index %d is out of range (only %d trees found).", selected_tree, treecount))
      }

      selected.tree.pyint <- reticulate::py_eval(paste0(selected.tree, "-1"), convert = FALSE)

      pred.prob <- NULL

      predictions <- self$modelcontainer$get_tree_at_idx(selected.tree.pyint)$predict(newdata)
      pred.response <- factor(predictions, levels = c(0L, 1L), labels = self$model$target.levels)

      if (self$predict_type == "prob") {
        if (is.null(self$model$data)) {
          stop("Training data not available. Please train with predict_type = 'prob'.")
        }
        if (is.null(private$.partycache) || is.null(private$.partycache.tree) ||
            private$.partycache.tree != pv$selected_tree) {
          private$.partycache <- jsonToParty(self$modelcontainer$get_tree_at_idx(selected.tree.pyint)$json())
          private$.partycache.tree <- pv$selected_tree
          private$.partycache$transformtbl <- copy(self$model$data)[,
            node := partykit::fitted_node(private$.partycache$tree, .SD), .SDcols = patterns("^V[0-9]+$")][,
            .(score = mean(target == 0)), keyby = node
          ]
        }
        colnames(newdata) <- paste0("V", seq_len(ncol(newdata)))
        nodes <- partykit::fitted_node(private$.partycache$tree, newdata)
        pred.prob.class1 <- private$.partycache$transformtbl[J(nodes), on = .(node), score]
        pred.prob <- cbind(pred.prob.class1, 1.0 - pred.prob.class1)
        colnames(pred.prob) <- self$model$target.levels
      }

      list(response = pred.response, prob = pred.prob)
    }
  )
)
