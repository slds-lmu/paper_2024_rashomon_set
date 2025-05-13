

#' Convert Tree JSON to partykit::constparty Object
jsonToParty <- function(json) {

  assertString(json)

  # ---- Load JSON ----
  tree.list <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  if (length(tree.list) == 1 && is.null(names(tree.list))) {
    tree.list <- tree.list[[1]]
  }

  countLeaves <- function(tree.list) {
    if (testList(tree.list$true) && testList(tree.list$false)) {
      countLeaves(tree.list$true) + countLeaves(tree.list$false)
    } else {
      1L
    }
  }

  n.leaves <- countLeaves(tree.list)

  # ---- ID generator (simple sequential IDs are sufficient) ----
  IdGenerator <- R6::R6Class("IdGenerator",
    public = list(
      i.terminal = 0L,
      i.nonterminal = n.leaves,
      next.id = function(terminal) {
        if (terminal) {
          self$i.terminal <- self$i.terminal + 1L
          self$i.terminal
        } else {
          self$i.nonterminal <- self$i.nonterminal + 1L
          self$i.nonterminal
        }
      }
    )
  )

  id.generator <- IdGenerator$new()

  predictions <- integer(n.leaves)

  # ---- Internal helper: build a partynode recursively ----
  buildNode <- function(node) {
    node.id <- id.generator$next.id(!is.null(node$prediction))

    ## Leaf Node -------------------------------------------------------------
    if (!is.null(node$prediction)) {
      # Basic validation
      assertInt(node$prediction, lower = 0, upper = 1, tol = 0)

      predictions[node.id] <<- node$prediction  # nolint

      # Store prediction in info slot. Use a consistent name e.g., "prediction".
      return(partykit::partynode(id = node.id))
    }

    ## Internal Node ---------------------------------------------------------
    # Basic validation for internal nodes
    assertInt(node$feature, lower = 0, tol = 0)
    assertCharacter(node$relation, len = 1, pattern = "^==$", null.ok = TRUE)
    assertCharacter(node$reference, len = 1, pattern = "^true$", null.ok = TRUE)  # can't handle anything else rn
    assertList(node$true)
    assertList(node$false)

    node.split <- partykit::partysplit(
      varid = as.integer(node$feature) + 1L,  # Convert 0-based JSON index to 1-based R varid
      breaks = 0.5
    )

    # Recursively build child nodes (ensure consistent order: false, true)
    node.kids <- list(
      buildNode(node$`false`),   # Child 1
      buildNode(node$`true`)     # Child 2
    )

    partykit::partynode(id = node.id, split = node.split, kids  = node.kids)
  } # end buildNode

  # ---- Build the tree structure ----
  list(tree = buildNode(tree.list), predictions = predictions)
}

