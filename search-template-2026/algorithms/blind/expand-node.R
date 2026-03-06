# =========================================================================
# NODE EXPANSION UTILITIES
# =========================================================================
# This file provides helper functions to expand nodes in the search tree.
#
# It supports two action representations:
#   1) Vector (recommended): actions_possible <- c("a", "b", "c")
#   2) data.frame/matrix: one action per row (possibly multi-column)
#
# IMPORTANT:
# - The search algorithms expect expand.node() and local.expand.node() to
#   return a list of successor nodes.
# - Node structure must remain consistent across the framework.
# -------------------------------------------------------------------------
#
# Authors / Maintainers:
#   - Roberto Carballedo
#   - Fernando Boto
#   - Enrique Onieva
#
# Last updated: February 2026
# Code generated & revised using: Gemini Pro.
#
# Educational use only — University of Deusto
# =========================================================================


# =========================================================================
# actions.length(actions_possible)
# =========================================================================
# PURPOSE:
#   Returns the number of available actions in actions_possible.
# =========================================================================
actions.length <- function(actions_possible) {
  if (is.null(actions_possible)) return(0)
  if (is.data.frame(actions_possible) || is.matrix(actions_possible)) return(nrow(actions_possible))
  return(length(actions_possible))
}


# =========================================================================
# actions.get(actions_possible, i)
# =========================================================================
# PURPOSE:
#   Returns action i from actions_possible, supporting vector and data.frame.
#
# NOTES:
#   - If actions_possible is a 1-column data.frame/matrix, the returned action
#     is converted to an atomic value (not a list), which avoids common errors.
# =========================================================================
actions.get <- function(actions_possible, i) {
  
  if (is.data.frame(actions_possible) || is.matrix(actions_possible)) {
    action <- actions_possible[i, , drop = FALSE]
    
    # If only one column, extract the single atomic value
    if (ncol(action) == 1) {
      return(action[[1]])
    }
    
    return(action)
  }
  
  # Vector case
  return(actions_possible[i])
}


# =========================================================================
# expand.node(node, actions_possible, problem)
# =========================================================================
# PURPOSE:
#   Generates all successor nodes from the given node (classical search).
#
# NOTES:
#   - successor$cost accumulates g(n)
#   - successor$evaluation stores h(n) (for informed algorithms)
# =========================================================================
expand.node <- function(node, actions_possible, problem) {
  
  successor_nodes <- list()
  num_actions <- actions.length(actions_possible)
  
  for (i in seq_len(num_actions)) {
    
    action <- actions.get(actions_possible, i)
    
    if (is.applicable(node$state, action, problem)) {
      
      successor <- list()
      successor$parent <- node
      successor$state  <- effect(node$state, action, problem)
      
      # Store actions as an atomic vector (recommended for student simplicity)
      successor$actions <- c(node$actions, action)
      
      successor$depth <- node$depth + 1
      
      # Accumulated cost g(n)
      step_cost <- get.cost(action, node$state, problem)
      successor$cost <- node$cost + step_cost
      
      # Heuristic evaluation h(n)
      successor$evaluation <- get.evaluation(successor$state, problem)
      
      successor_nodes <- append(successor_nodes, list(successor))
    }
  }
  
  return(successor_nodes)
}


# =========================================================================
# local.expand.node(node, actions_possible, problem)
# =========================================================================
# PURPOSE:
#   Generates all successor nodes for local search algorithms (e.g. hill climbing).
#
# NOTES:
#   - In local search, cost is NOT accumulated.
#   - successor$cost is stored only for consistency with the node structure.
# =========================================================================
local.expand.node <- function(node, actions_possible, problem) {
  
  successor_nodes <- list()
  num_actions <- actions.length(actions_possible)
  
  for (i in seq_len(num_actions)) {
    
    action <- actions.get(actions_possible, i)
    
    if (is.applicable(node$state, action, problem)) {
      
      successor <- list()
      successor$state  <- effect(node$state, action, problem)
      successor$evaluation <- get.evaluation(successor$state, problem)
      
      successor_nodes <- append(successor_nodes, list(successor))
    }
  }
  
  return(successor_nodes)
}