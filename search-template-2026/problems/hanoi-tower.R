# =========================================================================
# PROBLEM FORMULATION
# =========================================================================
# State representation:
#   An INTEGER VECTOR of length 'n_disks'.
#   - The index 'i' represents the disk size (1=smallest, n=largest).
#   - The value 'state[i]' represents the PEG number where that disk is.
#   - Example: c(1, 1, 1) -> All 3 disks are on Peg 1.
#
# Actions:
#   A vector of moves encoded as "from-to" (e.g., "1-3").
#
# Transition model:
#   Move the smallest disk from the source peg to the target peg.
#
# Constraints:
#   Cannot place a larger disk on top of a smaller disk.
#   (In vector terms: moving disk index must be < target top disk index).
#
# Goal test:
#   All elements in the state vector equal the goal peg index.
#
# Cost function:
#   Uniform cost: each move costs 1.
#
# Heuristic (if applicable):
#   Number of disks NOT on the goal peg.
#
# Notes for Graph Search (to.string):
#   The state vector is flattened to a comma-separated string.
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
# initialize.problem(n_pegs, n_disks)
# =========================================================================
# PURPOSE:
#   Builds and returns the "problem" object, which contains everything the
#   algorithms need to run on this specific instance.
#
# INPUT:
#   n_pegs        : number of pegs (supports)
#   n_disks       : number of disks
#
# OUTPUT:
#   problem (list) with compulsory fields:
#     - problem$name
#     - problem$state_initial
#     - problem$state_final (Contains the target vector)
#     - problem$actions_possible
#
# NOTES:
#   - Assumes Start Peg = 1 and Goal Peg = n_pegs.
# =========================================================================
initialize.problem <- function(n_pegs = 3, n_disks = 3) {
  
  problem <- list()
  
  # Compulsory attributes
  problem$name <- paste0("Hanoi Tower (pegs=", n_pegs,
                         ", disks=", n_disks, ")")
  
  # Constants (Implicit)
  peg_initial <- 1
  peg_final   <- n_pegs
  
  # Initial state: Vector of length n_disks, all values = peg_initial
  # (Disks 1..n are all on peg 1)
  problem$state_initial <- rep(peg_initial, n_disks)
  
  # Final state: Vector of length n_disks, all values = peg_final
  # (Disks 1..n are all on peg n_pegs)
  problem$state_final <- rep(peg_final, n_disks)
  
  # Actions: all possible "from-to" moves (excluding from == to)
  actions <- c()
  for (from in 1:n_pegs) {
    for (to in 1:n_pegs) {
      if (from != to) actions <- c(actions, paste0(from, "-", to))
    }
  }
  problem$actions_possible <- actions
  
  # Additional attributes (kept for reference or heuristics)
  problem$n_pegs <- n_pegs
  problem$n_disks <- n_disks
  problem$peg_final <- peg_final # Helper for heuristics
  
  return(problem)
}

# =========================================================================
# is.applicable(state, action, problem)
# =========================================================================
# PURPOSE:
#   Returns TRUE if the action can be legally applied in the given state.
#
# INPUT:
#   state   : current state (integer vector)
#   action  : one action, encoded as "from-to"
#   problem : problem definition
#
# OUTPUT:
#   TRUE  -> action can be applied
#   FALSE -> action is not allowed in this state
# =========================================================================
is.applicable <- function(state, action, problem) {
  
  # Safety: Unwrap if state is nested in a list (search algo artifact)
  if (is.list(state)) state <- state[[1]]
  
  # Decode action "from-to"
  parts <- as.integer(strsplit(action, "-")[[1]])
  from  <- parts[1]
  to    <- parts[2]
  
  # Find disks on source peg
  # In this representation, disk size = index. 
  # The "top" disk is the one with the MINIMUM index (smallest size).
  disks_on_from <- which(state == from)
  
  # If source peg is empty, cannot move
  if (length(disks_on_from) == 0) return(FALSE)
  
  top_disk_from <- min(disks_on_from)
  
  # Find disks on target peg
  disks_on_to <- which(state == to)
  
  # If target peg is empty, move is always allowed
  if (length(disks_on_to) == 0) return(TRUE)
  
  top_disk_to <- min(disks_on_to)
  
  # Constraint: Cannot place larger disk on smaller disk
  # (Since index = size, we just check indices)
  return(top_disk_from < top_disk_to)
}

# =========================================================================
# effect(state, action, problem)
# =========================================================================
# PURPOSE:
#   Applies the action to the state and returns the successor state.
#
# INPUT:
#   state   : current state (integer vector)
#   action  : action to apply
#   problem : problem definition
#
# OUTPUT:
#   successor state (same representation type as 'state')
#
# IMPORTANT:
#   - This function assumes the action is applicable.
# =========================================================================
effect <- function(state, action, problem) {
  
  # Safety: Unwrap if state is nested
  if (is.list(state)) state <- state[[1]]
  
  result <- state # Copy vector (fast)
  
  # Decode action "from-to"
  parts <- as.integer(strsplit(action, "-")[[1]])
  from  <- parts[1]
  to    <- parts[2]
  
  # Identify disk to move: The smallest index (top disk) on 'from' peg
  disk_to_move <- min(which(state == from))
  
  # Update the position of that disk
  result[disk_to_move] <- to
  
  return(result)
}

# =========================================================================
# is.final.state(state, final_state, problem)
# =========================================================================
# PURPOSE:
#   Returns TRUE if the given state is a goal state.
#
# INPUT:
#   state       : current state
#   final_state : goal state vector (pre-computed in initialize)
#   problem     : problem definition
# =========================================================================
is.final.state <- function(state, final_state, problem) {
  
  # Safety: Unwrap if state is nested
  if (is.list(state)) state <- state[[1]]
  
  # Direct vector comparison
  if (!is.null(final_state)) {
    return(all(state == final_state))
  } else {
    return(all(state == problem$state_final))
  }
}

# =========================================================================
# to.string(state, problem)
# =========================================================================
# PURPOSE:
#   Converts a state into a unique string representation.
#
# IMPORTANT:
#   Must encode the full configuration of all pegs.
# =========================================================================
to.string <- function(state, problem) {
  if (is.list(state)) state <- state[[1]]
  return(paste(state, collapse = ","))
}

# =========================================================================
# get.cost(action, state, problem)
# =========================================================================
# PURPOSE:
#   Returns the step cost of applying an action in the given state.
# =========================================================================
get.cost <- function(action, state, problem) {
  return(1)
}

# =========================================================================
# get.evaluation(state, problem)
# =========================================================================
# PURPOSE:
#   Heuristic function h(n) used by informed search algorithms.
#
# NOTE:
#   Number of disks not yet on the goal peg (lower is better).
# =========================================================================
get.evaluation <- function(state, problem) {
  if (is.list(state)) state <- state[[1]]
  
  goal_peg <- problem$peg_final
  
  # Count disks that are NOT on the goal peg
  return(sum(state != goal_peg))
}