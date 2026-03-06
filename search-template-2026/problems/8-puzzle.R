# =========================================================================
# PROBLEM FORMULATION
# =========================================================================
# State representation:
#   A matrix (rows x columns) with values 0..8. '0' represents the blank tile.
#
# Actions:
#   Move the blank tile in one of the four directions.
#   A vector of moves: c("Up","Down","Left","Right").
#
# Transition model:
#   Swap the blank tile with the adjacent tile in the chosen direction.
#
# Constraints:
#   Moves that go out of the board are not applicable.
#
# Goal test:
#   The state equals the goal configuration (problem$state_final).
#
# Cost function:
#   Uniform cost: each move costs 1.
#
# Heuristic (if applicable):
#   Provided as an evaluation function (e.g., misplaced tiles, Manhattan distance).
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
# initialize.problem(...)
# =========================================================================
# PURPOSE:
#   Builds and returns the "problem" object, which contains everything the
#   algorithms need to run on this specific instance.
#
# INPUT:
#   rows          : number of rows (default 3)
#   columns       : number of columns (default 3)
#   initial_state : vector with the initial permutation of tiles
#
# OUTPUT:
#   problem (list) with compulsory fields:
#     - problem$name
#     - problem$state_initial
#     - problem$state_final (target matrix; goal checked via is.final.state)
#     - problem$actions_possible
#
# NOTES:
#   - Search algorithms DO NOT call this function. The main script does.
# =========================================================================
initialize.problem <- function(rows = 3, columns = 3, initial_state = sample(0 : (rows * columns - 1))) {
  problem <- list()

  # Compulsory attributes
  problem$name              <- "8-Puzzle"
  problem$state_initial     <- matrix(initial_state, nrow = rows, byrow = TRUE)
  problem$state_final       <- matrix(0:(rows * columns - 1), nrow = rows, byrow = TRUE)
  # Actions: Move the blank tile in one of the four directions.
  problem$actions_possible  <- c("Up", "Down", "Left", "Right")

  # Additional attributes
  problem$rows              <- rows
  problem$columns           <- columns

  return(problem)
}

# =========================================================================
# is.applicable(state, action, problem)
# =========================================================================
# PURPOSE:
#   Returns TRUE if the given action can be applied in the given state.
#
# INPUT:
#   state   : current state (vector of length 9)
#   action  : one action (move blank tile Up, Down, Left, Right)
#   problem : problem definition
#
# OUTPUT:
#   TRUE  -> the action is valid in this state
#   FALSE -> the action is not valid in this state
# =========================================================================
is.applicable <- function (state, action, problem) {
  # Get the location of "blank" tile
  where <- which(state == 0, arr.ind = TRUE)
  row <- where[1]
  col <- where[2]

  # Explicit return is no needed: in R the last sentence is returned automatically.
  switch(action,
         "Up"    = row != 1,
         "Down"  = row != problem$rows,
         "Left"  = col != 1,
         "Right" = col != problem$columns,
         FALSE
  )
}

# =========================================================================
# effect(state, action, problem)
# =========================================================================
# PURPOSE:
#   Applies the action to the state and returns the successor state.
#
# INPUT:
#   state   : current state
#   action  : action to apply (must be applicable)
#   problem : problem definition
#
# OUTPUT:
#   successor state (same representation type as 'state')
#
# IMPORTANT:
#   - This function must NOT modify the original state in-place.
#   - It must also be deterministic.
# =========================================================================
effect <- function(state, action, problem) {
  result <- state
  
  where <- which(state == 0, arr.ind = TRUE)
  row <- where[1]
  col <- where[2]
  
  return(
    switch(action,
           
           "Up" = {
             result[row-1, col] <- state[row, col]
             result[row, col]   <- state[row-1, col]
             result
           },
           
           "Down" = {
             result[row+1, col] <- state[row, col]
             result[row, col]   <- state[row+1, col]
             result
           },
           
           "Left" = {
             result[row, col-1] <- state[row, col]
             result[row, col]   <- state[row, col-1]
             result
           },
           
           "Right" = {
             result[row, col+1] <- state[row, col]
             result[row, col]   <- state[row, col+1]
             result
           },
           
           # Default (unknown action)
           result
    )
  )
}

# =========================================================================
# is.final.state(state, final_state, problem)
# =========================================================================
# PURPOSE:
#   Returns TRUE if the given state is a goal state.
#
# INPUT:
#   state       : current state
#   final_state : goal description (may be NULL in some problems)
#   problem     : problem definition
#
# OUTPUT:
#   TRUE  -> state satisfies goal condition
#   FALSE -> not a goal state
#
# NOTE:
#   Many problems do not have a predefined final_state object; instead,
#   the goal condition is checked directly using the state.
# =========================================================================
is.final.state <- function(state, final_state, problem) {
  return(all(state == problem$state_final))
}

# =========================================================================
# to.string(state, problem)
# =========================================================================
# PURPOSE:
#   Converts a state into a unique string representation.
#   We transpose it first to ensure row-major reading order (0, 1, 2, 3...)
#   instead of R's default column-major order.
#
# OUTPUT:
#   A string that uniquely identifies the state.
# =========================================================================
to.string <- function(state, problem) {
  return(paste(t(state), collapse = ","))
}

# =========================================================================
# get.cost(action, state, problem)
# =========================================================================
# PURPOSE:
#   Returns the step cost of applying an action in the given state.
#
# INPUT:
#   action  : the action being applied
#   state   : the current state (before applying the action)
#   problem : problem definition
#
# OUTPUT:
#   Numeric cost of the action (NOT accumulated).
# =========================================================================
get.cost = function (action, state, problem) {
  return(1)
}

# =========================================================================
# get.evaluation(state, problem)
# =========================================================================
# PURPOSE:
#   Heuristic function h(n) used by informed search algorithms (Greedy, A*, etc.).
#
# INPUT:
#   state   : current state
#   problem : problem definition
#
# OUTPUT:
#   A numeric estimate of how far the state is from the goal.
# =========================================================================
get.evaluation <- function(state, problem) {
  # Number of misplaced tiles (excluding the blank tile)
  # We ignore the blank tile (0) because it does not represent a numbered tile that must be placed.
  return(sum(state != problem$state_final & state != 0))
}
