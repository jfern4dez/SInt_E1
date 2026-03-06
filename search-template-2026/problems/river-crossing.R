# =========================================================================
# PROBLEM FORMULATION
# =========================================================================
# State representation:
#   A logical vector of length 4: c(farmer, wolf, goat, cabbage)
#   FALSE = left/origin river bank, TRUE = right/destination bank.
#
# Actions:
#   A vector of possible crossings: c("none", "wolf", "goat", "cabbage")
#
# Transition model:
#   Farmer always crosses. Optionally, exactly one item (or none) crosses with him.
#
# Constraints:
#   Unsafe states are not allowed:
#     - Goat eats Cabbage if left alone together (without Farmer).
#     - Wolf eats Goat if left alone together (without Farmer).
#
# Goal test:
#   All elements on the right bank: c(TRUE, TRUE, TRUE, TRUE).
#
# Cost function:
#   Uniform cost: each crossing costs 1.
#
# Heuristic (if applicable):
#   Number of items remaining on the left bank (minimize).
#
# Notes for Graph Search (to.string):
#   IMPORTANT: to.string must uniquely encode the whole state.
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
#   (none)
#
# OUTPUT:
#   problem (list) with compulsory fields:
#     - problem$name
#     - problem$state_initial (all on the left bank = FALSE)
#     - problem$state_final (all on the right bank = TRUE)
#     - problem$actions_possible (c("none", "wolf", "goat", "cabbage"))
#
# NOTES:
#   - Search algorithms DO NOT call this function. The main script does.
# =========================================================================
initialize.problem <- function() {
  problem <- list()
  
  # Name of the problem
  problem$name <- "River Crossing"
  
  # Actions represent who crosses with the farmer (or none)
  problem$actions_possible <- c("none", "wolf", "goat", "cabbage")
  
  # Initial state: all on the left bank (FALSE)
  problem$state_initial <- c(farmer = FALSE, wolf = FALSE, goat = FALSE, cabbage = FALSE)
  
  # Goal state: all on the right bank (TRUE)
  problem$state_final <- c(farmer = TRUE, wolf = TRUE, goat = TRUE, cabbage = TRUE)
  
  return(problem)
}

# =========================================================================
# is.applicable(state, action, problem)
# =========================================================================
# PURPOSE:
#   Returns TRUE if the action can be legally applied in the given state.
#
# INPUT:
#   state   : current state (logical vector with named elements)
#   action  : one action ("none", "wolf", "goat", "cabbage")
#   problem : problem definition
#
# OUTPUT:
#   TRUE  -> action can be applied (physically possible AND safe result)
#   FALSE -> action is not allowed
# =========================================================================
is.applicable <- function(state, action, problem) {
  
  # -----------------------------------------------------------------------
  # 1. Pre-condition Check: Physical possibility
  # -----------------------------------------------------------------------
  # The Farmer is the only one who can row the boat.
  # If the action is to take an item (action != "none"), that item MUST be 
  # on the same bank as the Farmer to be transported.
  if (action != "none") {
    if (state[action] != state["farmer"]) {
      return(FALSE)
    }
  }
  
  # -----------------------------------------------------------------------
  # 2. Simulation: Construct the future state (Look-ahead)
  # -----------------------------------------------------------------------
  # We create a copy of the state to simulate the move without modifying the original.
  future_state <- state
  
  # The Farmer always moves to the opposite bank.
  future_state["farmer"] <- !state["farmer"]
  
  # If there is an item associated with the action, it also moves.
  # We use the 'action' string directly as the index for the state vector.
  if (action != "none") {
    future_state[action] <- !state[action]
  }
  
  # -----------------------------------------------------------------------
  # 3. Post-condition Check: Safety constraints
  # -----------------------------------------------------------------------
  # Internal helper function to check if a configuration is safe.
  # Logic: A state is UNSAFE if a predator/prey pair is together WITHOUT the farmer.
  is.safe <- function(s) {
    # Check: Goat vs Cabbage
    # If they are on the same bank AND the farmer is NOT with them -> Unsafe
    if (s["goat"] == s["cabbage"] && s["goat"] != s["farmer"]) {
      return(FALSE)
    }
    
    # Check: Wolf vs Goat
    # If they are on the same bank AND the farmer is NOT with them -> Unsafe
    if (s["wolf"] == s["goat"] && s["wolf"] != s["farmer"]) {
      return(FALSE)
    }
    
    # Otherwise, it is safe
    return(TRUE)
  }
  
  # Return whether the resulting state is safe
  return(is.safe(future_state))
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
  
  result <- state                       # Copy current state
  result["farmer"] <- !state["farmer"]   # Farmer always crosses
  
  if (action != "none") {               # If farmer takes an item...
    result[action] <- !state[action]    # ...that item also crosses
  }
  
  return(result)                        # Return the new state
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
# =========================================================================
is.final.state <- function (state, final_state, problem) {
  return(all(state == problem$state_final))
}

# =========================================================================
# to.string(state, problem)
# =========================================================================
# PURPOSE:
#   Converts a state into a unique string representation.
#
# OUTPUT:
#   A string that uniquely identifies the state.
# =========================================================================
to.string <- function(state, problem) {
  return(paste(names(state), state, sep = "=", collapse = ","))  
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
get.cost <- function(action, state, problem) {
  return(1)  # Uniform cost: each crossing has cost 1
}

# =========================================================================
# get.evaluation(state, problem)
# =========================================================================
# PURPOSE:
#   Heuristic function h(n) used by informed search algorithms.
#
# INPUT:
#   state   : current state
#   problem : problem definition
#
# OUTPUT:
#   A numeric estimate of how far the state is from the goal.
# =========================================================================
get.evaluation <- function(state, problem) {
  # Heuristic: Number of elements still on the left bank (including farmer).
  # Since FALSE = Left, !state converts them to TRUE (1), so sum counts Left items.
  return(sum(!state))
}