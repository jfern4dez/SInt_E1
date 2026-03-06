# =========================================================================
# Problem Template
# =========================================================================
# This file defines the problem interface required by the search algorithms.
#
# Each problem must implement the functions below using the EXACT same
# headers (do NOT modify them). The search algorithms are generic. They do
# not "know" the problem domain, so they rely on this interface as a contract.
#   - is.applicable()  : checks whether an action can be applied
#   - effect()         : returns the successor state
#   - is.final.state() : checks whether a state satisfies the goal condition
#   - to.string()      : unique state identifier (used in Graph Search)
#   - get.cost()       : step cost (used by UCS / A*)
#   - get.evaluation() : heuristic h(n) (used by Greedy / A*)
#
# -------------------------------------------------------------------------
# IMPORTANT DESIGN NOTES
# -------------------------------------------------------------------------
# 1) Actions representation
#    - Recommended (simple): actions_possible as an atomic vector
#        Example: c("left","right","up","down")
#      This makes the code simpler because actions are used directly by
#      is.applicable(), effect(), get.cost() and get.evaluation().
#
#    - Advanced: actions_possible as a data.frame/matrix (one row per action)
#      Use this only when each action needs multiple attributes (e.g., name + cost).
#        Example: data.frame(move=c("left","right"), cost=c(1,2))
#
# 2) State representation (critical for Graph Search correctness)
#    - The state MUST include ALL variables that affect future behaviour:
#        * which actions are applicable
#        * the successor state
#        * the action cost
#        * the heuristic evaluation
#    - If the state is incomplete, Graph Search may prune incorrectly and
#      return wrong results (suboptimal solution or even no solution found).
#
# 3) Determinism requirement
#    - Given the same (state, action), the functions must always return
#      the same output.
#    - Randomness must be handled outside these functions (e.g., inside
#      initialize.problem()).
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
#   ... : optional parameters passed from the main script.
#         Typical examples:
#           - file / filename : instance file to load
#
# OUTPUT:
#   problem (list) with compulsory fields:
#     - problem$name
#     - problem$state_initial
#     - problem$state_final
#     - problem$actions_possible
#
# NOTES:
#   - Search algorithms DO NOT call this function. The main script does.
#   - actions_possible is usually a simple vector of actions (recommended),
#     but can also be a data.frame/matrix if actions need multiple attributes.
# =========================================================================
initialize.problem <- function(...) {
  problem <- list()
  
  # ---------------------------------------------------------
  # COMPULSORY ATTRIBUTES (must be defined in each problem)
  # ---------------------------------------------------------
  # problem$name              <- <INSERT CODE HERE>
  # problem$state_initial     <- <INSERT CODE HERE>
  # problem$state_final       <- <INSERT CODE HERE>
  # problem$actions_possible  <- <INSERT CODE HERE>
  
  # ---------------------------------------------------------
  # OPTIONAL / ADDITIONAL ATTRIBUTES
  # ---------------------------------------------------------
  # Any extra information needed by the problem can be stored here:
  # problem$map, problem$obstacles, problem$params, ...
  
  # ---------------------------------------------------------
  # IMPORTANT: STATE REPRESENTATION (Graph Search correctness)
  # ---------------------------------------------------------
  # The state MUST contain ALL variables that affect:
  #   - which actions are applicable
  #   - the successor state
  #   - action costs
  #   - heuristic evaluation
  # Otherwise, Graph Search may incorrectly treat different states as equal.
  
  return(problem)
}

# =========================================================================
# is.applicable(state, action, problem)
# =========================================================================
# PURPOSE:
#   Returns TRUE if the action can be legally applied in the given state.
#
# INPUT:
#   state   : current state
#   action  : one action (usually a single value, e.g. "left", 3, etc.)
#   problem : problem definition
#
# OUTPUT:
#   TRUE  -> action can be applied
#   FALSE -> action is not allowed in this state
#
# IMPORTANT:
#   This function MUST be deterministic:
#   given the same (state, action), it must always return the same result.
# =========================================================================
is.applicable <- function(state, action, problem) {
  result <- FALSE
  
  # TODO STUDENT:
  # Return TRUE only if the action can be applied in this state.
  # Example constraints: boundaries, obstacles, resource limits, etc.
  
  return(result)
}

# =========================================================================
# effect(state, action, problem)
# =========================================================================
# PURPOSE:
#   Applies the action to the state and returns the successor state.
#
# INPUT:
#   state   : current state
#   action  : action to apply
#   problem : problem definition
#
# OUTPUT:
#   successor state (same representation type as 'state')
#
# IMPORTANT:
#   - This function must NOT modify the original state in-place. Return a NEW state object instead.
#   - It must also be deterministic.
# =========================================================================
effect <- function(state, action, problem) {
  result <- state
  
  # TODO STUDENT:
  # Modify the state according to the action and return it.
  #
  # Example (if state is c(x,y) and action is list(dx,dy)):
  # result[1] <- state[1] + action$dx
  # result[2] <- state[2] + action$dy
  
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
  result <- FALSE
  
  # TODO STUDENT:
  # Return TRUE when state satisfies the goal condition.
  
  return(result)
}

# =========================================================================
# to.string(state, problem)
# =========================================================================
# PURPOSE:
#   Converts a state into a unique string representation.
#
# OUTPUT:
#   A string that uniquely identifies the state.
#
# WHY THIS MATTERS:
#   - Used for printing states and debugging.
#   - Used by Graph Search to detect repeated states.
#
# IMPORTANT:
#   The string MUST include all information that defines the identity of the
#   state. If the state is incomplete in this string, Graph Search may prune
#   incorrectly and return wrong results.
# =========================================================================
to.string <- function(state, problem) {
  
  # TODO STUDENT:
  # Generate a unique string identifier for the state.
  # TIP: Use paste0(unlist(state), collapse = "_") to handle matrices or lists safely.
  
  return(paste(unlist(state), collapse="_"))
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
#
# IMPORTANT:
#   Costs must be NON-NEGATIVE for UCS and A* to behave correctly.
# =========================================================================
get.cost <- function(action, state, problem) {
  
  # TODO STUDENT:
  # Return the step cost of applying the action in this state.
  
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
#
# NOTES:
#   - For A* to be optimal, the heuristic should be admissible
#     (never overestimates the true remaining cost).
#   - Returning 0 is always admissible and makes A* behave like UCS.
# =========================================================================
get.evaluation <- function(state, problem) {
  
  # TODO STUDENT:
  # Return heuristic estimate of remaining cost.
  
  return(0)
}