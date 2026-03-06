# =========================================================================
# PROBLEM FORMULATION: p-Hub Median Problem
# =========================================================================
# State representation:
#   A vector of length 'p' containing the indices (1..N) of the selected hubs.
#   Example: c(1, 5, 8) means airports 1, 5, and 8 are hubs.
#
# Actions:
#   Indices 1..p representing which hub in the state vector to change.
#   NOTE: The new value is chosen randomly (Stochastic Local Search).
#
# Transition model:
#   Replace the selected hub with a random non-hub airport.
#
# Goal test:
#   Typically none for optimization (run until max iterations).
#
# Cost function:
#   The objective function of the p-Hub problem:
#   Sum of distances between all pairs (i, j) routing through their nearest hubs.
#   Cost(i,j) = Dist(i, Hub_i) + Dist(Hub_i, Hub_j) + Dist(Hub_j, j)
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

# =========================================================================
# initialize.problem(filename, p)
# =========================================================================
initialize.problem <- function(filename, p = 2) {
  
  if (!file.exists(filename)) stop(paste0("File not found: ", filename))
  
  problem <- list()
  problem$p <- p
  
  # --- 1. Parse File ---
  # Read size (N)
  # Size is the number of airports
  problem$size      <- as.numeric(unlist(read.csv(filename, header=FALSE, nrows=1)))
  
  # Read Distances Matrix
  # We skip the size line + the N lines of coordinates (problem$size) + 1 blank line usually
  # The adjustment 'skip=problem$size+2' you found is the key here.
  raw_dist <- read.csv(filename, header=FALSE, skip=problem$size+2, dec=".", sep=" ")
  # PERFORMANCE FIX: Convert data.frame to matrix for O(1) access
  problem$distances <- as.matrix(raw_dist)
  
  # --- 2. Initial and Final States ---
  # Randomly select p unique airports
  problem$state_initial <- sample(1:problem$size, problem$p)
  # No specific final state (optimization problem)
  problem$state_final <- NULL
  
  # --- 3. Actions ---
  # Action i means "Change the i-th hub in the vector"
  problem$actions_possible <- 1:problem$p
  
  problem$name <- paste0("p-Hub [N=", problem$size, " | p=", p, "]")
  
  return(problem)
}

# =========================================================================
# is.applicable(state, action, problem)
# =========================================================================
is.applicable <- function (state, action, problem) {
  # In local search, we can always attempt to change a hub
  return(TRUE)
}

# =========================================================================
# effect(state, action, problem)
# =========================================================================
# PURPOSE:
#   Replaces the hub at index 'action' with a random non-hub airport.
#   NOTE: This function is STOCHASTIC (contains random choice).
# =========================================================================
effect <- function (state, action, problem) {
  result <- state
  
  # 1. Identify all airports available (those that are not currently hubs)
  #    Using 1:problem$size is safer/cleaner than implicit c(1:...)
  non_hubs <- setdiff(1:problem$size, state)
  
  # 2. Pick one random new hub
  new_hub <- sample(non_hubs, 1)
  
  # 3. Apply change
  result[action] <- new_hub
  
  return(result)
}

# =========================================================================
# is.final.state(state, final_state, problem)
# =========================================================================
is.final.state <- function (state, final_state, problem) {
  return(FALSE)
}

# =========================================================================
# to.string(state, problem)
# =========================================================================
to.string = function (state, problem) {
  # Sort to ensure uniqueness (e.g., hubs {1,5} is same state as {5,1})
  return(paste(sort(state), collapse = ","))
}

# =========================================================================
# get.cost(action, state, problem)
# =========================================================================
get.cost <- function (action, state, problem) {
  return(1)
}

# =========================================================================
# get.evaluation(state, problem)
# =========================================================================
# PURPOSE:
#   Calculates the total transportation cost.
#   Optimized for vectorization and O(1) lookups.
# =========================================================================
get.evaluation <- function(state, problem) {
  total_distance <- 0
  
  # 1. Pre-calculate the nearest hub for EVERY airport.
  #    'assignments' is a vector of length N. 
  #    assignments[k] contains the Node ID of the hub for airport k.
  assignments <- get.hub.assignments(state, problem)
  
  # 2. Calculate sum of distances for all pairs
  #    Loop is O(N^2), but inside operations are O(1) thanks to 'assignments' vector
  N <- problem$size
  
  for (i in 1:(N-1)) {
    for (j in (i+1):N) {
      
      hub_i <- assignments[i]
      hub_j <- assignments[j]
      
      # Cost = Dist(i -> Hub_i) + Dist(Hub_i -> Hub_j) + Dist(Hub_j -> j)
      cost <- problem$distances[i, hub_i] + 
        problem$distances[hub_i, hub_j] + 
        problem$distances[hub_j, j]
      
      total_distance <- total_distance + cost
    }
  }
  
  return(total_distance)
}

# =========================================================================
# Helper: get.hub.assignments
# =========================================================================
# PURPOSE:
#   Returns a vector of length N where index 'i' holds the ID of the 
#   nearest hub for airport 'i'.
# =========================================================================
get.hub.assignments <- function(state, problem) {
  # state contains the indices of the p hubs
  
  # We can calculate this efficiently.
  # We need to find for each row in distance matrix, which of the columns 
  # (present in 'state') has the minimum value.
  
  # Extract the submatrix of distances: Rows=All, Cols=Only Hubs
  dist_to_hubs <- problem$distances[, state, drop=FALSE]
  
  # 'apply' with which.min gives the index (1..p) relative to the 'state' vector
  nearest_hub_indices <- apply(dist_to_hubs, 1, which.min)
  
  # Map back to real Node IDs
  nearest_hub_ids <- state[nearest_hub_indices]
  
  return(nearest_hub_ids)
}