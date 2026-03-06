# =========================================================================
# PROBLEM FORMULATION: ONE PIZZA (Binary State / Toggle Action)
# =========================================================================
# State representation:
#   A binary vector (0 | 1) where the i-th position represents the presence
#   of the i-th unique ingredient.
#   - 1: Ingredient is on the pizza.
#   - 0: Ingredient is NOT on the pizza.
#
# Actions:
#   Integer index representing the ingredient to TOGGLE.
#   - If 0 -> becomes 1 (Add).
#   - If 1 -> becomes 0 (Remove).
#   This allows the algorithm to backtrack (remove ingredients).
#
# Transition model:
#   Flips the bit at the specified index.
#
# Goal test:
#   None (Optimization problem). Usually runs until max iterations.
#
# Cost function:
#   Uniform cost: 1 per modification.
#
# Heuristic:
#   Number of UNSATISFIED clients (minimize).
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
# initialize.problem(filename, max_customers)
# =========================================================================
# PURPOSE:
#   Parses the input file, builds the problem object, and creates the 
#   initial random state.
#
# INPUT:
#   filename      : Path to the input file.
#   max_customers : Limit the number of clients to process (default 100).
# =========================================================================
initialize.problem <- function(filename, max_customers = 100) {
  problem <- list() # Default value is an empty list.
  
  if (!file.exists(filename)) stop(paste0("File not found: ", filename))
  
  # Read full file
  lines <- readLines(filename)
  
  # --- 1. Parse Number of Clients ---
  total_customers <- as.numeric(lines[1])
  
  # Remove the first line (header)
  lines <- lines[-1]
  
  # Limit the number of clients to max_customers (Safety check)
  actual_customers <- min(total_customers, max_customers)
  problem$customers <- actual_customers
  
  # We only take the lines corresponding to the chosen customers (2 lines per customer)
  lines <- lines[1:(actual_customers * 2)]
  
  # --- 2. Robust Parsing of Ingredients ---
  # Unlike substr, strsplit handles numbers with multiple digits correctly (e.g. "10 ing...")
  parsed_lines <- lapply(lines, function(x) {
    parts <- unlist(strsplit(trimws(x), " "))
    # If line has only the count (e.g., "0") or is empty, return empty char vector
    if (length(parts) <= 1) return(character(0)) 
    return(parts[-1]) # Return everything after the number
  })
  
  # Get like ingredients (odd lines: 1, 3, 5...)
  problem$like <- parsed_lines[seq(1, length(parsed_lines), by = 2)]
  
  # Get dislike ingredients (even lines: 2, 4, 6...)
  problem$dislike <- parsed_lines[seq(2, length(parsed_lines), by = 2)]
  
  # --- 3. Define State Space ---
  # Get ALL unique ingredients (from both Likes and Dislikes to be safe)
  problem$ingredients <- unique(c(unlist(problem$like), unlist(problem$dislike)))
  
  # Action is "Modify ingredient value". There are as many actions as ingredients.
  # We use the index (1..N) as the action.
  problem$actions_possible <- 1:length(problem$ingredients)
  
  # The state is a vector (0 | 1) whose size is equal to the number of different ingredients.
  # The value of each position indicates whether the ingredient is present in the one pizza or not.
  # We initialize with a random configuration (approx 50% ingredients).
  problem$state_initial <- sample(c(0,1), size = length(problem$ingredients), replace = TRUE, prob = c(0.5, 0.5))
  
  # Final state is unknown (Optimization problem)
  problem$state_final <- NULL
  
  problem$name <- paste0("One-pizza [", basename(filename), 
                         " | Clients = ", problem$customers, 
                         " | Ingredients = ", length(problem$ingredients), "]")
  return(problem)
}

# =========================================================================
# is.applicable(state, action, problem)
# =========================================================================
# PURPOSE:
#   Analyzes if an action can be applied in the received state.
#   In this problem, we can always toggle any ingredient index.
# =========================================================================
is.applicable <- function (state, action, problem) {
  return(TRUE)
}

# =========================================================================
# effect(state, action, problem)
# =========================================================================
# PURPOSE:
#   Returns the state resulting on applying the action over the state.
#   Action is the INDEX of the ingredient to toggle.
# =========================================================================
effect <- function (state, action, problem) {
  result <- state
  
  # The value of the position specified by the action is changed (1 to 0 & 0 to 1)
  # Logic: |x - 1| flips 0->1 and 1->0
  result[action] <- abs(result[action] - 1)
  
  return(result)
}

# =========================================================================
# is.final.state(state, final_state, problem)
# =========================================================================
# PURPOSE:
#   Analyzes if a state is final or not.
#   Since we want to find the BEST pizza, we usually let the algorithm 
#   run until max iterations, so we return FALSE.
# =========================================================================
is.final.state <- function (state, final_state, problem) {
  return(FALSE)
}

# =========================================================================
# to.string(state, problem)
# =========================================================================
# PURPOSE:
#   Transforms a state (binary vector) into a readable string of ingredient names.
# =========================================================================
to.string = function (state, problem) {
  # An array with the ingredients that are present in the pizza (where state == 1)
  present_ingredients <- problem$ingredients[state == 1]
  
  if(length(present_ingredients) == 0) return("<Empty Pizza>")
  
  return(paste0(present_ingredients, collapse = ", "))
}

# =========================================================================
# get.cost(action, state, problem)
# =========================================================================
# PURPOSE:
#   Returns the cost of applying an action over a state.
#   Uniform cost = 1.
# =========================================================================
get.cost <- function (action, state, problem) {
  return(1)
}

# =========================================================================
# get.evaluation(state, problem)
# =========================================================================
# PURPOSE:
#   Heuristic function used by Informed Search Algorithms.
#   Metric: Number of UNSATISFIED clients (Minimization).
# =========================================================================
get.evaluation <- function(state, problem) {
  # Get the names of ingredients currently on the pizza
  present_ingredients <- problem$ingredients[state == 1]
  
  # Calculate how many clients are UNHAPPY with this combination
  unsatisfied <- unsatisfied.clients(present_ingredients, problem)
  
  return(unsatisfied)
}

# =========================================================================
# satisfied.clients(ingredients, problem)
# =========================================================================
# PURPOSE:
#   Counts satisfied customers.
#   A client is satisfied ONLY if:
#     1. ALL liked ingredients are present.
#     2. NONE of the disliked ingredients are present.
# =========================================================================
satisfied.clients <- function(ingredients, problem) {
  satisfied <- 0
  
  for (i in 1:problem$customers) {
    # Check Likes: Must contain ALL liked
    has_all_likes <- all(problem$like[[i]] %in% ingredients)
    
    # Check Dislikes: Must contain NONE of the disliked
    # Logic: !any(...) means "It is NOT true that ANY disliked ingredient is present"
    has_no_dislikes <- !any(problem$dislike[[i]] %in% ingredients)
    
    if (has_all_likes && has_no_dislikes) {
      satisfied <- satisfied + 1
    }
  }
  
  return(satisfied)
}

# =========================================================================
# unsatisfied.clients(ingredients, problem)
# =========================================================================
# PURPOSE:
#   Counts unsatisfied customers (The complement of satisfied).
#   A client is unsatisfied IF:
#     1. MISSING at least one liked ingredient.
#     OR
#     2. CONTAINS at least one disliked ingredient.
# =========================================================================
unsatisfied.clients <- function(ingredients, problem) {
  unsatisfied <- 0
  
  for (i in 1:problem$customers) {
    # Check failure conditions
    missing_likes <- !all(problem$like[[i]] %in% ingredients)
    has_bad_ing   <- any(problem$dislike[[i]] %in% ingredients)
    
    if (missing_likes || has_bad_ing) {
      unsatisfied <- unsatisfied + 1
    }
  }
  
  return(unsatisfied)
}