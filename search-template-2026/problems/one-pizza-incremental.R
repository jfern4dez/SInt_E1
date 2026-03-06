# =========================================================================
# PROBLEM FORMULATION: ONE PIZZA (Incremental Search)
# =========================================================================
# State representation:
#   A character vector of unique ingredient names representing the pizza.
#   (Empty vector means no ingredients).
#
# Actions:
#   Ingredient names (strings) to ADD to the pizza.
#   Note: Only ingredients that at least one client LIKES are considered 
#   valid actions (adding a purely disliked ingredient is useless).
#
# Transition model:
#   Add the selected ingredient to the set.
#
# Constraints:
#   None (any combination of ingredients is a valid pizza state).
#
# Goal test:
#   Depends on the search strategy. For this problem, usually:
#   1. A specific % of satisfied clients (e.g., >= 100%).
#   2. Or simply finding the state that maximizes the heuristic (Optimization).
#
# Cost function:
#   Uniform cost: 1 per added ingredient.
#
# Heuristic:
#   Number of UNSATISFIED clients (minimize).
#   (Equivalent to maximizing satisfied clients).
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
# initialize.problem(filename, p, random_actions)
# =========================================================================
# PURPOSE:
#   Parses the input file and builds the problem object.
#
# INPUT:
#   filename       : Path to the input file.
#   p              : Target percentage of satisfied clients (0.0 to 1.0).
#   random_actions : If TRUE, shuffles the order of possible ingredients.
# =========================================================================
initialize.problem <- function(filename, p = 1.0, random_actions = FALSE) {
  
  if (!file.exists(filename)) stop(paste0("File not found: ", filename))
  
  lines <- readLines(filename)
  problem <- list()

  # --- 1. Parse Number of Clients ---
  problem$customers <- as.numeric(lines[1])
  
  # Remove the first line (header)
  lines <- lines[-1]
  
  # --- 2. Parse Likes and Dislikes ---
  # File format: 
  # Line 2*i - 1: Likes for client i
  # Line 2*i    : Dislikes for client i
  
  like_lines    <- lines[seq(1, length(lines), by = 2)]
  dislike_lines <- lines[seq(2, length(lines), by = 2)]
  
  # Helper to parse a line: "3 Ing1 Ing2 Ing3" -> c("Ing1", "Ing2", "Ing3")
  parse_line <- function(line) {
    parts <- unlist(strsplit(trimws(line), " "))
    count <- as.numeric(parts[1])
    if (count == 0) return(character(0))
    return(parts[-1]) # Return everything after the number
  }
  
  problem$like    <- lapply(like_lines, parse_line)
  problem$dislike <- lapply(dislike_lines, parse_line)
  
  # --- 3. Define Actions ---
  # Actions = All unique ingredients that appear in the "Likes" lists.
  # (It makes no sense to add an ingredient that strictly nobody likes)
  all_liked_ingredients <- unique(unlist(problem$like))
  problem$ingredients <- all_liked_ingredients
  
  if (random_actions) {
    problem$actions_possible <- sample(all_liked_ingredients)
  } else {
    problem$actions_possible <- sort(all_liked_ingredients)
  }
  
  # --- 4. Initial State & Parameters ---
  problem$state_initial <- character(0) # Empty pizza
  problem$state_final   <- NULL         # Defined by heuristic/goal test
  problem$p             <- p            # Goal threshold
  
  problem$name <- paste0("One-pizza [clients=", problem$customers, 
                         " | ingredients=", length(problem$actions_possible), "]")
  
  return(problem)
}

# =========================================================================
# is.applicable(state, action, problem)
# =========================================================================
# PURPOSE:
#   Checks if the ingredient is not already on the pizza.
# =========================================================================
is.applicable <- function (state, action, problem) {
  # Action is the ingredient string
  return(!(action %in% state))
}

# =========================================================================
# effect(state, action, problem)
# =========================================================================
# PURPOSE:
#   Adds the ingredient to the state and sorts it (canonical representation).
# =========================================================================
effect <- function (state, action, problem) {
  # Use sort to ensure unique string representation later
  return(sort(c(state, action)))
}

# =========================================================================
# is.final.state(state, final_state, problem)
# =========================================================================
# PURPOSE:
#   Checks if the percentage of satisfied clients meets the threshold 'p'.
# =========================================================================
is.final.state <- function (state, final_state, problem) {
  # Check for typo in arguments corrected: final_state
  satisfied_count <- satisfied.clients(state, problem)
  ratio <- satisfied_count / problem$customers
  
  return(ratio >= problem$p)
}

# =========================================================================
# to.string(state, problem)
# =========================================================================
# PURPOSE:
#   Unique string representation of the set of ingredients.
# =========================================================================
to.string = function (state, problem) {
  if (length(state) == 0) return("<EMPTY>")
  return(paste(state, collapse = ","))
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
#   Heuristic: Number of UNSATISFIED clients (Lower is better).
# =========================================================================
get.evaluation <- function(state, problem) {
  # We want to maximize satisfaction, so we minimize dissatisfaction
  return(problem$customers - satisfied.clients(state, problem))
}

# =========================================================================
# count.satisfied.clients(state, problem)
# =========================================================================
# PURPOSE:
#   Helper function to count how many clients are happy with the pizza.
#   - Happy IF: Pizza contains ALL liked AND contains NONE of the disliked.
# =========================================================================
satisfied.clients <- function(state, problem) {
  satisfied <- 0
  
  for (i in 1:problem$customers) {
    # Check Likes: Must contain ALL liked
    if (all(problem$like[[i]] %in% state)) {
      # Check Dislikes: Must contain NONE of the disliked
      if (!any(problem$dislike[[i]] %in% state)) {
        satisfied <- satisfied + 1
      }
    }
  }
  return(satisfied)
}