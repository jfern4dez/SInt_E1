# =========================================================================
# PRACTICAL TUTORIAL: THE NODE EXPANSION PROCESS (8-PUZZLE)
# =========================================================================
# This script dissects "in slow motion" how the expansion cycle works.
#
# OBJECTIVE:
# 1. Define a specific problem instance that is exactly 3 moves away from the goal.
# 2. Manually simulate the 3 expansion steps (creating the search tree branch).
# 3. Verify the goal and reconstruct the path using backtracking.
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

# -------------------------------------------------------------------------
# 0) INITIAL CONFIGURATION
# -------------------------------------------------------------------------
# Clean the environment to ensure we start from scratch
rm(list=ls())
cat("\014") # Clear console

# Set the working directory to the script's location
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# --- IMPORTING EXTERNAL CODE (source) ---
# The function source() reads and executes R code from a file.
# It acts like an 'import' in Java or 'include' in C.
# By sourcing '8-puzzle.R', we load the problem definition and helper functions
# (is.applicable, effect, etc.) into our current memory.
source("../problems/8-puzzle.R") 

# -------------------------------------------------------------------------
# 1) SETUP: A PROBLEM WITH A KNOWN 3-STEP SOLUTION
# -------------------------------------------------------------------------
cat("=== 1. DEFINITION OF A SPECIFIC INITIAL STATE ===\n")
# We force a state that is exactly 3 moves away from the solution.
# Target Solution:  (0 1 2 3 4 5 6 7 8)
# Current State:    (1 4 2 3 5 0 6 7 8)
# The required path will be: Left -> Up -> Left
specific_state <- c(1, 4, 2, 
                    3, 5, 0, 
                    6, 7, 8)

# Initialize problem with this specific state using the named parameter
problem <- initialize.problem(initial_state = specific_state)

cat("Initial State (3 moves from goal):\n")
print(problem$state_initial)

# -------------------------------------------------------------------------
# 2) MANUAL EXECUTION: STEP-BY-STEP EXPANSION
# -------------------------------------------------------------------------
cat("\n=== 2. MANUAL SIMULATION OF THE SEARCH PATH ===\n")

# --- STEP 0: ROOT NODE ---
# The root node wraps the initial state. It has no parent and cost is 0.
root_node <- list(
  state      = problem$state_initial,
  parent     = NULL,
  action     = "None",
  depth      = 0,
  cost       = 0
)
cat("[Depth 0] Root Node created.\n")

# --- STEP 1: Apply 'Left' ---
# We assume the algorithm 'decided' to move the blank tile Left.
# NOTE: Must match the case in problem$actions_possible ("Left", not "LEFT")
action_1 <- "Left"
cat(paste0("\n   --> Action 1: ", action_1, "\n"))

# Check if the action is valid in the current state
valid_1 <- is.applicable(root_node$state, action_1, problem)
cat("        Is applicable? ", valid_1, "\n")

# Create Node 1 (Child of Root)
node_1 <- list()
node_1$state  <- effect(root_node$state, action_1, problem) # Generate new matrix
node_1$parent <- root_node  # Point back to Root (Critical for backtracking!)
node_1$action <- action_1   # Store the action taken
node_1$depth  <- 1
node_1$cost   <- root_node$cost + get.cost(action_1, root_node$state, problem)

print(node_1$state)

# --- STEP 2: Apply 'Up' ---
# From the previous state (node_1), we move the blank tile Up.
action_2 <- "Up"
cat(paste0("\n   |--> Action 2: ", action_2, "\n"))

# Check if the action is valid
valid_2 <- is.applicable(node_1$state, action_2, problem)
cat("        Is applicable? ", valid_2, "\n")

# Create Node 2 (Child of Node 1)
node_2 <- list()
node_2$state  <- effect(node_1$state, action_2, problem)
node_2$parent <- node_1     # Point back to Node 1
node_2$action <- action_2
node_2$depth  <- 2
node_2$cost   <- node_1$cost + get.cost(action_2, node_1$state, problem)

print(node_2$state)

# --- STEP 3: Apply 'Left' ---
# From the previous state (node_2), we move the blank tile Left again.
action_3 <- "Left"
cat(paste0("\n   |--> Action 3: ", action_3, "\n"))

# Check if the action is valid
valid_3 <- is.applicable(node_2$state, action_3, problem)
cat("        Is applicable? ", valid_3, "\n")

# Create Node 3 (Child of Node 2)
node_3 <- list()
node_3$state  <- effect(node_2$state, action_3, problem)
node_3$parent <- node_2     # Point back to Node 2
node_3$action <- action_3
node_3$depth  <- 3
node_3$cost   <- node_2$cost + get.cost(action_3, node_2$state, problem)

print(node_3$state)

# -------------------------------------------------------------------------
# 3) GOAL CHECK AND BACKTRACKING
# -------------------------------------------------------------------------
cat("\n=== 3. GOAL CHECK ===\n")

# We check if the last node (node_3) satisfies the goal condition
if (is.final.state(node_3$state, problem$state_final, problem)) {
  
  cat("SUCCESS: The final state has been reached!\n")
  cat("Now, let's reconstruct the path by following the 'parent' pointers.\n")
  
  # --- BACKTRACKING ALGORITHM ---
  # We start at the goal and go up until we hit the root (parent is NULL)
  current_node <- node_3
  path_actions <- c()
  
  while (!is.null(current_node$parent)) {
    # 1. Capture the action used to reach this node
    path_actions <- c(current_node$action, path_actions)
    
    # 2. Move up to the parent
    current_node <- current_node$parent
  }
  
  # -----------------------------------------------------------------------
  # 4) FINAL SUMMARY REPORT
  # -----------------------------------------------------------------------
  cat("\n======================================================\n")
  cat("                 SOLUTION SUMMARY                     \n")
  cat("======================================================\n")
  cat("Total Cost:   ", node_3$cost, "\n")
  cat("Final Depth:  ", node_3$depth, "\n")
  cat("Solution Path:", paste(path_actions, collapse = " -> "), "\n")
  cat("Final State:\n")
  print(node_3$state)
  cat("======================================================\n")
  
} else {
  cat("FAILURE: The goal was not reached. Check the manual steps applied.\n")
}
