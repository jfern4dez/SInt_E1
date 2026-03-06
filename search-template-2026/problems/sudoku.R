# =========================================================================
# PROBLEM FORMULATION
# =========================================================================
# State representation:
#   A 9x9 matrix with values 0..9, where 0 means empty.
#
# Actions:
#   The numbers 1..9.
#   NOTE: To reduce the branching factor, the algorithm always acts on the 
#   *first empty cell* found (deterministic location).
#
# Transition model:
#   Fill the determined empty cell with the chosen value.
#
# Constraints:
#   Standard Sudoku rules:
#   - No repeated value in the current row, column, or 3x3 subgrid.
#
# Goal test:
#   The board is completely filled (no 0s remain).
#
# Cost function:
#   Uniform cost: each assignment costs 1.
#
# Heuristic (if applicable):
#   Number of empty cells (minimize).
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
#   file : Path to the CSV file containing the initial Sudoku board.
#
# OUTPUT:
#   problem (list) with compulsory fields:
#     - problem$name
#     - problem$state_initial
#     - problem$state_final (NULL, implicit goal)
#     - problem$actions_possible
# =========================================================================
initialize.problem <- function(file, ...) {
  # Validate file existence
  if (!file.exists(file)) {
    stop(paste0("File not found: ", file))
  }
  
  problem <- list()
  
  # Compulsory attributes
  problem$name             <- paste0("Sudoku - [", basename(file), "]")
  
  # Actions are just the numbers 1..9 (target cell is deterministic)
  problem$actions_possible <- 1:9
  
  # The initial state is the one read from the file
  problem$state_initial <- as.matrix(read.csv(file, header = FALSE))
  
  # In this problem final_state is unknown (checked by properties)
  problem$state_final <- NULL
  
  return(problem)
}

# =========================================================================
# is.applicable(state, action, problem)
# =========================================================================
# PURPOSE:
#   Returns TRUE if the action (number 1..9) can be placed in the 
#   FIRST available empty cell without violating Sudoku rules.
#
# INPUT:
#   state   : current 9x9 matrix
#   action  : number to place (1..9)
#   problem : problem definition
#
# OUTPUT:
#   TRUE  -> valid move
#   FALSE -> violates row, col, or box constraints
# =========================================================================
is.applicable <- function (state, action, problem) {
  # Action is just the number (1..9)
  value <- action
  
  # 1. Find the target cell (First empty cell)
  # NOTE: which(..., arr.ind=TRUE) returns a matrix. We take the first row.
  empty_cells <- which(state == 0, arr.ind = TRUE)
  
  # Safety check: If board is full, no action is applicable
  if (nrow(empty_cells) == 0) return(FALSE)
  
  target_row <- empty_cells[1, 1]
  target_col <- empty_cells[1, 2]
  
  # 2. Check Row Constraint (Is 'value' already in this row?)
  if (any(state[target_row, ] == value)) return(FALSE)
  
  # 3. Check Column Constraint (Is 'value' already in this col?)
  if (any(state[, target_col] == value)) return(FALSE)
  
  # 4. Check Square (3x3) Constraint
  # Math to find the top-left corner of the 3x3 block
  start_row <- ((target_row - 1) %/% 3) * 3 + 1
  start_col <- ((target_col - 1) %/% 3) * 3 + 1
  
  block <- state[start_row:(start_row+2), start_col:(start_col+2)]
  if (any(block == value)) return(FALSE)
  
  return(TRUE)
}

# =========================================================================
# effect(state, action, problem)
# =========================================================================
# PURPOSE:
#   Applies the action to the state.
#   It finds the first empty cell (same as is.applicable) and fills it.
# =========================================================================
effect <- function (state, action, problem) {
  result <- state
  
  # Find the first empty cell again (Deterministic logic)
  empty_cells <- which(state == 0, arr.ind = TRUE)
  target_row <- empty_cells[1, 1]
  target_col <- empty_cells[1, 2]
  
  # Apply action
  result[target_row, target_col] <- action
  
  return(result)
}

# =========================================================================
# is.final.state(state, final_state, problem)
# =========================================================================
# PURPOSE:
#   Checks if the board is full. 
#   (Since is.applicable ensures validity, a full board is a solution).
# =========================================================================
is.final.state <- function (state, final_state, problem) {
  return(!any(state == 0))
}

# =========================================================================
# to.string(state, problem)
# =========================================================================
# PURPOSE:
#   Converts the matrix to a single string for state identification.
# =========================================================================
to.string <- function (state, problem) {
  return(paste(state, collapse = ""))
}

# =========================================================================
# get.cost(action, state, problem)
# =========================================================================
# PURPOSE:
#   Returns the step cost (Uniform = 1).
# =========================================================================
get.cost <- function (action, state, problem) {
  return(1)
}

# =========================================================================
# get.evaluation(state, problem)
# =========================================================================
# PURPOSE:
#   Heuristic function: Counts the number of empty cells remaining.
# =========================================================================
get.evaluation <- function(state, problem) {
  return(sum(state == 0))
}

# =========================================================================
# print.sudoku(state)
# =========================================================================
# PURPOSE:
#   Utility to pretty-print the board to the console.
# =========================================================================
print.sudoku <- function(state) {
  for(i in 1:9) {
    if (i %% 3 == 1 && i != 1) cat("------+-------+------\n")
    row_str <- ""
    for(j in 1:9) {
      val <- ifelse(state[i,j] == 0, ".", state[i,j])
      row_str <- paste0(row_str, val, " ")
      if (j %% 3 == 0 && j != 9) row_str <- paste0(row_str, "| ")
    }
    cat(row_str, "\n")
  }
}