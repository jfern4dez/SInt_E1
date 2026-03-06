# =========================================================================
# PROBLEM FORMULATION: FEET MAZE
# =========================================================================
# State representation:
#   A vector of length 2: c(col, row).
#   NOTE: The "foot" (Left/Right) is NOT stored in the state vector because
#   it is an intrinsic property of the cell (lookup in problem$maze).
#
# Actions:
#   A vector of moves: c("UP", "DOWN", "LEFT", "RIGHT").
#
# Transition model:
#   Move one cell in the chosen direction (updates coordinates).
#
# Constraints:
#   1. Boundaries: Cannot leave the grid.
#   2. Walls: Cannot cross a wall (defined in the instance file).
#   3. Feet Alternation: The foot at the NEW cell must be different from
#      the foot at the CURRENT cell (Left -> Right -> Left...).
#
# Goal test:
#   The current coordinates match the final coordinates defined in the file.
#
# Cost function:
#   Uniform cost: each move costs 1.
#
# Heuristic (if applicable):
#   Manhattan distance to goal: |x1-x2| + |y1-y2|.
#   (Note: This heuristic ignores walls and feet constraints, but is admissible).
#
# Notes for Graph Search (to.string):
#   to.string encodes "col,row". Since the foot is static for that cell,
#   coordinates are sufficient to uniquely identify the state.
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
# initialize.problem(file, random_actions)
# =========================================================================
#
# INPUT:
#   file           : Path to the maze file.
#   random_actions : Logical (TRUE/FALSE). If TRUE, shuffles the order of
#                    actions. Useful to demonstrate variability in search.
# =========================================================================
initialize.problem <- function(file, random_actions = FALSE) {
  # Robust file reading
  if (!file.exists(file)) stop(paste0("File not found: ", file))
  
  lines <- readLines(file)
  problem <- list()
  problem$name <- paste0("Feet Maze - [", basename(file), "]")
  
  # --- 1. Dimensions (Line 1: "Rows;Cols") ---
  dims <- as.numeric(unlist(strsplit(lines[1], ";")))
  problem$rows <- dims[1]
  problem$cols <- dims[2]
  
  # --- 2. Maze Matrix (L/R) ---
  raw_maze <- lines[2:(1 + problem$rows)]
  problem$maze <- do.call(rbind, lapply(raw_maze, function(x) unlist(strsplit(x, ";"))))
  
  # --- 3. Start & End Positions ---
  # Convert File Index (0-based) to R Index (1-based)
  idx_start_line <- 1 + problem$rows + 1
  
  start_raw <- as.numeric(unlist(strsplit(lines[idx_start_line], ",")))
  problem$state_initial <- c(start_raw[1] + 1, start_raw[2] + 1)
  
  end_raw <- as.numeric(unlist(strsplit(lines[idx_start_line + 1], ",")))
  problem$state_final <- c(end_raw[1] + 1, end_raw[2] + 1)
  
  # --- 4. Walls ---
  # Keep as strings "X,Y" (0-based)
  problem$walls_left  <- unlist(strsplit(lines[idx_start_line + 2], ";"))
  problem$walls_right <- unlist(strsplit(lines[idx_start_line + 3], ";"))
  problem$walls_down  <- unlist(strsplit(lines[idx_start_line + 4], ";"))
  problem$walls_top   <- unlist(strsplit(lines[idx_start_line + 5], ";"))
  
  # --- 5. Actions (Randomizable) ---
  problem$actions_possible <- c("UP", "DOWN", "LEFT", "RIGHT")
  
  if (random_actions) {
    # Shuffle the actions using sample()
    problem$actions_possible <- sample(problem$actions_possible)
  }
  
  return(problem)
}

# Analyzes if an action can be applied in a state.
is.applicable <- function (state, action, problem) {
  # state is c(col, row) in 1-based indexing
  col <- state[1]
  row <- state[2]
  
  # Get current foot (L or R)
  current_foot <- problem$maze[row, col]
  
  # Variable to store the potential next position (1-based)
  next_col <- col
  next_row <- row
  
  # Variables to store the string representation for wall checking (0-based "X,Y")
  # We need to check walls at the CURRENT cell and the DESTINATION cell.
  # Example: Moving UP. 
  #   - Is there a TOP wall in current cell?
  #   - OR is there a DOWN wall in the upper cell?
  pos_current_str <- paste(col - 1, row - 1, sep = ",")
  
  # --- LOGIC PER DIRECTION ---
  if (action == "UP") {
    next_row <- row - 1
    # 1. Boundary Check
    if (next_row < 1) return(FALSE)
    
    # 2. Wall Check
    pos_next_str <- paste(next_col - 1, next_row - 1, sep = ",")
    has_wall <- (pos_current_str %in% problem$walls_top) || (pos_next_str %in% problem$walls_down)
    if (has_wall) return(FALSE)
    
  } else if (action == "DOWN") {
    next_row <- row + 1
    # 1. Boundary Check
    if (next_row > problem$rows) return(FALSE)
    
    # 2. Wall Check
    pos_next_str <- paste(next_col - 1, next_row - 1, sep = ",")
    has_wall <- (pos_current_str %in% problem$walls_down) || (pos_next_str %in% problem$walls_top)
    if (has_wall) return(FALSE)
    
  } else if (action == "LEFT") {
    next_col <- col - 1
    # 1. Boundary Check
    if (next_col < 1) return(FALSE)
    
    # 2. Wall Check
    pos_next_str <- paste(next_col - 1, next_row - 1, sep = ",")
    has_wall <- (pos_current_str %in% problem$walls_left) || (pos_next_str %in% problem$walls_right)
    if (has_wall) return(FALSE)
    
  } else if (action == "RIGHT") {
    next_col <- col + 1
    # 1. Boundary Check
    if (next_col > problem$cols) return(FALSE)
    
    # 2. Wall Check
    pos_next_str <- paste(next_col - 1, next_row - 1, sep = ",")
    has_wall <- (pos_current_str %in% problem$walls_right) || (pos_next_str %in% problem$walls_left)
    if (has_wall) return(FALSE)
    
  } else {
    return(FALSE)
  }
  
  # 3. Foot Alternation Check
  # Must move from L to R or R to L. If they are equal, move is invalid.
  next_foot <- problem$maze[next_row, next_col]
  
  return(current_foot != next_foot)
}

# Returns the state resulting on applying the action over the state.
effect <- function (state, action, problem) {
  result <- state
  
  # Action logic assumes is.applicable() returned TRUE
  if (action == "UP")    result[2] <- result[2] - 1
  if (action == "DOWN")  result[2] <- result[2] + 1
  if (action == "LEFT")  result[1] <- result[1] - 1
  if (action == "RIGHT") result[1] <- result[1] + 1
  
  return(result)
}

# Analyzes if a state is final or not
is.final.state <- function (state, final_state, problem) {
  return(all(state == problem$state_final))
}

# Transforms a state into a string
to.string <- function (state, problem) {
  return(paste(state, collapse = ","))
}

# Returns the cost of applying an action over a state
get.cost <- function (action, state, problem) {
  return(1)
}

# Heuristic function used by Informed Search Algorithms
get.evaluation <- function(state, problem) {
  # Manhattan distance: |x1 - x2| + |y1 - y2|
  return(abs(state[1] - problem$state_final[1]) + abs(state[2] - problem$state_final[2]))
}