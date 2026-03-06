# =========================================================================
# Main Script Template - Path Search Algorithms
# =========================================================================
# This script shows the typical workflow to test search algorithms:
#
#   - Load the search framework (algorithms + analysis utilities)
#   - Load a problem definition (your "problem.R" file)
#   - Solve one or more problem instances
#   - Generate a comparative results table
#   - Optionally export results to CSV
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
# 1) Clear environment and console
# -------------------------------------------------------------------------
rm(list = ls()) # Clear all variables
cat("\014")     # Clear console (RStudio)
graphics.off()  # Close all plots

# -------------------------------------------------------------------------
# 2) Working directory (IMPORTANT)
# -------------------------------------------------------------------------
# We try to detect it automatically using rstudioapi.
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# -------------------------------------------------------------------------
# 3) Load algorithm utilities (required by all search algorithms)
# -------------------------------------------------------------------------
source("../algorithms/blind/expand-node.R")

# -------------------------------------------------------------------------
# 4) Load search algorithms
# -------------------------------------------------------------------------
# Blind search algorithms
source("../algorithms/blind/breadth-first-search.R")
source("../algorithms/blind/depth-first-search.R")
source("../algorithms/blind/depth-limited-search.R")
source("../algorithms/blind/iterative-deepening-search.R")

# Informed search algorithms
source("../algorithms/informed/uniform-cost-search.R")
source("../algorithms/informed/greedy-best-first-search.R")
source("../algorithms/informed/a-star-search.R")

# -------------------------------------------------------------------------
# 5) Load result analysis utilities
# -------------------------------------------------------------------------
source("../algorithms/results-analysis/analyze-results.R")

# -------------------------------------------------------------------------
# 6) Load the problem definition
# -------------------------------------------------------------------------
# IMPORTANT: Uncomment ONLY the problem you want to solve.
# Ensure the corresponding block in Section 8 matches the problem type.

# --- Type A: Logic Puzzles (No files required) ---
source("../problems/8-puzzle.R")
# source("../problems/river-crossing.R")
# source("../problems/hanoi-tower.R")

# --- Type B: Problems with Instance Files ---
# source("../problems/sudoku.R")
# source("../problems/feet-maze.R")
# source("../problems/one-pizza-incremental.R")

# -------------------------------------------------------------------------
# 7) solve.instance()
# -------------------------------------------------------------------------
# PURPOSE:
#   Solves ONE problem instance using several algorithms and returns a results table.
#
# PARAMETERS:
#   file           : path to the instance file (NULL if not used)
#   max_iterations : maximum iterations for each algorithm
#   depth_limit    : depth limit used in Depth-Limited Search (DLS)
#   max_depth      : maximum depth used in Iterative Deepening Search (IDS)
#   export_csv     : TRUE/FALSE, export summary table to CSV
#   csv_dir        : output directory for CSV file
#   verbose        : TRUE/FALSE, prints extra information during analysis
#   ...            : extra parameters forwarded to initialize.problem()
#                    (e.g., random_actions = TRUE, n_disks = 3, p = 2)
# -------------------------------------------------------------------------
solve.instance <- function(file = NULL,
                           max_iterations = 2500,
                           depth_limit = 20,
                           max_depth = 20,
                           export_csv = TRUE,
                           csv_dir = "../results",
                           verbose = TRUE,
                           ...) {
  
  # 1. Create results folder if needed
  if (export_csv && !dir.exists(csv_dir)) {
    dir.create(csv_dir, recursive = TRUE)
  }
  
  # 2. Initialize problem instance
  # We handle two cases: with file and without file.
  if (!is.null(file)) {
    # Case A: Problem uses a file (Sudoku, Maze, OnePizza...)
    # We pass 'file' and any extra parameters (...)
    problem <- initialize.problem(file = file, ...)
    instance_name <- basename(file)
  } else {
    # Case B: Problem defined by parameters (8-Puzzle, Hanoi, River...)
    # We pass ONLY the extra parameters (...)
    problem <- initialize.problem(...)
    instance_name <- "Parameters Instance"
  }
  
  cat("\n============================================================\n")
  cat(paste0("Solving Instance: ", instance_name, "\n"))
  cat(paste0("Problem Name:     ", problem$name, "\n"))
  cat("============================================================\n\n")
  
  # 3. Run algorithms
  # ---------------------------------------------------------
  # You can add or remove algorithms from this block.
  # ---------------------------------------------------------
  
  bfs_ts   <- breadth.first.search(problem, max_iterations = max_iterations, count_print = 1000)
  bfs_gs   <- breadth.first.search(problem, max_iterations = max_iterations, count_print = 1000, graph_search = TRUE)
  
  dfs_ts   <- depth.first.search(problem, max_iterations = max_iterations, count_print = 1000)
  dfs_gs   <- depth.first.search(problem, max_iterations = max_iterations, count_print = 1000, graph_search = TRUE)
  
  dls_ts   <- depth.limited.search(problem, max_iterations = max_iterations,
                                   depth_limit = depth_limit, count_print = 1000)
  dls_gs   <- depth.limited.search(problem, max_iterations = max_iterations,
                                   depth_limit = depth_limit, count_print = 1000, graph_search = TRUE)
  
  ids_ts   <- iterative.deepening.search(problem, max_iterations = max_iterations,
                                         max_depth = max_depth, count_print = 1000)
  ids_gs   <- iterative.deepening.search(problem, max_iterations = max_iterations,
                                         max_depth = max_depth, count_print = 1000, graph_search = TRUE)
  
  ucs_ts   <- uniform.cost.search(problem, max_iterations = max_iterations, count_print = 1000)
  ucs_gs   <- uniform.cost.search(problem, max_iterations = max_iterations, count_print = 1000, graph_search = TRUE)
  
  gbfs_ts  <- greedy.best.first.search(problem, max_iterations = max_iterations, count_print = 1000)
  gbfs_gs  <- greedy.best.first.search(problem, max_iterations = max_iterations, count_print = 1000, graph_search = TRUE)
  
  astar_ts <- a.star.search(problem, max_iterations = max_iterations, count_print = 1000)
  astar_gs <- a.star.search(problem, max_iterations = max_iterations, count_print = 1000, graph_search = TRUE)
  
  # 4. Analyze results
  results <- analyze.results(
    results = list(
      bfs_ts, bfs_gs,
      dfs_ts, dfs_gs,
      dls_ts, dls_gs,
      ids_ts, ids_gs,
      ucs_ts, ucs_gs,
      gbfs_ts, gbfs_gs,
      astar_ts, astar_gs
    ),
    problem = problem,
    verbose = verbose,
    export_csv = export_csv,
    csv_dir = csv_dir
  )
  
  # 5. Visual Output (HTML Table)
  # Displays a nice table in RStudio Viewer if 'kableExtra' is installed
  if (requireNamespace("kableExtra", quietly = TRUE) && requireNamespace("knitr", quietly = TRUE)) {
    print(kableExtra::kable_material(kableExtra::kbl(results, caption = paste("Results:", problem$name)),
          c("striped", "hover", "condensed")))
  }
  
  return(results)
}

# -------------------------------------------------------------------------
# 8) Experiment block (run several instances)
# -------------------------------------------------------------------------

# --- OPTION A: PROBLEMS WITHOUT FILES (Active by default) ---
# Examples: 8-Puzzle, River Crossing, Hanoi Tower
# You can pass parameters directly to solve.instance

# 8-Puzzle (Default 3x3 with random start)
# 6 steps needed to be solved
# try(solve.instance(rows = 3, columns = 3, initial_state = c(1,2,5,3,4,8,0,6,7))) 
# 12 steps needed to be solved
try(solve.instance(rows = 3, columns = 3, initial_state = c(2,3,5,1,4,8,0,6,7))) 

# River Crossing
# try(solve.instance())

# Hanoi Tower
# 3 pegs and 4 disks (optimal solution has 15 steps)
# try(solve.instance(n_pegs = 3, n_disks = 4))
# 6 pegs and 3 disks (optimal solution has 5 steps)
#try(solve.instance(n_pegs = 6, n_disks = 3))

# --- OPTION B: PROBLEMS WITH FILES (Commented) ---
# Examples: Sudoku, Feet Maze, One Pizza
# Uncomment logic below to use

#files <- c("../data/sudoku/sudoku-1.txt", "../data/sudoku/sudoku-2.txt")
#files <- c("../data/feet-maze/feet-maze-1a.txt", "../data/feet-maze/feet-maze-2a.txt")
#files <- c("../data/one-pizza/s_spanish.in.txt")

#for (f in files) {
#   try({
#     # Pass the file and any extra parameters (like random_actions = TRUE)
#     solve.instance(file = f)
#   }, silent = FALSE)
#}