# =========================================================================
# Main Script: SUDOKU
# =========================================================================
# Solves Sudoku instances loaded from text files.
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

# 1. Clear environment
rm(list = ls())
cat("\014")

# 2. Set working directory
if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# 3. Load Algorithms & Utilities
source("../algorithms/blind/expand-node.R")
source("../algorithms/blind/breadth-first-search.R")
source("../algorithms/blind/depth-first-search.R")
source("../algorithms/blind/depth-limited-search.R")
source("../algorithms/blind/iterative-deepening-search.R")
source("../algorithms/informed/uniform-cost-search.R")
source("../algorithms/informed/greedy-best-first-search.R")
source("../algorithms/informed/a-star-search.R")
source("../algorithms/results-analysis/analyze-results.R")

# 4. Load Problem Definition
source("../problems/sudoku.R")

# 5. Execution Logic
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
  
  problem <- initialize.problem(file = file)
  
  cat("\n============================================================\n")
  cat(paste0("Problem Name:     ", problem$name, "\n"))
  cat("============================================================\n\n")
  
  # 3. Run algorithms
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

# 6. Run File Instances
files <- c("../data/sudoku/sudoku-2.txt")

for (f in files) {
  tryCatch({
    solve.instance(file = f)
  }, error = function(e) {
    cat("Error solving", f, ":", e$message, "\n")
  })
}
