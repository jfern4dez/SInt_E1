# =========================================================================
# Main Script Template - Local Search Algorithms
# =========================================================================
# This script shows the typical workflow to test LOCAL SEARCH algorithms.
#
# Key features:
#   - Runs algorithms multiple times (stochastic nature).
#   - Re-initializes the problem in each run to get random starting states.
#   - Collects all runs into a single results structure for analysis.
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
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# -------------------------------------------------------------------------
# 3) Load algorithm utilities
# -------------------------------------------------------------------------
source("../algorithms/blind/expand-node.R")

# -------------------------------------------------------------------------
# 4) Load Local Search Algorithms
# -------------------------------------------------------------------------
# BASE ALGORITHM:
source("../algorithms/informed/hill-climbing-search.R")

# TODO: STUDENTS MUST ADD THEIR ALGORITHMS HERE
# Uncomment the lines below when you implement new variants:
# source("../algorithms/informed/hill-climbing-random-restart-search.R")
# source("../algorithms/informed/stochastic-hill-climbing-search.R")

# -------------------------------------------------------------------------
# 5) Load result analysis utilities
# -------------------------------------------------------------------------
source("../algorithms/results-analysis/analyze-results.R")

# -------------------------------------------------------------------------
# 6) Load the problem definition
# -------------------------------------------------------------------------
# IMPORTANT: Uncomment ONLY the problem you want to solve.

# --- One Pizza (Optimization) ---
# source("../problems/one-pizza-complete.R")

# --- P-Hub (Optimization) ---
source("../problems/p-hub.R")

# -------------------------------------------------------------------------
# 7) solve.instance()
# -------------------------------------------------------------------------
# PURPOSE:
#   Executes Local Search algorithms multiple times on a problem instance.
#
# PARAMETERS:
#   file           : path to the instance file
#   times          : NUMBER OF RUNS per algorithm (Crucial for local search)
#   max_iterations : maximum steps allowed before stopping
#   count_print    : iterations interval to print progress
#   export_csv     : TRUE/FALSE, export summary table to CSV
#   csv_dir        : output directory for CSV file
#   verbose        : TRUE/FALSE, prints extra main script info
#   ...            : extra parameters for initialize.problem()
# -------------------------------------------------------------------------
solve.instance <- function(file,
                           times = 10,
                           max_iterations = 500,
                           count_print = 10,
                           export_csv = TRUE,
                           csv_dir = "../results",
                           verbose = TRUE,
                           ...) {
  
  # 1. Create results folder if needed
  if (export_csv && !dir.exists(csv_dir)) {
    dir.create(csv_dir, recursive = TRUE)
  }
  
  # 2. Initial Setup (Metadata only)
  # We initialize once to get the name/metadata, but we will 
  # re-initialize inside the loop to get random starts.
  temp_prob <- initialize.problem(file = file, ...)
  instance_name <- basename(file)
  
  if (verbose) {
    cat("\n============================================================\n")
    cat(paste0("LOCAL SEARCH on:  ", instance_name, "\n"))
    cat(paste0("Problem Name:     ", temp_prob$name, "\n"))
    cat(paste0("Executions/Algo:  ", times, "\n"))
    cat("============================================================\n\n")
  }
  
  # 3. Run Algorithms
  # ---------------------------------------------------------
  # We collect ALL runs in a single list 'results_list'.
  # analyze.results() will handle the aggregation.
  # ---------------------------------------------------------
  results_list <- list()
  
  # --- ALGORITHM 1: Hill Climbing Search ---
  if (verbose) cat(paste0("Running Hill Climbing Search (", times, " runs)...\n"))
  
  for (i in 1:times) {
    # CRITICAL: Re-initialize problem for RANDOM start in each run
    # This ensures that each execution starts from a different point.
    problem <- initialize.problem(file = file, ...)
    
    # Run Algorithm
    res <- hill.climbing.search(problem, 
                                max_iterations = max_iterations, 
                                count_print = count_print)
    
    # Add to list
    results_list[[length(results_list) + 1]] <- res
  }
  
  # --- ALGORITHM 2: Random Restart (Example for Students) ---
  # if (exists("hill.climbing.random.restart.search")) {
  #   if (verbose) cat(paste0("Running HC Random Restart (", times, " runs)...\n"))
  #   
  #   for (i in 1:times) {
  #     problem <- initialize.problem(file = file, ...)
  #     
  #     res <- hill.climbing.random.restart.search(problem, 
  #                                                max_iterations = max_iterations, 
  #                                                count_print = count_print,
  #                                                restarts = 5) # Example param
  #     
  #     results_list[[length(results_list) + 1]] <- res
  #   }
  # }
  
  # 4. Analyze results (Standard Utility)
  # ---------------------------------------------------------
  results <- local.analyze.results(
    results = results_list,
    problem = temp_prob, # Metadata from first init
    verbose = verbose,
    export_csv = export_csv,
    csv_dir = csv_dir
  )
  
  # 6. Visual Output (HTML Table)
  # ---------------------------------------------------------
  if (requireNamespace("kableExtra", quietly = TRUE) && requireNamespace("knitr", quietly = TRUE)) {
    print(kableExtra::kable_material(kableExtra::kbl(results, caption = paste("Local Search Stats:", temp_prob$name)),
                                     c("striped", "hover", "condensed")))
  }
  
  return(results)
}

# -------------------------------------------------------------------------
# 8) Experiment block
# -------------------------------------------------------------------------

# --- EXAMPLE 1: One Pizza ---
# try(solve.instance(file = "../data/one-pizza/elaborate.txt"))

# --- EXAMPLE 2: P-Hub ---
try(solve.instance(file = "../data/p-hub/AP200.txt"))
