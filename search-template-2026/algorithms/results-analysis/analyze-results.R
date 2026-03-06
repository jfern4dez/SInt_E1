# =========================================================================
# RESULTS ANALYSIS UTILITIES
# =========================================================================
# This file provides helper functions to summarize and analyze the results
# obtained by the different search algorithms implemented in this framework.
#
# Two analysis functions are provided:
#
# 1) analyze.results():
#    - Designed for Classical Search algorithms (BFS, DFS, A*, Greedy, etc.).
#    - Produces a summary table including:
#        * Solution found (TRUE/FALSE)
#        * End reason (Solution / Frontier empty / Cutoff / Iterations)
#        * Actions length (Solution depth)
#        * Solution cost (Step cost or path cost)
#        * Expanded (Nodes expanded / Iterations)
#        * Max depth expanded (Search tree depth)
#        * Max frontier size (Space complexity metric)
#        * Runtime in seconds
#        * Final state string representation
#    - Optionally exports the summary to CSV.
#
# 2) local.analyze.results():
#    - Designed for Local Search algorithms (e.g., Hill Climbing, Simulated Annealing).
#    - Produces a summary table including:
#        * Iterations (Steps taken)
#        * End reason (Local_Best / Dead_End / Max_Iterations)
#        * Initial Evaluation (Quality of starting state)
#        * Final Evaluation (Quality of resulting state)
#        * Runtime in seconds
#        * Initial & Final state string representations
#    - Optionally exports the summary to CSV.
#
# IMPORTANT:
# - Classical algorithms store the final result as a "node" (list) in result$node_final,
#   or NA if no solution was found.
# - Local search algorithms always return a final node (local optimum), so 
#   "solution_found" is not a relevant field in that context.
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
# Utility: safe.max()
# =========================================================================
safe.max <- function(x) {
  if (is.null(x)) return(NA)
  if (length(x) == 0) return(NA)
  if (is.list(x)) x <- unlist(x)
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  return(max(x))
}

# =========================================================================
# Utility: safe.nrow()
# =========================================================================
safe.nrow <- function(df) {
  if (is.null(df)) return(NA)
  if (!is.data.frame(df)) return(NA)
  return(nrow(df))
}

# =========================================================================
# Utility: sanitize.filename()
# =========================================================================
sanitize.filename <- function(x) {
  x <- gsub("[[:space:]]+", "_", x)
  x <- gsub("[^[:alnum:]_\\-\\.]", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  return(x)
}

# =========================================================================
# export.to.csv
# =========================================================================
# Purpose:
#   Exports a data frame to a CSV file with a timestamped name.
#   Ensures the directory exists and the filename is safe.
# =========================================================================
export.to.csv <- function(df, csv_file = NULL, csv_dir = ".", default_prefix = "results") {
  
  # If no filename is provided, generate one automatically
  if (is.null(csv_file) || nchar(csv_file) == 0) {
    # TIMESTAMP FORMAT: YYYY-Mon-DD_HH-MM-SS
    timestamp <- format(Sys.time(), "%Y-%b-%d_%H-%M-%S")
    csv_file <- paste0(default_prefix, "_", timestamp, ".csv")
  }
  
  # Sanitize filename
  if (exists("sanitize.filename")) {
    csv_file <- sanitize.filename(csv_file)
  }
  
  # Create output path
  out_path <- file.path(csv_dir, csv_file)
  
  # Create directory if it does not exist
  if (!dir.exists(csv_dir)) {
    dir.create(csv_dir, recursive = TRUE)
  }
  
  # Write CSV (without row names)
  utils::write.csv(df, out_path, row.names = FALSE)
  
  return(out_path)
}


# =========================================================================
# analyze.results() - CLASSICAL SEARCH VERSION
# =========================================================================
analyze.results <- function(results,
                            problem,
                            verbose = TRUE,
                            export_csv = FALSE,
                            csv_file = NULL,
                            csv_dir = ".") {
  
  # Automatic wrapping if a single result object is passed
  if (!is.null(results$name)) {
    results <- list(results)
  }
  
  # Use lapply to process each result individually (faster than loop + rbind)
  rows_list <- lapply(results, function(result) {
    
    if (verbose) print(result$name)
    
    # 1. Determine if Solution Found
    solution_found <- !(is.atomic(result$node_final) &&
                          length(result$node_final) == 1 &&
                          is.na(result$node_final))
    
    # 2. Extract Basic Info
    end_reason <- if (!is.null(result$end_reason)) result$end_reason else "Unknown"
    expanded   <- safe.nrow(result$report)
    
    # 3. Extract Report Stats
    max_depth    <- NA
    max_frontier <- NA
    if (!is.null(result$report) && is.data.frame(result$report)) {
      if ("depth_of_expanded" %in% colnames(result$report)) {
        max_depth <- safe.max(result$report$depth_of_expanded)
      }
      if ("nodes_frontier" %in% colnames(result$report)) {
        max_frontier <- safe.max(result$report$nodes_frontier)
      }
    }
    
    # 4. Extract Runtime
    runtime_secs <- NA
    if (!is.null(result$runtime)) {
      runtime_secs <- round(as.numeric(result$runtime, units = "secs"), digits = 4)
    }
    
    # 5. Extract Solution Details (if found)
    solution_length <- NA
    solution_cost   <- NA
    final_state_str <- "?"
    actions_str     <- ""
    
    if (solution_found) {
      # Actions
      if (!is.null(result$node_final$actions)) {
        actions_raw <- result$node_final$actions
        
        if (is.data.frame(actions_raw) && nrow(actions_raw) > 0) {
          solution_length <- nrow(actions_raw)
          actions_str <- paste(actions_raw[, 1], collapse = ", ")
        } else if (is.vector(actions_raw) && length(actions_raw) > 0) {
          solution_length <- length(actions_raw)
          actions_str <- paste(actions_raw, collapse = ", ")
        } else {
          solution_length <- 0
        }
      } else {
        solution_length <- 0
      }
      
      # Cost
      if (!is.null(result$node_final$cost) && length(result$node_final$cost) > 0) {
        solution_cost <- result$node_final$cost
      }
      
      # Final State
      if (!is.null(result$node_final$state)) {
        final_state_str <- to.string(result$node_final$state, problem)
      }
      
      # Verbose Output
      if (verbose) {
        print(paste0(" * Solution found after ", solution_length, " actions! :)"), quote = FALSE)
        if (solution_length > 0 && nchar(actions_str) > 0) {
          if (nchar(actions_str) > 100) actions_str_print <- paste0(substr(actions_str, 1, 97), "...")
          else actions_str_print <- actions_str
          print(paste0(" * Actions: ", actions_str_print), quote = FALSE)
        }
        print(paste0(" * Final state: ", final_state_str), quote = FALSE)
      }
      
    } else {
      if (verbose) print(" * No Solution Found :(", quote = FALSE)
    }
    
    # 6. Build Single Row Data Frame
    return(data.frame(Name           = result$name,
                      Solution_Found = solution_found,
                      End_Reason     = end_reason,
                      Actions        = ifelse(length(solution_length) > 0, solution_length, NA),
                      Cost           = ifelse(length(solution_cost) > 0, solution_cost, NA),
                      Expanded       = ifelse(length(expanded) > 0, expanded, NA),
                      Max_Depth      = ifelse(length(max_depth) > 0, max_depth, NA),
                      Max_Frontier   = ifelse(length(max_frontier) > 0, max_frontier, NA),
                      Runtime        = ifelse(length(runtime_secs) > 0, runtime_secs, NA),
                      Final_State    = final_state_str,
                      stringsAsFactors = FALSE))
  })
  
  # Combine all rows efficiently
  analyzed_results <- do.call(rbind, rows_list)
  
  # Optional CSV export
  if (export_csv) {
    default_prefix <- sanitize.filename(problem$name)
    out_path <- export.to.csv(analyzed_results, csv_file, csv_dir, default_prefix)
    if (verbose) {
      print(paste0("CSV exported to: ", out_path), quote = FALSE)
    }
  }
  
  return(analyzed_results)
}


# =========================================================================
# local.analyze.results() - LOCAL SEARCH VERSION
# =========================================================================
local.analyze.results <- function(results,
                                  problem,
                                  verbose = TRUE,
                                  export_csv = FALSE,
                                  csv_file = NULL,
                                  csv_dir = ".") {
  
  if (!is.null(results$name)) {
    results <- list(results)
  }
  
  # Use lapply for cleaner processing
  rows_list <- lapply(results, function(result) {
    
    if (verbose) print(result$name)
    
    # Runtime
    runtime_secs <- NA
    if (!is.null(result$runtime)) {
      runtime_secs <- round(as.numeric(result$runtime, units = "secs"), digits = 4)
    }
    
    # Iterations
    iterations <- 0
    if (!is.null(result$report)) {
      iterations <- nrow(result$report)
    }
    
    # Final Evaluation
    final_eval <- NA
    if (!is.null(result$node_final$evaluation)) {
      final_eval <- result$node_final$evaluation
    }
    
    # Initial Evaluation
    initial_eval <- NA
    if (!is.null(result$report) && nrow(result$report) > 0) {
      if ("Eval_Current" %in% colnames(result$report)) {
        initial_eval <- result$report[1, "Eval_Current"]
      }
    }
    
    # Initial State String
    init_state_str <- "?"
    if (!is.null(problem$state_initial)) {
      init_state_str <- to.string(problem$state_initial, problem)
    }
    
    # Final State String
    final_state_str <- "?"
    if (!is.null(result$node_final$state)) {
      final_state_str <- to.string(result$node_final$state, problem)
    }
    
    # Build Row
    return(data.frame(
      Name          = result$name,
      Iterations    = iterations,
      End_Reason    = ifelse(!is.null(result$end_reason), result$end_reason, "Unknown"),
      Initial_Eval  = ifelse(length(initial_eval) > 0, initial_eval, NA),
      Final_Eval    = ifelse(length(final_eval) > 0, final_eval, NA),
      Runtime       = ifelse(length(runtime_secs) > 0, runtime_secs, NA),
      Initial_State = init_state_str,
      Final_State   = final_state_str,
      stringsAsFactors = FALSE
    ))
  })
  
  # Combine rows
  analyzed_results <- do.call(rbind, rows_list)
  
  # CSV Export
  if (export_csv) {
    if (!dir.exists(csv_dir)) dir.create(csv_dir, recursive = TRUE)
    
    file_name <- ifelse(is.null(csv_file), 
                        paste0(problem$name, "_results.csv"), 
                        csv_file)
    
    full_path <- file.path(csv_dir, file_name)
    write.csv(analyzed_results, full_path, row.names = FALSE)
    
    if (verbose) cat(paste0("CSV exported to: ", full_path, "\n"))
  }
  
  return(analyzed_results)
}