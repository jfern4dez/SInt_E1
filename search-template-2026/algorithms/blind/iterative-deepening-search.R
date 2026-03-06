# =========================================================================
# Iterative Deepening Search (IDS)
# =========================================================================
# Iterative Deepening Search (IDS) repeatedly runs Depth-Limited Search (DLS)
# with increasing depth limits (0, 1, 2, ...), combining DFS space efficiency
# with BFS-like completeness (when step costs are equal).
#
# Arguments:
#   problem       : The problem object.
#   max_iterations: Maximum total iterations allowed (across all DLS runs).
#   count_print   : Interval for printing trace information.
#   graph_search  : If TRUE, checks for repeated states.
#   max_depth     : Maximum depth limit for the IDS process itself.
# =========================================================================

iterative.deepening.search <- function(problem,
                                       max_iterations = 1000,
                                       count_print = 100,
                                       graph_search = FALSE,
                                       max_depth = 20) {
  
  name_method <- paste0("Iterative Deepening Search", ifelse(graph_search, " + GS", ""))
  
  # Get Start time
  print(paste0("* START: ", name_method), quote = FALSE)
  start_time <- Sys.time()
  
  current_limit <- 0
  total_iterations <- 0
  solved <- FALSE
  end_reason <- NULL
  dls_result <- NULL
  
  # Accumulate report rows
  report_total_list <- list()
  
  # Run Depth-Limited Search for increasing limits
  while (current_limit <= max_depth && !solved && total_iterations < max_iterations) {
    
    # Calculate remaining iterations allowed
    remaining_iters <- max_iterations - total_iterations
    
    if (remaining_iters <= 0) {
      end_reason <- "Iterations"
      break
    }
    
    # Run DLS
    dls_result <- depth.limited.search(problem,
                                       max_iterations = remaining_iters,
                                       count_print = count_print,
                                       graph_search = graph_search,
                                       depth_limit = current_limit)
    
    # --- REPORT MERGING LOGIC ---
    if (!is.null(dls_result$report) && nrow(dls_result$report) > 0) {
      current_report <- dls_result$report
      
      # Make iterations cumulative
      current_report$iteration <- current_report$iteration + total_iterations
      
      # Add column to track which limit produced these rows
      current_report$depth_limit <- current_limit
      
      report_total_list[[length(report_total_list) + 1]] <- current_report
      
      # Update total global iterations
      total_iterations <- total_iterations + nrow(current_report)
    }
    
    # --- CHECK RESULT ---
    if (dls_result$end_reason == "Solution") {
      solved <- TRUE
      end_reason <- "Solution"
      
    } else if (dls_result$end_reason == "Frontier") {
      # If DLS returns "Frontier", it means it explored the ENTIRE reachable space 
      # within that limit AND found no solution AND did not hit the depth cutoff.
      # This implies the solution does not exist anywhere.
      end_reason <- "Frontier"
      break
      
    } else if (dls_result$end_reason == "Iterations") {
      # Global max_iterations reached inside DLS
      end_reason <- "Iterations"
      break
    }
    # If end_reason was "Cutoff", we simply continue to the next limit.
    
    current_limit <- current_limit + 1
  }
  
  # Handle loop termination if no specific reason set yet
  if (is.null(end_reason)) {
    if (total_iterations >= max_iterations) {
      end_reason <- "Iterations"
    } else if (current_limit > max_depth) {
      end_reason <- "Cutoff" # Reached max global depth for IDS
    } else {
      end_reason <- "Unknown"
    }
  }
  
  end_time <- Sys.time()
  
  # Combine reports into a single Data Frame
  if (length(report_total_list) == 0) {
    report_total <- data.frame(iteration=numeric(),
                               nodes_frontier=numeric(),
                               depth_of_expanded=numeric(),
                               nodes_added_frontier=numeric(),
                               depth_limit=numeric())
  } else {
    report_total <- do.call(rbind, report_total_list)
  }
  
  # Construct Result Object
  result <- list()
  result$name       <- paste0("Iterative Deepening Search - Final Limit=", current_limit, ifelse(graph_search, " + GS", ""))
  result$runtime    <- end_time - start_time
  result$node_final <- if (!is.null(dls_result)) dls_result$node_final else NA
  result$report     <- report_total
  result$end_reason <- end_reason
  
  # Final Output
  if (end_reason == "Solution") {
    print("Solution found!", quote = FALSE)
  } else if (end_reason == "Frontier") {
    print("Frontier is empty. No Solution found in the entire space.", quote = FALSE)
  } else if (end_reason == "Cutoff") {
    print("Maximum IDS depth reached. No Solution found within the global limit.", quote = FALSE)
  } else {
    print("Maximum total iterations reached. Stopping IDS.", quote = FALSE)
  }
  
  print(paste0("* END: ", name_method), quote = FALSE)
  
  return(result)
}