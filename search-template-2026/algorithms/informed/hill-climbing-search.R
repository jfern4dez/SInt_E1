# =========================================================================
# Hill Climbing Search (Steepest Ascent)
# =========================================================================
# Hill Climbing is a local search algorithm that iteratively moves to the
# best neighbor according to the evaluation function h(n).
#
# NOTES:
# - It does not guarantee completeness or optimality.
# - It may get stuck in local minima / plateaus / ridges.
# - This implementation represents "Steepest Ascent": it examines ALL 
#   neighbors and selects the absolute best one.
# - It requires STRICT improvement (<). If the best neighbor has the same
#   evaluation (plateau) or worse, the algorithm stops.
#
# This algorithm uses local.expand.node(), which computes a "local" cost
# (not accumulated).
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

hill.climbing.search <- function(problem,
                                 max_iterations = 50,
                                 count_print = 5) {
  
  name_method      <- "Hill Climbing Search"
  state_initial    <- problem$state_initial
  actions_possible <- problem$actions_possible
  
  print(paste0("* START: ", name_method), quote = FALSE)
  start_time <- Sys.time()
  
  # Current node initialization
  node_current <- list(state = state_initial,
                       evaluation = get.evaluation(state_initial, problem))
  
  # Pre-allocate report vectors for performance
  rep_iteration     <- numeric(max_iterations)
  rep_eval_current  <- numeric(max_iterations)
  rep_eval_neighbor <- numeric(max_iterations)
  
  count <- 1
  end_reason <- NULL
  
  while (count <= max_iterations) {
    
    # 1. Check Goal (Optional in optimization, but good practice)
    if (!is.null(problem$state_final) && is.final.state(node_current$state, problem$state_final, problem)) {
      end_reason <- "Solution"
      break
    }
    
    # Trace print
    if (count %% count_print == 0) {
      print(paste0("Iteration: ", count, ", evaluation=", node_current$evaluation), quote = FALSE)
    }
    
    # 2. Expand: Generate all neighbors
    # (Assuming local.expand.node returns a list of nodes with computed evaluation)
    successor_nodes <- local.expand.node(node_current, actions_possible, problem)
    
    # Dead-end check (No neighbors generated)
    if (length(successor_nodes) == 0) {
      end_reason <- "Dead_End"
      # Log current state
      rep_iteration[count]     <- count
      rep_eval_current[count]  <- node_current$evaluation
      rep_eval_neighbor[count] <- NA
      break
    }
    
    # 3. Find Best Neighbor (Steepest Ascent)
    # Optimization: Use which.min instead of sorting the whole list.
    # Sorting is O(N log N), which.min is O(N).
    evals <- sapply(successor_nodes, function(x) x$evaluation)
    best_idx <- which.min(evals)
    node_best_successor <- successor_nodes[[best_idx]]
    
    # Update Report
    rep_iteration[count]     <- count
    rep_eval_current[count]  <- node_current$evaluation
    rep_eval_neighbor[count] <- node_best_successor$evaluation
    
    # 4. Move Decision
    # Strict improvement required to avoid infinite loops on plateaus
    if (node_best_successor$evaluation < node_current$evaluation) {
      # Accept move
      node_current <- node_best_successor
    } else {
      # Stop: Local Maximum or Plateau reached
      end_reason <- "Local_Best"
      break
    }
    
    count <- count + 1
  }
  
  # Handle loop termination
  if (is.null(end_reason)) {
    end_reason <- "Iterations"
  }
  
  end_time <- Sys.time()
  
  # Build Final Report
  # Filter valid rows (where iteration > 0)
  valid_idx <- rep_iteration > 0
  
  report <- data.frame(Iteration     = rep_iteration[valid_idx],
                       Eval_Current  = rep_eval_current[valid_idx],
                       Eval_Neighbor = rep_eval_neighbor[valid_idx])
  
  # Construct Result Object
  result <- list()
  result$name       <- name_method
  result$runtime    <- end_time - start_time
  result$node_final <- node_current
  result$report     <- report
  result$end_reason <- end_reason
  
  # Final Print
  if (end_reason == "Solution") {
    print("Solution found (Global Optimum)!", quote = FALSE)
  } else if (end_reason == "Local_Best") {
    print("Local best found (Optimization stopped).", quote = FALSE)
  } else {
    print(paste0("Stopped: ", end_reason), quote = FALSE)
  }
  
  print(paste0("Final State: ", to.string(state = node_current$state, problem = problem)), quote = FALSE)
  print(paste0("* END: ", name_method), quote = FALSE)
  
  return(result)
}