# =========================================================================
# A Star Search (A*)
# =========================================================================
# A* expands the node with the lowest f(n) = g(n) + h(n),
# where g(n) is the accumulated path cost and h(n) is an admissible heuristic.
# It is complete and optimal if h(n) is admissible (never overestimates).
#
# Arguments:
#   problem       : The problem object.
#   max_iterations: Maximum number of nodes to expand.
#   count_print   : Interval for printing trace information.
#   graph_search  : If TRUE, checks for repeated states.
# -------------------------------------------------------------------------
# NOTE ON GRAPH SEARCH AND REPEATED STATES
# -------------------------------------------------------------------------
# In Tree Search (graph_search = FALSE), the same state may be generated many
# times through different paths.
#
# In Graph Search (graph_search = TRUE), repeated-state handling MUST be
# COST-AWARE. A state can be reached by multiple paths with different
# accumulated costs g(n). If a cheaper path to the same state is found later,
# the algorithm must process it (re-open) to ensure optimality.
#
# Therefore, A* maintains:
#   - best_g[state_id] : the lowest g(n) found so far for each state.
#
# When a successor is generated:
#   1. If state is NEW -> Add to frontier & Update best_g.
#   2. If state represents a BETTER path (lower g) -> Add to frontier & Update best_g.
#   3. If state represents a WORSE path -> Discard it.
#
# IMPORTANT:
# - The repeated-state test uses g(n) (path cost), NOT f(n).
# - Frontier ordering uses f(n) = g(n) + h(n).
#
# NOTE: best_g is implemented as a hash environment for O(1) access.
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

a.star.search <- function(problem,
                          max_iterations = 1000,
                          count_print = 100,
                          graph_search = FALSE) {
  
  name_method      <- paste0("A* Search", ifelse(graph_search, " + GS", ""))
  state_initial    <- problem$state_initial
  state_final      <- problem$state_final
  actions_possible <- problem$actions_possible
  
  # Initialization
  print(paste0("* START: ", name_method), quote = FALSE)
  start_time <- Sys.time()
  
  # Root node
  # node$evaluation stores h(n)
  # node$cost stores g(n)
  node <- list(parent = NULL,
               state = state_initial,
               actions = NULL,
               depth = 0,
               cost = 0,
               evaluation = get.evaluation(state_initial, problem))
  
  frontier <- list(node)
  
  # Graph Search: Map of best cost g(n)
  if (graph_search) {
    best_g <- new.env(hash = TRUE, parent = emptyenv())
    best_g[[to.string(state_initial, problem)]] <- 0
  }
  
  # Pre-allocate report vectors
  rep_iteration <- numeric(max_iterations)
  rep_frontier  <- numeric(max_iterations)
  rep_depth     <- numeric(max_iterations)
  rep_added     <- numeric(max_iterations)
  
  count <- 1
  end_reason <- NULL
  
  # Main Loop
  while (count <= max_iterations) {
    
    # 1. Priority Queue Simulation: Sort frontier by f(n) = g(n) + h(n)
    if (length(frontier) > 1) {
      # Calculate f(n) for all nodes
      f_values <- sapply(frontier, function(x) x$cost + x$evaluation)
      frontier <- frontier[order(f_values)]
    }
    
    # Trace print
    if (count %% count_print == 0) {
      print(paste0("Iteration: ", count, ", Nodes in the frontier: ", length(frontier)), quote = FALSE)
    }
    
    # 2. Check if Frontier is empty
    if (length(frontier) == 0) {
      end_reason <- "Frontier"
      break
    }
    
    # 3. Pop the lowest-f node
    node_first <- frontier[[1]]
    frontier[[1]] <- NULL
    
    # ---------------------------------------------------------------------
    # GRAPH SEARCH: LAZY DELETION CHECK
    # ---------------------------------------------------------------------
    # Check if this node is outdated (a cheaper path to this state was 
    # found and processed while this node was waiting in the queue).
    # ---------------------------------------------------------------------
    if (graph_search) {
      sid_first <- to.string(node_first$state, problem)
      if (node_first$cost > best_g[[sid_first]]) {
        next 
      }
    }
    
    # 4. Goal Test (At Expansion Time)
    if (is.final.state(node_first$state, state_final, problem)) {
      end_reason <- "Solution"
      
      # Record stats for final iteration
      rep_iteration[count] <- count
      rep_frontier[count]  <- length(frontier)
      rep_depth[count]     <- node_first$depth
      rep_added[count]     <- 0
      break
    }
    
    # 5. Expand Node
    successor_nodes <- expand.node(node_first, actions_possible, problem)
    nodes_added_curr <- 0
    
    if (length(successor_nodes) > 0) {
      
      # Graph Search: Cost-Aware Filtering (Based on g(n))
      if (graph_search) {
        valid_successors <- list()
        
        for (succ in successor_nodes) {
          sid_succ <- to.string(succ$state, problem)
          new_g    <- succ$cost
          old_g    <- best_g[[sid_succ]]
          
          # Accept if new state OR strictly better path found
          if (is.null(old_g) || new_g < old_g) {
            best_g[[sid_succ]] <- new_g
            valid_successors[[length(valid_successors) + 1]] <- succ
          }
        }
        successor_nodes <- valid_successors
      }
      
      # Add successors to frontier
      if (length(successor_nodes) > 0) {
        frontier <- c(frontier, successor_nodes)
        nodes_added_curr <- length(successor_nodes)
      }
    }
    
    # Update report vectors
    rep_iteration[count] <- count
    rep_frontier[count]  <- length(frontier)
    rep_depth[count]     <- node_first$depth
    rep_added[count]     <- nodes_added_curr
    
    count <- count + 1
  }
  
  # Handle loop termination
  if (is.null(end_reason)) {
    end_reason <- "Iterations"
  }
  
  # Build final Report Data Frame
  valid_idx <- rep_iteration > 0
  
  report <- data.frame(iteration            = rep_iteration[valid_idx],
                       nodes_frontier       = rep_frontier[valid_idx],
                       depth_of_expanded    = rep_depth[valid_idx],
                       nodes_added_frontier = rep_added[valid_idx])
  
  end_time <- Sys.time()
  
  # Construct Result Object
  result <- list()
  result$name       <- name_method
  result$runtime    <- end_time - start_time
  result$report     <- report
  result$end_reason <- end_reason
  result$node_final <- if (end_reason == "Solution") node_first else NA
  
  # Final Output
  if (end_reason == "Solution") {
    print("Solution found!", quote = FALSE)
    print(to.string(node_first$state, problem), quote = FALSE)
    print("Executed Actions: ", quote = FALSE)
    print(node_first$actions, quote = FALSE)
  } else {
    print(paste0("No Solution found. Reason: ", end_reason), quote = FALSE)
  }
  
  print(paste0("* END: ", name_method), quote = FALSE)
  
  return(result)
}