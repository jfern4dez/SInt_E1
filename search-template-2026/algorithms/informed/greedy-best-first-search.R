# =========================================================================
# Greedy Best-First Search (GBFS)
# =========================================================================
# Greedy Best-First Search (GBFS) expands the node with the lowest heuristic 
# value h(n). It DOES NOT consider path cost g(n) for selection, so it is 
# neither optimal nor complete (it can get stuck in loops if no GS used).
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
# times through different paths. This can lead to infinite loops in cyclic
# graphs and a huge increase in the number of expanded nodes.
#
# In Graph Search (graph_search = TRUE), we avoid expanding repeated states:
#   - frontier_set : states that are currently in the frontier (waiting to be expanded)
#   - expanded_set : states that have already been expanded
#
# Although frontier is already a list of nodes, checking membership in that
# list would be expensive (O(n)). Therefore, we use an additional hash-based
# set (frontier_set) to test membership in O(1). This redundancy keeps GBFS
# simple and efficient, especially in problems with many repeated states.
#
# NOTE: In GBFS, repeated-state handling is "state-based" (like BFS/DFS), 
# not "cost-based" (like UCS/A*).
# Since GBFS orders nodes solely by the heuristic h(n), and h(n) depends 
# only on the state itself (it is constant for a given state), we never need 
# to re-open a state based on path cost g(n), as GBFS ignores g(n) for 
# ordering.
# -------------------------------------------------------------------------
#
# NOTE ON NODE FIELDS:
# - node$evaluation stores h(n) (Used for ordering the frontier)
# - node$cost stores g(n) (Calculated but NOT used for ordering)
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

greedy.best.first.search <- function(problem,
                                     max_iterations = 1000,
                                     count_print = 100,
                                     graph_search = FALSE) {
  
  name_method      <- paste0("Greedy Best-First Search", ifelse(graph_search, " + GS", ""))
  state_initial    <- problem$state_initial
  state_final      <- problem$state_final
  actions_possible <- problem$actions_possible
  
  # Initialization
  print(paste0("* START: ", name_method), quote = FALSE)
  start_time <- Sys.time()
  
  # Root node
  # node$evaluation stores h(n)
  node <- list(parent = NULL,
               state = state_initial,
               actions = NULL,
               depth = 0,
               cost = 0,
               evaluation = get.evaluation(state_initial, problem))
  
  frontier <- list(node)
  
  # Graph Search: Hash sets for O(1) repeated-state pruning
  if (graph_search) {
    expanded_set <- new.env(hash = TRUE, parent = emptyenv())
    frontier_set <- new.env(hash = TRUE, parent = emptyenv())
    
    sid_init <- to.string(state_initial, problem)
    frontier_set[[sid_init]] <- TRUE
  }
  
  # Pre-allocate report vectors for performance
  rep_iteration <- numeric(max_iterations)
  rep_frontier  <- numeric(max_iterations)
  rep_depth     <- numeric(max_iterations)
  rep_added     <- numeric(max_iterations)
  
  count <- 1
  end_reason <- NULL
  
  # Main Loop
  while (count <= max_iterations) {
    
    # 1. Priority Queue Simulation: Sort frontier by Heuristic h(n) (Ascending)
    if (length(frontier) > 1) {
      evals <- sapply(frontier, function(x) x$evaluation)
      frontier <- frontier[order(evals)]
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
    
    # 3. Pop the lowest-heuristic node
    node_first <- frontier[[1]]
    frontier[[1]] <- NULL
    
    # Graph Search maintenance (Move from Frontier set to Expanded set)
    if (graph_search) {
      sid_first <- to.string(node_first$state, problem)
      if (!is.null(frontier_set[[sid_first]])) frontier_set[[sid_first]] <- NULL
      expanded_set[[sid_first]] <- TRUE
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
      
      # Graph Search: Filter repeated states
      if (graph_search) {
        valid_successors <- list()
        
        for (succ in successor_nodes) {
          sid_succ <- to.string(succ$state, problem)
          
          # Since h(n) depends only on state, if we have seen the state, 
          # we have seen its h(n). We can safely discard duplicates.
          if (is.null(frontier_set[[sid_succ]]) && is.null(expanded_set[[sid_succ]])) {
            valid_successors[[length(valid_successors) + 1]] <- succ
            frontier_set[[sid_succ]] <- TRUE
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