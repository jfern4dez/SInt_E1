# =========================================================================
# Depth First Search (DFS)
# =========================================================================
# Depth-First Search (DFS) expands the deepest node first (LIFO order).
# It explores one branch as far as possible before backtracking.
# DFS is not optimal and may not be complete in infinite or cyclic search spaces.
#
# Arguments:
#   problem       : The problem object.
#   max_iterations: Maximum number of nodes to expand.
#   count_print   : Interval for printing trace information.
#   graph_search  : If TRUE, checks for repeated states to avoid loops.
# -------------------------------------------------------------------------
# NOTE ON GRAPH SEARCH AND REPEATED STATES
# -------------------------------------------------------------------------
# In Tree Search (graph_search = FALSE), the same state may be generated many
# times through different paths. In DFS, this is critical because it leads 
# to infinite loops in cyclic graphs (going deeper indefinitely).
#
# In Graph Search (graph_search = TRUE), we avoid expanding repeated states:
#   - frontier_set : States currently in the stack (prevents cycles).
#   - expanded_set : States already processed (prevents redundant paths).
#
# EFFICIENCY NOTE:
# Although 'frontier' is a list/stack of nodes, checking membership in a 
# list is expensive (O(n)). Therefore, we use an auxiliary hash-based set 
# (frontier_set) to test membership in O(1). This redundancy keeps DFS 
# simple and extremely fast, even with large frontiers.
#
# THEORETICAL NOTE:
# This "discard-if-seen" pruning strategy is correct for BFS and DFS because
# they do not guarantee optimal cost. For optimal algorithms like UCS or A*, 
# repeated-state handling must be cost-aware (checking g-values).
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

depth.first.search <- function(problem,
                               max_iterations = 1000,
                               count_print = 100,
                               graph_search = FALSE) {
  
  name_method      <- paste0("Depth First Search", ifelse(graph_search, " + GS", ""))
  state_initial    <- problem$state_initial
  state_final      <- problem$state_final
  actions_possible <- problem$actions_possible
  
  # Initialization
  print(paste0("* START: ", name_method), quote = FALSE)
  start_time <- Sys.time()
  
  # Root node
  node <- list(parent = NULL,
               state = state_initial,
               actions = NULL,
               depth = 0,
               cost = 0,
               evaluation = 0)
  
  frontier <- list(node)
  
  # Graph Search: Hash sets for O(1) repeated-state checking
  if (graph_search) {
    expanded_set <- new.env(hash = TRUE, parent = emptyenv())
    frontier_set <- new.env(hash = TRUE, parent = emptyenv())
    
    sid_init <- to.string(state_initial, problem)
    if (nchar(sid_init) == 0) stop("to.string returned empty string")
    
    frontier_set[[sid_init]] <- TRUE
  }
  
  # Pre-allocate report vectors for performance
  # (Replaces the slow list-of-lists approach)
  rep_iteration <- numeric(max_iterations)
  rep_frontier  <- numeric(max_iterations)
  rep_depth     <- numeric(max_iterations)
  rep_added     <- numeric(max_iterations)
  
  count <- 1
  end_reason <- NULL
  
  # Main Loop
  while (count <= max_iterations) {
    
    # Print trace
    if (count %% count_print == 0) {
      print(paste0("Iteration: ", count, ", Nodes in the frontier: ", length(frontier)), quote = FALSE)
    }
    
    # 1. Check if Frontier is empty
    if (length(frontier) == 0) {
      end_reason <- "Frontier"
      break
    }
    
    # 2. Pop first node (LIFO behavior depends on how we ADD successors later)
    node_first <- frontier[[1]]
    frontier[[1]] <- NULL
    
    # Graph Search maintenance (Move from Frontier set to Expanded set)
    if (graph_search) {
      sid_first <- to.string(node_first$state, problem)
      if (!is.null(frontier_set[[sid_first]])) frontier_set[[sid_first]] <- NULL
      expanded_set[[sid_first]] <- TRUE
    }
    
    # 3. Goal Test
    if (is.final.state(node_first$state, state_final, problem)) {
      end_reason <- "Solution"
      
      # Save stats for this final iteration
      rep_iteration[count] <- count
      rep_frontier[count]  <- length(frontier)
      rep_depth[count]     <- node_first$depth
      rep_added[count]     <- 0
      break
    }
    
    # 4. Expand Node
    successor_nodes <- expand.node(node_first, actions_possible, problem)
    nodes_added_curr <- 0
    
    if (length(successor_nodes) > 0) {
      
      # Graph Search: Filter repeated states
      if (graph_search) {
        valid_successors <- list()
        
        for (succ in successor_nodes) {
          sid_succ <- to.string(succ$state, problem)
          
          # Only add if NOT in frontier AND NOT in expanded
          if (is.null(frontier_set[[sid_succ]]) && is.null(expanded_set[[sid_succ]])) {
            valid_successors[[length(valid_successors) + 1]] <- succ
            frontier_set[[sid_succ]] <- TRUE
          }
        }
        successor_nodes <- valid_successors
      }
      
      # Add successors to the FRONT of the frontier (LIFO / Stack)
      if (length(successor_nodes) > 0) {
        frontier <- c(successor_nodes, frontier)
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
  # Filter only the valid rows (where iteration > 0)
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