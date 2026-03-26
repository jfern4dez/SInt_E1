# =========================================================================
# Main Script: LLM Inference Orchestration
# =========================================================================
# Solves LLM Orchestration instances loaded from text files.
# Muestra por cada algoritmo:
#   - Coste unificado (ms equivalentes) — usado internamente por A* etc.
#   - Latencia total real (ms)
#   - Coste económico real (€)
# =========================================================================

# 1. Limpiar entorno
rm(list = ls())
cat("\014")

# 2. Directorio de trabajo
if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# 3. Cargar algoritmos y utilidades
source("../algorithms/blind/expand-node.R")
source("../algorithms/blind/breadth-first-search.R")
source("../algorithms/blind/depth-first-search.R")
source("../algorithms/blind/depth-limited-search.R")
source("../algorithms/blind/iterative-deepening-search.R")
source("../algorithms/informed/uniform-cost-search.R")
source("../algorithms/informed/greedy-best-first-search.R")
source("../algorithms/informed/a-star-search.R")
source("../algorithms/results-analysis/analyze-results.R")

# 4. Cargar definición del problema
source("../problems/LLM.R")


# =========================================================================
# decompose.solution(result, problem)
# =========================================================================
# Recorre el path de acciones de una solución y calcula por separado:
#   - latencia_ms : suma de todos los tiempos reales (GEN_LAT, SWITCH_LAT,
#                   latencias RAG) — sin ninguna conversión de euros
#   - coste_eur   : suma de los tickets RAG abiertos durante el camino
#
# Esto deshace la unificación que hace get.cost() y permite mostrar
# ambas métricas de forma comprensible al usuario.
#
# Devuelve una lista con: latencia_ms, coste_eur
# Si el algoritmo no encontró solución devuelve NA en ambos campos.
# =========================================================================


# decompose.solution <- function(result, problem) {
#   
#   # Sin solución → devolver NA
#   if (is.null(result$actions) || length(result$actions) == 0) {
#     return(list(latencia_ms = NA, coste_eur = NA))
#   }
#   
#   # Helper para buscar proveedor por nombre
#   get_provider <- function(nombre) {
#     for (p in problem$providers) {
#       if (p$name == nombre) return(p)
#     }
#     stop(paste("Proveedor no encontrado:", nombre))
#   }
#   
#   latencia_ms <- 0
#   coste_eur   <- 0
#   
#   # Recorremos las acciones desde el estado inicial
#   state <- problem$state_initial
#   
#   for (action in result$actions) {
#     
#     modo_anterior <- state[3]   # 0=GEN, 1=PRO, 2=ECO
#     
#     # --- GEN: solo latencia de paso + posible switch ---
#     if (action %in% c("UP", "DOWN", "LEFT", "RIGHT",
#                       "UP_RIGHT", "DOWN_RIGHT", "DOWN_LEFT", "UP_LEFT")) {
#       latencia_ms <- latencia_ms + problem$gen_lat
#       if (modo_anterior != 0L) {
#         # Venía de RAG: penalización de tiempo por cambio de modo
#         latencia_ms <- latencia_ms + problem$switch_lat
#       }
#     }
#     
#     # --- JUMP_PRO ---
#     else if (action == "JUMP_PRO") {
#       pro <- get_provider("VectorDB_PRO")
#       latencia_ms <- latencia_ms + pro$lat
#       if (modo_anterior != 1L) {
#         # Primer uso de PRO en este tramo: switch + apertura de ticket
#         latencia_ms <- latencia_ms + problem$switch_lat
#         coste_eur   <- coste_eur   + pro$cost
#       }
#       # Si ya estábamos en PRO: ticket ya pagado, el salto es gratis en €
#     }
#     
#     # --- JUMP_ECO ---
#     else if (action == "JUMP_ECO") {
#       eco <- get_provider("VectorDB_ECO")
#       latencia_ms <- latencia_ms + eco$lat
#       if (modo_anterior != 2L) {
#         latencia_ms <- latencia_ms + problem$switch_lat
#         coste_eur   <- coste_eur   + eco$cost
#       }
#     }
#     
#     # Avanzar el estado
#     state <- effect(state, action, problem)
#   }
#   
#   return(list(latencia_ms = latencia_ms, coste_eur = coste_eur))
# }
# 

decompose.solution <- function(result, problem) {
  
  if (is.null(result$actions) || length(result$actions) == 0) {
    return(list(latencia_ms = NA, coste_eur = NA))
  }
  
  get_provider <- function(nombre) {
    for (p in problem$providers) {
      if (p$name == nombre) return(p)
    }
    stop(paste("Proveedor no encontrado:", nombre))
  }
  
  latencia_ms <- 0
  coste_eur   <- 0
  
  state <- problem$state_initial
  
  for (action in result$actions) {
    
    modo_anterior <- state[3]   # 0=GEN, 1=PRO, 2=ECO
    
    # --- GEN ---
    if (action %in% c("UP", "DOWN", "LEFT", "RIGHT",
                      "UP_RIGHT", "DOWN_RIGHT", "DOWN_LEFT", "UP_LEFT")) {
      latencia_ms <- latencia_ms + problem$gen_lat
      if (modo_anterior != 0L) {
        latencia_ms <- latencia_ms + problem$switch_lat
      }
    }
    
    # --- JUMP_PRO ---
    else if (action == "JUMP_PRO") {
      pro <- get_provider("VectorDB_PRO")
      # SIEMPRE se paga la latencia del salto
      latencia_ms <- latencia_ms + pro$lat
      if (modo_anterior != 1L) {
        # Primera apertura del ticket PRO: switch + coste económico
        latencia_ms <- latencia_ms + problem$switch_lat
        coste_eur   <- coste_eur   + pro$cost
      }
      # Si ya estábamos en PRO: solo la latencia (ya sumada arriba), sin coste adicional
    }
    
    # --- JUMP_ECO ---
    else if (action == "JUMP_ECO") {
      eco <- get_provider("VectorDB_ECO")
      # SIEMPRE se paga la latencia del salto
      latencia_ms <- latencia_ms + eco$lat
      if (modo_anterior != 2L) {
        # Primera apertura del ticket ECO: switch + coste económico
        latencia_ms <- latencia_ms + problem$switch_lat
        coste_eur   <- coste_eur   + eco$cost
      }
    }
    
    # Avanzar el estado
    state <- effect(state, action, problem)
  }
  
  return(list(latencia_ms = latencia_ms, coste_eur = coste_eur))
}
# 
# # =========================================================================
# # print.solution.table(results_list, labels, problem)
# # =========================================================================
# # Imprime una tabla legible con las métricas reales de cada algoritmo:
# #   Algoritmo | ¿Solución? | Pasos | Latencia (ms) | Coste (€) | Nodos expandidos
# # =========================================================================
# print.solution.table <- function(results_list, labels, problem) {
#   
#   cat("\n")
#   cat(sprintf("%-22s | %-10s | %-6s | %-14s | %-10s | %-8s\n",
#               "Algoritmo", "Solución", "Pasos", "Latencia (ms)", "Coste (€)", "Nodos"))
#   cat(strrep("-", 85), "\n")
#   
#   for (i in seq_along(results_list)) {
#     res   <- results_list[[i]]
#     label <- labels[[i]]
#     
#     tiene_sol <- !is.null(res$actions) && length(res$actions) > 0
#     pasos     <- if (tiene_sol) length(res$actions) else "-"
#     nodos     <- if (!is.null(res$nodes_expanded)) res$nodes_expanded else "-"
#     
#     if (tiene_sol) {
#       descomp   <- decompose.solution(res, problem)
#       lat_str   <- sprintf("%.1f", descomp$latencia_ms)
#       eur_str   <- sprintf("%.4f", descomp$coste_eur)
#     } else {
#       lat_str <- "-"
#       eur_str <- "-"
#     }
#     
#     cat(sprintf("%-22s | %-10s | %-6s | %-14s | %-10s | %-8s\n",
#                 label,
#                 if (tiene_sol) "SI" else "NO",
#                 pasos,
#                 lat_str,
#                 eur_str,
#                 nodos))
#   }
#   cat("\n")
# }


# =========================================================================
# solve.instance(file, ...)
# =========================================================================
solve.instance <- function(file = NULL,
                           max_iterations = 2500,
                           depth_limit = 20,
                           max_depth = 20,
                           export_csv = TRUE,
                           csv_dir = "../results",
                           verbose = TRUE) {
  
  if (export_csv && !dir.exists(csv_dir)) {
    dir.create(csv_dir, recursive = TRUE)
  }
  
  problem <- initialize.problem(file = file)
  
  cat("\n============================================================\n")
  cat(paste0("Problema: ", problem$name, "\n"))
  cat(paste0("Inicio:   fila=", problem$state_initial[1],
             " col=", problem$state_initial[2], "\n"))
  cat(paste0("Meta:     fila=", problem$state_final[1],
             " col=", problem$state_final[2], "\n"))
  cat(paste0("GEN_LAT=", problem$gen_lat, "ms  |  ",
             "SWITCH_LAT=", problem$switch_lat, "ms  |  ",
             "EUR_PER_SEC=", problem$eur_per_sec, "\n"))
  cat("============================================================\n\n")
  
  # --- Ejecutar todos los algoritmos ---
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
  
  results_list <- list(
    bfs_ts, bfs_gs,
    dfs_ts, dfs_gs,
    dls_ts, dls_gs,
    ids_ts, ids_gs,
    ucs_ts, ucs_gs,
    gbfs_ts, gbfs_gs,
    astar_ts, astar_gs
  )
  
  labels <- list(
    "BFS (Tree)", "BFS (Graph)",
    "DFS (Tree)", "DFS (Graph)",
    "DLS (Tree)", "DLS (Graph)",
    "IDS (Tree)", "IDS (Graph)",
    "UCS (Tree)", "UCS (Graph)",
    "GBFS (Tree)", "GBFS (Graph)",
    "A* (Tree)", "A* (Graph)"
  )
  
  # --- Tabla estándar de analyze.results (coste unificado) ---
  results_df <- analyze.results(
    results  = results_list,
    problem  = problem,
    verbose  = verbose,
    export_csv = export_csv,
    csv_dir  = csv_dir
  )
  
  # --- Tabla con latencia real y coste en € desglosados ---
  cat("\n>>> Desglose real de latencia y coste económico por algoritmo:\n")
  print.solution.table(results_list, labels, problem)
  
  # --- Tabla bonita en RStudio Viewer si está disponible ---
  if (requireNamespace("kableExtra", quietly = TRUE) &&
      requireNamespace("knitr", quietly = TRUE)) {
    print(kableExtra::kable_material(
      kableExtra::kbl(results_df, caption = paste("Resultados:", problem$name)),
      c("striped", "hover", "condensed")
    ))
  }
  
  return(invisible(list(results = results_df, results_list = results_list)))
}


# =========================================================================
# 6. Ejecutar instancias
# =========================================================================
files <- c(
  "../data/llm-orchestration/01-loop-trap.txt"
  # Añade más instancias aquí:
  # "../data/llm-orchestration/02-otra-instancia.txt"
)

for (f in files) {
  solve.instance(file = f)
}

