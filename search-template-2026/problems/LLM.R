# =========================================================================
# PROBLEM FORMULATION: LLM INFERENCE ORCHESTRATION
# =========================================================================
#
# ESTADO AMPLIADO — vector de longitud 3: c(row, col, modo)
#
#   row  : fila actual en la cuadrícula (1-based)
#   col  : columna actual en la cuadrícula (1-based)
#   modo : entero que indica el modo de operación actual
#            0 = GEN        (generación interna, paso a celda contigua)
#            1 = VectorDB_PRO  (ticket RAG abierto con proveedor PRO)
#            2 = VectorDB_ECO  (ticket RAG abierto con proveedor ECO)
#
# Por qué incluir el modo en el estado:
#   El coste de cada acción depende del modo ANTERIOR (si cambia de modo
#   se aplica SWITCH_LAT y se paga el ticket del nuevo proveedor RAG).
#   Sin el modo en el estado, get.cost() no puede calcularse correctamente,
#   y Graph-Search trataría como iguales dos nodos en la misma celda pero
#   con distintos modos, lo que impediría encontrar el camino óptimo.
#
# Acciones posibles:
#   GEN  : UP, DOWN, LEFT, RIGHT, UP_RIGHT, DOWN_RIGHT, DOWN_LEFT, UP_LEFT
#   RAG  : JUMP_ECO, JUMP_PRO
#
# Modelo de transición:
#   GEN  → mueve una celda en la dirección indicada, actualiza modo a 0
#   RAG  → salta al destino definido en rag_links, actualiza modo a 1 o 2
#
# Restricciones:
#   1. Límites del mapa: no se puede salir de la cuadrícula
#   2. Obstáculos (#): no se puede entrar en una celda bloqueada
#   3. RAG Links: el salto solo es aplicable si existe el enlace desde
#      la posición actual para ese proveedor
#
# Test de estado final:
#   Las coordenadas (row, col) coinciden con state_final
#   (el modo no importa para el test de meta)
#
# Función de coste — unidad: milisegundos equivalentes
#   Se unifica tiempo y dinero mediante: ms_equiv = euros × (1000 / EUR_PER_SEC)
#   Esto permite que A* minimice simultáneamente latencia y coste económico.
#
# Heurística (problema relajado):
#   Distancia de Chebyshev × GEN_LAT
#   Chebyshev en lugar de Manhattan porque los movimientos diagonales están
#   permitidos. Se ignoran obstáculos, penalizaciones de cambio y costes
#   de ticket → la heurística nunca sobreestima → admisible.
#
# to.string():
#   Codifica "row,col,modo" — el modo es necesario para que Graph-Search
#   distinga correctamente entre estados con igual posición pero modo distinto.
#
# Authors / Maintainers:
#   - Roberto Carballedo
#   - Fernando Boto
#   - Enrique Onieva
#
# Last updated: March 2026
# Educational use only — University of Deusto
# =========================================================================


# =========================================================================
# initialize.problem(file, random_actions)
# =========================================================================
initialize.problem <- function(file, random_actions = FALSE) {
  if (!file.exists(file)) stop(paste0("File not found: ", file))
  
  lines <- readLines(file)
  # Eliminar comentarios (#) y líneas vacías
  lines <- lines[!grepl("^#", lines) & nchar(trimws(lines)) > 0]
  
  problem <- list()
  problem$name <- paste0("LLM Orchestration - [", basename(file), "]")
  
  # --- Helper: extraer sección entre [HEADER] ---
  get_section <- function(lines, header) {
    start <- which(lines == paste0("[", header, "]"))
    if (length(start) == 0) return(character(0))
    end_candidates <- which(grepl("^\\[", lines))
    end_candidates <- end_candidates[end_candidates > start]
    end <- if (length(end_candidates) > 0) end_candidates[1] - 1 else length(lines)
    lines[(start + 1):end]
  }
  
  # -----------------------------------------------
  # 1. PARAMS
  # -----------------------------------------------
  params_lines <- get_section(lines, "PARAMS")
  
  parse_kv <- function(key, lines) {
    match <- grep(paste0("^", key, "="), lines, value = TRUE)
    if (length(match) == 0) return(NULL)
    as.numeric(sub(paste0("^", key, "="), "", match[1]))
  }
  
  problem$gen_lat     <- parse_kv("GEN_LAT",    params_lines)
  problem$switch_lat  <- parse_kv("SWITCH_LAT", params_lines)
  problem$eur_per_sec <- parse_kv("EUR_PER_SEC", params_lines)
  
  # Providers: lista de listas con name, lat, cost
  provider_lines <- grep("^PROVIDER=", params_lines, value = TRUE)
  problem$providers <- lapply(provider_lines, function(p) {
    parts <- unlist(strsplit(sub("^PROVIDER=", "", p), ";"))
    kv <- setNames(
      as.numeric(sub(".*=", "", parts[-1])),
      sub("=.*", "", parts[-1])
    )
    list(name = parts[1], lat = kv["LAT"], cost = kv["COST"])
  })
  
  # -----------------------------------------------
  # 2. MAP
  # -----------------------------------------------
  map_lines <- get_section(lines, "MAP")
  
  problem$rows <- as.numeric(sub("^ROWS=", "", grep("^ROWS=", map_lines, value = TRUE)))
  problem$cols <- as.numeric(sub("^COLS=", "", grep("^COLS=", map_lines, value = TRUE)))
  
  # Leer la cuadrícula (líneas que no son clave=valor)
  grid_lines <- map_lines[!grepl("^[A-Z]+=", map_lines)]
  problem$grid <- do.call(rbind, lapply(grid_lines, function(row) {
    unlist(strsplit(trimws(row), " "))
  }))
  
  # Extraer posición Start (S) y Goal (G) desde la cuadrícula (1-based)
  find_cell <- function(grid, symbol) {
    pos <- which(grid == symbol, arr.ind = TRUE)
    c(pos[1, 1], pos[1, 2])  # c(row, col) en 1-based
  }
  
  # Estado inicial: c(row, col, modo=0)  — siempre arranca en modo GEN
  start_pos             <- find_cell(problem$grid, "S")
  problem$state_initial <- c(start_pos[1], start_pos[2], 0L)
  
  # Estado final: solo coordenadas (el modo no importa para el test de meta)
  goal_pos              <- find_cell(problem$grid, "G")
  problem$state_final   <- c(goal_pos[1], goal_pos[2])
  
  # -----------------------------------------------
  # 3. RAG_LINKS
  # -----------------------------------------------
  # Se almacenan como lista de listas: provider, from c(row,col), to c(row,col)
  # NOTA: en el fichero las coordenadas son X,Y (col,row en 0-based)
  #        → convertimos a row,col en 1-based para coherencia con el estado
  rag_lines <- get_section(lines, "RAG_LINKS")
  
  problem$rag_links <- list()
  
  for (l in rag_lines) {
    parts    <- unlist(strsplit(l, ";"))
    provider <- parts[1]
    
    # Origen: X,Y (0-based) → c(row, col) (1-based)
    from_xy  <- as.numeric(unlist(strsplit(parts[2], ",")))
    from_rc  <- c(from_xy[2] + 1, from_xy[1] + 1)   # row = Y+1, col = X+1
    
    # Destino: X,Y (0-based) → c(row, col) (1-based)
    to_xy <- as.numeric(unlist(strsplit(parts[3], ",")))
    to_rc <- c(to_xy[2] + 1, to_xy[1] + 1)
    
    # Clave de búsqueda: "row,col" del origen
    clave <- paste(from_rc, collapse = ",")
    
    if (is.null(problem$rag_links[[provider]])) {
      problem$rag_links[[provider]] <- list()
    }
    problem$rag_links[[provider]][[clave]] <- to_rc
  }
  
  # -----------------------------------------------
  # 4. Acciones posibles
  # -----------------------------------------------
  problem$actions_possible <- c(
    "UP", "DOWN", "LEFT", "RIGHT",
    "UP_RIGHT", "DOWN_RIGHT", "DOWN_LEFT", "UP_LEFT",
    "JUMP_ECO", "JUMP_PRO"
  )
  
  if (random_actions) {
    problem$actions_possible <- sample(problem$actions_possible)
  }
  
  return(problem)
}


# =========================================================================
# is.applicable(state, action, problem)
# =========================================================================
# Comprueba si una acción es ejecutable desde el estado actual.
# Condiciones generales para GEN: destino dentro del mapa y sin obstáculo.
# Condiciones para RAG: existe un enlace desde la posición actual para ese proveedor.
# =========================================================================
is.applicable <- function(state, action, problem) {
  row <- state[1]
  col <- state[2]
  # modo <- state[3]   # no afecta a la aplicabilidad, solo al coste
  
  next_row <- row
  next_col <- col
  
  # --- Movimientos GEN cardinales ---
  if (action == "UP") {
    next_row <- row - 1
    if (next_row < 1) return(FALSE)
    
  } else if (action == "DOWN") {
    next_row <- row + 1
    if (next_row > problem$rows) return(FALSE)
    
  } else if (action == "LEFT") {
    next_col <- col - 1
    if (next_col < 1) return(FALSE)
    
  } else if (action == "RIGHT") {
    next_col <- col + 1
    if (next_col > problem$cols) return(FALSE)
    
    # --- Movimientos GEN diagonales ---
  } else if (action == "UP_RIGHT") {
    next_row <- row - 1
    next_col <- col + 1
    if (next_row < 1 || next_col > problem$cols) return(FALSE)
    
  } else if (action == "DOWN_RIGHT") {
    next_row <- row + 1
    next_col <- col + 1
    if (next_row > problem$rows || next_col > problem$cols) return(FALSE)
    
  } else if (action == "DOWN_LEFT") {
    next_row <- row + 1
    next_col <- col - 1
    if (next_row > problem$rows || next_col < 1) return(FALSE)
    
  } else if (action == "UP_LEFT") {
    next_row <- row - 1
    next_col <- col - 1
    if (next_row < 1 || next_col < 1) return(FALSE)
    
    # --- Saltos RAG ---
  } else if (action == "JUMP_ECO") {
    clave <- paste(row, col, sep = ",")
    if (is.null(problem$rag_links[["VectorDB_ECO"]][[clave]])) return(FALSE)
    return(TRUE)   # el salto no requiere más comprobaciones de límite
    
  } else if (action == "JUMP_PRO") {
    clave <- paste(row, col, sep = ",")
    if (is.null(problem$rag_links[["VectorDB_PRO"]][[clave]])) return(FALSE)
    return(TRUE)
    
  } else {
    return(FALSE)
  }
  
  # Comprobación de obstáculo para movimientos GEN
  if (problem$grid[next_row, next_col] == "#") return(FALSE)
  
  return(TRUE)
}


# =========================================================================
# effect(state, action, problem)
# =========================================================================
# Devuelve el nuevo estado tras aplicar la acción.
# El estado resultado es siempre c(new_row, new_col, new_modo).
# =========================================================================
effect <- function(state, action, problem) {
  row  <- state[1]
  col  <- state[2]
  # modo anterior no se necesita aquí; el nuevo modo se asigna según la acción
  
  # --- Movimientos GEN: modo pasa a 0 ---
  if (action == "UP")         return(c(row - 1, col,     0L))
  if (action == "DOWN")       return(c(row + 1, col,     0L))
  if (action == "LEFT")       return(c(row,     col - 1, 0L))
  if (action == "RIGHT")      return(c(row,     col + 1, 0L))
  if (action == "UP_RIGHT")   return(c(row - 1, col + 1, 0L))
  if (action == "DOWN_RIGHT") return(c(row + 1, col + 1, 0L))
  if (action == "DOWN_LEFT")  return(c(row + 1, col - 1, 0L))
  if (action == "UP_LEFT")    return(c(row - 1, col - 1, 0L))
  
  # --- Saltos RAG: modo pasa a 1 (PRO) o 2 (ECO) ---
  if (action == "JUMP_PRO") {
    clave   <- paste(row, col, sep = ",")
    destino <- problem$rag_links[["VectorDB_PRO"]][[clave]]
    return(c(destino[1], destino[2], 1L))
  }
  
  if (action == "JUMP_ECO") {
    clave   <- paste(row, col, sep = ",")
    destino <- problem$rag_links[["VectorDB_ECO"]][[clave]]
    return(c(destino[1], destino[2], 2L))
  }
  
  stop(paste("Acción desconocida en effect():", action))
}


# =========================================================================
# is.final.state(state, problem)
# =========================================================================
# Un estado es final si las coordenadas (row, col) coinciden con la meta.
# El modo no forma parte del criterio de parada.
# =========================================================================
is.final.state <- function(state, state_final, problem) {
  return(state[1] == state_final[1] &&
           state[2] == state_final[2])
}


# =========================================================================
# to.string(state, problem)
# =========================================================================
# Genera la representación del estado como string.
# Incluye los 3 elementos: "row,col,modo"
#
# Esencial para Graph-Search: sin el modo, dos nodos en la misma celda
# pero con distintos modos (distinto coste acumulado real) se tratarían
# como el mismo estado y uno de ellos se descartaría incorrectamente.
# =========================================================================
to.string <- function(state, problem) {
  return(paste(state, collapse = ","))
}


# =========================================================================
# get.cost(action, state, problem)
# =========================================================================
# Devuelve el coste de ejecutar una acción desde un estado.
# Unidad: milisegundos equivalentes (ms_equiv).
#
# Conversión dinero → tiempo:
#   ms_equiv = euros × (1000 / EUR_PER_SEC)
#   Ejemplo: si EUR_PER_SEC = 0.01, entonces 1€ = 100.000 ms
#   Esto hace que los costes económicos altos sean muy disuasorios.
#
# Lógica por tipo de acción:
#
#   GEN (UP/DOWN/LEFT/RIGHT y diagonales):
#     - Venía de GEN  → GEN_LAT
#     - Venía de RAG  → SWITCH_LAT + GEN_LAT   (cerrar ticket RAG)
#
#   JUMP_PRO:
#     - Venía de PRO  → LAT_PRO                (ticket ya abierto, gratis)
#     - Venía de GEN o ECO → SWITCH_LAT + LAT_PRO + COST_PRO×ms_per_euro
#
#   JUMP_ECO:
#     - Venía de ECO  → LAT_ECO                (ticket ya abierto, gratis)
#     - Venía de GEN o PRO → SWITCH_LAT + LAT_ECO + COST_ECO×ms_per_euro
# =========================================================================
# get.cost <- function(action, state, problem) {
#   
#   modo_anterior <- state[3]   # 0=GEN, 1=PRO, 2=ECO
#   
#   # Factor de conversión: 1 euro = cuántos ms equivalentes
#   ms_per_euro <- 1000 / problem$eur_per_sec
#   
#   # Helper: obtener proveedor por nombre
#   get_provider <- function(nombre) {
#     for (p in problem$providers) {
#       if (p$name == nombre) return(p)
#     }
#     stop(paste("Proveedor no encontrado:", nombre))
#   }
#   
#   # --- Acciones GEN ---
#   if (action %in% c("UP", "DOWN", "LEFT", "RIGHT",
#                     "UP_RIGHT", "DOWN_RIGHT", "DOWN_LEFT", "UP_LEFT")) {
#     if (modo_anterior == 0L) {
#       # Continuamos en GEN → sin penalización
#       return(problem$gen_lat)
#     } else {
#       # Veníamos de un proveedor RAG → penalización por cierre de ticket
#       return(problem$switch_lat + problem$gen_lat)
#     }
#   }
#   
#   # --- Acción JUMP_PRO ---
#   if (action == "JUMP_PRO") {
#     pro <- get_provider("VectorDB_PRO")
#     if (modo_anterior == 1L) {
#       # Ticket PRO ya activo → solo latencia del salto
#       return(pro$lat)
#     } else {
#       # Abrimos ticket PRO → switch + latencia + coste del ticket
#       return(problem$switch_lat + pro$lat + pro$cost * ms_per_euro)
#     }
#   }
#   
#   # --- Acción JUMP_ECO ---
#   if (action == "JUMP_ECO") {
#     eco <- get_provider("VectorDB_ECO")
#     if (modo_anterior == 2L) {
#       # Ticket ECO ya activo → solo latencia del salto
#       return(eco$lat)
#     } else {
#       # Abrimos ticket ECO → switch + latencia + coste del ticket
#       return(problem$switch_lat + eco$lat + eco$cost * ms_per_euro)
#     }
#   }
#   
#   stop(paste("Acción desconocida en get.cost():", action))
# }

get.cost <- function(action, state, problem) {
  
  modo_anterior <- state[3]   # 0=GEN, 1=PRO, 2=ECO
  
  ms_per_euro <- 1000 / problem$eur_per_sec
  
  get_provider <- function(nombre) {
    for (p in problem$providers) {
      if (p$name == nombre) return(p)
    }
    stop(paste("Proveedor no encontrado:", nombre))
  }
  
  # --- Acciones GEN ---
  if (action %in% c("UP", "DOWN", "LEFT", "RIGHT",
                    "UP_RIGHT", "DOWN_RIGHT", "DOWN_LEFT", "UP_LEFT")) {
    if (modo_anterior == 0L) {
      return(problem$gen_lat)
    } else {
      # Veníamos de RAG → penalización por cierre de ticket
      return(problem$switch_lat + problem$gen_lat)
    }
  }
  
  # --- Acción JUMP_PRO ---
  if (action == "JUMP_PRO") {
    pro <- get_provider("VectorDB_PRO")
    if (modo_anterior == 1L) {
      # Ticket PRO ya activo → solo latencia del salto, sin coste económico
      return(pro$lat)
    } else {
      # Abrimos ticket PRO → switch + latencia + coste del ticket
      return(problem$switch_lat + pro$lat + pro$cost * ms_per_euro)
    }
  }
  
  # --- Acción JUMP_ECO ---
  if (action == "JUMP_ECO") {
    eco <- get_provider("VectorDB_ECO")
    if (modo_anterior == 2L) {
      # Ticket ECO ya activo → solo latencia del salto, sin coste económico
      return(eco$lat)
    } else {
      # Abrimos ticket ECO → switch + latencia + coste del ticket
      return(problem$switch_lat + eco$lat + eco$cost * ms_per_euro)
    }
  }
  
  stop(paste("Acción desconocida en get.cost():", action))
}





# =========================================================================
# get.evaluation(state, problem)
# =========================================================================
# Heurística admisible h(n) para búsquedas informadas (GBFS, A*).
#
# Técnica del problema relajado:
#   Se eliminan las restricciones del problema real:
#     1. No hay obstáculos → el sistema puede ir en línea recta
#     2. No hay penalizaciones de cambio de modo (SWITCH_LAT = 0)
#     3. No hay costes de apertura de ticket RAG
#     4. Se asume siempre el coste mínimo por paso (GEN_LAT)
#
# Distancia de Chebyshev:
#   max(|Δrow|, |Δcol|)
#   Es la distancia correcta cuando se permiten los 8 movimientos
#   (cardinales + diagonales), ya que una diagonal avanza simultáneamente
#   en dos ejes. Con Manhattan se sobreestimaría → no sería admisible.
#
# Unidad: ms equivalentes (coherente con get.cost())
#   h(n) = Chebyshev × GEN_LAT
#
# Admisibilidad garantizada:
#   - GEN_LAT es el coste mínimo posible por paso
#   - Chebyshev es el mínimo número de pasos sin obstáculos ni switches
#   - Por tanto h(n) ≤ coste_real(n → goal) siempre
# =========================================================================
get.evaluation <- function(state, problem) {
  row <- state[1]
  col <- state[2]
  
  goal_row <- problem$state_final[1]
  goal_col <- problem$state_final[2]
  
  # Distancia de Chebyshev: mínimo de pasos con movimiento en 8 direcciones
  distancia_chebyshev <- max(abs(row - goal_row), abs(col - goal_col))
  
  # Heurística en ms equivalentes
  return(distancia_chebyshev * problem$gen_lat)
}


# =========================================================================
# load.problem(filename)   — helper de conveniencia
# =========================================================================
load.problem <- function(filename) {
  file_path <- paste0(
    "C:/Users/ibai.velada/ProyectoR/SInt_E1/search-template-2026/data/llm-orchestration/",
    filename
  )
  return(initialize.problem(file_path))
}

# Prueba rápida:
# p <- load.problem("01-loop-trap.txt")
# cat("Estado inicial:", p$state_initial, "\n")
# cat("Estado final:  ", p$state_final, "\n")