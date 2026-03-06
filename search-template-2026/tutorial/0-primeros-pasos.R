# =========================================================================
# TUTORIAL DE INTRODUCCIÓN A R
# =========================================================================
# Este documento constituye una guía acelerada sobre la sintaxis y las 
# estructuras de datos del lenguaje R, diseñada específicamente para la 
# asignatura de Sistemas Inteligentes.
#
# CONSIDERACIONES INICIALES:
#
# 1. NATURALEZA DEL LENGUAJE: R es un lenguaje interpretado y dinámico. 
#    No requiere compilación previa y el tipado de variables es implícito.
#
# 2. INDEXACIÓN: A diferencia de la mayoría de lenguajes de programación,
#    R utiliza una INDEXACIÓN BASADA EN 1. El primer elemento de cualquier
#    estructura (vector o matriz) ocupa la posición 1.
#
# 3. PARADIGMA VECTORIAL: El motor de R está optimizado para operaciones 
#    sobre vectores completos. Se debe priorizar la vectorización frente al 
#    uso de bucles 'for' explícitos para maximizar la eficiencia.
#
# 4. CONVENCIONES DE NOMENCLATURA (ESTILO DEL CURSO):
#    - Ficheros: Uso de minúsculas y guion medio (ej: 8-puzzle.R).
#    - Variables: Uso de minúsculas y guion bajo (ej: initial_state).
#    - Funciones: Uso de minúsculas y punto (ej: is.applicable()).
#    - Capitalización: No se utiliza CamelCase ni PascalCase.
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# 0) EJECUCIÓN DE CÓDIGO Y CONFIGURACIÓN INICIAL
# -------------------------------------------------------------------------
# A diferencia de Java, R permite un flujo de trabajo interactivo.
#
# --- Formas de ejecución en RStudio ---
#
# OPCIÓN A: Ejecución línea a línea o bloque de líneas
#    Se sitúa el cursor en la línea (o se selecciona un bloque) y se pulsa:
#    - Windows / Linux: Ctrl + Enter
#    - MacOS: Cmd + Enter
#
# OPCIÓN B: Ejecución del script completo (Source)
#    Equivale a la ejecución del método 'main'. Se pulsa el botón "Source" 
#    o mediante el atajo:
#    - Windows / Linux: Ctrl + Shift + S
#    - MacOS: Cmd + Shift + S
#
# --- Gestión de Errores ---
# Los errores se notifican con el prefijo "Error..." en color rojo y detienen
# la ejecución. Los "Warnings" son avisos que no interrumpen el proceso.

# --- Configuración del Directorio de Trabajo ---
# NOTA: Este bloque intenta configurar el directorio automáticamente.
# Es necesario tener instalado el paquete 'rstudioapi'.
tryCatch({
  if (rstudioapi::isAvailable()) {
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  }
}, error = function(e) {
  cat("No se pudo configurar el directorio de trabajo automáticamente.\n")
  # Utiliza el menú "Session > Set Working Directory > To Source File Location"
  # para hacerlo manualmente.
})

# --- Herramientas de Depuración (Debugging) ---
# A diferencia de Java, donde pones un 'breakpoint' visual (punto rojo),
# en R se utiliza la instrucción browser().
#
# debug_example <- function() {
#   x <- 10
#   browser()  # <-- La ejecución se DETIENE aquí.
#   # En la consola aparecerá 'Browse[1]>'. Podrás inspeccionar variables
#   # escribiendo sus nombres o avanzar línea a línea pulsando 'n'.
#   return(x * 2)
# }

# --- Limpieza del entorno (Invocación de funciones) ---
# En R, el uso de 'nombre.funcion()' representa la INVOCACIÓN de una función,
# equivalente a llamar a un método estático en Java.

rm(list = ls()) 
# rm() es la invocación a la función "remove". 
# Se le pasa como argumento una lista con todos los objetos actuales (ls()) 
# para limpiar la memoria (Panel Environment) y empezar desde cero.
# ADVERTENCIA: Esto eliminará todas las variables, funciones y objetos definidos.

cat("\014")      
# cat() es la invocación a la función "concatenate and print". 
# El argumento "\014" es un carácter especial que limpia el texto de la consola.

# -------------------------------------------------------------------------
# 1) TIPOS DE DATOS Y COMPROBACIONES
# -------------------------------------------------------------------------
# NOTA DE DISEÑO: En R no existe la declaración de variables sin asignación.
# No existen valores por defecto automáticos (como el 0 en int de Java).

# --- A) Tipos Atómicos Principales ---
un_logico   <- TRUE      # Logical (Equivalente a boolean: TRUE/FALSE o T/F)
un_numero   <- 42.5      # Numeric (double por defecto)
un_integer  <- 42L       # Integer (la 'L' fuerza el tipo entero)
un_string <- "State"     # String

# --- B) Valores Especiales ---
# NULL: Representa la ausencia de objeto (como null en Java).
# NA: (Not Available) Representa un dato faltante o desconocido.
# NaN: (Not a Number) Resultado de operaciones matemáticas imposibles.

# --- C) Funciones de Comprobación (is.*) ---
# En Java usarías 'instanceof'. En R usamos la familia is.xxx(). 
# Devuelven siempre un valor lógico.
is.numeric(un_numero)    # TRUE
is.logical(un_logico)    # TRUE
is.null(NULL)            # TRUE
is.na(un_numero)         # FALSE
is.character(un_string)  # TRUE

# -------------------------------------------------------------------------
# 2) PAUTAS DE SINTAXIS Y TRABAJO CON VECTORES
# -------------------------------------------------------------------------

# --- A) Sintaxis Básica (Diferencias con Java) ---
# 1. Punto y coma (;): No se utiliza al final de las instrucciones.
# 2. Asignación: Se emplea el operador <- para asignar valores a variables.
#    Aunque también se puede usar '=', el uso de '<-' es la convención más.
# 3. Operador de igualdad: Para comparar valores se usa '==' (doble igual), no '='.
#    El operador '=' se reserva para asignación de parámetros en funciones.  
# 4. Comentarios: Se utilizan '#' para comentarios de una sola línea. No hay
#    comentarios multilínea como /* */ de Java.
# 5. Impresión por consola: Para visualizar el contenido de una variable, 
#    basta con ejecutar la línea con su nombre.
#    Si no se asigna un valor a la invocación de una función, su resultado se 
#    muestra en la consola.

initial_state <- c(0, 0, 0) # Estilo correcto para inicializar variables
max_iterations <- 1000
max_iterations # Muestra el valor en consola

# --- B) Bloques de código ---
# El uso de llaves { } y paréntesis ( ) para estructuras de control es idéntico
# a Java. Se recomienda una tabulación de 2 espacios.
if (max_iterations > 500) {
  # Código indentado a 2 espacios
  # cat() es la función recomendada para imprimir texto en consola.
  # Es más eficiente que print().
  cat("El límite es correcto") 
}

# --- C) Vectores (Equivalente a Arrays) ---
# Un vector es una colección homogénea ordenada. Se crea con la función c().
# Java: int[] valores = {1, 2, 3, 4, 5};
board_values <- c(1, 2, 3, 4, 5)

# Vectores con nombre (Named Vectors):
# Permiten asociar una clave de texto a cada valor, similar a un Map<String, T>.
# Muy útil para definir los costes de las acciones en un problema de búsqueda.
action_costs <- c("UP" = 1, "DOWN" = 1, "LEFT" = 2, "RIGHT" = 2)

# Alternativamente se pueden asignar los nombres después de crear el vector:
action_costs <- c(1, 1, 2, 2)
names(action_costs) <- c("UP", "DOWN", "LEFT", "RIGHT")

# Acceso por nombre (clave):
action_cost <- action_costs["DOWN"] 
action_cost # Muestra el valor con nombre (Named num)

# Acceso "limpio" al valor:
action_costs[["DOWN"]]   # Opción recomendada: Doble corchete
unname(action_costs["DOWN"]) # Opción alternativa: unname()

# Acceso por posición:
# IMPORTANTE: Los índices en R comienzan en 1 (no en 0 como en Java).
first_cost <- action_costs[1]

# --- D) Generación de Secuencias y Aleatoriedad ---
# Herramientas comunes para inicializar estados iniciales:
puzzle_8 <- 1:9           # Genera un vector del 1 al 9
puzzle_empty <- rep(0, 9) # Crea un vector con 9 ceros
puzzle_random <- sample(0:8) # Desordena (shuffle) los números del 0 al 8

# -------------------------------------------------------------------------
# 3) MATRICES (Estructuras 2D Homogéneas)
# -------------------------------------------------------------------------
# NOTA DE DISEÑO: Las matrices son estructuras ATÓMICAS y HOMOGÉNEAS. 
# Todos los elementos deben ser del mismo tipo (ej. numéricos). Si se intenta
# insertar un texto en una matriz numérica, R forzará la conversión (coerción)
# de toda la matriz a tipo carácter, invalidando cálculos matemáticos.

# --- A) Gestión de Memoria y Eficiencia ---
# NOTA DE DISEÑO: R almacena las matrices en "Column-Major Order" (por columnas).
# En memoria, los elementos de una columna están contiguos. Por ello, las
# operaciones que procesan columnas suelen ser ligeramente más rápidas que
# las que procesan filas en estructuras de gran escala.

# Creación con orden de lectura humano (fila a fila):
values <- c(1, 2, 3, 4, 0, 5, 6, 7, 8)
board  <- matrix(values, nrow = 3, ncol = 3, byrow = TRUE)

# --- B) Nombres de Filas y Columnas (Etiquetado) ---
# NOTA DE DISEÑO: Es posible asignar etiquetas para facilitar la legibilidad.
# Muy útil en matrices de costes o distancias entre nodos/ciudades.
rownames(board) <- c("row_1", "row_2", "row_3")
colnames(board) <- c("col_a", "col_b", "col_c")

# Acceso mediante etiquetas (similar a un mapa de Java):
val <- board["row_2", "col_b"] 

# --- C) Indexación y Selección (Slicing) ---
# NOTA DE DISEÑO: La indexación comienza en 1. El formato es [Fila, Columna].
# Al extraer un slice, si se deja un índice vacío, se obtiene la dimensión completa.
board[1, ] # Devuelve un SLICE de la fila 1 (como VECTOR)
board[, 1] # Devuelve un SLICE de la columna 1 (como VECTOR)

# --- D) Modificación Masiva (Vectorización) ---
# NOTA DE DISEÑO: Al igual que en Java se operaría con un bucle anidado,
# en R se debe priorizar la asignación directa sobre el rango para que el
# motor interno (escrito en C) realice la operación de forma eficiente.
board[3, ] <- 0        # Asigna 0 a toda la tercera fila
board[, 2] <- c(9,5,1) # Sustituye la segunda columna por un nuevo vector

# --- E) Consultas de Dimensión ---
# Fundamentales para algoritmos genéricos donde el tamaño del problema varía.
nrow(board)
ncol(board)
dim(board) # Devuelve un vector c(3, 3) con el tamaño de la matriz.

# -------------------------------------------------------------------------
# 4) ESTRUCTURAS DE CONTROL (Diferencias con Java)
# -------------------------------------------------------------------------
# El control de flujo en R presenta similitudes sintácticas con Java, pero 
# carece de ciertas estructuras y utiliza palabras clave distintas.

# --- A) Condicionales y Selección ---
# 1. if / else if / else: Funcionamiento idéntico a Java.
# 2. switch(): CUIDADO CON LOS TIPOS.
# Si 'selected_action' fuera el número 1, switch saltaría a la primera opción
# ignorando las etiquetas. Para evitar este bug silencioso, usa as.character():
selected_action <- "UP" 
action_vector <- switch(as.character(selected_action),
                        "UP"   = c(-1, 0),
                        "DOWN" = c(1, 0))

# --- B) Bucles y Sentencias de Interrupción ---
# 1. for: Siempre actúa como un "for-each". Se itera sobre un vector o lista.
# 2. while: Idéntico a Java.
# 3. repeat: Bucle infinito (como 'while(true)'). Se usa para emular 'do-while'.

for (i in 1:10) {
  # SENTENCIA 'next': Equivalente a 'continue' en Java. 
  # Salta a la siguiente iteración del bucle.
  if (i %% 2 == 0) {
    next 
  }
  
  # SENTENCIA 'break': Idéntica a Java. Sale del bucle.
  if (i > 7) {
    break
  }
  
  cat("Processing odd value: ", i, "\n")
}

# -------------------------------------------------------------------------
# 5) LA FAMILIA 'APPLY' (Iteración Funcional)
# -------------------------------------------------------------------------
# Estas funciones actúan como un bucle "for-each" interno. R extrae un 
# slice de la estructura y le aplica la función indicada.
# En R son más eficientes que los bucles explícitos, ya que el motor interno está
# optimizado para estas operaciones vectorizadas.
# No se espera que se conozcan todas las variantes, pero sí entender su 
# uso general y diferencias.

# --- Preparación de datos de ejemplo ---
board_matrix <- matrix(c(1, 2, 3, 0, 5, 6, 7, 8, 0), nrow = 3, byrow = TRUE)

# Lista que simula una frontier con dos nodos (cada nodo es una lista)
node_list <- list(
  node_a = list(id = 1, cost = 10, evaluation = 5),
  node_b = list(id = 2, cost = 15, evaluation = 3)
)

# --- A) apply(): Específica para MATRICES ---
# El argumento MARGIN define la dirección del "corte":

# MARGIN = 1 (por Filas): La función "camina" por las filas.
# Útil para: contar huecos en cada fila, sumar costes por fila.
zeros_per_row <- apply(board_matrix, 1, function(row) sum(row == 0))

# MARGIN = 2 (por Columnas): La función "cae" por las columnas.
max_per_col <- apply(board_matrix, 2, max)

# --- B) lapply() y sapply(): Específicas para LISTAS o VECTORES ---
# Las listas son unidimensionales; no requieren margen.

# 1. lapply (List apply): Devuelve siempre una LISTA.
# Ejemplo: Incrementar el cost de todos los nodos en 1 unidad.
updated_nodes <- lapply(node_list, function(node, increment) {
  node$cost <- node$cost + increment
  return(node)
}, increment = 1)

# 2. sapply (Simplified apply): Intenta devolver un VECTOR.
# Ejemplo: Extraer solo el valor de la evaluation (h) de cada nodo.
h_values <- sapply(node_list, function(node) {
  return(node$evaluation)
})

# --- C) Invocación con funciones existentes y parámetros extra ---
# Se utiliza el nombre de la función (sin paréntesis). Los argumentos 
# adicionales de dicha función se añaden al final del apply.
matrix_na <- board_matrix
matrix_na[1,1] <- NA
clean_sums <- apply(matrix_na, 1, sum, na.rm = TRUE)

# -------------------------------------------------------------------------
# 6) DEFINICIÓN DE FUNCIONES
# -------------------------------------------------------------------------
# Las funciones en R se comportan como métodos estáticos en Java. Aparecen en el
# panel "Environment" y se pueden invocar desde cualquier punto del script 
# (incluso antes de su definición).

# --- A) Estructura y Parámetros ---
# 1. Parámetros por defecto: Se pueden predefinir valores en la cabecera.
# 2. Elipsis (...): Permite recibir un número variable de argumentos,
#    equivalente a 'Object... args' en Java.

get.evaluation <- function(state, problem, verbose = FALSE, ...) {
  # 'verbose' tiene un valor por defecto.
  # '...' capturará cualquier otro argumento extra pasado a la función.
  
  # RETORNO: Se puede usar return() o dejar que R devuelva la última expresión.
  result <- sum(state == 0) 
  result 
}

# --- B) Invocación Nominal vs Posicional ---
# R permite ignorar el orden de los parámetros si se especifican sus nombres:
# get.evaluation(problem = p, state = s)
# Se recomienda usar esta forma para mejorar la legibilidad, y evitar errores.

# --- C) Paso por Valor (Copy-on-Write) ---
# A diferencia de Java, donde los objetos se pasan por referencia, R usa 
# semántica copy-on-write: conceptualmente es paso por valor, pero la copia solo
# se materializa si hay modificación.
# Si una función modifica un objeto, debe devolver el objeto modificado:
update.cost <- function(cost) {
  cost <- cost + 1
  return(cost) # Es buena práctica hacerlo explícito
}

cost <- 10
# update.cost(cost) # Esto NO modifica la variable 'cost' externa
cost <- update.cost(cost) # Esto SÍ la modifica (reasignación)
cost 

# --- D) Retorno ---
# R devuelve el resultado de la última expresión evaluada. No es obligatorio usar
# return(), aunque es recomendable para mejorar la claridad del código.
# Si se ejecuta una función sin asignar su resultado a una variable, el valor se
# mostrará en consola automáticamente.

# -------------------------------------------------------------------------
# 7) LISTAS (Equivalente a Clases, Objetos y ArrayLists)
# -------------------------------------------------------------------------
# En R no se definen clases. Se utilizan listas para agrupar datos de 
# diferentes tipos.
# Una lista representa una colección HETEROGÉNEA de elementos, cada uno con un 
# nombre (clave) asociado.

# --- A) Creación de una Lista (Estructura de Nodo) ---
# Los nombres a la izquierda del '=' actúan como los campos de un objeto.
# NOTA DE DISEÑO: Se usan nombre en INGLÉS para coherencia con los algoritmos.
node_example <- list(
  state      = c(1, 2, 3, 4, 0, 5, 6, 7, 8), # Vector (Tablero)
  parent     = NULL,                         # Sin nodo antecesor (Nodo raíz)
  action     = "None",                       # Operador aplicado
  cost       = 10,                           # Coste acumulado (g)
  depth      = 0,                            # Profundidad (depth)
  evaluation = 0                             # Heurística (f = g + h)
)

# --- B) Acceso y Modificación (Operador $) ---
# NOTA DE DISEÑO: El operador '$' equivale al punto '.' de Java para acceder 
# a atributos (node$cost es como node.cost). 
# IMPORTANTE: No usar 'node.cost', ya que R interpretará que el punto es 
# parte del nombre de una variable o el nombre de una función.

current_g <- node_example$cost
node_example$cost <- 15 # Modificación del valor

# --- C) Acceso Avanzado: La Regla de la "Caja de Amazon" ---
# ERROR COMÚN: Confundir [ ] con [[ ]].
# - [1]  : Te da la CAJA con el producto dentro. (Devuelve una LISTA de 1 elemento).
# - [[1]]: Abre la caja y te da el PRODUCTO en la mano. (Devuelve el OBJETO real).

# Ejemplo práctico:
# frontier[1]   -> Devuelve una LISTA (tipo List). No puedes operar con ella.
# frontier[[1]] -> Devuelve el NODO (tipo List interna). SÍ puedes acceder a $cost.

node_example[4]   # Devuelve una lista (clave 'cost' y valor 10)
node_example[[4]] # Devuelve el número 10. ¡Correcto!

# --- D) Gestión de la FRONTIER (Lista de Listas / ArrayList) ---
# Una lista FRONTIER contiene listas (cada una con un nodo).
frontier <- list() 

# La lista puede utilizarse como COLA o PILA añadiendo elementos al principio
# o al final, según el algoritmo (DFS o BFS).

# Ejemplo de inserción al final (COLA para BFS):
frontier <- c(frontier, list(node_example)) # Añade al final (COLA)
frontier[[length(frontier) + 1]] <- node_example # Añade al final (COLA)
# Ambas formas son equivalentes. La primera más legible y la segunda más eficiente.

# Ejemplo de inserción al principio (PILA para DFS):
frontier <- c(list(node_example), frontier) # Añade al principio (PILA)

# Dequeue (Sacar el primer nodo de la lista FRONTIER):
first_node <- frontier[[1]]   # Extraer
frontier   <- frontier[-1]    # Eliminar (re-asignando sin el primero)

# Tamaño: length() equivale al .size() de un ArrayList en Java.
length(frontier)

# Comprobar si la FRONTIER está vacía:
if (length(frontier) == 0) {
  cat("La búsqueda ha finalizado.\n")
}

# --- Ordenación de la Frontier (Priority Queue) ---
# Para algoritmos con información (A*, UCS, Greedy-Best-First), necesitamos ordenar
# la lista FRONTIER de acuerdo al valor de la función de evaluación.
#
# Java: Collections.sort(frontier, comparator);
# R:    Reordenamos usando los índices que devuelve la función order().

# Paso 1: Extraemos los valores de 'evaluation' de todos los nodos (vector).
evals <- sapply(frontier, function(n) n$evaluation)
# Paso 2: Obtenemos los índices ordenados y reasignamos la lista.
frontier <- frontier[order(evals)] 
# Ahora frontier[[1]] es el nodo con MENOR evaluación (el mejor).

# --- E) Acceso Dinámico (Reflexión) ---
# Error común de programadores Java: Intentar acceder a un campo usando
# una variable String con el operador $.
#
# Supongamos: field_name <- "cost"
# INCORRECTO: node_example$field_name  -> Devuelve NULL (busca campo "field_name")
# CORRECTO:   node_example[[field_name]] -> Devuelve 15 (evalúa la variable)

# -------------------------------------------------------------------------
# 8) FILTRADO Y LÓGICA BOOLEANA (Querying)
# -------------------------------------------------------------------------
# En R, las comparaciones son VECTORIALES. A diferencia de Java, donde 
# 'x < 10' devuelve un solo booleano, en R devuelve un VECTOR BOOLEANO.

dataset <- c(5, 12, 8, 20, 3)

# --- A) ¿Qué es una Máscara Booleana? ---
# Es un vector de TRUE/FALSE generado al aplicar una condición a un vector.
# Actúa como una "plantilla" que indica qué elementos cumplen la regla.
mask <- dataset < 10  # Resultado interno: [TRUE, FALSE, TRUE, FALSE, TRUE]

# --- B) Filtrado por Máscara (Subsetting) ---
# Al pasar la máscara entre corchetes [ ], R "deja pasar" solo los valores
# donde la posición es TRUE y "bloquea" los FALSE.
low_values <- dataset[dataset < 10] # Resultado: c(5, 8, 3)

# --- C) Localización con which() ---
# Transforma la máscara (booleanos) en ÍNDICES (números de posición).
# Útil para saber: "¿En qué posición está el hueco (0) en el tablero?"
indices <- which(dataset < 10) # Resultado: c(1, 3, 5)

# --- D) Comparación de Estructuras Completas ---
# Para comparar si dos matrices (nodos) son idénticas (Estado Actual == Objetivo).
state_a <- c(1, 2, 3)
state_b <- c(1, 0, 3)

# 1. Comparación elemento a elemento:
state_a == state_b  # [TRUE, FALSE, TRUE] (Vector booleano)

# 2. Resumen de la comparación:
# all(): ¿Son TODOS los elementos iguales? (Equivalente a equals() en Java)
# any(): ¿Hay AL MENOS UNO igual?
if (all(state_a == state_b)) {
  cat("Los estados son iguales (comparación elemento a elemento).")
}

# 3. Función identical(): Comprobación estricta de objeto (tipo y valor).
identical(state_a, state_b) # FALSE

# --- E) Operador de Pertenencia (%in%) ---
# Comprueba si un valor existe dentro de un conjunto.
# Java: contains()
5 %in% dataset # TRUE

# -------------------------------------------------------------------------
# 9) IMPRESIÓN Y FORMATEO DE TEXTO (Logging)
# -------------------------------------------------------------------------
# Java: System.out.println("Valor: " + x); o String.format(...)

# --- A) Concatenación Básica (paste / paste0) ---
# paste(): Concatena vectores convirtiéndolos a texto, con separador.
texto <- paste("El coste es", 10, "y la heurística", 5, sep = " ")
# Resultado: "El coste es 10 y la heurística 5"

# paste0(): Igual que paste, pero sin separador (sep=""). Más eficiente.
fichero <- paste0("puzzle_", 8, ".txt") 
# Resultado: "puzzle_8.txt"

# --- B) Impresión por Consola (cat) ---
# cat(): Imprime directamente. Interpreta caracteres especiales como \n.
# NOTA: cat() NO devuelve nada (NULL), solo imprime. No asignar a variable.
cat("Iteración:", 1, "\nCoste actual:", 15, "\n")

# --- C) Formateo tipo C/Java (sprintf) ---
# sprintf(): Idéntico a String.format() de Java. Devuelve un String.
# %s = string, %d = entero, %.2f = decimal con 2 precisiones.
mensaje <- sprintf("Nodo ID: %d | Evaluación: %.2f", 142, 10.5678)
cat(mensaje)

# -------------------------------------------------------------------------
# 10) LECTURA DE FICHEROS DE TEXTO (Input/Output)
# -------------------------------------------------------------------------
# Para algoritmos de búsqueda, solemos leer escenarios desde ficheros .txt.

# --- A) Caso Sudoku (Matriz Homogénea con Comas) ---
# El fichero 'sudoku-1.txt' contiene los valores de las filas separados por comas.
load.sudoku <- function(path_sudoku) {
  # read.table genera un data.frame. El argumento sep="," define el separador.
  data <- read.table(path_sudoku, header = FALSE, sep = ",")
  
  # Convertimos a matriz para operar eficientemente con lógica de tablero.
  return(as.matrix(data))
}

# --- B) Caso Feet-Maze (El fichero contiene varias líneas) ---
# El fichero 'feet-maze-1b.txt' usa ';' como separador y tiene secciones mixtas.
load.maze <- function(path) {
  # Leemos el fichero línea a línea como texto y devuelve un vector de strings.
  lines <- readLines(path)
  
  # 1. Tamaño del escenario: Primera línea (ej: 7;7).
  dims <- as.numeric(unlist(strsplit(lines[1], ";")))
  
  # 2. Matriz de Pies (L/R): Líneas 2 a la N+1.
  feet_block <- lines[2:(1 + dims[1])]
  feet_matrix <- t(sapply(strsplit(feet_block, ";"), as.character))
  
  # 3. Coordenadas Inicial y Final: Líneas N+2 y N+3.
  start_pos <- as.numeric(unlist(strsplit(lines[dims[1] + 2], ",")))
  end_pos   <- as.numeric(unlist(strsplit(lines[dims[1] + 3], ",")))
  
  # 4. Procesamiento de Muros para uso eficiente en búsqueda
  # NOTA DE DISEÑO: Convertimos las coordenadas a Strings (ej: "2,1") para
  # poder usar el operador %in% (pertenencia) de forma directa en el algoritmo.
  # Si usáramos matrices numéricas, la comparación sería mucho más lenta.
  
  parse.walls <- function(chain) {
    if (chain == "-") return(character(0)) # Vector vacío si no hay muros
    return(unlist(strsplit(chain, ";")))   # Retorna vector de strings "x,y"
  }
  
  # Retornamos una LISTA con toda la información estructurada.
  # Usamos nombres en inglés (walls_left, etc) para coherencia con 'feet-maze.R'.
  return(list(
    size         = dims,
    maze         = feet_matrix,
    state_final = end_pos,   # Usamos state_final para estandarizar con otros problemas
    start_pos    = start_pos, # Guardamos el inicio por separado
    walls_left   = parse.walls(lines[dims[1] + 4]), 
    walls_right = parse.walls(lines[dims[1] + 5]), 
    walls_bottom= parse.walls(lines[dims[1] + 6]), 
    walls_up     = parse.walls(lines[dims[1] + 7]) 
  ))
}

# --- Invocación de prueba (Descomentar si existen los ficheros) ---
# sudoku_board <- load.sudoku("../data/sudoku/sudoku-1.txt")
# maze_problem <- load.maze("../data/feet-maze/feet-maze-1b.txt")

# -------------------------------------------------------------------------
# 11) MANEJO DE EXCEPCIONES (Try-Catch)
# -------------------------------------------------------------------------
# Equivalente al bloque try { } catch (Exception e) { } de Java.
# Útil si la lectura de un fichero puede fallar o un cálculo matemático
# puede dar error y no queremos detener el algoritmo.

resultado <- tryCatch({
  # --- BLOQUE TRY ---
  # Código propenso a fallar
  numerador <- 10
  denominador <- "texto" # Esto provocará error
  val <- numerador / denominador
  return(val)
  
}, error = function(e) {
  # --- BLOQUE CATCH ---
  # 'e' contiene el objeto del error
  cat("Ha ocurrido un error controlado:", e$message, "\n")
  return(NA) # Devolvemos un valor seguro (ej. NA o NULL)
  
}, finally = {
  # --- BLOQUE FINALLY ---
  # Se ejecuta siempre (ej. cerrar conexiones a BBDD o ficheros)
  cat("Bloque finalizado.\n")
})

# -------------------------------------------------------------------------
# 12) DATA FRAMES (Tablas de Datos)
# -------------------------------------------------------------------------
# NOTA DE DISEÑO: Un data.frame es la estructura más utilizada en R para 
# Ciencia de Datos. Es, en esencia, una TABLA donde cada columna puede 
# contener un tipo de dato distinto (numérico, carácter, lógico, etc.).

# --- A) Características Fundamentales ---
# 1. HETEROGENEIDAD: A diferencia de las matrices, permite mezclar tipos.
#    Columna A puede ser el ID del nodo (int) y Columna B el nombre (string).
# 2. ESTRUCTURA DE LISTA: Técnicamente, un data.frame es una LISTA donde 
#    cada elemento es un vector de la misma longitud.
# 3. DIMENSIONES: Posee filas y columnas, comportándose como una matriz 
#    en cuanto a indexación.

# --- B) Creación y Manipulación ---
df_nodes <- data.frame(
  id = c(1, 2, 3),
  name = c("Root", "Child_1", "Child_2"),
  visited = c(TRUE, FALSE, FALSE)
)

# --- C) Acceso a los Datos (Híbrido Matriz/Lista) ---
# Al ser una "lista de vectores", podemos usar ambos sistemas de acceso:

# 1. Acceso tipo Matriz [fila, columna]:
val <- df_nodes[2, 1]           # Fila 2, Columna 1
sub_table <- df_nodes[1:2, ]    # SLICE de las dos primeras filas

# 2. Acceso tipo Lista (Por nombre de columna):
# Es la forma más común y legible de trabajar con tablas.
names_col <- df_nodes$name # Devuelve el vector de nombres

# --- D) ¿Por qué NO usarlos en algoritmos de búsqueda? ---
# NOTA DE DISEÑO PARA EL ALUMNO:
# - SOBRECARGA (Overhead): Cada vez que accedes a un data.frame, R realiza
#    comprobaciones de metadatos. En un bucle que se repite 100.000 veces
#    para expandir nodos, esto supone una penalización de tiempo enorme.
# - COERCIÓN: Si intentas convertir un data.frame con texto a matriz para 
#    hacer cálculos, toda la matriz se convertirá a texto, rompiendo la lógica.

# --- E) Comprobación y Conversión ---
# Si leemos un Sudoku y queremos pasarlo a una estructura eficiente:
# if (is.data.frame(object_read)) {
#    board_search <- as.matrix(object_read)
# }
# =========================================================================
# FIN DEL TUTORIAL
# =========================================================================
# Autor:
#    - Roberto Carballedo
#
# Último cambio: febrero de 2026
# Código generado y revisado con: Gemini Pro.
#
# Para uso exclusivamente académico - Universidad de Deusto
# =========================================================================