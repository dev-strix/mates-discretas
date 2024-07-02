#
#######################################
# Instalar y cargar la librería igraph
if(!require(igraph)) install.packages("igraph", dependencies=TRUE)
library(igraph)

# Definir las aristas y sus capacidades
edges <- c(1, 3, 16,  # N1 -> N3
           3, 9, 4,   # N3 -> N9
           9, 10, 5,  # N9 -> N10
           10, 11, 5, # N10 -> N11
           3, 8, 10,  # N3 -> N8
           8, 9, 10,  # N8 -> N9
           8, 10, 10, # N8 -> N10
           1, 2, 50,  # N1 -> N2
           2, 3, 2,   # N2 -> N3
           2, 5, 10,  # N2 -> N5
           2, 4, 30,  # N2 -> N4
           4, 5, 20,  # N4 -> N5
           4, 7, 30,  # N4 -> N7
           4, 6, 20,  # N4 -> N6
           5, 7, 20,  # N5 -> N7
           6, 7, 20,  # N6 -> N7
           6, 11, 15, # N6 -> N11
           7, 11, 12) # N7 -> N11

# Crear el grafo dirigido
g <- graph_from_edgelist(matrix(edges, ncol = 3, byrow = TRUE)[, 1:2], directed = TRUE)

# Añadir capacidades a las aristas
E(g)$capacity <- matrix(edges, ncol = 3, byrow = TRUE)[, 3]

# Función para encontrar el flujo máximo usando Ford-Fulkerson (Edmonds-Karp)
ford_fulkerson <- function(graph, source, sink) {
  V(graph)$name <- V(graph)
  flow <- 0
  
  repeat {
    parent <- rep(NA, vcount(graph))
    visited <- rep(FALSE, vcount(graph))
    queue <- list(source)
    visited[source] <- TRUE
    
    while (length(queue) > 0) {
      u <- queue[[1]]
      queue <- queue[-1]
      
      if (u == sink) break
      
      for (v in neighbors(graph, u, mode = "out")) {
        if (!visited[v] && E(graph)[u %->% v]$capacity > 0) {
          parent[v] <- u
          visited[v] <- TRUE
          queue <- append(queue, v)
        }
      }
    }
    
    if (!visited[sink]) break
    
    path_flow <- Inf
    v <- sink
    while (!is.na(parent[v])) {
      u <- parent[v]
      path_flow <- min(path_flow, E(graph)[u %->% v]$capacity)
      v <- u
    }
    
    v <- sink
    while (!is.na(parent[v])) {
      u <- parent[v]
      E(graph)[u %->% v]$capacity <- E(graph)[u %->% v]$capacity - path_flow
      E(graph)[v %->% u]$capacity <- E(graph)[v %->% u]$capacity + path_flow
      v <- u
    }
    
    flow <- flow + path_flow
  }
  
  return(flow)
}

# Definir la fuente y el sumidero
source <- 1
sink <- 11

# Calcular el flujo máximo
max_flow <- ford_fulkerson(g, source, sink)
print(paste("El flujo máximo desde N1 hasta N11 es:", max_flow))

# Plot del grafo
plot(g, edge.label = E(g)$capacity, vertex.color = "skyblue", 
     vertex.size = 30, vertex.label.cex = 0.8, edge.arrow.size = 0.5,
     main = "Grafo de Flujo con Capacidades")



