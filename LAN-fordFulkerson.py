import numpy as np
# Número de nodos
num_nodes = 11
# Definir la capacidad del grafo
capacities = np.zeros((num_nodes, num_nodes))
# Añadir las capacidades de las aristas
edges = [
    (0, 2, 16), (2, 8, 4), (8, 9, 5), (9, 10, 5),
    (2, 7, 10), (7, 8, 10), (7, 9, 10), (0, 1, 50),
    (1, 2, 2), (1, 4, 10), (1, 3, 30), (3, 4, 20),
    (3, 6, 30), (3, 5, 20), (4, 6, 20), (5, 6, 20),
    (5, 10, 15), (6, 10, 12)
]

for u, v, capacity in edges:
    capacities[u][v] = capacity

def bfs(source, sink, parent):
    visited = [False] * num_nodes
    queue = []
    
    queue.append(source)
    visited[source] = True
    
    while queue:
        u = queue.pop(0)
        
        for ind, val in enumerate(capacities[u]):
            if visited[ind] == False and val > 0:
                queue.append(ind)
                visited[ind] = True
                parent[ind] = u
                if ind == sink:
                    return True
    return False

def ford_fulkerson(source, sink):
    parent = [-1] * num_nodes
    max_flow = 0

    while bfs(source, sink, parent):
        path_flow = float('Inf')
        s = sink
        
        while s != source:
            path_flow = min(path_flow, capacities[parent[s]][s])
            s = parent[s]

        v = sink
        while v != source:
            u = parent[v]
            capacities[u][v] -= path_flow
            capacities[v][u] += path_flow
            v = parent[v]
        
        max_flow += path_flow

    return max_flow

source = 0  # Nodo N1
sink = 10  # Nodo N11

max_flow = ford_fulkerson(source, sink)
print(f"El flujo máximo desde N1 hasta N11 es: {max_flow}")
