#' 
#' @title Dijkstras algorithm 
#' 
#' @name Dijkstra
#' 
#' @description 
#' Find the shortest path between nodes in a weighted graph.
#' The algorithm itself and pseudo-code is from
#' https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm.
#' 
#' @param graph A data.frame containing three variables v1, v2, and w that contains 
#'  the edges of the graph (from v1 to v2) with the weight of the edge (w).
#' 
#' @param init_node A numeric scalar that exists in the graphs (alas in the v1, v2).
#' 
#' @returns A vector consisting of the closest path to each graph from the initial node
#' 
#' @export
#' 
#' @source <https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm>

dijkstra <-
function(graph, init_node){
  # checking the input
  stopifnot(is.data.frame(graph), colnames(graph) %in% c('v1','v2','w'),
            is.numeric(init_node) && length(init_node) == 1,
            init_node %in% graph$v1 ||init_node %in% graph$v2 )
  
  Q <- unique(graph$v1) # Creating variable Q(each node)
  
  dist <- rep(Inf,  length(Q)) # Creating the dist and prev vectors
  
  dist[init_node] <- 0 # The distance to itself is 0
  
  while(length(Q) > 0) { # While the Q variable is longer than 0
    u <- Q[which.min(dist[Q])] # Takes the node with the shortest distance
    
    Q <- setdiff(Q,u) # Remove that node from Q
    
    # Indice all elements in v1 for the rows in v1 that are the value of u
    neighbors <- c(graph$v2[graph$v1 == u])
    
    for (v in neighbors){ # looping over all neighbors
      # Distance u + distance for all neighbors of u and v
      alt <-  dist[u] + graph$w[which((graph$v1 == u & graph$v2 == v ))]
      if (alt < dist[v]){ # updating the distance if its shorter than to the previous neighbor
        dist[v] <- alt
        
      }
    }
    
  }
  return(dist)   
}