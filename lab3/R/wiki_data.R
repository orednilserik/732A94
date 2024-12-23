#' Graph data from wikipedia
#'
#' Dijkstra algorithm example from wikipedia
#'
#' @format ## `wiki_graph`
#' A data frame with 18 rows and 6 columns:
#' \describe{
#'   \item{v1}{nodes, number of each node depends on number of neighbours of the node}
#'   \item{v2}{connection between the nodes in v1}
#'   \item{w}{distance between each pair of node}
#'   ...
#' }
#' @source <https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm>
"wiki_graph"

wiki_graph <-
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
