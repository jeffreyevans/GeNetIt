#' @title Binary adjacency matrix 
#' @description Creates a binary matrix of adjacencies based on 
#'              from-to graph relationships (joins)
#'
#' @param i   a vector or, if j = NULL a data.frame with two 
#'            columns indicating from-to relationships (joins) 
#' @param j   If specified, i must be a vector of same length and
#'            the i,j vectors must represent joins
#'
#' @return    A binary matrix
#'
#' @examples
#'  library(sf)
#'    data(ralu.site, package="GeNetIt")
#'    
#'  p <- as(ralu.site, "sf")
#'  g <- knn.graph(p[c(1,5,8,10,20,31),])
#'    plot(st_geometry(g))
#'
#'  ( ind <- sf::st_drop_geometry(g[,1:2])[1:10,] ) 
#'
#'  adj_matrix(ind)
#'
#'  adj_matrix(g$i[1:10], g$j[1:10])
#'
#' @export
adj_matrix <- function(i, j=NULL) {
  if(missing(i))
    stop("i must be defined")
  if(class(i)[1] == "data.frame") {
    if(ncol(i) < 2)
	  stop("Incorrect dim, need 2 columns")
    ind <- i[,1:2]
	  names(ind) <- c("i","j")
  }
  if(!missing(i) & !is.null(j)) {
    if(!any(c(is.vector(i),is.vector(j))))
      stop("Data is not vector")
    if(length(i) != length(j)) 
      stop("From-To vectors are not equal")
    ind <- data.frame(i=i, j=j)
      names(ind) <- c("i","j")	
  }	
  adj <- matrix(0, nrow(ind), nrow(ind)) 
    for (p in 1:nrow(ind)){ 
      adj[ind[p,1], ind[p,2]] <- 1
      adj[ind[p,2], ind[p,1]] <- 1
    } 
  return(adj)
}
