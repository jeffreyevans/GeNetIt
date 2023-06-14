#' @title Graph Metrics
#' @description Metrics on structural properties of graph (at nodes)
#'
#' @param x              knn graph object from GeNetIt::knn.graph (sf LINESTRING)  
#' @param node.pts       sf POINT or sp SpatialPointsDataFrame object used as nodes to build x   
#' @param node.name      Column name in node.pts object that acts as the provides the unique ID.
#'                       If not defined, defaults to row.names of node.pts
#' @param direct         (FALSE/TRUE) Evaluate directed graph
#' @param metric         ...
#'  
#' @note Please note; graph metrics are not valid for a saturated graph (all connections)
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> and 
#'         Melanie A. Murphy <melanie.murphy@@uwyo.edu>
#'
#' @examples
#'  library(sf)
#'  data(ralu.site, package="GeNetIt")
#' 
#'  graph <- knn.graph(ralu.site, row.names=ralu.site$SiteName, 
#'	                   max.dist = 2500)
#'      plot(st_geometry(graph))
#'
#'  ( m <- graph.metrics(graph, ralu.site, "SiteName") )
#'  
#'   ralu.site <- merge(ralu.site, m, by="SiteName")
#'     # plot node betweenness
#'     plot(st_geometry(graph), col="grey")
#'	   plot(ralu.site["betweenness"], pch=19, cex=1.25, add=TRUE)
#'	# plot node degree
#'     plot(st_geometry(graph), col="grey")
#'	   plot(ralu.site["degree"], pch=19, cex=1.25, add=TRUE)	   
#'
#' @export
graph.metrics <- function(x, node.pts, node.name=NULL, direct = FALSE, 
                     metric = c("betweenness", "degree", "closeness")) {
   if(!any(which(utils::installed.packages()[,1] %in% c("igraph", "sfnetworks") )))
     stop("please install igraph and sfnetworks packages before running this function")
    m <- c("betweenness", "degree", "closeness") 
      m <- m[m %in% metric]

    if(!inherits(x, "sf")) 
      stop("x must be a sf LINESTRING object") 	  
    if(attributes(x$geometry)$class[1] != "sfc_LINESTRING")
      stop("x must be a sf sfc_LINE object") 
    if (!inherits(node.pts, "sf")) 
      stop("x must be a sf POINT object") 	  	
    if(attributes(node.pts$geometry)$class[1] != "sfc_POINT")
      stop("node.pts must be a sf sfc_LINE object")
    if(is.null(node.name)) {
      node.name = row.names(node.pts)
    } else {
    if(!node.name %in% names(node.pts))
	  stop("specified node.name not in node.pts")
    }  
	g <- sfnetworks::as_sfnetwork(x = x, edges = x, nodes = node.pts, 
                                 directed = direct, node_key = node.name, 
	 						     length_as_weight = TRUE,
   							     edges_as_lines = TRUE)
    gm <- data.frame(sf::st_drop_geometry(node.pts[,node.name]))
	  # w <- g$weight/sum(g$weight)
	  w <- g |> 
	    tidygraph::activate("edges") |> 
		  dplyr::pull("weight") |> 
		    as.numeric()	
        w[w <= 0] <- 1
          w = w / sum(w)
	  if("betweenness" %in% m)
        gm$betweenness <- igraph::betweenness(g, directed=FALSE, weights=w)
	  if("degree" %in% m)	
	    gm$degree <- igraph::degree(g)
      if("closeness" %in% m)
	    gm$closeness <- igraph::closeness(g, weights=w)
  return(gm)
}
