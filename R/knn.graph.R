#' @title Saturated or K Nearest Neighbor Graph
#' @description Creates a kNN or saturated graph SpatialLinesDataFrame object  
#'
#' @param x              sf POINTS object
#' @param row.names      Unique row.names assigned to results  
#' @param k              K nearest neighbors, defaults to saturated (n(x) - 1)
#' @param max.dist       Maximum length of an edge (used for distance constraint)
#' @param drop.lower     (FALSE/TRUE) Drop lower triangle of matrix representing  
#'                        duplicate edges ie, from-to and to-from 
#' @param long.lat       (FALSE/TRUE) Coordinates are longitude-latitude decimal degrees, 
#'                        in which case distances are measured in kilometers
#' 
#' @return   SpatialLinesDataFrame object with:
#' * i        Name of column in x with FROM (origin) index
#' * j        Name of column in x with TO (destination) index
#' * from_ID  Name of column in x with FROM (origin) region ID
#' * to_ID    Name of column in x with TO (destination) region ID
#' * length   Length of each edge (line) in projection units or kilometers if not projected
#' @md
#'
#' @note ...
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> and 
#'         Melanie A. Murphy <melanie.murphy@@uwyo.edu>
#'
#' @references
#' Murphy, M. A. & J.S. Evans. (in prep). "GenNetIt: gravity analysis in R for landscape 
#'   genetics" 
#' @references
#' Murphy M.A., R. Dezzani, D.S. Pilliod & A.S. Storfer (2010) Landscape genetics of 
#'   high mountain frog metapopulations. Molecular Ecology 19(17):3634-3649 
#'
#' @examples
#'  library(sf)
#'    data(ralu.site, package="GeNetIt")
#'	
#'  # Saturated spatial graph
#'  sat.graph <- knn.graph(ralu.site, row.names=ralu.site$SiteName)
#'    head(sat.graph)
#'  
#'  # Distanced constrained spatial graph
#'  dist.graph <- knn.graph(ralu.site, row.names=ralu.site$SiteName, 
#'                          max.dist = 5000)
#'	
#' opar <- par(no.readonly=TRUE)
#'  par(mfrow=c(1,2))	
#'	plot(st_geometry(sat.graph), col="grey")
#'	  points(st_coordinates(ralu.site), col="red", pch=20, cex=1.5)
#'      box()
#'      title("Saturated graph")	
#'	plot(st_geometry(dist.graph), col="grey")
#'	  points(st_coordinates(ralu.site), col="red", pch=20, cex=1.5)
#'      box()
#'      title("Distance constrained graph")
#' par(opar)	  
#'		
#' @export			  
knn.graph <- function (x, row.names = NULL, k = NULL, max.dist = NULL, 
                       long.lat = FALSE, drop.lower = FALSE) 
   {
 
  if (!inherits(x, "sf")) 
    stop("x must be a sf POINT object") 
  if(attributes(x$geometry)$class[1] != "sfc_POINT")
    stop("x must be a sf sfc_POINT object")   		  
  if(is.null(k)) k=(dim(x)[1] - 1)
	knn <- suppressWarnings( spdep::knearneigh(sf::st_coordinates(x), k = k, 
	                                           longlat = long.lat) )
    knn.nb <- suppressWarnings( spdep::knn2nb(knn, row.names = row.names, 
	                                          sym = FALSE) )
    if(!is.na(sf::st_crs(x))) { 
	  prj <- sf::st_crs(x)
	} else { 
	  prj <- sf::st_crs(NA)
	} 
    if (!is.null(row.names)) {
      if (length(row.names) != knn$np) 
	    stop("row.names wrong length")
      if (length(unique(row.names)) != length(row.names)) 
	    stop("non-unique row.names given")
    }
	if (knn$np < 1) 
	  stop("non-positive number of spatial units")   
	if (is.null(row.names)) row.names <- as.character(1:knn$np)
	graph <- spdep::nb2lines(knn.nb, coords = sf::st_coordinates(x), 
	                         proj4string = prj, as_sf=TRUE)
	  graph$length <- as.numeric(sf::st_length(graph))						 
	    names(graph)[3:4] <- c("from_ID","to_ID")
	      graph$from_ID <- as.character(graph$from_ID)
          graph$to_ID <- as.character(graph$to_ID)	
	rm.lower <- function(x) {
      ldiag <- function (x, diag=FALSE) {
            x <- as.matrix(x)
          if (diag) 
            row(x) >= col(x)
          else row(x) > col(x) 
        }  
        ctmx <- table(x$i, x$j) 	
          ctmx[ldiag(ctmx)] <- 0
            ctmx <- dmatrix.df(as.matrix(ctmx)) 
        ctmx <- data.frame(ij=paste(ctmx[,1], ctmx[,2], sep="."), dup=ctmx[,3])
        x$ij <- paste(x$i, x$j, sep=".")
          x <- merge(x, ctmx, by="ij")
          x <- x[x$dup == 1,]
        x <- x[,-which(names(x) %in% c("ij","dup"))]
      return(x)
    }
  	    if(drop.lower == TRUE) { graph <- rm.lower(graph)  }  
	  if(!is.null(max.dist)) graph <- graph[graph$length <= max.dist,] 
  return( graph )
}
