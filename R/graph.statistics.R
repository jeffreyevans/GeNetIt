#' @title Statistics for edges (lines)
#' @description Extracts raster values for each edge and calculates specified statistics
#'
#' @param x          sp SpatialLinesDataFrame or sf LINE object 
#' @param r          A rasterLayer, rasterStack or rasterBrick object
#' @param stats      Statistics to calculate. If vectorized, can pass a custom 
#'                   statistic function. 
#' @param buffer     Buffer distance, radius in projection units. For statistics 
#'                   based on edge buffer distance  
#'	
#' @return data.frame object of statistics 
#'
#' @note 
#' If the buffer argument is specified that, raster values within the specified 
#' buffer radius are extracted and included in the derived statistic(s). Else-wise,
#' the statistics are derived from raster values that directly intersect each edge.  
#'  
#' @examples
#' \donttest{
#'  library(sp)
#'  library(spdep)
#'  library(raster)
#'    data(rasters)
#'    data(ralu.site)
#'  
#'  xvars <- stack(rasters)
#'  
#'   dist.graph <- knn.graph(ralu.site, row.names = ralu.site$SiteName, 
#'                           max.dist = 1500)
#'    str(dist.graph@data)
#'    
#'  skew <- function(x, na.rm = TRUE) {  
#'            if (na.rm) x <- x[!is.na(x)]
#'            sum( (x - mean(x)) ^ 3) / ( length(x) * sd(x) ^ 3 )  
#'  		}
#' 
#' # Moments on continuous raster data
#'  system.time( {		
#'   stats <- graph.statistics(dist.graph, r = xvars[[-6]],  
#'               stats = c("min", "median", "max", "var", "skew")) 
#'  } ) 
#' 
#' # Proportional function on nominal raster data		
#' p <- function(x) { length(x[x < 52]) / length(x) }	
#' 	
#'   system.time( {		
#'    nstats <- graph.statistics(dist.graph, r = xvars[[6]],
#'                stats = "p") 
#'   } ) 	
#' 
#' # Based on 500m buffer distance around line(s)
#'  system.time( {		
#'   stats <- graph.statistics(dist.graph, r = xvars[[-6]],  
#'               stats = c("min", "median", "max", "var", "skew"),
#' 			      buffer = 500) 
#'  } )
#' 
#'  dist.graph@@data <- data.frame(dist.graph@@data, stats, nstats)
#'    str(dist.graph@@data)
#' }
#' 
#' @export graph.statistics
graph.statistics <- function(x, r, stats = c("min", "mean", "max"), buffer = NULL) {							 
  if (!any(class(r)[1] == c("RasterLayer", "RasterStack", "RasterBrick"))) 
      stop("r must be a raster (layer, stack, brick) class object")
  if (!any(class(x)[1] == c("SpatialLinesDataFrame", "sf"))) 
      stop("x must be a SpatialLinesDataFrame or sf LINESTRING object")
  if(class(x)[1] == "sf"){
    if(attributes(x$geometry)$class[1] != "LINESTRING")
      stop("x must be a sf sfc_LINE object")
  }	
  if(!sp::is.projected(x))
    warning("Projection is not defined or in lat/long, is it recommended that you 
      project your data to prevent planiar distortions")
  if(inherits(x, "SpatialLinesDataFrame")) {
	  x <- sf::st_as_sf(x)
  }    
  #### Extract all values intersecting lines
  if(is.null(buffer)) {
    buffer <- raster::res(r)[1] * 0.05
  } else {	
    message(paste0("Using ", buffer, " distance for statistics"))
  }
  x <- sf::st_buffer(x, dist = buffer)  
  ldf <- exactextractr::exact_extract(r, x, progress = FALSE)
    ldf <- lapply(ldf, FUN = function(x) as.data.frame(x[,-which(names(x) %in% "coverage_fraction")]))	
	  names(ldf) <- row.names(x)
    stats.fun <- function(x, m = stats) {
	  slist <- list()
        for(i in 1:length(m)) {
	      slist[[i]] <- apply(x, MARGIN=2, m[i])
	    }
	  return( as.data.frame(t(unlist(slist))) )
    }
    results <- lapply(ldf, FUN=stats.fun)
	  results <- do.call("rbind", results)
	    rn <- vector()
	for(n in stats) { rn <- append(rn, paste(n, names(r), sep="."))}
	  names(results) <- rn 
  return( results )
}
