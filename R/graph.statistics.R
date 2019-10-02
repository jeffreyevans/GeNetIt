#' @title Point sample and statistics for edges (lines)
#' @description Samples rasters for each edge and calculates specified statistics
#'
#' @param x          sp class SpatialLinesDataFrame object 
#' @param r          A rasterLayer, rasterStack or rasterBrick object
#' @param d          Sample distance along edge, for alternate sampling options see sample.line. 
#' @param stats      Statistics to calculate. If vectorized, can pass a custom statistic function. 
#' @param buffer     Buffer distance, radius in projection units. For statistics based on edge buffer distance 
#' @param subsample  (FALSE/TRUE) Draw a point subsample of lines
#' @param ...        Additional argument passed to sample.line and spsample  
#'	
#' @return data.frame object of statistics 
#'
#' @note ...
#'  
#' @examples
#' \dontrun{
#' library(sp)
#' library(spdep)
#' library(raster)
#'   data(rasters)
#'   data(ralu.site)
#' 
#' xvars <- stack(rasters)
#' 
#'  dist.graph <- knn.graph(ralu.site, row.names = ralu.site$SiteName, 
#'                          max.dist = 1500)
#'   str(dist.graph@data)
#'   
#' skew <- function(x, na.rm = TRUE) {  
#'           if (na.rm) x <- x[!is.na(x)]
#'           sum( (x - mean(x)) ^ 3) / ( length(x) * sd(x) ^ 3 )  
#' 		}
#'
#'# Moments on continuous raster data
#' system.time( {		
#'  stats <- graph.statistics(dist.graph, r = xvars[[-6]],  
#'              stats = c("min", "median", "max", "var", "skew")) 
#' } ) 
#'
#'# Proportional function on nominal raster data		
#'p <- function(x) { length(x[x < 52]) / length(x) }	
#'	
#'  system.time( {		
#'   nstats <- graph.statistics(dist.graph, r = xvars[[6]],  
#'               stats = "p") 
#'  } ) 	
#'
#'# Based on 500m buffer distance around line(s)
#' system.time( {		
#'  stats <- graph.statistics(dist.graph, r = xvars[[-6]],  
#'              stats = c("min", "median", "max", "var", "skew"),
#'			  buffer = 500) 
#' } )
#'
#' dist.graph@@data <- data.frame(dist.graph@@data, stats, nstats)
#'   str(dist.graph@@data)
#' }
#' 
#' @import velox
#' @export graph.statistics
graph.statistics <- function(x, r, stats = c("min", "mean", "max"), buffer = NULL,  
                             subsample = FALSE, d = 30, ...) {
  if(!inherits(x, "SpatialLinesDataFrame")) 
    stop("x is not a SpatialLinesDataFrame object")
  if (!inherits(r, "RasterLayer") &  !inherits(r, "RasterStack") &
	  !inherits(r, "RasterBrick") ) 
	    stop("r is not a raster object")
  if(!is.null(buffer) & subsample == TRUE) stop("Cannot buffer point subsample")		
    rvx <- velox::velox(r)
  if(subsample) {
    #### Subsample line using points 				
    dots <- as.list(match.call(expand.dots = TRUE)[-1])
      dots[["x"]] <- x
  	    dots[["d"]] <- d
    if (is.null(dots[["min.samp"]]) & "min.samp" %in% names(dots) == FALSE) 
	  dots[["min.samp"]] <-  2
    if (is.null(dots[["type"]]) & "type" %in% names(dots) == FALSE) 
	  dots[["type"]] <-  "regular"
    samp <- do.call(spatialEco::sample.line, dots)
      rdf <- as.data.frame(rvx$extract_points(sp = samp))
  	  names(rdf) <- names(r)
          samp@data <- data.frame(samp@data, rdf)
    results <- NULL
    if(dim(r)[3] > 1) {   
      for( p in names(r) ) {
        for(s in 1:length(stats)) {
          results <- cbind(results, as.vector(tapply(samp@data[,p], 
		                   samp@data[,"LID"], stats[s])))
        }
	  }
	} else {
	  names(samp@data)[2] <- names(r)
      for(s in 1:length(stats)) {
        results <- cbind(results, as.vector(tapply(samp@data[,2], 
		                 samp@data[,"LID"], stats[s])))
      } 
    }	  
    results <- as.data.frame(results)
	  rn <- vector()
	    for(n in names(r)) { rn <- append(rn, paste(stats, n, sep="."))}
	      names(results) <- rn  
  } else {
  #### Extract all values intersecting lines
  if(!is.null(buffer)) {
    message(paste0("Using ", buffer, " distance for statistics"))
    x <- rgeos::gBuffer(x, byid = TRUE, id = row.names(x), 
                        width = buffer, quadseg = 100)
  }
    ldf <- rvx$extract(sp = x)
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
  }
  return( results )
}
