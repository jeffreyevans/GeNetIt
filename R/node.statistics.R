#' @title raster statistics for nodes
#' @description returns raster value or statistics (based on specified radius) for node 
#'
#' @param x        sp class SpatialPointsDataFrame object 
#' @param r        A rasterLayer, rasterStack or rasterBrick object
#' @param buffer   Buffer distance, radius in projection units
#' @param stats    Statistics to calculate. If vectorized, can pass a custom statistic function. 
#'
#'	
#' @return data.frame object of at-node raster values or statistics 
#'
#' @note If no buffer is specified, at-node raster values are returned 
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
#' skew <- function(x, na.rm = TRUE) {  
#'           if (na.rm) x <- x[!is.na(x)]
#'           sum( (x - mean(x)) ^ 3) / ( length(x) * sd(x) ^ 3 )  
#' 		}
#'
#' # without buffer (values at point)
#' system.time( {		
#'  stats <- node.statistics(ralu.site, r = xvars[[-6]],
#'              stats = c("min", "median", "max", "var", "skew")) 
#' } )
#'
#' # with 1000m buffer (values around points)
#' system.time( {		
#'  stats <- node.statistics(ralu.site, r = xvars[[-6]], buffer = 1000,  
#'              stats = c("min", "median", "max", "var", "skew")) 
#' } ) 
#'
#'
#' dist.graph@@data <- data.frame(dist.graph@@data, stats, nstats)
#'   str(dist.graph@@data)
#' }
#' 
#' @import velox
#' @export graph.statistics
node.statistics <- function(x, r, buffer = NULL, 
              stats = c("min", "median", "max") ) {
  if(!inherits(x, "SpatialPointsDataFrame")) 
    stop("x is not a SpatialPointsDataFrame object")
  if (!inherits(r, "RasterLayer") &  !inherits(r, "RasterStack") &
	  !inherits(r, "RasterBrick") ) 
	    stop("r is not a raster object")
    rvx <- velox::velox(r)
  if(is.null(buffer)) {
    message("Only at-node values being returned")
    results <- as.data.frame(rvx$extract_points(sp = x))
	  names(results) <- names(r) 
  } else if(!is.null(buffer)) {
  if(!is.numeric(buffer)) stop("Buffer radius needs to be numeric")  
  message(paste0("Using ", buffer, " distance for statistics"))
    x <- rgeos::gBuffer(x, byid = TRUE, id = row.names(x), 
                        width = buffer, quadseg = 100)
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
