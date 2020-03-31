#' @title raster statistics for nodes
#' @description returns raster value or statistics 
#'             (based on specified radius) for node 
#'
#' @param x        sp class SpatialPointsDataFrame object 
#' @param r        A rasterLayer, rasterStack or rasterBrick object
#' @param buffer   Buffer distance, radius in projection units
#' @param stats    Statistics to calculate. If vectorized, can pass a 
#'                 custom statistic function. 
#'
#'	
#' @return data.frame object of at-node raster values or statistics 
#'
#' @note 
#' If no buffer is specified, at-node raster values are returned 
#'  
#' @examples
#' \donttest{
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
#' dist.graph@@data <- data.frame(dist.graph@@data, stats, nstats)
#'   str(dist.graph@@data)
#' }
#' 
#' @export graph.statistics
node.statistics <- function(x, r, buffer = NULL, 
                            stats = c("min", "median", "max") ) {				
  if (!any(class(r)[1] == c("RasterLayer", "RasterStack", "RasterBrick"))) 
      stop("r must be a raster (layer, stack, brick) class object")
  if (!any(class(x)[1] == c("SpatialPointsDataFrame", "sf"))) 
      stop("x must be a SpatialPointsDataFrame or sf POINT object")
  if(class(x)[1] == "sf"){
    if(attributes(x$geometry)$class[1] != "POINT")
      stop("x must be a sf sfc_LINE object")
  }	
  if(!sp::is.projected(methods::as(x,"Spatial")))
    warning("Projection is not defined or in lat/long, is it recommended that you 
      project your data to prevent planiar distortions")
  if(inherits(x, "SpatialPointsDataFrame")) {
    x <- sf::st_as_sf(x)
  } 	  
  if(is.null(buffer)) {
    message("At-node ([x,y] point) values being returned")
      results <- data.frame(raster::extract(r, x))
	    names(results) <- names(r) 
  } else if(!is.null(buffer)) {
  if(!is.numeric(buffer)) stop("Buffer radius needs to be numeric")  
    message(paste0("Using ", buffer, " distance for statistics"))
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
  }
  return( results )
}
