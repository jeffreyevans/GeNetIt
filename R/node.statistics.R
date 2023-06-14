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
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> and 
#'         Melanie A. Murphy <melanie.murphy@@uwyo.edu>
#'  
#' @examples
#' \donttest{
#'  library(sf)
#'  library(terra)  
#'
#'  data(ralu.site)
#'  xvars <- rast(system.file("extdata/covariates.tif", package="GeNetIt"))
#'    
#'  skew <- function(x, na.rm = TRUE) {  
#'            if (na.rm) x <- x[!is.na(x)]
#'            sum( (x - mean(x)) ^ 3) / ( length(x) * sd(x) ^ 3 )  
#'  		}
#' 
#'  # without buffer (values at point)
#'  system.time( {		
#'   stats <- node.statistics(ralu.site, r = xvars[[-6]]) 
#'  } )
#' 
#'  # with 1000m buffer (values around points)
#'  system.time( {		
#'   stats <- node.statistics(ralu.site, r = xvars[[-6]], buffer = 1000, 
#'               stats = c("min", "median", "max", "var", "skew")) 
#'  } ) 
#'  }
#' 
#' @export node.statistics
node.statistics <- function(x, r, buffer = NULL, 
                            stats = c("min", "median", "max") ) {

  if (!inherits(r, "SpatRaster")) 
    stop("r must be a terra SpatRaster class object") 
  if (!inherits(x, "sf")) 
    stop("x must be a sf POINT object") 
  if(attributes(x$geometry)$class[1] != "sfc_POINT")
    stop("x must be a sf sfc_POINT object")   	
  if(sf::st_is_longlat(x))
    warning("Projection is not defined or in lat/long, is it recommended that you 
      project your data to prevent planar distortions in the buffer")
  if(!sf::st_crs(x) == sf::st_crs(terra::crs(r)))
    warning("x and r projections do not match")	  
  
  #### Extract raster values intersecting points or buffers
  if(is.null(buffer)) {
    message("At-node ([x,y] point) values being returned")
    results <- terra::extract(r, terra::vect(x))
  } else {	
    message(paste0("Using ", buffer, " distance for statistics"))
	  b <- sf::st_buffer(x, dist = buffer)
      if(!nrow(b) == nrow(x))
        stop("Sorry, something went wrong with buffering, features do not match")
      ldf <- exactextractr::exact_extract(r, b, progress = FALSE)
	    ldf <- lapply(ldf, FUN = function(x) {
		  j <- as.data.frame(x[,-which(names(x) %in% "coverage_fraction")])
		    names(j) <- names(r)
		  return(j)})
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
