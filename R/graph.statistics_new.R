library(sp)
library(spdep)
library(raster)
library(GeNetIt)
  data(rasters)
  data(ralu.site)

xvars <- stack(rasters)
dist.graph <- knn.graph(ralu.site, row.names = ralu.site@data[,"SiteName"], 
                        max.dist = 1500)
 
skew <- function(x, na.rm = TRUE) {  
          if (na.rm) x <- x[!is.na(x)]
          sum( (x - mean(x)) ^ 3) / ( length(x) * sd(x) ^ 3 )  
		}
		
stats <- graph.statistics(dist.graph, r = xvars[[1:5]],
             stats = c("min", "median", "max", "var", "skew")) 

dist.graph <- merge(dist.graph, stats, by="row.names", all=TRUE)

###############################
graph.statistics <- function(x, r, stats = c("min", "mean", "max")) {
    if(!inherits(x, "SpatialLinesDataFrame")) stop("x is not a SpatialLinesDataFrame object")
	  if (!inherits(r, "RasterLayer") &  
            !inherits(r, "RasterStack") &
		      !inherits(r, "RasterBrick") ) 
	            stop("r is not a raster object")				
    # tr <- terra::rast(r[[1]])
    # tv <- terra::vect(x, type="lines")
    # e <- terra::extract(tr, tv)	
  results <- NULL
    for(i in 1:length(x)) {
      #idx <- which(l[] %in% i)
	  idx <- extract(r[[1]], x[1,], cellnumbers=TRUE)[[1]][,1]
	    stats.results <- NULL
	  for(p in 1:nlayers(r)) {
	    lstat <- list()
	    for(s in 1:length(stats)) {
		  lstat[[s]] <- tapply(r[[p]][idx], rep("1", length(idx)), stats[s], na.rm=TRUE)
	    }
		xstat <- unlist(lstat)
		  names(xstat) <- paste(stats, names(r)[p], sep=".") 
		    stats.results <- c(stats.results, xstat) 
	  } 
	results <- rbind(results, stats.results)  
	}
    row.names(results) <- row.names(x@data)	
  return( results )  
}
