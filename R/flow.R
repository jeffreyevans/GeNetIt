#' @title Convert distance to flow
#'
#' @description Converts distance to flow (1-d) with or without data standardization
#' 
#' @param x             A numeric vector or matrix object representing distances
#' @param standardize   (FALSE/TRUE) Row-standardize the data before calculating flow
#' @param rm.na         (TRUE/FALSE) Should NA's be removed, if FALSE (default) the will be retained in the results 
#' @param diag.value    If x is a matrix, what diagonal matrix values should be used (default is NA) 
#'
#' @return A vector or matrix representing flow values  
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> and Melanie Murphy <melanie.murphy@@uwyo.edu>
#'
#' @examples 
#' #### On a distance vector
#' flow(runif(10,0,1))
#' flow(runif(10,0,500), standardize = TRUE)
#' 
#' # With NA's
#' d <- runif(10, 0,1)
#'   d[2] <- NA
#' flow(d)
#' flow(d, rm.na=TRUE)
#' 
#' #### On a distance matrix
#' dm <- as.matrix(dist(runif(5,0,1), diag = TRUE, upper = TRUE))
#' flow(dm)
#'
#' @export flow
flow <- function(x, standardize = FALSE, rm.na = FALSE, diag.value = NA) {
  if(!inherits(x, "numeric") & !inherits(x, "matrix")) 
    stop(deparse(substitute(x)), "x must be a vector or matrix")
 
  if(inherits(x, "numeric")) {
    if(standardize) { 
      d = 1 - (x[!is.na(x)] / max(x, na.rm=TRUE)) 
    } else {
      d = 1 - x[!is.na(x)]
    }
      if(rm.na == FALSE) {
        na.idx <- which(is.na(x))
		  if(length(na.idx) > 0) {
		    z <- numeric(length(d) + length(na.idx))
            z[na.idx] <- NA
            z[-na.idx] <- d
			d <- z
		  }
	  }
    }
  if(inherits(x, "matrix")) {
      if(standardize) { 
        d = 1 - (x / max(x, na.rm=TRUE)) 
      } else {
        d = 1 - x
      }
    diag(d) <- diag.value 	  
  }
return(d)  
}
