#' @title Distance matrix to data.frame
#' @description Coerces distance matrix to a data.frame object
#' 
#' @param    x        Symmetrical distance matrix
#' @param    rm.diag  (TRUE/FALSE) remove matrix diagonal, self values. 
#' @return   data.frame object representing to and from values
#'
#' @note 
#' Function results in data.frame object with "X1" (FROM), "X2" (TO) and 
#' "distance" columns. The FROM column represents to origin ID, TO represents 
#' destination ID and distance is the associated matrix distance. These 
#' results can be joined back to the graph object using either the origin or 
#' destination ID's.  
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org> and 
#'         Melanie A. Murphy <melanie.murphy@@uwyo.edu>
#'
#' @examples 
#'  library(sf)
#'  pts <- data.frame(ID=paste0("ob",1:15), x=runif(15, 480933, 504250), 
#'                    y=runif(15, 4479433, 4535122))
#'    pts <- st_as_sf(pts, coords = c("x", "y"), 
#'                    crs = 32611, agr = "constant") 
#'  
#'  # Create distance matrix  
#'  dm <- st_distance(pts)
#'    class(dm) <- setdiff(class(dm), "units")  
#'      attr(dm, "units") <- NULL
#'    colnames(dm) <- pts$ID 
#'    rownames(dm) <- pts$ID
#'  
#'  # Coerce to data.frame with TO and FROM ID's and associated distance
#'  dm.df <- dmatrix.df(dm)
#'    head(dm.df)
#'
#' @export
dmatrix.df <- function(x, rm.diag = TRUE) {
  if( nrow(x) != ncol(x))
    stop("Matrix is not symmetrical")
  if(rm.diag == TRUE) { diag(x) <- NA }
  if(is.null(rownames(x)) && is.null(colnames(x)))
    stop("Either rows or columns need names")  
  if(is.null(rownames(x))) {
    rownames(x) <- colnames(x)  
  }
  if(is.null(colnames(x))) {
    colnames(x) <- rownames(x)  
  }
  if(any(rownames(x) != colnames(x))){
    message("names do not match; defaulting to column names") 
	rownames(x) <- colnames(x) 
  }	
    varnames = list(colnames(x),colnames(x))
      values <- as.vector(x)
      dn <- dimnames(x)
        char <- sapply(dn, is.character)
    dn[char] <- lapply(dn[char], utils::type.convert, as.is=TRUE)
    indices <- do.call(expand.grid, dn)
        names(indices) <- c("from","to")
      indices <- data.frame(indices, distance = values)
    indices <- stats::na.omit(indices)	
  return( indices )
}
