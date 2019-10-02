#' @title Statistics for edges (lines) based on a defined scale (area).
#' @description Samples rasters for each edge and calculates specified statistics
#'   
#' @param ... Parameters to be passed to the modern version of the function
#'   
#' @export
area.graph.statistics <- function(...) {
     .Deprecated("area.graph.statistics", package="GeNetIt", 
	 msg="this function is depreciated, please use graph.statistics with buffer argument")
  area.graph.statistics(...)
}
