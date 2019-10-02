#' @title Statistics for edges (lines) based on a defined scale (area).
#' @description Samples rasters for each edge and calculates specified 
#'              statistics for buffer distance
#'   
#' @param ... Parameters to be passed to the modern version of the function
#'   
#' @note Please note that this function has been deprecated, please use graph.statistics 
#'       with the buffer argument.
#'
#' @export
area.graph.statistics <- function(...) {
     .Deprecated("area.graph.statistics", package="GeNetIt", 
	 msg="this function is depreciated, please use graph.statistics with buffer argument")
  graph.statistics(...)
}
