% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/node.statistics.R
\name{node.statistics}
\alias{node.statistics}
\title{raster statistics for nodes}
\usage{
node.statistics(x, r, buffer = NULL, stats = c("min", "median", "max"))
}
\arguments{
\item{x}{sp class SpatialPointsDataFrame object}

\item{r}{A rasterLayer, rasterStack or rasterBrick object}

\item{buffer}{Buffer distance, radius in projection units}

\item{stats}{Statistics to calculate. If vectorized, can pass a 
custom statistic function.}
}
\value{
data.frame object of at-node raster values or statistics
}
\description{
returns raster value or statistics 
            (based on specified radius) for node
}
\note{
If no buffer is specified, at-node raster values are returned
}
\examples{
\donttest{
 library(sf)
 library(terra)  

 data(ralu.site)
 xvars <- rast(system.file("extdata/covariates.tif", package="GeNetIt"))
   
 skew <- function(x, na.rm = TRUE) {  
           if (na.rm) x <- x[!is.na(x)]
           sum( (x - mean(x)) ^ 3) / ( length(x) * sd(x) ^ 3 )  
 		}

 # without buffer (values at point)
 system.time( {		
  stats <- node.statistics(ralu.site, r = xvars[[-6]]) 
 } )

 # with 1000m buffer (values around points)
 system.time( {		
  stats <- node.statistics(ralu.site, r = xvars[[-6]], buffer = 1000, 
              stats = c("min", "median", "max", "var", "skew")) 
 } ) 
 }

}
\author{
Jeffrey S. Evans  <jeffrey_evans@tnc.org> and 
        Melanie A. Murphy <melanie.murphy@uwyo.edu>
}
