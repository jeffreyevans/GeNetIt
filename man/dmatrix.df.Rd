% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dmatrix.df.R
\name{dmatrix.df}
\alias{dmatrix.df}
\title{Distance matrix to data.frame}
\usage{
dmatrix.df(x, rm.diag = TRUE)
}
\arguments{
\item{x}{Symmetrical distance matrix}

\item{rm.diag}{(TRUE/FALSE) remove matrix diagonal, self values.}
}
\value{
data.frame object representing to and from values
}
\description{
Coerces distance matrix to a data.frame object
}
\note{
Function results in data.frame object with "X1" (FROM), "X2" (TO) and 
"distance" columns. The FROM column represents to origin ID, TO represents 
destination ID and distance is the associated matrix distance. These 
results can be joined back to the graph object using either the origin or 
destination ID's.
}
\examples{
 library(sf)
 pts <- data.frame(ID=paste0("ob",1:15), x=runif(15, 480933, 504250), 
                   y=runif(15, 4479433, 4535122))
   pts <- st_as_sf(pts, coords = c("x", "y"), 
                   crs = 32611, agr = "constant") 
 
 # Create distance matrix  
 dm <- st_distance(pts)
   class(dm) <- setdiff(class(dm), "units")  
     attr(dm, "units") <- NULL
   colnames(dm) <- pts$ID 
   rownames(dm) <- pts$ID
 
 # Coerce to data.frame with TO and FROM ID's and associated distance
 dm.df <- dmatrix.df(dm)
   head(dm.df)

}
\author{
Jeffrey S. Evans  <jeffrey_evans@tnc.org> and 
        Melanie A. Murphy <melanie.murphy@uwyo.edu>
}
