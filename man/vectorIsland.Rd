% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vectorIsland_pkg.R
\name{vectorIsland}
\alias{vectorIsland}
\title{Count how many pts per poly for economic isolation}
\usage{
vectorIsland(pts, polys)
}
\arguments{
\item{pts}{SpatialPointsDataFrame of whatever data you want to count per island (ex port shapefile to see how many ports per island).}

\item{polys}{SpatialPolygonsDataFrame of islands. polys@data must have a column 'uniqueID'}
}
\value{
Returns a dataframe of how many points per polygon
}
\description{
This function counts how many points per island.
}
\author{
Tyler J Tran, \email{tylerjtran@gmail.com}
}
