#################################################################################
#
# Calculating distances between points and polygons with reprojection
# TJT, April 2016
#
# This function requires points and polygons as inputs. It was written to find
# distances between an island's centroid and other islands, but is adaptable. The
# function iterates through however many centroid points there are, and for each
# point, finds the distance between that point and the closest polygon in the
# poly input. It also, for each iteration, centers the azimuthal equidistant
# projection around the centroid point. Can be a very slow script.
#
#################################################################################


# rm(list=ls())

# require(rgdal); require(rgeos)



#' Find distances between island centroids and other polygons
#'
#' This function calculates the distance between island centroids (input) and the nearest polygons. The function iterates through however many centroid points are given, and for each point, reprojects to an azimuthal equidistant projection that is centered around the centroid (see Weigelt and Kreft 2013). Because of the use of a for loop and reprojections, this can be a very slow script.
#' @param pts SpatialPointsDataFrame of island centroids. Assumed unprojected lat/long.
#' @param polys SpatialPolygonsDataFrame of islands for which you want to calculate distances between centroids and polys. Assumed unprojected lat/long.
#' @return Returns a vector of distances between each of the centroids and the closest island. The vector is in the same order as the centroid input.
#' @author Tyler J Tran, \email{tylerjtran@@gmail.com}
#' @export
#' @rdname distance.reproj
#' @import rgdal
#' @import rgeos
#'


distance.reproj <- function(pts, polys){
  nearestPoly <- vector()
  centroids.lat <- pts@coords$lat
  centroids.long <- pts@coords$long

  for(i in 1:dim(pts)[1]) {
    pDist <- vector()
    ptsTemp <- spTransform(pts, CRS(paste("+proj=aeqd +lat_0=",centroids.lat[i], "+lon_0=",centroids.long[i], "+x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))
    polysTemp <- spTransform(polys, CRS(paste("+proj=aeqd +lat_0=",centroids.lat[i], "+lon_0=",centroids.long[i], "+x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))

    for(j in 1:dim(polys)[1]) {
      pDist <- append(pDist, gDistance(ptsTemp[i,],polysTemp[j,]))
    }
    pDist <- pDist[pDist != 0]
    nearestPoly[i] <- min(pDist)
  }
  return(nearestPoly)
}

