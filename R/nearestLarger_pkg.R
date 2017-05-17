#################################################################################
#
# Finding distances btwn islands and the nearest equivalent or larger island
# TJT, April 2017
#
# The point and polygon shapefiles should be projected into azimuthal equidistant
# or function will not run. The function finds distances between a centroid point
# and the nearest polygon (will disregard a distance of 0 if the point is inside
# of a polygon).
#
#################################################################################

# require(rgdal); require(rgeos)



#' Calculating distance to nearest island of equal or larger area
#'
#' This function calculates distances, for each island, to the nearest island of equal or larger area. It calculates area using the calcIslandArea() function from this package, and then reprojects into the azimuthal equidistant projection with the projection centered on the centroid for each iteration.
#' @param polys SpatialPolygonsDataFrame of islands in the world. Example GADM.
#' @param pts SpatialPointsDataFrame of centroids of your islands. Can use findCentroids() function.
#' @return Returns vector of distances between each of the centroids and the nearest island of equal or larger area.
#' @author Tyler J Tran, \email{tylerjtran@@gmail.com}
#' @export
#' @name nearestLarger
#' @import rgdal
#' @import rgeos
#'



nearestLarger <- function(pts, polys){

  nearestLargerDist <- vector()

  for(i in 1:dim(pts)[1]) {
    polys@data$calculatedArea <- calcIslandArea(polys)
    currentArea <- polys@data$calculatedArea[i]
    largerIslands <- polys[polys@data$calculatedArea > currentArea,]
    pDist <- vector()
    ptsTemp <- spTransform(pts, CRS(paste("+proj=aeqd +lat_0=",centroids.lat[i], "+lon_0=",centroids.long[i], "+x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))
    polysTemp <- spTransform(polys, CRS(paste("+proj=aeqd +lat_0=",centroids.lat[i], "+lon_0=",centroids.long[i], "+x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0", sep='')))

    for(j in 1:dim(largerIslands)[1]) {
      pDist <- append(pDist, gDistance(pts[i,],largerIslands[j,]))
    }

    pDist <- pDist[pDist != 0,]
    nearestLargerDist[i] <- min(pDist)
  }
  # nearestLargerDist <- data.frame(uniqueID=pts@data$uniqueID, nearestLargerDist=nearestLargerDist)
  return(nearestLargerDist)
}
