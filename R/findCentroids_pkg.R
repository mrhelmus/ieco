#################################################################################
#
# Finding centroids of polygons (islands)
# TJT, April 2016
#
# This script finds centroids of our island polygons.
#
#################################################################################


# rm(list=ls())

# require(rgdal); require(rgeos)

# masterIslands <- readOGR(dsn='C:/Users/tuh09869/Google Drive/Global Islands/shapefiles', layer='masterIslands_v2')
# masterArchs <- readOGR(dsn='C:/Users/tuh09869/Google Drive/Global Islands/shapefiles', layer='masterArchs_v2')
# continents <- readOGR(dsn='C:/Users/tuh09869/Google Drive/Geographic_Isolation/shapefiles', layer='gadm_continents')

#' Find centroids of island polygons (global islands)
#'
#' This function takes a polygon shapefile as input and finds the centroids. These can then be used to calculate geographic isolation (distances between island centroids and other islands, continents, etc.)
#' @param polys SpatialPolygonsDataFrame of islands for which you want centroids. Assumed unprojected lat/long.
#' @return Returns data frame of lat/long coordinates of the centroids for each island.
#' @author Tyler J Tran, \email{tylerjtran@@gmail.com}
#' @name findCentroids
#' @export
#' @import sp
#'

findCentroids <- function(polys){
  isl.centroids <- getSpPPolygonsLabptSlots(polys)
  isl.centroids <- as.data.frame(isl.centroids)
  centroids.latlong <- isl.centroids; colnames(centroids.latlong) <- c('long','lat')
  centroids.lat <- centroids.latlong$lat
  centroids.long <- centroids.latlong$long
  # isl.centroids <- cbind(polys@data$uniqueID, isl.centroids)
  # colnames(isl.centroids) <- c('uniqueID','long','lat')
  return(centroids.latlong)

  # coordinates(isl.centroids) <- ~long+lat
  # proj4string(isl.centroids) <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  # isl.centroids <- spTransform(isl.centroids, CRS("+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
}

