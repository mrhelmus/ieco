#################################################################################
#
# Calculates areas of polygons in GADM shapefile
# TJT, April 2017
#
# This function projects the gadm shapefile into World Cylindrical Equal Area
# projection and calculates 'polyArea' as a field. Shapefile saved as gadm_v3.
# Areas have been saved to be in m2
#################################################################################


# require(rgdal); require(rgeos)


#' Calculating island area
#'
#' This function re-projects a spatialPolygonsDataFrame (from shapefile) of islands in the global islands dataset to the World cylindrical equal area projection, and calculates area for each island (or each polygon, rather).
#' @param polys SpatialPolygonsDataFrame of your global islands dataset. Unprojected lat/long.
#' @return Returns vector of polygon areas. Can be added as a field in attribute table of SPDF of global islands.
#' @author Tyler J Tran, \email{tylerjtran@@gmail.com}
#' @export
#' @name calcIslandArea
#' @import rgdal


calcIslandArea <- function(polys){
  polys.ea <- spTransform(polys, CRS('+proj=cea +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs '))
  polyArea <- vector()

  for (i in 1:dim(polys.ea@data)[1]){
    polyArea[i] <- polys.ea@polygons[[i]]@area #or can use gArea function from rgeos pkg
  }

  return(polyArea)
}



