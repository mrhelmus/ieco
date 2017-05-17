#################################################################################
#
# Species richness by clade from Dimensions data
# TJT, April 2016
#
# This script counts species richness and creates lists of species for each
# island in gadm for each clade from species range maps in the Dimensions data
# shared by Gio
#
#################################################################################


# rm(list=ls())

# require(rgdal); require(rgeos); require(maptools)

# amphib <- readOGR(dsn='C:/Users/tuh09869/Google Drive/dimensions data/open access/distribution/amphibians/global/shp', layer='TerrestrialAmphibians_TTOL')
# mammals <- readOGR(dsn='C:/Users/tuh09869/Google Drive/dimensions data/open access/distribution/mammals/global/shp', layer='TerrestrialMammals_TTOL')
# turtles <- readShapePoly('C:/Users/tuh09869/Google Drive/dimensions data/open access/distribution/reptiles/FW_TURTLES') # For turtles, use readShapePoly instead of readOGR bc readOGR has orphaned holes problem. But then must define projection.
# proj4string(turtles) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#
# masterIslands <- readOGR(dsn='C:/Users/tuh09869/Google Drive/Global Islands/shapefiles', layer='masterIslands_v2')

#' Get a list of species for islands
#'
#' This function takes species range maps and a shapefile of islands and outputs a list of species on each island. A few notes: if having problems with orphaned holes, try readShapePoly instead of readOGR. Also, if the output list is not the same length as the number of islands, you might need to manually add elements to the end of the list that are NA. The last element of the output list will likely be an island that is not NA (i.e. an island with species ranges that overlap).
#' @param islands SpatialPolygonsDataFrame of islands
#' @param sppRanges SpatialPolygonsDataFrame of species ranges
#' @param IDvar A character string. The name of the column in shapefile sppRanges with the species name to be output in the list.
#' @return Returns a list, with one entry for each island, of the species ranges that overlap for that island.
#' @author Tyler J Tran, \email{tylerjtran@@gmail.com}
#' @export
#' @name sppList
#' @import rgeos
#'

sppList <- function(islands, sppRanges, IDvar){
  speciesList <- list() # empty list to output into...each element of the list object will be one island
  for (i in 1:dim(islands)[1]){
    tempSpp <- vector()
    for (j in 1:dim(sppRanges)[1]){ # Nested loops...for each island (i=1581) compare to each polygon in species range maps (j, these range from ~150-6000 depending on clade) with gIntersects
      if (gIntersects(islands[i,], sppRanges[j,])==TRUE){ #gIntersects returns T if the two polygons intersect, F if not
        tempSpp <- append(tempSpp, as.character(paste(sppRanges[j,],'$', IDvar))) # If the two polygons intersect (TRUE output from gIntersects), then put the species name(s) in the list element for that island
        speciesList[[i]] <- tempSpp
      }
    }
  }
  return(speciesList)
}

# Lists might be a little shorter than they should be bc apparently it doesn't allow the last entries in a list to be null. Manually create them, but make them NA.

