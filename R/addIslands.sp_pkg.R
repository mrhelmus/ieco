# TJT, April 2017

###### Notes on unique IDs
# Island polygons begin at 10001
# Arch polygons begin at 20001
# Island points (from gs) begin at 30001 (THESE ARE TEMPORARY)
# Arch points (from gs) begin at 40001 (THESE ARE TEMPORARY)

# rm(list=ls())

# require(googlesheets);require(dplyr); require(raster); require(spatialEco); require(sp); require(maptools); require(rgdal)


#####################################################
# Add Burbidge 1997 islands
#####################################################

# newIslands <- gs_title('ibt_globalislands_41317')
# newIslands <- newIslands %>%
#   gs_read(ws = "toAdd42717")
# newIslands <- as.data.frame(newIslands)
# newIslands <- newIslands[,1:22] # Get rid of unnecessary columns for now
#
# gadm <- readOGR(dsn='C:/Users/tuh09869/Google Drive/Global Islands/GADM_WORLD.shp/gadm28.shp', layer='Gadm28_merged_edited')
# isl.polys <- readOGR(dsn='C:/Users/tuh09869/Google Drive/Global Islands/shapefiles', layer='masterIslands_v2')
# gadm <- readOGR(dsn='C:/Users/tuh09869/Google Drive/Global Islands/GADM_WORLD.shp/gadm28.shp', layer='gadm_v3')

#' Adding new islands to global islands dataset
#'
#' This function adds new islands to the global island dataset, and assigns unique numeric IDs to the new islands based off the ID system of the old islands.
#' @param newIslands Data frame of new islands, with a row for each new island. Must have columns called 'lat' and 'long' but no other requirements.
#' @param currentIslands SpatialPolygonsDataFrame of current global island dataset. MUST have numeric ID column in attributes named 'uniqueID'. Unprojected lat/long.
#' @param polys SpatialPolygonsDataFrame of all land on earth (GADM shapefile, for example). Unprojected lat/long.
#' @return Returns spatialPolygonsDataFrame of newIslands. This SPDF can be merged with currentIslands easily using rbind if the columns are the same...see newCurrentIslands <- rbind(currentIslands, polys2add, makeUniqueIDs = T)
#' @author Tyler J Tran, \email{tylerjtran@@gmail.com}
#' @name addIslands.sp
#' @export
#'

addIslands.sp <- function(newIslands, currentIslands, polys){

  removeNA <- !is.na(newIslands$lat) # Sometimes there are islands in spreadsheets without coordinates. Remove them.
  newIslands <- newIslands[removeNA,]

  coordinates(newIslands) <- ~long+lat # Turn this new island data into spatial data
  proj4string(newIslands) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  currentIslands@data$tempUniqueID <- 1:dim(currentIslands)[1] # Assign temporary unique IDs to the currentIsland layer (so that we know, after we join the new pts, which of the new pts hit the polys from the currentIslands shapefile (those with tempUniqueIDs attached to them hit current polys))

  new.joinPts <- over(newIslands, currentIslands)
  new.joinPts <- spCbind(newIslands, new.joinPts)

  currentIslandsHit <- unique(new.joinPts@data$tempUniqueID) # IDs of polygons from currentIslands that had points from newIslands that hit them
  currentIslandsHit <- na.omit(currentIslandsHit)
  currentIslandsWnewPts <- currentIslands@data$tempUniqueID[(currentIslands@data$tempUniqueID %in% currentIslandsHit)]
  currentIslandsWnewPts <- currentIslands[currentIslandsWnewPts,] # Now it is polygons from currentIslands that were struck by pts from newIslands

  # Now we want to keep only the points from newIslands that DID NOT intersect with polys from currentIslands
  trulyNewPts <- new.joinPts[is.na(new.joinPts@data$tempUniqueID),]

  polys@data$tempGadmID <- 1:dim(polys)[1]
  new.joinPts <- over(trulyNewPts, polys)
  new.joinPts <- spCbind(trulyNewPts, new.joinPts)
  new.joinPts <- new.joinPts[!is.na(new.joinPts@data$tempGadmID),] # Get rid of newIslands pts that do not hit polys in gadm

  polys2add <- polys[(polys@data$tempGadmID %in% unique(new.joinPts@data$tempGadmID)),] # These are the polygons that you will be adding to your dataset (derived from poly/gadm)
  mergeAttributes <- merge(x=polys2add@data, y=new.joinPts@data, by='tempGadmID', all.x=T) # Do a join
  polys2add@data <- mergeAttributes

  # Now add uniqueIDs to the new islands (starting where the currentIslands uniqueIDs left off)
  startID <- currentIslands@data$uniqueID[dim(currentIslands@data)[1]] + 1
  polys2add@data$uniqueID <- startID:(startID + (length(polys2add@data$uniqueID)-1))

  # Put currentIslands and newIslands together into a single spatial object
  # newCurrentIslands <- rbind(currentIslands, polys2add, makeUniqueIDs = T)
  return(polys2add) # This can now be merged with currentIslands using rbind if the columns are the same (see line of code above, with makeUniqueIDs=T)
}



###########

# removeNA <- !is.na(newIslands$lat)
# newIslands <- newIslands[removeNA,]
#
# coordinates(newIslands) <- ~long+lat
# proj4string(newIslands) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#
# newIslands@data$ptID <- 50001:(50000 + dim(newIslands@data)[1])
#
# new.joinPts <- over(newIslands, isl.polys)
# new.joinPts <- spCbind(newIslands, new.joinPts)
#
# new.uniquePolys <- unique(new.joinPts@data$uniqueID) # number of polygons from island.shp that had points from islands that hit them
# new.uniquePolys <- na.omit(new.uniquePolys)
# new.uniquePts <- unique(new.joinPts@data$ptID)
#
# # Find which are the polygons (from isl.polys) that DO have points fall within them
# new.polysWPts <- isl.polys@data$uniqueID[(isl.polys@data$uniqueID %in% new.uniquePolys)]
# new.polysWPts <- isl.polys[(new.polysWPts-10000),]
#
# # Keep only the entries in new.joinPts that have NA for uniqueID (b/c they did not intersect with an already-present polygon)
# new.joinThese <- new.joinPts[is.na(new.joinPts@data$uniqueID),]
#
# new.joinPts <- over(new.joinThese, gadm)
# new.joinPts <- spCbind(new.joinThese, new.joinPts)
# new.joinPts <- new.joinPts[!is.na(new.joinPts@data$gadmID),] # Get rid of points that do not hit polys in gadm
#
# sum(is.na(new.joinPts$gadmID)) # To see how many of the remaining pts do not hit polys in gadm
#
# new.joinPts <- new.joinPts[!new.joinPts@data$gadmID==14257,] # Remove bc this is mainland Australia
# new.joinPts <- new.joinPts[!new.joinPts@data$gadmID==11806,] # Remove bc this one is a mess
#
# polys2add <- gadm[(gadm@data$gadmID %in% unique(new.joinPts@data$gadmID)),]
# mergeAttributes <- merge(x=polys2add@data, y=new.joinPts@data, by='gadmID', all.x=T)
# polys2add@data <- mergeAttributes
#
#
# # Now add unique IDs starting where the previous ones left off
# startID <- isl.polys@data$uniqueID[dim(isl.polys@data)[1]] + 1
# polys2add@data$uniqueID <- startID:(startID + (length(polys2add@data$uniqueID)-1))
# polys2add@data <- data.frame(country=polys2add@data$country.x, island=polys2add@data$Island.IBT,
#                              uniqueID=polys2add@data$uniqueID)
#
# masterIslands <- rbind(isl.polys, polys2add, makeUniqueIDs = T)
# writeOGR(obj=masterIslands, dsn="C:/Users/tuh09869/Google Drive/Global Islands/shapefiles", layer="masterIslands_v2", driver="ESRI Shapefile")



############################################
# Now add the uniqueIDs into the googlesheet
############################################

# allIslandsGS <- gs_title('ibt_globalislands')
# allIslandsGS <- allIslandsGS %>%
#   gs_read(ws = "Sheet1")
# allIslandsGS <- as.data.frame(allIslandsGS)
# allIslands <- allIslandsGS[allIslandsGS$island_or_==1,]
# allArchs <- allIslandsGS[allIslandsGS$island_or_==2,]
#
# newIslands <- gs_title('Islands to Add')
# newIslands <- newIslands %>%
#   gs_read(ws = "New Burbidge")
# newIslands <- as.data.frame(newIslands)
#
# masterIslands <- readOGR(dsn='C:/Users/tuh09869/Google Drive/Global Islands/shapefiles', layer='masterIslands_v2')
# masterArchs <- readOGR(dsn='C:/Users/tuh09869/Google Drive/Global Islands/shapefiles', layer='masterArchs')
# # masterArchs@data$uniqueID <- 20001:(20000 + dim(masterArchs@data)[1])
# # writeOGR(obj=masterArchs, dsn="C:/Users/tuh09869/Google Drive/Global Islands/shapefiles", layer="masterArchs_v2", driver="ESRI Shapefile")
#
# # Add the new data (in this case, Burbidge data) in before doing spatial join
# allIslands$bats <- as.integer(allIslands$bats)
# allIslands$Human_popu <- as.integer(allIslands$Human_popu)
# combinedIslands <- bind_rows(allIslands, newIslands)
#
# # Some of the new data had NA for lat or long...get rid of those rows
# combinedIslands <- combinedIslands[!is.na(combinedIslands$lat),]
#
# # Make spatial objects
# coordinates(allIslands) <- ~long+lat
# proj4string(allIslands) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# coordinates(combinedIslands) <- ~long+lat
# proj4string(combinedIslands) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# coordinates(allArchs) <- ~long+lat
# proj4string(allArchs) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#
# allIslandsJoin <- over(combinedIslands, masterIslands)
# allIslandsJoin <- spCbind(combinedIslands, allIslandsJoin) # This is a spatial points df with old data and new data (ready to be put back into GS)
#
# # joinID <- data.frame(ptID=allIslandsJoin@data$ptID, uniqueID=allIslandsJoin@data$uniqueID)
# #
# # uniqueIDsJoined <- merge(x=allIslandsGS, y=joinID, by='ptID', all.x=T)
#
# allArchsJoin <- over(allArchs, masterArchs)
# allArchsJoin <- spCbind(allArchs, allArchsJoin)
#
#
# allIslands$bats <- as.integer(allIslands$bats)
# allIslands@data$Human_popu <- as.character(allIslands@data$Human_popu)
# allArchsJoin@data$bats <- as.integer(allArchs@data$bats)
# allArchsJoin@data$Human_popu <- as.integer(allArchs@data$Human_popu)
# allIslandsJoin <- cbind(allIslandsJoin@data, allIslandsJoin@coords)
# allArchsJoin <- cbind(allArchsJoin@data, allArchsJoin@coords)
# everythingIBT <- bind_rows(allIslandsJoin, allArchsJoin)
#
# ## Don't use googlesheets to write...takes forever
# # ibt_globalislands_41317 <- gs_new('ibt_globalislands_41317', ws_title = 'data', input = allIslandsJoin@data)
#
#
#
# write.csv(everythingIBT, file='C:/Users/tuh09869/Google Drive/Global Islands/test41317.csv', sep=',')
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# (my_sheets <- gs_ls())
#
# allIslands <- gs_title('ibt_globalislands')
# allIslands <- allIslands %>%
#   gs_read(ws = "Sheet1")
# allIslands <- as.data.frame(allIslands)
#
# allArchs <- allIslands[allIslands$island_or_==2,]
#
#
# archs <- allArchs[!duplicated(allArchs$Island_Arc),]
# islands <- allIslands[!duplicated(allIslands$Island_Arc),]
# isl.names.gs <- islands$Island_Arc
# arch.names.gs <- archs$Island_Arc
#
# island.shp <- readOGR(dsn='C:/Users/tuh09869/Google Drive/Global Islands/shapefiles', layer='gadmIslands')
# arch.shp <- readOGR(dsn='C:/Users/tuh09869/Google Drive/Global Islands/shapefiles', layer='gadmArch_diss')
# isl.names.shp <- island.shp@data$island
# isl.names.shp <- as.character(isl.names.shp)
# isl.names.shp <- isl.names.shp[!duplicated(isl.names.shp)]
#
# matchNames <- intersect(isl.names.shp, isl.names.gs)
#
# notMatchNames.gs <- isl.names.gs[!(isl.names.gs %in% matchNames)] # Islands that are in gs but not in shapefile
# notMatchNames.shp <- isl.names.shp[!(isl.names.shp %in% matchNames)] # Islands that are in shapefile but not in gs
#
# coordinates(islands) <- ~long+lat
# proj4string(islands) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# coordinates(archs) <- ~long+lat
# proj4string(archs) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#
# isl.pts <- islands
# isl.polys <- island.shp
#
# isl.pts@data <- isl.pts@data[,c(1,2,5)] # IslandOrArch, study, Island_Arc
# isl.pts@data$ptID <- 30001:(30000 + dim(isl.pts@data)[1])
# isl.polys@data <- isl.polys@data[,c(8,11)]# island (name)
# isl.polys@data$uniqueID <- 10001:(10000 + dim(isl.polys@data)[1])
#
# isl.joinPts <- over(isl.pts, isl.polys)
# isl.joinPts <- spCbind(isl.pts, isl.joinPts)
#
# isl.uniquePolys <- unique(isl.joinPts@data$uniqueID) # number of polygons from island.shp that had points from islands that hit them
# isl.uniquePts <- unique(isl.joinPts@data$ptID)
#
#
# # Now find out which are the polygons that don't have points fall within them
# isl.polysNoPts <- isl.polys@data$uniqueID[!(isl.polys@data$uniqueID %in% isl.uniquePolys)] # Which of the islands in isl.polys are not present in isl.uniquePolys
# isl.polysNoPts <- isl.polys[(isl.polysNoPts-10000),]
#
# foo <- isl.polys[(isl.polys@data$uniqueID %in% isl.uniquePolys),]
#
# isl.moreThan1Pt <- isl.joinPts@data$uniqueID[duplicated(isl.joinPts@data$uniqueID)]
# isl.moreThan1Pt <- isl.polys[(isl.moreThan1Pt-10000),]
#
#
#
# #################################################
#
# writeOGR(obj=masterIslands_v3, dsn="C:/Users/tuh09869/Google Drive/Global Islands/shapefiles", layer="masterIslands_v3", driver="ESRI Shapefile")
# writeOGR(obj=isl.pts, dsn="C:/Users/tuh09869/Google Drive/Global Islands/qualityCheck", layer="isl.pts", driver="ESRI Shapefile")
# writeOGR(obj=arch.polys, dsn="C:/Users/tuh09869/Google Drive/Global Islands/qualityCheck", layer="arch.polys", driver="ESRI Shapefile")
# writeOGR(obj=polysNoPts, dsn="C:/Users/tuh09869/Google Drive/Global Islands/qualityCheck", layer="polysNoPts", driver="ESRI Shapefile")
# writeOGR(obj=moreThan1Pt, dsn="C:/Users/tuh09869/Google Drive/Global Islands/qualityCheck", layer="moreThan1Pt", driver="ESRI Shapefile")
# writeOGR(obj=joinPts, dsn="C:/Users/tuh09869/Google Drive/Global Islands/qualityCheck", layer="joinPts", driver="ESRI Shapefile")


#################################################

