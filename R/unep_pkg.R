#################################################################################
#
# Calculating UNEP isolation index for islands
# TJT, April 2016
#
# See Weigelt and Kreft 2013
#
#################################################################################


# rm(list=ls())

# require(rgdal); require(rgeos)

# nearestLarger <- read.csv('C:/Users/tuh09869/Google Drive/Geographic_Isolation/geoDistances/dist_nearestLarger.csv')
# nearestArch <- read.csv('C:/Users/tuh09869/Google Drive/Geographic_Isolation/geoDistances/dist_nearestArch.csv')
# nearestContinent <- read.csv('C:/Users/tuh09869/Google Drive/Geographic_Isolation/geoDistances/dist_nearestLarger.csv')

#' UNEP Isolation Index
#'
#' This function calculates the UNEP isolation index for islands
#' @param nearestLarger Numeric object or vector indicating the distance between the island of interest and the nearest island of equivalent or larger size
#' @param nearestArch Numeric object or vector for distance to nearest archipelago or island group
#' @param nearestContinent Numeric object or vector for didstance to nearest continent
#' @author Tyler J Tran, \email{tylerjtran@@gmail.com}
#' @export
#'

unep <- function(nearestLarger, nearestArch, nearestContinent){
  unep <- sqrt(nearestLarger) + sqrt(nearestArch) + sqrt(nearestContinent)
  return(unep)
}


# unep <- sqrt(nearestLarger$nearestLarger) + sqrt(nearestArch$nearestArch) + sqrt(nearestContinent$nearestContinent)
# unep <- data.frame(uniqueID=nearestLarger$uniqueID, unep=unep)
#
# write.csv(unep, 'C:/Users/tuh09869/Google Drive/Geographic_Isolation/geoDistances/unepIsolation.csv', row.names = F)

