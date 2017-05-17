#' Count how many pts per poly for economic isolation
#'
#' This function counts how many points per island.
#' @param pts SpatialPointsDataFrame of whatever data you want to count per island (ex port shapefile to see how many ports per island).
#' @param polys SpatialPolygonsDataFrame of islands. polys@data must have a column 'uniqueID'
#' @return Returns a dataframe of how many points per polygon
#' @author Tyler J Tran, \email{tylerjtran@@gmail.com}
#' @export
#' @import rgdal
#' @import rgeos
#'


vectorIsland <- function(pts, polys){
  joined <- over(pts, polys)
  joined <- table(joined$uniqueID)
  joined <- as.data.frame(joined)
  return(joined)
}
