#' Clean and organize data from phylogenetic trees
#' @name phylo_parse
#' @description functions to parse tip data and make matrices of data that can be inserted into our postSQLgis database
#' @param a a character vector of a strsplit tip.label
#' @details returns a matrix with species, specimen id, and location for each tip on a tree
#' @return matricies of the parsed and cleaned data or phlo objects with cleaned tips
#' @author Matthew R. Helmus
# @examples None None num<
# @seealso None None
# @references None None
#' @rdname phylo_parse
#' @export
#' @importFrom dplyr mutate select filter


tip_parse<-function(a) {
  hold<-rep("missing!",3)
  ind<-suppressWarnings(!is.na(sapply(a,as.numeric)))
  if(length(id)==1 & !is.na(id))id<-(1:length(a))[ind]
  if(id) hold[2]<-a[id]
  if(id>1){hold[1]<-paste(a[1:(id-1)],collapse = " ")}
  if(length(a)>2){hold[3]<-paste(a[(id+1):length(a)],collapse = " ")}
  return(hold)
}