#' Statistics to summarize Global Traits
#' @name 
#' @description basic checking functions for morphology data to see if ids match spp, misspellings etc.
#' @param x Dataset to be parsed
#' @param ids specimen id numbers that you want to check otherwise all in x
# @details NONE
#' @return the value of the identified statistic
#' @author Matthew R. Helmus
# @examples None None
# @seealso None None
# @references None None
#' @rdname trait.check
#' @export


dp.spp<-function(x,ids=NULL){
  if(!is.null(ids))
  {
    xx<-x[!is.na(match(x$specimen.id.number, ids)),]
  } else {
    xx<-x
  }
  return(rowSums(table(xx$specimen.id.number,xx$species)>0))
}

