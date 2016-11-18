#' General functions for googlesheet data manipulation
#' @name bindsheet
#' @description concatenates sheets of similar data 
#' @param sheets character vector of sheet titles to bind
#' @details bindsheet.morph assumes that sheets to bind have the same columns
#' @return concatenated datatable
#' @author Matthew R. Helmus
# @examples None None
# @seealso None None
# @references None None
#' @rdname bindsheet.morph
#' @export


bindsheet.morph<-function(sheets){
  xx<-NULL
  for(i in sheets){
    x<-readsheet.morph(i)
    xx<-rbind(xx,x[,1:grep("SEX",colnames(x))])
  }
  return(xx)
}

#' @rdname readsheet.morph
#' @export

readsheet.morph<-function(tle){return(gs_read(gs_title(tle,verbose=FALSE),verbose=FALSE))}

