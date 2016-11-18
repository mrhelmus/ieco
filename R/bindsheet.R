#' General functions for googlesheet data manipulation
#' @name sheet.data
#' @description concatenates sheets of similar data 
#' @param title character object of the title of the sheet to load
#' @param sheets character vector of sheet titles to bind
#' @details bindsheet.morph assumes that sheets to bind have the same columns
#' @return concatenated datatable
#' @author Matthew R. Helmus
# @examples None None
# @seealso None None
# @references None None
#' @rdname sheet.data
#' @export

readsheet.morph<-function(tle){return(gs_read(gs_title(tle,verbose=FALSE),verbose=FALSE))}


#' @rdname sheet.data
#' @export

bindsheet.morph<-function(sheets){
  xx<-NULL
  for(i in sheets){
    x<-readsheet.morph(i)
    xx<-rbind(xx,x[,1:grep("SEX",colnames(x))])
  }
  return(xx)
}


