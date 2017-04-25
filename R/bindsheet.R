#' General functions for googlesheet data manipulation
#' @name sheet.data
#' @description concatenates sheets of similar data 
#' @param tle character object of the title of the sheet to load
#' @param sheets character vector of sheet titles to bind
#' @param fnadd add the title of the sheets as a column in the bind
#' @details bindsheet.morph assumes that sheets to bind have the same columns and is only for herp morphology datasheets
#' @return concatenated datatable
#' @author Matthew R. Helmus
# @examples None None 
# @seealso None None
# @references None None
#' @rdname sheet.data
#' @export
#' @import googlesheets
#' @import tibble

readsheet<-function(tle){return(gs_read(gs_title(tle,verbose=FALSE),verbose=FALSE))}


#' @rdname sheet.data
#' @export

bindsheet.morph<-function(sheets, fnadd=TRUE)
{
  xx<-NULL
  
  if(fnadd){
    for(i in sheets){
      x<-readsheet(i)
      x<-cbind(i,x)
      xx<-rbind(xx,x[,1:grep("SEX",colnames(x))])
    }
    return(as_tibble(xx))
    
  } else {
    
    for(i in sheets){
      x<-readsheet(i)
      xx<-rbind(xx,x[,1:grep("SEX",colnames(x))])
    }
    return(as_tibble(xx))
  }
}


