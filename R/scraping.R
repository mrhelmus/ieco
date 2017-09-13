#' Calculate distribution of grades
#' @name web_scrape
#' @description various functions to scrape data from the web
#' @param s a search term

#' @details Number of approximate google search hits
#' @return numeric 
#' @author Drew Conway, with edits by M.R. Helmus, https://gist.github.com/drewconway/791559
#' @examples 
#' google.counts("iecolab")

#' @importFrom RCurl getURL
#' @importFrom XML htmlTreeParse getNodeSet xmlValue
#' @rdname scrape
#' @export

google.counts<-function(s){
  require(RCurl)
  require(XML)
  
  search.url<-paste("http://www.google.com/search?q=",gsub(" ","+",s),sep="")
  search.html<-getURL(search.url)
  parse.search<-htmlTreeParse(search.html,useInternalNodes = TRUE)
  search.nodes<-getNodeSet(parse.search,"//div[@id='resultStats']")
  search.value<-strsplit(xmlValue(search.nodes[[1]])," ",fixed=TRUE)[[1]][2]
  return(as.numeric(gsub(",","",search.value,fixed=TRUE)))
}