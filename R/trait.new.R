#' Add and edit traits to a global traits data set
#' @name new_trait
#' @description combine or regress individual traits 
#' @param x Dataset that traits should be added
#' @param num specimen number
#' @param newnumber Number of the new trait
#' @param newtrait Name of the trait that you want to add
#' @param ids vector of specimen ids
#' @param counts unique of the counts column, vector of the unique id for each time a specimen was measured
#' @param these trait numbers to correct for svl
#' @details for an individual measured specimen these functions combine traits
#' @return the data set with the appended traits
#' @author Matthew R. Helmus
# @examples None None num<
# @seealso None None
# @references None None
#' @rdname trait.new
#' @export
#' @importFrom dplyr mutate select filter %>%

total_tail<-function(num,x,newnumber=9.1,newtrait="tail total length"){
  send=NULL
  hold<-filter(x,specimen.id.number==num)
  condition<-hold %>% filter(trait.number==8.0) %>% select(value)
  if(any(condition==3)){
    counts<-unique(hold$count)[condition==3]
    T2<-hold %>% filter(count %in% counts,trait.number %in% 10.0) %>% select(value)
    Tt<-hold %>% filter(count %in% counts,trait.number %in% 9.0) %>% select(value)
    if(dim(T2)[1]==0){T2<-tibble(value=0)}
    if(any(is.na(T2))) {T2[is.na(T2),]<-0}
    Tt<-as.numeric(Tt$value)+as.numeric(T2$value)
    send<- hold %>% 
      filter(count %in% counts,trait.number==9.0) %>% 
      mutate(value=replace(value, trait.number==9.0, Tt)) %>%
      mutate(trait.number=replace(trait.number, trait.number==9.0, newnumber)) %>%
      mutate(trait=replace(trait, trait.number==newnumber, newtrait)) %>%
      mutate(FLAG=replace(FLAG, trait.number==newnumber, 3))
  }
  if(any(condition==2)){
    counts<-unique(hold$count)[condition==2]
    T2<-hold %>% filter(count %in% counts,trait.number %in% 10.0) %>% select(value)
    Tt<-hold %>% filter(count %in% counts,trait.number %in% 9.0) %>% select(value)
    if(dim(T2)[1]==0){T2<-tibble(value=0)}
    if(any(is.na(T2))) {T2[is.na(T2),]<-0}
    Tt<-as.numeric(Tt$value)+as.numeric(T2$value)
    send<- hold %>% 
      filter(count %in% counts,trait.number==9.0) %>% 
      mutate(value=replace(value, trait.number==9.0, Tt)) %>%
      mutate(trait.number=replace(trait.number, trait.number==9.0, newnumber)) %>%
      mutate(trait=replace(trait, trait.number==newnumber, newtrait)) %>%
      mutate(FLAG=replace(FLAG, trait.number==newnumber, 2)) %>% rbind(send)
  }
  if(any(condition==1)){
    counts<-unique(hold$count)[condition==1]
    T2<-hold %>% filter(count %in% counts,trait.number %in% 10.0) %>% select(value)
    Tt<-hold %>% filter(count %in% counts,trait.number %in% 9.0) %>% select(value)
    if(dim(T2)[1]==0){T2<-tibble(value=0)}
    if(any(is.na(T2))) {T2[is.na(T2),]<-0}
    Tt<-as.numeric(Tt$value)+as.numeric(T2$value)
    send<- hold %>% 
      filter(count %in% counts,trait.number==9.0) %>% 
      mutate(value=replace(value, trait.number==9.0, Tt)) %>%
      mutate(trait.number=replace(trait.number, trait.number==9.0, newnumber)) %>%
      mutate(trait=replace(trait, trait.number==newnumber, newtrait)) %>%
      mutate(FLAG=replace(FLAG, trait.number==newnumber, 1))  %>% rbind(send)
  }
  return(send)
}

#' @rdname trait.new
#' @export

total_tails<-function(ids,x,newnumber=9.1,newtrait="tail total length"){
  gone<-NULL
  for(j in ids){
   gone<-rbind(gone,total_tail(j,x,newnumber,newtrait)) 
  }
  return(gone)
}

#' @rdname trait.new
#' @export

svl_trait<-function(x,counts=NULL,these=c(2,3,4,5,6,7,9,10,11,12,13,14,15,15.2,
                                           16.2,17,18,19.3,19.4,20,21,22.2,23,24,25.3,25.4,26,27))
{
  if(is.null(counts)) counts<-unique(x$count)
  xx<-x[x$count %in% counts,]
  lik<-NULL
  for(k in counts){
    hold<-xx[xx$count %in% k,]
    svl.<-as.numeric(hold$value[hold$trait.number==1])
    ind<-hold$trait.number %in% these
    trt<-as.numeric(hold$value)/svl.
    hold$value[ind]<-trt[ind]
    #hold$value[hold$trait.number==1]<-svl.
    lik<-rbind(lik,hold)
  }
  return(lik)
}




