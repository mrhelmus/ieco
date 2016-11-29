#' Statistics to summarize Global Traits
#' @name trat.stat
#' @description Means, ranges etc. for each individual specimens measured.
#' @param num specimen id number
#' @param x Dataset to be parsed
#' @details best when used with apply across a species
#' @return the value of the identified statistic
#' @author Matthew R. Helmus
# @examples None None
# @seealso None None
# @references None None
#' @rdname trait.stat
#' @export


trait.mean<-function(num,x){
  out<-unique(x$trait.number)
  names(out)<-out
  out<-replace(out,1:length(out),NA)
  hold<-x[x$specimen.id.number==num,]
  it0<-aggregate(as.numeric(hold$value),list(hold$trait.number),mean,na.rm=TRUE,simplify=TRUE)
  out2<-it0[,2]
  names(out2)<-it0[,1]
  out[names(out2)]<-out2
  out<-out[order(as.numeric(names(out)))]
  return(out)
}

#' @rdname trait.stat
#' @export

trait.n<-function(num,x){
  out<-unique(x$trait.number)
  names(out)<-out
  out<-replace(out,1:length(out),NA)
  hold<-x[x$specimen.id.number==num,]
  out2<-table(hold$trait.number,useNA="no")
  out[names(out2)]<-out2
  out<-out[order(as.numeric(names(out)))]
  return(out)
}

#' @rdname trait.stat
#' @export

trait.sd<-function(num,x){
  out<-unique(x$trait.number)
  names(out)<-out
  out<-replace(out,1:length(out),NA)
  hold<-x[x$specimen.id.number==num,]
  it0<-aggregate(as.numeric(hold$value),list(hold$trait.number),sd,na.rm=TRUE,simplify=TRUE)
  out2<-it0[,2]
  names(out2)<-it0[,1]
  out[names(out2)]<-out2
  out<-out[order(as.numeric(names(out)))]
  return(out)
}

#' @rdname trait.stat
#' @export

trait.range<-function(num,x){
  out<-unique(x$trait.number)
  names(out)<-out
  out<-replace(out,1:length(out),NA)
  hold<-x[x$specimen.id.number==num,]
  r.ange<-function(x){ diff(range(x,na.rm=TRUE)) }
  it0<-aggregate(as.numeric(hold$value),list(hold$trait.number),r.ange,simplify=TRUE)
  out2<-it0[,2]
  names(out2)<-it0[,1]
  out[names(out2)]<-out2
  out<-out[order(as.numeric(names(out)))]
  return(out)
}

#' @rdname trait.stat
#' @export

specimen.mean<-function(x,ids){
  dd<-sapply(ids,trait.mean,x=x)
  colnames(dd)<-ids
  return(dd)
}

#' @rdname trait.stat
#' @export

spp.mean<-function(x,ids,spp=NULL){
  k<-specimen.mean(x,ids)
  gad<-unique(cbind(x$species,x$specimen.id.number))
  match(gad[,2],ids)
  hold<-matrix(NA,dim(gad)[1],dim(k)[1])
  rownames(hold)<-gad[,2]
  colnames(hold)<-rownames(k)
  ind<-match(colnames(k),rownames(hold))
  hold[ind,]<-t(k)
  rt<-aggregate(hold,by=list(species=gad[,1]),FUN=mean,na.rm=TRUE)
  return(rt)
}


