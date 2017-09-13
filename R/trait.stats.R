#' Statistics to summarize Global Traits
#' @name trat.stat
#' @description Means, ranges etc. for each individual specimens measured.
#' @param num specimen id number
#' @param x Dataset to be parsed
#' @param ids Vector of specimen id numbers
#' @details The spp functions build on the specimen functions which build on the trait functions. If you just want a data set of traits aggregated to species, use the spp functions
#' @return the value of the identified statistic
#' @author Matthew R. Helmus
# @examples None None
# @seealso None None
# @references None None
#' @rdname trait.stat
#' @export
#' @importFrom dplyr mutate
#' @importFrom stats aggregate
#' 
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

specimen.n<-function(x,ids){
  dd<-sapply(ids,trait.n,x=x)
  colnames(dd)<-ids
  return(dd)
}

#' @rdname trait.stat
#' @export

specimen.sd<-function(x,ids){
  dd<-sapply(ids,trait.sd,x=x)
  colnames(dd)<-ids
  return(dd)
}

#' @rdname trait.stat
#' @export

specimen.range<-function(x,ids){
  dd<-sapply(ids,trait.range,x=x)
  colnames(dd)<-ids
  return(dd)
}


#' @rdname trait.stat
#' @export

spp.mean<-function(x,ids){
  xs<-x[x$specimen.id.number %in% ids,]
  k<-specimen.mean(xs,ids)
  gad<-unique(cbind(xs$species,xs$specimen.id.number))
  match(gad[,2],ids)
  hold<-matrix(NA,dim(gad)[1],dim(k)[1])
  rownames(hold)<-gad[,2]
  colnames(hold)<-rownames(k)
  ind<-match(colnames(k),rownames(hold))
  hold[ind,]<-t(k)
  rt<-aggregate(hold,by=list(species=gad[,1]),FUN=mean,na.rm=TRUE)
  return(rt)
}

#' @rdname trait.stat
#' @export

spp.n<-function(x,ids){
  xs<-x[x$specimen.id.number %in% ids,]
  k<-specimen.n(xs,ids)
  gad<-unique(cbind(xs$species,xs$specimen.id.number))
  match(gad[,2],ids)
  hold<-matrix(NA,dim(gad)[1],dim(k)[1])
  rownames(hold)<-gad[,2]
  colnames(hold)<-rownames(k)
  ind<-match(colnames(k),rownames(hold))
  hold[ind,]<-t(k)
  rt<-aggregate(hold,by=list(species=gad[,1]),FUN=mean,na.rm=TRUE)
  return(rt)
}

#' @rdname trait.stat
#' @export

spp.sd<-function(x,ids){
  xs<-x[x$specimen.id.number %in% ids,]
  k<-specimen.sd(xs,ids)
  gad<-unique(cbind(xs$species,xs$specimen.id.number))
  match(gad[,2],ids)
  hold<-matrix(NA,dim(gad)[1],dim(k)[1])
  rownames(hold)<-gad[,2]
  colnames(hold)<-rownames(k)
  ind<-match(colnames(k),rownames(hold))
  hold[ind,]<-t(k)
  rt<-aggregate(hold,by=list(species=gad[,1]),FUN=mean,na.rm=TRUE)
  return(rt)
}

#' @rdname trait.stat
#' @export

spp.range<-function(x,ids){
  xs<-x[x$specimen.id.number %in% ids,]
  k<-specimen.range(xs,ids)
  gad<-unique(cbind(xs$species,xs$specimen.id.number))
  match(gad[,2],ids)
  hold<-matrix(NA,dim(gad)[1],dim(k)[1])
  rownames(hold)<-gad[,2]
  colnames(hold)<-rownames(k)
  ind<-match(colnames(k),rownames(hold))
  hold[ind,]<-t(k)
  rt<-aggregate(hold,by=list(species=gad[,1]),FUN=mean,na.rm=TRUE)
  return(rt)
}
