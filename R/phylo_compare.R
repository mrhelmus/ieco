#' @name phylo_compare
#' @title Compare the structure of phylogenies
#' @description These functions are wrappers of existing functions that makes it easy to compare and then summarize the toplogy, structure and ages of phylogenies.
#' @description \code{compare_multiPhylo} 
#' @param multitree list of phylogenies of class \code{multiPhylo}
#' @param which.phylo index location of the phylogeny with multitree to compare, if NULL then all trees are compared to each other
#' @param ... further arguments to be passed to \code{\link[ape]{all.equal.phylo}}
#' @return a data.frame of comparisons
#' @author Matthew R. Helmus inspired by code from Andrew L. Hipp in \code{\link[RADami]{compare.all.trees}}
# @examples None None num<
#' @seealso \code{\link[RADami]{compare.all.trees}} \code{\link[ape]{all.equal.phylo}}
# @references None None
#' @importFrom ape all.equal.phylo

#' @rdname phylo_compare
#' @export
#multitree<-jetz.pass
#x.phylo<-jetz.pass[[1]] #Z out
#focal<-jetz.pass[[2]] #S out
#focal<-jetz.pass[[5]] #Z out
#which.phylo=10

#which.phylo=NULL

compare_multiPhylo <- function(multitree, which.phylo=1, ...) {
  #TODO: change which.phylo to use names as well as index numbers
    n = length(multitree)
    if(is.null(names(multitree))) {names(multitree)<-(1:n)}
    
    #cpme<-function(x.phylo) {all.equal.phylo(x,focal)} #function to apply across phylogenies
    
    if(is.null(which.phylo))
    { #TODO: edit this to make it go faster with lapply on the cpme
      out = matrix(NA, n, n)
      for(i in 1:n) {
        for(j in 1:i) {
          out[i, j] <- all.equal.phylo(multitree[[i]], multitree[[j]], ...)#use.edge.length = FALSE)
        }
      }
      colnames(out)<-rownames(out)<-names(multitree)
      out.f<-data.frame(tree.1.ind=rep(1:n,each=n),
                        tree.2.ind=1:n,
                        tree.1=rep(colnames(out),each=n),
                        tree.2=rownames(out),
                        same=matrix(out,byrow = FALSE))
      #dim(out.f)
      out.f<-out.f[!is.na(out.f$same),]
      out.f<-out.f[out.f$tree.1!=out.f$tree.2,]
      #dim(out.f)
      return(out.f)
    } else {
      #make sure that which.phylo is within the multitree object
      if(length(intersect(which.phylo,(1:n)))!=1){stop("which.phylo incorrect!")} 
      
      focal<-multitree[[which.phylo]]
      out = matrix(NA, nrow=n)
      for(i in 1:n) {
          out[i,] <- all.equal.phylo(focal, multitree[[i]],...)# use.edge.length = FALSE)
      }
      rownames(out)<-names(multitree)
      #colnames(out)<-names(multitree)[which.phylo]
      out.f<-data.frame(tree.1.ind=which.phylo,
                        tree.2.ind=1:n,
                        tree.1=names(multitree)[which.phylo],
                        tree.2=rownames(out),
                        same=matrix(out,byrow = FALSE),stringsAsFactors = FALSE)
      #dim(out.f)
      out.f<-out.f[!is.na(out.f$same),]
      out.f<-out.f[out.f$tree.1!=out.f$tree.2,]
      #dim(out.f)
      return(out.f)
    }
}

#out.h<-compare_multiPhylo(multitree,which.phylo=NULL,use.edge.length=FALSE)
#head(out.h)
#out.h<-compare_multiPhylo(multitree,which.phylo=2,use.edge.length=FALSE)
#head(out.h)
