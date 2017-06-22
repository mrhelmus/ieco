#' @name tip_grafts
#' @title Grafts tips on phylogenies
#' @description Places tips where you want a tip to go on a phylogeny. These functions are good for adding missing taxa in ecophylogenetic analyses when you have a good phylogeny, and only need to graft a few missing taxa with a good hypothesis as to where they should go.
#' \code{tipXtip} adds a tip halfway the distance between a supplied tip and its MRCA. 
#' @param tree phylgeny of class \code{\link[ape]{phylo}}
#' @param addtip name of the tip to add
#' @param where.tip name of the tip 
#' @param where.nodes vector of two node numbers to insert the tip between 
#' @return a \code{\link[ape]{phylo}} with the grafted tip
#' @author Matthew R. Helmus
# @examples None None num<
#' @seealso \code{\link[phytools]{bind.tip}} \code{\link[ape]{bind.tree}}
# @references None None
#' @importFrom geiger tips 
#' @importFrom ape read.tree bind.tree which.edge dist.nodes

#' @rdname tip_grafts
#' @export


#Grafts a tip on a tree halfway the distance between a supplied tip and its node

tipXtip<-function(tree,addtip,where.tip=NULL){
  emat<-tree$edge
  efoc<-which.edge(tree, where.tip)
  foctip<-match(where.tip,tree$tip)
  elen<-tree$edge.length
  nedg<-elen[efoc]/2
  txt<-paste("(",addtip,":",nedg,");",sep="")
  add<-read.tree(text=txt)
  nieuw<-bind.tree(tree,add,where=foctip,position=nedg)
  return(nieuw)
}

#' @rdname tip_grafts
#' @export

tipXnode<-function(tree,addtip,where.nodes=NULL){
  emat<-tree$edge
  elen<-tree$edge.length[apply(emat,1,paste,collapse="")==paste(where.nodes,collapse="")]
  nedg<-elen/2
  tps<-tips(tree,where.nodes[2])
  ind<-sapply(tps,match,tree$tip)
  kl<-dist.nodes(tree)[where.nodes[2],ind]
  nedgadd<-mean(kl)+nedg
  txt<-paste("(",addtip,":",nedgadd,");",sep="")
  add<-read.tree(text=txt)
  nieuw<-bind.tree(tree,add,where=where.nodes[2],position=nedg)
  return(nieuw)
}