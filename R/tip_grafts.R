#' @name tip_grafts
#' @title Grafts tips on phylogenies
#' @description Places tips where you want a tip to go on a phylogeny. These functions are good for adding missing taxa in ecophylogenetic analyses when you have a good phylogeny, and only need to graft a few missing taxa with a good hypothesis as to where they should go.
#' \code{tipXtip} adds a tip halfway the distance between a supplied tip and its MRCA. 
#' @param tree phylgeny of class \code{\link[ape]{phylo}}
#' @param addtip name of the tip to add
#' @param where.tip name of the tip 
#' @return a \code{\link[ape]{phylo}} with the grafted tip
#' @author Matthew R. Helmus
# @examples None None num<
#' @seealso \code{\link[phytools]{bind.tip}} \code{\link[ape]{bind.tree}}
# @references None None
# @importFrom phytools 
#' @importFrom ape read.tree bind.tree which.edge

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