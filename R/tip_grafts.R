#' @name tip_grafts
#' @title Grafts tips on phylogenies
#' @description Places tips and clades where you want on a phylogeny. These functions are good for adding missing taxa in ecophylogenetic analyses when you have a good phylogeny, and only need to graft a few missing taxa with a good hypothesis as to where they should go.
#' @description \code{tipXtip} adds a tip halfway the distance between a supplied tip and its MRCA. 
#' @description \code{tipXnode} adds a tip halfway the distance between a supplied set of nodes that define one edge 
#' @description \code{treeXnode} adds a clade halfway the distance between a supplied set of nodes that define one edge 
#' @param tree phylgeny of class \code{\link[ape]{phylo}}, this is the starting tree
#' @param addtip name of the tip to add
#' @param addtree phylgeny of class \code{\link[ape]{phylo}} to add
#' @param where.tip name of the tip 
#' @param where.nodes vector of two node numbers to insert the tip between 
#' @param tip.out tip label of the outgroup tip that defines the stem of the clade to be grafted
#' @return a \code{\link[ape]{phylo}} with the grafted tip
#' @author Matthew R. Helmus
# @examples None None num<
#' @seealso \code{\link[phytools]{bind.tip}} \code{\link[ape]{bind.tree}}
# @references None None
#' @importFrom geiger tips 
#' @importFrom ape read.tree bind.tree which.edge dist.nodes branching.times drop.tip ladderize Ntip

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

#' @rdname tip_grafts
#' @export

treeXnode<-function(tree,addtree,where.nodes=NULL,tip.out="tip.out"){
  addtree.<-addtree.o<-addtree
  if(!any(addtree$tip.label==tip.out)){stop("must supply a tree with a tip labled as in tip.out")}
  if(Ntip(addtree)<3){stop("must supply tree with at least three tips one labeld as supplied in tip.out that will be dropped. For one species additions, use tipXnode")}
  emat<-tree$edge
  elen<-tree$edge.length[apply(emat,1,paste,collapse="")==paste(where.nodes,collapse="")]
  nedg<-elen/2
  tps<-tips(tree,where.nodes[2])
  ind<-sapply(tps,match,tree$tip)
  kl<-dist.nodes(tree)[where.nodes[2],ind]
  nedgadd<-mean(kl)+nedg
  
  #addtree.<-ladderize(addtree, right = FALSE)
  stm<-c(1+Ntip(addtree),2+Ntip(addtree))
  stm.ind<-which(apply(addtree.$edge, 1, function(x) all(x == stm)))
  addtree.$edge.length[stm.ind]<-nedgadd-sort(branching.times(addtree.),decreasing = TRUE)[2]
  nieuw<-bind.tree(tree,addtree.,where=where.nodes[2],position=nedg)
  nieuw<-drop.tip(nieuw,tip.out)
  return(reorder(nieuw,"pruningwise"))
}