## Correlation matrix with p-values. See http://goo.gl/nahmV for documentation of this function
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

## Use this to dump the cor.prob output to a 4 column matrix
## with row/column indices, correlation, and p-value.
## See StackOverflow question: http://goo.gl/fCUcQ
flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.") 
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}

#get genera from a phylo object
genus<-function(tree,split="_",ncol=2){matrix(unlist(strsplit(tree$tip.label,split = split)),ncol=ncol,byrow = TRUE)[,1]}


library(ape)
library(picante)
library(geiger)
phy<-read.tree("mytree.txt")
node.age(phy)->phy.age
cbind(phy.age$edge,phy.age$age, phy$edge.length)->BL.position
max(phy.age$age)-BL.position[,3]->dist.tip
cbind(BL.position,dist.tip)->BL.positions
BL.positions[,5]+BL.positions[,4]->ages
cbind(BL.positions,ages)->BL.positions
as.data.frame(BL.positions)->node.ages
names(node.ages)<-c("parental.node","daughter.node","dist.root","BL","dist.
tip","mrca.age")
## node.ages is a data frame listing as variables the identity of parental and
#daughter nodes, the distance from the root and from the present of each node,
#the branch lenght and the age of the most recent common ancestor
node.ages[node.ages[,2]<length(phy$tip)+1,]->species.ages
row.names(species.ages)<-phy$tip
## species ages is node.ages data frame reduced to the tips (species)
