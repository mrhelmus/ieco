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

polyXnode<-function(tree,tips,where=NULL,edge.length=NULL,position=0){
  emat<-tree$edge
efoc<-which.edge(tree, where.tip)
foctip<-match(where.tip,tree$tip)
elen<-tree$edge.length
nedg<-elen[efoc]/2

txt<-NULL
for(addtip in tips){txt<-paste(txt,addtip,":",nedg,",",sep="")}
txt<-substr(txt,1,(nchar(txt)-1))
txt<-paste("(",txt,");",sep="")
add<-read.tree(text=txt)

nieuw<-bind.tree(tree,add,where=foctip,position=nedg)
return(nieuw)
}


