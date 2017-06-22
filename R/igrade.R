#' Calculate distribution of grades
#' @name igrade
#' @description Calculate the grades of a class of students, given raw scores on exam

#' @param grades Raw grades
#' @param divs divisions for grades (optional)
#' @param cut low end Cut off to remove 0 from statistics
#' @param tit Title for Figure
#' @param breaks breaks for the histogram, default=length(grades)/3
#' @param labels grade labels, default are the U.S.A. system of five letter grades, ABCDF 
# @param linear default assignment of subgrades by interpolation
#' @param full.divs divisions for all grades not just main grades as in div
#' @param full.labels names of all the grades
#' @param noAplus remove the first (A+) listed grade and assign the second (A) 
#' @param \dots other parameters for hist

#' @details To remove students who do not take the test a low end cut off is used to excise any grades below that level.  Both mean, and standard deviations are shown as well as median and quartiles.
#' @return the data set 
#' @author Jonathan M. Lees from the ProfessR package, with edits by M.R. Helmus
#' @examples 
#' g = rnorm(n=130, m=82, sd=10)
#' g[g>100] = 100
#' g[g<1] = 1
#' 
#' B = boxplot(g)

#' ##########   set divisions automatically:

#' divs = c(min(g), B$stats[1:4] + diff(B$stats)/2, max(g) )

#' D1 = do.grades.ieco(g, divs=divs, tit="GEOL 105 Exam 1")
# @seealso None None
# @references None None
#' @importFrom graphics hist points axis boxplot abline box locator mtext par text
#' @importFrom stats var
#' @importFrom grDevices rgb
#' @rdname igrade
#' @export

do.grades.ieco <-
function(grades, divs=NULL, cut=0, tit="Exam Grades", breaks=length(grades)/3, labels=c("F","D", "C", "B", "A"), ...)
{
  if(missing(divs)) { divs=NULL }
  if(missing(cut)) { cut=NULL }
  if(missing(tit)) { tit = "Exam Grades" }
  if(missing(breaks)) { breaks=length(grades)/3 }

  if(!is.null(cut))
    {
      agrades = grades[grades>cut]
    }
  else
    {
      agrades = grades
    }
  
  BIGN = length(grades)
  
  HA = hist(agrades,  main=tit, xlab="Scores", breaks=breaks, ...)

  points(agrades, rep(0, length(agrades)), col='purple')

  
   axis(1, at=seq(from=10*round(min(agrades[agrades>0])/10), to=max(agrades), by=10), labels=TRUE)
   axis(1, at=seq(from=10*round(min(agrades[agrades>0])/10), to=max(agrades), by=2), labels=FALSE)

  m = mean(agrades)
  s = sqrt(var(agrades))

  B = boxplot(agrades,  plot = FALSE)
  Bdivs = c(min(agrades), B$stats[1:4] + diff(B$stats)/2, max(agrades) )

  mstats = c( m-2*s,m-s, m,  m+s, m+2*s)
  abline(v=mstats, lty=2, col=rgb(1, .7, .7) )
  mtext(text="mean", side=3, at=m, line=0)

  abline(v=Bdivs, lty=2, col=rgb(.1, .7, .7) )
  
  u = par("usr")
###	print(u)
  if(is.null(divs))
    {	
      text( c(20, (m-2*s+m-s)/2,(m-s+ m)/2, (m+m+s)/2, (m+s+m+2*s)/2 ), rep(u[4], 5),
           labels=labels, pos=1 , col=rgb(1, .7, .7)  )
    }
  box()
  if(is.null(divs))
    {
      mtext(text="Click 4 divisions from LOW to HIGH", side=3, at = u[1], line=2, adj=0)
      K = list(x=NULL, y=NULL)
      
      for(LK in 1:4)
        {
          K1 = locator(type='p', col='blue', n=1)
          abline(v=K1$x, col='blue', lwd=2)
          K = list(x=c(K$x, K1$x) , y=c(K$y, K1$y))
        }

       divs =  c(min(grades), K$x, max(grades))
    }

  divs = sort(divs)

  abline(v=divs[2:(length(divs)-1)], col=rgb(0,0,1) )	

  ddivs = diff(divs)
  xgrad = divs[1:(length(divs)-1)] + ddivs/2

  xmin = HA$breaks[1]+(divs[2]-HA$breaks[1])/2
  xgrad[1]= xmin
  text(xgrad , rep(u[4], 5), labels=labels, pos=1   )
  
##### print(divs) 
  divs = sort(divs)

  cat("Grade divisions:", sep="\n")
  cat(divs, sep="\n")

#####  divisions are determined, now allocate grades.
###  this can be run independent of the divs determination

  KAPPA = getlet.ieco(grades, divs)	
###grades, lett=letts, scor=scores, divs=divs, LETS=LETS, SCRS=SCRS
  LETS = KAPPA$LETS
  letts = KAPPA$lett
  scores = KAPPA$scor	
  SCRS = KAPPA$SCRS
  cat("Letter Grade Distribution:", sep="\n")
  for(i in 1:length(LETS))
    {
      cat(paste(sep=' ', i, LETS[i], length(letts[letts==LETS[i]])), sep="\n")
    }

 # cat("Numeric Grade Distribution:", sep="\n")
 #  for(i in 1:length(SCRS))
 #   {
 #     cat(paste(sep=' ', i, SCRS[i], length(scores[scores==SCRS[i]])), sep="\n")
 #   }

  print(paste(sep=' ', "Mean Score=",mean(scores)))

  return(list(grades=grades, lett=letts, scor=scores, divs=divs, LETS=LETS, SCRS=SCRS, hist=HA, mstats=mstats, bstats=B$stats,  Bdivs=Bdivs))
}



########################################


#' @rdname igrade
#' @export
getlet.ieco <-
  function(grades, divs, full.divs=c(0,60,63,67,70,73,77,80,83,87,90,93,97),
           full.labels=c("F","D-","D","D+","C-","C","C+","B-","B","B+","A-","A","A+"), noAplus=TRUE)
  {
    SCRS = seq(from=100, by=(-4), length=length(full.labels))
    LETS = full.labels
    J<-c(full.divs)
    
    Jinval = findInterval(grades, J, all.inside = TRUE)
    
    rLETS = LETS
    rSCRS = rev(SCRS)
    
    JLET = rLETS[Jinval]
    
    scores = rSCRS[Jinval]+ 4*(grades-J[Jinval])/(J[Jinval+1]-J[Jinval])
    
    letts = JLET
    
    scores[scores>=100] = 100
    scores
    if(!noAplus) letts[scores>=100]=LETS[1]
    #if(noAplus) letts[letts=="A+"]<-"A"
    ############################
    ######################################
    
    return(list(grades=grades, lett=letts, scor=scores,divs=divs, LETS=LETS, SCRS=SCRS))
  }


