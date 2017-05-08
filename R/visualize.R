#' General functions to visualize large data sets
#' @name visualize
#' @description general functions to visualize large data sets 
#' @param df a matrix or a data.frame. The values of variables (e.g., indices) to be compared are in columns. 
#' @param method \code{pearson}, \code{spearman} or \code{kendall}. This is the method to be used to produce the plot, according to \code{\link[stats:cor.test]{cor.test}}. See details.
#' @param digits Number of digits to round the correlation values on the plot.
#' @param na.action for controlling the treatment of NAs in \code{spearman} or \code{kendall} plots. If TRUE, missing values in the data are put last; if FALSE, they are put first; if NA, they are removed; if "keep" they are kept with rank NA. See \code{\link[base:rank]{rank}}.
#' @param ties.method \code{average}, \code{first}, \code{random}, \code{max}, or \code{min}; a character string specifying how ties are treated in \code{spearman} or \code{kendall} plots. See \code{\link[base:rank]{rank}} for details.
#' @param title Title of the plot.
#' @param xlab a character string for labelling x axes. \code{variable.name} (default value) will produce automatic labelling according to column names of \code{df}. Otherwise, either a single string can be entered, or a vector of strings of length equal to the number of columns of \code{df}.
#' @param ylab a character string for labelling y axes. \code{variable.name} (default value) will produce automatic labelling according to column names of \code{df}. Otherwise, either a single string can be entered, or a vector of strings of length equal to the number of columns of \code{df}.
#' @param \dots Further arguments to be passed to the individual plots. See \code{\link[graphics:plot]{plot}} and \code{\link[graphics:par]{par}}
#' @details   Produces a scatter plot with the distribution of points according to the tested variables in the lower left triangle and the correlation values in the upper left triangle. The lower half shows the scatter plots of values or ranks of variables. The upper half shows the corresponding correlation coefficients (significativity: 0 '***' 0.001 '**' 0.01 '*' 0.05 '-' 0.1 ' ' 1). The diagonal shows the considered variables and the number of individuals available for each. 
#' If the chosen method is \code{pearson}, then the actual values of the variables will be plotted. 
#' If the chosen method is a rank-based method, \code{spearman} or \code{kendall}, then the ranks will be plotted.
#' @note  A high number of variables will likely result in a slow generation of plots and a poor readability. Above 10 variables, the readability is greatly reduced.
#' @return plots and visualizations
#' @author Boris Leroy in the Rarity package
#' @examples   # Correlation between different variables measured on the same individuals
#' data(iris)
#' corPlot(iris[, 1:4], method = "pearson")
#' corPlot(iris[, 1:4], method = "spearman") 
#' @seealso \code{\link[arm:corrplot]{corrplot}} in package \code{arm}
# @references None None
#' @importFrom stats cor.test
#' @importFrom graphics par plot text mtext axis
#' @rdname visualize
#' @export


######################################
######    Correlation plots    #######
######################################
#Frome the Rarity Package

corPlot <- function(df, method = "spearman", digits = 2, na.action = "keep", ties.method = "average",
                    title = "", xlab = "variable.name", ylab = "variable.name", ...)
{
  rankDf <- apply(df, 2, .rankings, ties.method = ties.method, na.action = na.action)
  
  if(length(xlab) > 1) if(length(xlab) != (ncol(df))) stop("Provide as many labels for x axes as there are variables, in the same order as the column order.")
  if(length(ylab) > 1) if(length(ylab) != (ncol(df))) stop("Provide as many labels for y axes as there are variables, in the same order as the column order.")
  
  crossCongR <- NULL
  crossCongP <- NULL
  for (i in seq_len(ncol(rankDf)))
  {
    r <- NULL
    p <- NULL
    for (j in 1:ncol(df))
    {
      r <- c(r, cor.test(df[, i], df[, j], method = method)$estimate)
      p <- c(p, cor.test(df[, i], df[, j], method = method)$p.value)
    }
    crossCongR <- rbind(crossCongR, r)
    crossCongP <- rbind(crossCongP, p)
  }
  colnames(crossCongP) <- colnames(rankDf)[1:ncol(rankDf)]
  rownames(crossCongP) <- colnames(rankDf)
  colnames(crossCongR) <- colnames(rankDf)[1:ncol(rankDf)]
  rownames(crossCongR) <- colnames(rankDf)
  
  crossCongR <- round(crossCongR, 2)
  
  
  par(mfrow = c(ncol(df), ncol(df)), oma = c(4.1, 4.1, 0.6, 0.6), mar = c(.5, .5, .5, .5))
  
  for (i in 1:ncol(df))
  {
    for (j in 1:ncol(df))
    {
      if (j == i)
      {
        plot(1, 1, pch ="", bty = "o", xaxt = "n", yaxt = "n")
        text(x = 1, y = 1, paste(ifelse(is.null(colnames(df)),
                                        paste("Var", i, sep = "."),
                                        colnames(df)[i]),
                                 "\nn = ", length(which(!is.na(df[, i])))), 
             font = 2)
      } else
        if (j > i)
        {
          plot(1, 1, pch ="", xaxt = "n", yaxt = "n", bty = "n")
          text(x = 1, y = 1, paste(formatC(crossCongR[i, j], format = "f", digits = digits), 
                                   ifelse(crossCongP[i, j] > 0.1, "",
                                          ifelse(crossCongP[i, j] > 0.05, "-",
                                                 ifelse(crossCongP[i, j] > 0.01, "*",
                                                        ifelse(crossCongP[i, j] > 0.001, "**", "***"))))), font = 2)
        } else
        {
          if (length(ylab) > 1)
          {
            if (j == 1) mtext(paste(ylab[i]), outer = T, side = 2, line = 2, at = (ncol(df)-i + .5)/ncol(df), cex = .8)
          } else if (ylab == "variable.name")
          {
            if (j == 1) mtext(paste(ifelse(is.null(colnames(rankDf)),
                                           ifelse(method == "pearson",
                                                  paste("Var. ", i, sep = ""),
                                                  paste("Var. ", i, " rank", sep = "")),
                                           ifelse(method == "pearson",
                                                  colnames(rankDf)[i],
                                                  paste(colnames(rankDf)[i], " rank", sep = "")))), outer = T, side = 2, line = 2, at = (ncol(df)-i + .5)/ncol(df), cex = .8)
          } else
          {
            if (j == 1) mtext(paste(ylab), outer = T, side = 2, line = 2, at = (ncol(df)-i + .5)/ncol(df), cex = .8)
          }
          if (length(xlab) > 1)
          {
            if (i == ncol(df)) mtext(paste(xlab[j]), outer = T, side = 1, line = 2, at = (j - .5)/ncol(df), cex = .8)
          } else if(xlab == "variable.name")
          {
            if (i == ncol(df)) mtext(paste(ifelse(is.null(colnames(rankDf)),
                                                  ifelse(method == "pearson",
                                                         paste("Var. ", j, sep = ""),
                                                         paste("Var. ", j, " rank", sep = "")),
                                                  ifelse(method == "pearson",
                                                         colnames(rankDf)[j],
                                                         paste(colnames(rankDf)[j], " rank", sep = "")))), outer = T, side = 1, line = 2, at = (j - .5)/ncol(df), cex = .8)
          } else
          {
            if (i == ncol(df)) mtext(paste(xlab), outer = T, side = 1, line = 2, at = (j - .5)/ncol(df), cex = .8)
          }
          if(method == "spearman" | method == "kendall")
          {
            plot(rankDf[, i] ~ rankDf[, j], xaxt = "n", yaxt = "n", xlim = c(0, nrow(df)), ylim = c(0, nrow(df)), xlab = "", ylab = "", ...)
            if (i == ncol(df)) axis(1, at = c(0, round(nrow(df)/2, 0), nrow(df)), las = 1)
            if (j == 1) axis(2, at = c(0, round(nrow(df)/2, 0), nrow(df)), las = 1)
          } else if (method == "pearson")
          {
            dx <- diff(c(min(df[, j], na.rm = T), max(df[, j], na.rm = T)))
            dy <- diff(c(min(df[, i], na.rm = T), max(df[, i], na.rm = T)))
            plot(df[, i] ~ df[, j], xaxt = "n", yaxt = "n", xlim = c(min(df[, j], na.rm = T) - 0.05 * dx, 
                                                                     max(df[, j], na.rm = T) + 0.05 * dx), 
                 ylim = c(min(df[, i], na.rm = T) - 0.05 * dy, 
                          max(df[, i], na.rm = T) + 0.05 * dy), 
                 xlab = "", ylab = "", ...)
            if (i == ncol(df)) axis(1, las = 1)
            if (j == 1) axis(2, las = 1)
          }
        }
    }
  }
  mtext(paste(title), outer = T, side = 3, line = -1, at = .55, cex = 1.2)
}

.rankings <- function(x, ties.method = "average", na.action = "keep") length(x) - length(which(is.na(x))) + 1 - rank(x, ties.method = ties.method, na.last = na.action)