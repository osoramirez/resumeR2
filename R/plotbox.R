#'@title plotbox
#'
#'@description get a boxplot, showing the mean in plot
#'@param x is a numeric value, could be a  a vector or data.frame
#'@export
#'@keywords
#'@seealso \code{\link[utils]resume}
#'@return a plotbox (a elegant boxplot)
#'@export
#'
#'@examples
#'x<-rnorm(25,2,3)
#'plotbox(x)

plotbox <- function(x) {
  require(graphics)
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  par(mfrow=c(1,1))
  ylab <- deparse(substitute(x))  # get the expression passed as y
  boxplot(x,main=paste("Boxplot of", ylab, NULL), xlab=NULL,
          ylab=ylab, cex=.9, boxwex = .65,col = "#EBEBEB")
  points(mean(x), pch=16, cex = 1.8, col="black")
  summary(x)
}

