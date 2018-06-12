#'@title histogram of data
#'
#'@description Get a histogram, showing the mean and median of your data
#'@param x is a numeric value, could be a  a vector or data.frame
#'@export
#'@keywords
#'@seealso \code{\link[utils]resume2data}
#'@return a summary
#'@export
#'
#'@examples
#'x<-rnorm(25,2,3)
#'plothist(x)
#

plothist <- function(x) {
  require(graphics)
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  par(mfrow=c(1,1))
  xlab <- deparse(substitute(x))  # get the expression passed as y
  hist(x, main=paste("Histogram of",  xlab), col = "#EBEBEB", xlab = "", cex=.9)
  abline(v = mean(x), lwd = 3, col="red")
  abline(v = median(x), col = "#3F689E", lwd = 2.5,lty=2)
  legend(x = "topright", pch = 1, title = "Legend",box.lty=0,cex=.7,bg="transparent",
         c("Mean", "Median"),
         col = c( "red", "#3F689E"),
         lwd = c(2, 2), lty=c(1,2))
}
