plotbox <- function(x) {
  require(graphics)
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  par(mfrow=c(1,1))
  ylab <- deparse(substitute(x))  # get the expression passed as y
  boxplot(x,main=paste("Boxplot of", ylab, NULL), xlab=NULL,
          ylab=ylab, cex=.9, boxwex = .65, notch = TRUE,col = "#EBEBEB")
  points(mean(x), pch=16, cex = 1.8, col="red")
}

