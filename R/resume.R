#'@title Summary Statitical Data
#'
#'@description Get a complete descriptive summary statistics
#'@param x is a numeric value, could be a  a vector or data.frame
#'@export
#'@keywords
#'@seealso \code{\link[utils]resume2data}
#'@return a summary
#'@export
#'
#'@examples
#'x<-rnorm(25,2,3)
#'resume(x)
#
resume<-function (data, na.rm = FALSE,col = "#EDEDED")
{
  require(e1071)
  require(car)
  if (na.rm)
  options(digits=3)
  data <- na.omit(data)
  def.par <- par(no.readonly = TRUE)
  on.exit(par(def.par))
  par(mfrow=c(2,2))
  hist(data, main = NULL, col = col, xlab = "", prob = TRUE, cex=.9)
  abline(v = mean(data), lwd = 3, col="red")
  abline(v = median(data), col = "#3F689E", lwd = 2.5,lty=2)
  rug(data)
  lines(density(data), lwd = 1.75)
  legend(x = "topright", pch = 1, title = "Legend",box.lty=0,cex=.7,bg="transparent",
         c("Mean", "Median"),
         col = c( "red", "#3F689E"),
         lwd = c(2, 2), lty=c(1,2))
  x <- NULL
  curve(dnorm(x, mean(data), sd(data)), add = TRUE, col = "red")
  title(main = "Histogram and density")
  boxplot(data, col = col, lwd = 0.5,  boxwex = .85, frame.plot = FALSE)
  points(mean(data), pch=20, cex = 1.8)
  axis(side = 1)
  title(main="Standard\nBoxplot")
  qqPlot(data, main = "Quantile-quantile plot")
  n <- length(data)
  shapi <- shapiro.test(data)
  shapiW <- round(shapi$statistic, 3)
  shapiP <- round(shapi$p.value, 3)
  x1 <- shapiP
  x2<-if(x1 > 0.05){
    print("You got Normal distribution.")
  } else {
    print("You got Asymmetrical Distribution")
  }
  Mean<-round(mean(data, na.rm = TRUE), 3)
  Sd<-round(sd(data, na.rm = TRUE), 3)
  Median <- round(median(data, na.rm = TRUE), 3)
  Max <- round(max(data, na.rm = TRUE), 3)
  Min <- round(min(data, na.rm = TRUE), 3)
  ci<-.95
  error <- qt(ci + (1 - ci)/2, df = n - 1) * Sd/sqrt(n)
  lwr.ci1<-Mean - error
  lwr.ci<-round(lwr.ci1,2)
  upr.ci1 = Mean + error
  upr.ci<-round(upr.ci1,2)
  x3<-if(n > 30){
    print("You got a good sample size (n>=30)")
  } else {
    print("Warning: You have a low sample size (n<30)")
  }
  par(mar = rep(1, 4))
  plot.new()
  text(cex=.95, x = 0.5, y = 0.5, paste("DESCRIPTIVE", "\nSample size: n=",n,
                                        sep = "", "\nMean=", as.numeric(Mean),
                                        "  sd=", as.numeric(Sd),"\nMedian=", as.numeric(Median),
                                        "\nMax=", as.numeric(Max),
                                        "   Min=", as.numeric(Min),
                                        "\nConfident.Intervals= ", as.numeric(lwr.ci), " - ", as.numeric(upr.ci),
                                        "\nShapiro-Wilk test:  P-value=", as.character(shapiP),
                                        "\nDistr.=", as.character(x2), "\n", as.character(x3)))
}
