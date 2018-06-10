#'@title A Summary Statitical Data
#'
#'@description The most complete table descriptive in summary statistics
#'@param x is a numeric value, could be a  a vector or data.frame
#'@export
#'@keywords
#'@seealso \code{\link[utils]resume}
#'@return a table summary
#'@export
#'
#'@examples
#'x<-rnorm(25,2,3)
#'resume2data(x)
#
resume2data<-function (x, trim = 0.05, col = "#EBEBEB")
{
  require(e1071)
  options(digits=3)
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  par(mfrow=c(1,2))
  hist(x, main = "Histogram", col = col, xlab = "",  cex.lab=.9, cex.axis=.9)
  rug(x)
  boxplot(x, col = col, lwd = 0.5,  boxwex = .85, frame.plot = FALSE, cex=0.9, cex.lab=.9, cex.axis=.9, cex.main=.9)
  points(mean(x), pch=20, cex = 1.8)
  axis(side = 1, cex=0.9, cex.axis=.9, cex.lab=.9)
  title(main="Standard\nBoxplot")
  N <- length(x)
  UM <- round(sum(is.na(x)), 0)
  x <- x[is.finite(x)]
  n <- length(x)
  Nbrval <- length(x)
  LQ1 <- (n + 1)/4
  LQ3 <- (3 * (n + 1))/4
  Sort <- sort(x)
  V1 <- floor(LQ1)
  V2 <- floor(LQ3)
  V3 <- V1 + 1
  V4 <- V2 + 1
  Q1 <- round(quantile(x)[2], digits = 3)
  Q3 <- round(quantile(x)[4], digits = 3)
  IQR <- round(Q3 - Q1, 3)
  Min <- round(min(x), 3)
  Max <- round(max(x), 3)
  Stdev <- round(sd(x, na.rm = TRUE), 3)
  Mean <- round(mean(x, na.rm = TRUE), 3)
  Median <- round(median(x, na.rm = TRUE), 3)
  TrMean <- round(mean(x, trim = trim), 3)
  Var <- round(var(x, na.rm = TRUE), 3)
  SE <- round(Stdev/sqrt(n), 3)
  Range <- round(Max - Min, 3)
  l.out <- x[x < (Q1 - 1.5 * IQR)]
  r.out <- x[x > (Q3 + 1.5 * IQR)]
  outliers <- c(l.out, r.out)
  rest <- x[x > (Q1 - 1.5 * IQR) & x < (Q3 + 1.5 * IQR)]
  Minrest <- min(rest)
  Maxrest <- max(rest)
  SW <- shapiro.test(x)
  K <- round(kurtosis(x), digits = 2)
  S <- round(skewness(x), digits = 2)
  CV <- Stdev/Mean
  CIMean <- qt((0.5 + 0.05/2), (Nbrval - 1)) * SE
  ci<-.95
  error <- qt(ci + (1 - ci)/2, df = n - 1) * Stdev/sqrt(n)
  SUM<-sum(x)
  lwr.ci1<-Mean - error
  lwr.ci<-round(lwr.ci1,2)
  upr.ci1 = Mean + error
  upr.ci<-round(upr.ci1,2)
  SWpval <- round(SW$p.value, 3)
  TOT <- c(n, UM, Min, Q1, Median, Q3,  Max, Mean, Stdev,
           Var, SE, TrMean,IQR, Range, K, S, CV, CIMean, lwr.ci, upr.ci, SUM, SWpval)
  names(TOT) <- c("Size (n)", "Missing", "Minimum", "1st Qu","Median", "3st Qu",
                "Max" , "Mean", "sd","Var", "SE Mean",  "TrMean",
                  " IQR", "Range","Kurtosis", "Skewness","CV", "CI.Mean","lwr.ci", "upr.ci","Sum", "     Shapiro.test p-val")
  return(TOT)
}
#'Resumen estadistico
#'@param x is a numeric value
#'@retun Return a descriptive statistics

