#'@title A standard error
#'
#'@description The standard error (SE) of a statistic is the standard deviation of its sampling distribution.
#'@param x is a numeric value, could be a  a vector or data.frame
#'@export
#'@keywords
#'@seealso \code{\link[utils]resume2data}
#'@return a standard error
#'@export
#'
#'@examples
#'x<-rnorm(25,2,3)
#'se(x)
#


se<-function (x,na.rm = TRUE)
{
  options(digits=3)
  n <- length(x)
  Stdev <- round(sd(x, na.rm = TRUE), 3)
  SE <- round(Stdev/sqrt(n), 3)
  Ma<-round(mean(x, na.rm = TRUE), 3)
  print(SE)
  SE3 <-message(paste0("mean= ", Ma, "\u00b1", SE, " =standard error"))
}
