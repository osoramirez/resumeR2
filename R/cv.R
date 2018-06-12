#'@title coefficient of variation
#'
#'@description Compute the coefficient of variation
#'@param x is a numeric value, could be a  a vector or data.frame
#'@export
#'@keywords
#'@seealso \code{\link[utils]resume}
#'@return a table summary
#'@export
#'
#'@examples
#'x<-rnorm(25,2,3)
#'cv(x)
#
cv <- function(x, na.rm = TRUE)
{
  options(digits=3)
  data <- na.omit(x)
  sqrt(var(data,na.rm=TRUE))/mean(data,na.rm=TRUE)
  }


