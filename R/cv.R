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
cv <- function(x) sqrt(var(x,na.rm=TRUE))/mean(x,na.rm=TRUE)


