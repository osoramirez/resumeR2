#'@title Geometric mean
#'
#'@description get a geometric mean
#'@param x is a numeric value, could be a  a vector or data.frame
#'@export
#'@keywords
#'@seealso \code{\link[utils]resume}
#'@return a geometric mean
#'@export
#'
#'@examples
#'x<-rnorm(25,2,3)
#'g_mean(x)
#
g_mean<-function (x, na.rm = TRUE)
{
  if (is.null(nrow(x))) {
    exp(mean(log(x), na.rm = TRUE))
  }
  else {
    exp(apply(log(x), 2, mean, na.rm = na.rm))
  }
}
