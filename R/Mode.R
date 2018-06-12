#'@title Mode
#'
#'@description get a Mode
#'@param x is a numeric value, could be a  a vector or data.frame
#'@export
#'@keywords
#'@seealso \code{\link[utils]resume}
#'@return a mode
#'@export
#'
#'@examples
#'x<-rnorm(25,2,3)
#'Mode(x)


Mode<-function (x)
{
  if (!is.atomic(x) | is.matrix(x))
    stop("Supports only vectors.")
  if (na.rm)
    x <- na.omit(x)
  tab <- table(x)
  res <- names(which(tab == max(tab)))
  if (!inherits(x, "factor"))
    class(res) <- class(x)
  return(as.vector(res))
}

