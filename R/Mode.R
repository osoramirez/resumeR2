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



Mode <- function(x){
  x1 <- na.omit(x)
  ta <-table(x1)
  tam <- max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x1))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}

