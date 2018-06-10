resumendf<-function (formula, data)
{
  dp = as.character(formula)
  DNAME <- paste(dp[[2L]], "and", dp[[3L]])
  if (any(colnames(data) == dp[[3L]]) == FALSE)
    stop("The name of group variable must be one factor.")
  if (any(colnames(data) == dp[[2L]]) == FALSE)
    stop("The name of response variable does not match.")
  y = data[, dp[[2L]]]
  group = data[, dp[[3L]]]
  if (!is.factor(group))
    stop("The group variable must be a factor.")
  if (!is.numeric(y))
    stop("The response must be a numeric variable.")
  if (length(y) != length(group))
    stop("Lengths of y and group variables are not equal.")
  x.levels <- levels(factor(group))
  y.NAs <- NULL
  for (i in x.levels) {
    y.NAs[i] <- length(y[group == i][is.na(y[group == i])])
  }
  completeObs <- complete.cases(y)
  y <- y[completeObs]
  group <- group[completeObs]
  y.skew <- y.kurtosis <- y.means <- y.n <- y.medians <- y.firstqs <- y.thirdqs <- y.mins <- y.maxs <- y.sds<-  y.se <-NULL
  for (i in x.levels) {
    y.means[i] <- round(mean(y[group == i]),3)
    y.n[i] <- length(y[group == i])
    y.medians[i] <- round(median(y[group == i]),3)
    y.firstqs[i] <- round(as.numeric(quantile(y[group == i])[2]),3)
    y.thirdqs[i] <- round(as.numeric(quantile(y[group == i])[4]),3)
    y.mins[i] <- round(min(y[group == i]),3)
    y.maxs[i] <- round(max(y[group == i]),3)
    y.sds[i] <- round(sd(y[group == i]),3)
    y.se[i] <-  round(sd(y[group == i])/sqrt(length(y[group == i])),3)
    }
  out = as.data.frame(cbind(y.n, y.means, y.sds, y.medians,
                            y.mins, y.maxs, y.firstqs, y.thirdqs,y.se,y.NAs))
  colnames(out) = c("n", "Mean", "sd", "Median", "Min",
                    "Max", "1st Qu", "3st Qu ",  "se", "Missing")
  return(out)
}
#We use the base of function "decribe" from "onewaytests" package.
