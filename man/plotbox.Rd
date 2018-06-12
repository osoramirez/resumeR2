\name{plotbox}
\alias{plotbox}
\title{Get a boxplot with mean}
\usage{
  plotbox()
}
\description{
  Prints a 'boxplot with mean'.
}
\examples{
  set.seed(12345)
  data<-rnorm(100,25,3)
  plotbox(data)
}
