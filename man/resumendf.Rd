\name{resumendf}
\alias{resumendf}
\title{Get a summary for data.frame}
\usage{
  resumendf()
}
\description{
  Prints 'a summary for your data.frame'.
}
\examples{
  data(CO2)
  resumendf(conc ~Type, data=CO2)
}
