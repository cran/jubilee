% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jubilee-repo-config.R
\name{jubilee.repo.config}
\alias{jubilee.repo.config}
\title{Configuration of jubilee's data repository}
\usage{
jubilee.repo.config()
}
\value{
The list of data elements and their attributes.
}
\description{
This utility stores the data configuration for the jubilee's data repository.
This is used internally to provide proper abstraction to the data sources,
such as file name, URL, FRED symbol, column name, decimal format, etc.
}
\examples{
  c <- jubilee.repo.config()
  c$ie$url

}
\author{
Stephen H. Lihn
}
\keyword{data}
