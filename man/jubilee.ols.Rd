% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jubilee-ols-method.R
\name{jubilee.ols}
\alias{jubilee.ols}
\title{Internal utility to calculate OLS regression}
\usage{
jubilee.ols(fraction, ts, lookback.channel, tol.frac = 1/6)
}
\arguments{
\item{fraction}{numeric, the ending fraction of regression}

\item{ts}{numeric, the time series data}

\item{lookback.channel}{numeric, the backward-looking regression period}

\item{tol.frac}{numeric, tolerance of missing data in the beginning, expressed as fraction.
Default is 1/6, that is, two months.}
}
\value{
data.table with columns of \code{fraction, lm.a, lm.y, lm.r}
}
\description{
Calculate the OLS regression for a given time series and fraction
}
\examples{
\dontrun{
  dtb <- jubilee.repo(online=FALSE)@ie
  df <- jubilee.ols(dtb$fraction, dtb$log.tri, 50)
  subset(df, fraction > 1970 & fraction < 1970.05)
  # fraction     lm.a      lm.r       lm.y
  # 1970.042 11.86401 0.1007617 0.02103105
}
}
\references{
See Section 2.3 of Stephen H.T. Lihn, "Jubilee Tectonic Model:
    Forecasting Long-Term Growth and Mean Reversion in the U.S. Stock Market."
    Available at \url{http://dx.doi.org/10.2139/ssrn.3156574}
}
\author{
Stephen H. Lihn
}
\keyword{utility}
