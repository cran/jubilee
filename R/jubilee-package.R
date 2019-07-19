#' jubilee: A package to forecast long-term growth of the US stock market
#'          and business cycles
#'
#' The jubilee package provides the core class and functions
#' to forecast long-term growth of the U.S. stock market.
#' It also contains a module for business cycles, optimal interest rate,
#' and recession forecasts.
#' A tutorial is provided to demonstrate how to use this package
#' and explain the relation between the mathematical notations and
#' the functions and data columns in this package.
#'
#' @author Stephen H-T. Lihn
#'
#' @docType package
#' @name jubilee-package
#' @import yaml xts methods graphics parallel splines
#'
#' @references Stephen H.T. Lihn, "Jubilee Tectonic Model:
#'     Forecasting Long-Term Growth and Mean Reversion in the U.S. Stock Market."
#'     Available at SSRN: \url{https://ssrn.com/abstract=3156574} or
#'     via DOI: \url{http://dx.doi.org/10.2139/ssrn.3156574}
#' @references Stephen H.T. Lihn, "Business Cycles, Optimal Interest Rate,
#'     and Recession Forecast From Yield Curve, Unemployment, GDP, and Payrolls."
#'     Available at SSRN: \url{https://ssrn.com/abstract=3422278}
#'
NULL

# Some areas of this package require multi-core capability
cores <- switch( Sys.info()[['sysname']],
    Windows = 1,
    Linux   = parallel::detectCores(),
    Darwin  = parallel::detectCores(),
    parallel::detectCores()
    )

if (is.null(getOption("mc.cores"))) {
    options("mc.cores"=cores)
}

setOldClass("xts")

# end
