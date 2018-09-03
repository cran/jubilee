#' Internal utility to calculate OLS regression for log total return index
#'
#' Calculate the OLS regression for log total return index
#'
#' @param dtb data.table that contains \code{fraction} and \code{log.tri} columns.
#' @param end.frac numeric, the ending fraction of regression.
#' @param lookback.channel numeric, the backward-looking regression period
#' @param tol.frac numeric, tolerance of missing data in the beginning, expressed as fraction.
#'                 Default is 1/6, that is, two months.
#'
#' @return two-element array \code{c(a,R)} if \code{end.frac} is length-one;
#'      data.table with \code{end.frac} as fraction column if \code{end.frac} is an array.
#'
#' @keywords utility
#'
#' @author Stephen H. Lihn
#'
#' @export jubilee.eqty_ols
#'
#' @importFrom parallel mclapply
#' @importFrom data.table setkey
#' @importFrom data.table data.table
#' @importFrom stats cov
#' @importFrom stats var
#'
#' @examples
#'   \dontrun{
#'     dtb <- jubilee.repo(online=FALSE)@ie
#'     jubilee.eqty_ols(dtb, 1970, 50) # c(11.8671626, 0.1008371)
#'  }
### <======================================================================>
jubilee.eqty_ols <- function(dtb, end.frac, lookback.channel, tol.frac=1/6) {

    jubilee.assert_column(dtb, c("fraction", "log.tri"))

    if (length(end.frac) > 1) {
        f <- function(x) {
            s <- jubilee.eqty_ols(dtb, x, lookback.channel, tol.frac)
            c(x, s)
        }
        t <- do.call(rbind, parallel::mclapply(end.frac, f)) # matrix of 3 columns
        colnames(t) <- c("fraction", "eqty.lm.a", "eqty.lm.r")
        t <- data.table(t)
        fraction <- NULL
        data.table::setkey(t,fraction)
        return(t)
    }

    start.frac <- end.frac - lookback.channel
    I <- which(dtb$fraction >= start.frac & dtb$fraction <= end.frac & !is.na(dtb$log.tri))
    if (length(I)==0) return(c(NA,NA))
    dtb2 <- dtb[I,]
    if (min(dtb2$fraction) > start.frac+tol.frac) return(c(NA,NA))

    t <- dtb2$fraction - end.frac
    X <- dtb2$log.tri
    R <- stats::cov(X,t)/stats::var(t)
    a <- mean(X)-R*mean(t)
    return(c(a,R))
}
### <---------------------------------------------------------------------->
