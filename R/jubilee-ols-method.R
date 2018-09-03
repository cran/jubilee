#' Internal utility to calculate OLS regression
#'
#' Calculate the OLS regression for a given time series and fraction
#'
#' @param fraction numeric, the ending fraction of regression
#' @param ts numeric, the time series data
#' @param lookback.channel numeric, the backward-looking regression period
#' @param tol.frac numeric, tolerance of missing data in the beginning, expressed as fraction.
#'                 Default is 1/6, that is, two months.
#'
#' @return data.table with columns of \code{fraction, lm.a, lm.y, lm.r}
#'
#' @keywords utility
#'
#' @author Stephen H. Lihn
#'
#' @export jubilee.ols
#'
#' @importFrom parallel mclapply
#' @importFrom data.table setkey
#' @importFrom data.table data.table
#' @importFrom stats cov
#' @importFrom stats var
#'
#' @references See Section 2.3 of Stephen H.T. Lihn, "Jubilee Tectonic Model:
#'     Forecasting Long-Term Growth and Mean Reversion in the U.S. Stock Market."
#'     Available at \url{http://dx.doi.org/10.2139/ssrn.3156574}
#'
#' @examples
#' \dontrun{
#'   dtb <- jubilee.repo(online=FALSE)@ie
#'   df <- jubilee.ols(dtb$fraction, dtb$log.tri, 50)
#'   subset(df, fraction > 1970 & fraction < 1970.05)
#'   # fraction     lm.a      lm.r       lm.y
#'   # 1970.042 11.86401 0.1007617 0.02103105
#' }
### <======================================================================>
jubilee.ols <- function(fraction, ts, lookback.channel, tol.frac=1/6) {

    if (length(fraction) != length(ts)) stop("length of fraction must be equal to length of ts")
    
    ols <- function(end.frac) {
        if (length(end.frac) != 1) stop("ERROR: end.frac is not length 1")
        start.frac <- end.frac - lookback.channel
        I <- which(fraction >= start.frac & fraction <= end.frac & !is.na(ts))
        if (length(I)==0) return(c(end.frac,NA,NA))
        if (!is.na(tol.frac)) {
            if (min(fraction[I]) > start.frac+tol.frac) return(c(end.frac,NA,NA))
        }
        
        t <- fraction[I] - end.frac
        X <- ts[I]
        R <- stats::cov(X,t)/stats::var(t)
        a <- mean(X,na.rm=TRUE) - R*mean(t,na.rm=TRUE)
        return(c(end.frac, a, R))
    }
    dtb <- do.call(rbind, parallel::mclapply(fraction, ols)) # matrix of 3 columns
    colnames(dtb) <- c("fraction", "lm.a", "lm.r")
    fraction <- NULL
    dtb <- data.table(dtb)
    data.table::setkey(dtb,fraction)
    dtb$lm.y <- ts - dtb$lm.a
    return(dtb)
}
### <---------------------------------------------------------------------->
