#' Internal utility to calculate annualized forward and backward (log) return
#'
#' These two internal utilities are intended to be used to calculate the annualized
#' forward and backward log-return on the given time series. It is really calculating
#' the speed of change, aka log-return, expecting the input to be in logrithmic scale.
#' The forward return is typically the response variable in a forecast.
#' The backward return is often used as explanatory variable in a regression.
#'
#' @param fraction numeric, the ending fraction of regression
#' @param ts numeric, the time series data, typically in log-scale
#' @param fwd.rtn.duration numeric, the forward-looking regression period
#' @param bwd.rtn.duration numeric, the backward-looking regression period
#' @param tol.frac numeric, tolerance of missing data in the beginning of backward return,
#'                 or the ending of the forward return, expressed as fraction.
#'                 Default is 1/12, that is, one month.
#'
#' @return numeric, the same length as \code{fraction}
#'
#' @keywords utility
#'
#' @author Stephen H. Lihn
#'
#' @export jubilee.forward_rtn
#' @export jubilee.backward_rtn
#'
#' @examples
#' \dontrun{
#'   dtb <- jubilee.repo(online=FALSE)@ie
#'   dtb$fwd.logr.10 <- jubilee.forward_rtn(dtb$fraction, dtb$log.tri, 10)
#'   dtb$bwd.logr.10 <- jubilee.backward_rtn(dtb$fraction, dtb$log.tri, 10)
#'   head(subset(dtb, fraction >= 1990),1)$fwd.logr.10 # 1/1990+10y: 0.16745
#'   tail(subset(dtb, fraction <= 2000+1/12),1)$bwd.logr.10 # the same as above
#' }
### <======================================================================>
jubilee.forward_rtn <- function(fraction, ts, fwd.rtn.duration, tol.frac=1/12) {
    eps <- 1/365/24 # about 0.0001, allows one hour error
   
    calc_forward_rtn1 <- function(start.frac) {
        end.frac <- start.frac + fwd.rtn.duration + eps
        I <- which(fraction >= start.frac & fraction <= end.frac & !is.na(ts))
        if (length(I)==0) return(NA)
        t <- fraction[I]
        X <- ts[I]
        if (max(t) < end.frac-tol.frac) return(NA)
        (X[length(X)]-X[1])/(max(t)-min(t))
    }
    return(jubilee.mcsapply(fraction, calc_forward_rtn1))
}
### <---------------------------------------------------------------------->
#' @rdname jubilee.forward_rtn
jubilee.backward_rtn <- function(fraction, ts, bwd.rtn.duration, tol.frac=1/12) {
    eps <- 1/365/24 # about 0.0001, allows one hour error
    
    calc_backward_rtn1 <- function(end.frac) {
        start.frac <- end.frac - bwd.rtn.duration - eps
        I <- which(fraction >= start.frac & fraction <= end.frac & !is.na(ts))
        if (length(I)==0) return(NA)
        t <- fraction[I]
        X <- ts[I]
        if (min(t) > start.frac+tol.frac) return(NA)
        (X[length(X)]-X[1])/(max(t)-min(t))
    }
    return(jubilee.mcsapply(fraction, calc_backward_rtn1))
}
### <---------------------------------------------------------------------->
