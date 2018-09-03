#' Make prediction based on linear regression
#'
#' Make prediction based on the linear regression of the forward return.
#' Refer to the tutorial for more detail.
#'
#' @param object object of jubilee class
#' @param lm the linear model
#' @param data data used to predict (similar to \code{newdata} of \code{stats::predict})
#'
#' @return data.table containing the prediction
#'
#' @keywords model
#'
#' @author Stephen H. Lihn
#'
#' @export jubilee.predict
#' @export jubilee.predict_real
#'
#' @importFrom data.table setkey
#' @importFrom data.table data.table
#' @importFrom stats lm
#' @importFrom stats predict
#' @importFrom stats approx
#'
#' @references See Section 7 of Stephen H.T. Lihn, "Jubilee Tectonic Model:
#'     Forecasting Long-Term Growth and Mean Reversion in the U.S. Stock Market."
#'     Available at \url{http://dx.doi.org/10.2139/ssrn.3156574}
#'
### <======================================================================>
"jubilee.predict" <- function(object, lm, data)
{
    dt <- object@dtb
    dj <- object@reg.dtb
    
    frac <- dj$fraction
    for (f in dj$fraction+object@fwd.rtn.duration) {
        if (f > max(frac)+0.00001) frac <- c(frac, f)    
    }
    
    J2 <- which(dj$fraction %in% data$fraction)
    pred.logr <- stats::predict(lm, newdata=data)
    approx.log.tri <- stats::approx(dj$fraction[J2] + object@fwd.rtn.duration,
                             y=dj$log.tri[J2] + pred.logr*object@fwd.rtn.duration, 
                             xout=frac)
    pred.dtb <- data.table(fraction=approx.log.tri$x,
                           log.tri=approx.log.tri$y)
    fraction <- NULL # for R CMD check
    data.table::setkey(pred.dtb, fraction)
    
    pred.dtb$logr <- approx(dj$fraction[J2], y=pred.logr, xout=frac)$y
    
    get_log_tri_source <- function(frac) {
        I <- which(dj$fraction == frac)
        if (length(I)==1) dj$log.tri[I] else NA
    }
    get_lm_a_source <- function(frac) {
        I <- which(dj$fraction == frac)
        if (length(I)==1) dj$eqty.lm.a[I] else NA
    }
    
    pred.dtb$log.tri.source <- sapply(pred.dtb$fraction, get_log_tri_source)
    pred.dtb$eqty.lm.a.source <- sapply(pred.dtb$fraction, get_lm_a_source)
    pred.dtb$eqty.lm.y <- pred.dtb$log.tri-pred.dtb$eqty.lm.a.source
    
    # short-horizon mean-reversion
    pred.dtb$log.tri.smr <- pred.dtb$log.tri.source-pred.dtb$log.tri

    # fill up log.tri
    # pred.dtb$log.tri <- ifelse(is.na(pred.dtb$log.tri),
    #                            sapply(pred.dtb$fraction, get_log_tri_source),
    #                            pred.dtb$log.tri)
    
    fraction.end <- max(data$fraction)
    price.source.end <- dt$price[dt$fraction == fraction.end]
    log.tri.source.end <- dt$log.tri[dt$fraction == fraction.end]
    log.tri.end <- approx.log.tri$y[approx.log.tri$x == fraction.end]
    
    pred.dtb$tri.source <- exp(pred.dtb$log.tri.source-log.tri.source.end)*price.source.end
    pred.dtb$price <- exp(approx.log.tri$y-log.tri.source.end)*price.source.end
    
    attr(pred.dtb, "fraction.end") <- fraction.end
    attr(pred.dtb, "price.source.end") <- price.source.end
    attr(pred.dtb, "log.tri.source.end") <- log.tri.source.end
    attr(pred.dtb, "log.tri.end") <- log.tri.end
    
    invisible(pred.dtb)
}
### <---------------------------------------------------------------------->
#' @rdname jubilee.predict
"jubilee.predict_real" <- function(object, lm, data)
{
    dt <- object@dtb
    dj <- object@reg.dtb
    
    frac <- dj$fraction
    for (f in dj$fraction+object@fwd.rtn.duration) {
        if (f > max(frac)+0.00001) frac <- c(frac, f)    
    }
    
    J2 <- which(dj$fraction %in% data$fraction)
    pred.logr <- stats::predict(lm, newdata=data)
    approx.real.log.tri <- stats::approx(dj$fraction[J2] + object@fwd.rtn.duration, 
                                  y=dj$real.log.tri[J2] + pred.logr*object@fwd.rtn.duration, 
                                  xout=frac)
    pred.dtb <- data.table(fraction=approx.real.log.tri$x,
                           real.log.tri=approx.real.log.tri$y)
    fraction <- NULL # for R CMD check
    data.table::setkey(pred.dtb, fraction)
    
    pred.dtb$logr <- approx(dj$fraction[J2], y=pred.logr, xout=frac)$y
    
    get_real_log_tri_source <- function(frac) {
        I <- which(dj$fraction == frac)
        if (length(I)==1) dj$real.log.tri[I] else NA
    }
    
    # pred.dtb$real.log.tri <- ifelse(is.na(pred.dtb$real.log.tri),
    #                                 sapply(pred.dtb$fraction, get_real_log_tri_source),
    #                                 pred.dtb$real.log.tri)
    
    pred.dtb$real.log.tri.source <- sapply(pred.dtb$fraction, get_real_log_tri_source)

    fraction.end <- max(data$fraction)
    real.log.tri.source.end <- dt$real.log.tri[dt$fraction == fraction.end]
    real.log.tri.end <- approx.real.log.tri$y[approx.real.log.tri$x == fraction.end]
    
    
    attr(pred.dtb, "fraction.end") <- fraction.end
    attr(pred.dtb, "real.log.tri.source.end") <- real.log.tri.source.end
    attr(pred.dtb, "real.log.tri.end") <- real.log.tri.end
    
    invisible(pred.dtb)
}
### <---------------------------------------------------------------------->
