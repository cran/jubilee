#' Constructor of the jubilee class
#'
#' Construct an jubilee object which holds raw and derived data,
#' channel regression results, and other derived analytical quantities.
#' This object is the main object to perform various forecasts and analyses.
#'
#' @param dtb data.table from the \code{jubilee.repo} object, typically it is the \code{ie} slot.
#'            The user is allowed to provide custom data object to research different markets,
#'            as long as the column names are compliant.
#' @param lookback.channel numeric, look-back channel in years to calculate mean-reversion. Default is 45.
#' @param fwd.rtn.duration numeric, forward return duration in years. Default is 20.
#' @param force logical, if FALSE, allowed to retrieve previous object stored in option. Default is \code{TRUE}.
#'
#' @return an object of the jubilee class
#'
#' @keywords constructor
#'
#' @author Stephen H. Lihn
#'
#' @export
#'
#' @importFrom data.table setkey
#' @importFrom data.table setattr
#' @importFrom data.table data.table
#' @importFrom data.table shift
#'
#' @examples
#' \dontrun{
#'   repo <- jubilee.repo(online=FALSE)
#'   ju <- jubilee(repo@ie, 45, 20)
#' }
#'
### <======================================================================>
"jubilee" <- function(dtb, lookback.channel=45, fwd.rtn.duration=20, force=TRUE)
{
    d <- getOption("jubilee.object")
    if (! is.null(d)) {
        # if the two input parameters are not the same, nullify the stored object
        if (d@lookback.channel != lookback.channel | d@fwd.rtn.duration != fwd.rtn.duration) d <- NULL
    }
    if (!(is.null(d) | force)) return(d)
    
    # ---------------------------------------------------------------------------------
    jubilee.assert_column(dtb, c("log.tri", "cape10"))
    # ---------------------------------------------------------------------------------
    dtb$deflator <- dtb$gdp/dtb$real.gdp * 100
    dtb$cape20 <- jubilee.calc_cape(dtb, period=20)
    
    reg.dtb <- data.table(epoch=dtb$epoch,
                          fraction=dtb$fraction,
                          log.tri=dtb$log.tri,
                          real.log.tri=dtb$real.log.tri,
                          cape10=dtb$cape10,
                          cape20=dtb$cape20,
                          rate.gs10=dtb$rate.gs10,
                          rate.tb3ms=dtb$rate.tb3ms,
                          rate.ed3ms=dtb$rate.ed3ms,
                          rate.fedfunds=dtb$rate.fedfunds,
                          unrate=dtb$unrate,
                          tcu=dtb$tcu,
                          gdp=dtb$gdp,
                          real.gdp=dtb$real.gdp,
                          deflator=dtb$deflator,
                          payroll=dtb$payroll,
                          cp=dtb$cp,
                          usrec.nber=dtb$usrec.nber,
                          usrec.cp=dtb$usrec.cp
                        )

    fraction <- NULL
    data.table::setkey(reg.dtb, fraction)

    # yield spread related
    reg.dtb$rate.spread <- dtb$rate.gs10-dtb$rate.tb3ms
    reg.dtb$rate.spread.lag12 <- data.table::shift(reg.dtb$rate.spread, n=12, type="lag")
    reg.dtb$rate.spread.lag24 <- data.table::shift(reg.dtb$rate.spread, n=24, type="lag")
    
    rate.spread.mean <- mean(reg.dtb$rate.spread, na.rm=TRUE)
    reg.dtb$rate.spread.norm <- (reg.dtb$rate.spread-rate.spread.mean)/reg.dtb$rate.gs10
    reg.dtb$rate.gs10.modinv <- rate.spread.mean/reg.dtb$rate.gs10
    reg.dtb$rate.gs10.logr.10 <- jubilee.backward_rtn(dtb$fraction, dtb$rate.gs10.log.tri, 10)
    reg.dtb$rate.gs10.logr.20 <- jubilee.backward_rtn(dtb$fraction, dtb$rate.gs10.log.tri, 20)
    
    # derived quantities and fix of CAPE
    reg.dtb$log.cape10 <- log(dtb$cape10)
    reg.dtb$log.cape20 <- log(dtb$cape20)

    # CPI related
    reg.dtb$cpi.logr.1  <- jubilee.backward_rtn(dtb$fraction, log(dtb$cpi), 1)
    reg.dtb$cpi.logr.2  <- jubilee.backward_rtn(dtb$fraction, log(dtb$cpi), 2)
    reg.dtb$cpi.logr.5  <- jubilee.backward_rtn(dtb$fraction, log(dtb$cpi), 5)
    reg.dtb$cpi.logr.10 <- jubilee.backward_rtn(dtb$fraction, log(dtb$cpi), 10)
    reg.dtb$cpi.logr.20 <- jubilee.backward_rtn(dtb$fraction, log(dtb$cpi), 20)

    reg.dtb$cpi.logr.f1  <- jubilee.forward_rtn(dtb$fraction, log(dtb$cpi), 1)

    # equity OLS
    # ols <- jubilee.eqty_ols(dtb, reg.dtb$fraction, lookback.channel)
    # reg.dtb <- merge(reg.dtb, ols, all=FALSE)
    # reg.dtb$eqty.lm.y <- reg.dtb$log.tri - reg.dtb$eqty.lm.a

    eqty.lm <- jubilee.ols(dtb$fraction, dtb$log.tri, lookback.channel = lookback.channel)
    reg.dtb$eqty.lm.a <- eqty.lm$lm.a
    reg.dtb$eqty.lm.y <- eqty.lm$lm.y
    reg.dtb$eqty.lm.r <- eqty.lm$lm.r

    reg.dtb$eqty.y10  <- jubilee.backward_rtn(reg.dtb$fraction, reg.dtb$eqty.lm.y, 10)*10 # difference of y and y-10

    eqty.real.lm <- jubilee.ols(dtb$fraction, dtb$real.log.tri, lookback.channel = lookback.channel)
    reg.dtb$eqty.real.lm.a <- eqty.real.lm$lm.a
    reg.dtb$eqty.real.lm.y <- eqty.real.lm$lm.y
    reg.dtb$eqty.real.lm.r <- eqty.real.lm$lm.r
    
    reg.dtb$eqty.logr.fwd  <- jubilee.forward_rtn(dtb$fraction, dtb$log.tri, fwd.rtn.duration)
    reg.dtb$eqty.logr.fwd2 <- jubilee.forward_rtn(dtb$fraction, dtb$log.tri, fwd.rtn.duration/2) # half of forward period
    reg.dtb$eqty.real.logr.fwd  <- jubilee.forward_rtn(dtb$fraction, dtb$real.log.tri, fwd.rtn.duration)
    reg.dtb$eqty.real.logr.fwd2 <- jubilee.forward_rtn(dtb$fraction, dtb$real.log.tri, fwd.rtn.duration/2) # half of forward period

    reg.dtb$eqty.logr.6m <- jubilee.backward_rtn(dtb$fraction, dtb$log.tri, 0.5) # 6M bwd return, short-term
    reg.dtb$eqty.logr.1 <- jubilee.backward_rtn(dtb$fraction, dtb$log.tri, 1) # 1-year bwd return, short-term
    reg.dtb$eqty.logr.f1 <- jubilee.forward_rtn(dtb$fraction, dtb$log.tri, 1) # 1-year fwd return, short-term
    reg.dtb$eqty.logr.f3 <- jubilee.forward_rtn(dtb$fraction, dtb$log.tri, 3) # 3-year fwd return, short-term

    # 1m log-return for simulation
    reg.dtb$eqty.logr.1m <- jubilee.backward_rtn(dtb$fraction, dtb$log.tri, 1/12) # 1-month bwd return, short-term
    reg.dtb$eqty.logr.f1m <- jubilee.forward_rtn(dtb$fraction, dtb$log.tri, 1/12) # 1-month fwd return, short-term

    reg.dtb$eqty.logr.1.6m <- jubilee.backward_rtn(reg.dtb$fraction, reg.dtb$eqty.logr.1, 0.5) # 6m acceleration of 1y bwd return

    reg.dtb$eqty.logr.10 <- jubilee.backward_rtn(dtb$fraction, dtb$log.tri, 10)
    reg.dtb$eqty.logr.20 <- jubilee.backward_rtn(dtb$fraction, dtb$log.tri, 20)
    reg.dtb$eqty.logr.f10 <- jubilee.forward_rtn(dtb$fraction, dtb$log.tri, 10)
    reg.dtb$eqty.logr.f20 <- jubilee.forward_rtn(dtb$fraction, dtb$log.tri, 20)
    reg.dtb$eqty.logr.f40 <- jubilee.forward_rtn(dtb$fraction, dtb$log.tri, 40)
    reg.dtb$eqty.real.logr.f10 <- jubilee.forward_rtn(dtb$fraction, dtb$real.log.tri, 10)
    reg.dtb$eqty.real.logr.f20 <- jubilee.forward_rtn(dtb$fraction, dtb$real.log.tri, 20)
    reg.dtb$eqty.real.logr.f40 <- jubilee.forward_rtn(dtb$fraction, dtb$real.log.tri, 40)
    
    reg.dtb$gold.real.logp <- dtb$gold.real.logp <- log(dtb$gold/dtb$cpi)
    reg.dtb$gold.lm.y <- jubilee.ols(dtb$fraction, dtb$gold.real.logp, lookback.channel = lookback.channel)$lm.y
    reg.dtb$gold.real.logr.f10 <- jubilee.forward_rtn(reg.dtb$fraction, reg.dtb$gold.real.logp, 10)
    reg.dtb$gold.real.logr.f20 <- jubilee.forward_rtn(reg.dtb$fraction, reg.dtb$gold.real.logp, 20)

    reg.dtb$unrate.diff.1  <- jubilee.backward_rtn(dtb$fraction, dtb$unrate, 1)  # one-year difference of unrate
    reg.dtb$unrate.logr.1  <- jubilee.backward_rtn(dtb$fraction, log(dtb$unrate), 1) # one-year log-return
    reg.dtb$unrate.logr.6m  <- jubilee.backward_rtn(dtb$fraction, log(dtb$unrate), 0.5) # 6m log-return

    reg.dtb$unrate.logr.1.6m  <- jubilee.backward_rtn(reg.dtb$fraction, reg.dtb$unrate.logr.1, 0.5) # 6m acceleration of 1y momentum

    reg.dtb$payroll.nom.logr.1  <- jubilee.backward_rtn(dtb$fraction, log(dtb$payroll), 1) # one-year log-return

    # Sahm unemployment index
    unrate_get_dtb1 <- function(frac) {
        eps <- 5/365
        J <- which(reg.dtb$fraction <= frac + eps & reg.dtb$fraction >= frac - 12*1/12 - eps)
        reg.dtb[J, c("fraction", "unrate")]
    }
    
    unrate_sahm_3m <- function(frac) {
        df1 <- unrate_get_dtb1(frac)
        mean(tail(df1,3)$unrate)
    }
    unrate_sahm_12m <- function(frac) {
        df1 <- unrate_get_dtb1(frac)
        min(tail(df1,12)$unrate)
    }
    
    reg.dtb$unrate.3m_avg <- sapply(reg.dtb$fraction, unrate_sahm_3m)
    reg.dtb$unrate.12m_min <- sapply(reg.dtb$fraction, unrate_sahm_12m)
    reg.dtb$unrate.sahm_idx <- reg.dtb$unrate.3m_avg - reg.dtb$unrate.12m_min
    
    # rate.spread.mean
    setattr(reg.dtb, "rate.spread.mean", rate.spread.mean)

    call <- match.call()
    d <- new("jubilee",
             call = call,
             dtb = dtb,
             reg.dtb = reg.dtb,
             lookback.channel = lookback.channel,
             fwd.rtn.duration = fwd.rtn.duration,
             rate.spread.mean = rate.spread.mean,
             create.time = Sys.time()
            )

    options("jubilee.object"=d)
    invisible(d)
}
### <---------------------------------------------------------------------->
jubilee.assert_column <- function(dtb, col) {
    cols <- colnames(dtb)
    for (c in col) {
        if (!(c %in% cols)) stop(paste("Error asserting column", c, "in data table"))
    }
}
### <---------------------------------------------------------------------->
