#' The GUPTY macro model
#'
#' This utility contains the macro regression models, covering GUPTY: three types of GDP,
#' UNRATE (unemployment rate), Payroll, and Treasury yield curve.
#' TCU (total capacity utilization) is also covered in the model but less recommended.
#' Given the in-sample time periods, it will perform model regressions
#' and return a list storing relavant information about the result. 
#' The purpose of this method is to automate the regression and facilitate
#' programatic cross validation.
#' 
#' @param dtb data table, usually this is the reg.dtb of the jubilee object
#' @param N numeric, number of years for GDP log-return calculation in GDP models
#' @param K numeric, number of years for GDP log-return calculation in Payroll and TCU models
#' @param unrate.frac.start numeric, starting fraction of unrate regression time period
#' @param gdp.frac.start numeric, starting fraction of gdp regression time period
#' @param frac.end numeric, ending fraction of regression time period.
#'                 This is also the starting fraction of cross-validation.
#' @param cv.frac.end numeric, ending fraction of cross-validation time period.
#'                    Cross validation can be disabled by setting it to NA.
#'
#' @return The list of data elements and their attributes.
#'
#' @keywords data
#'
#' @author Stephen H. Lihn
#'
#' @export jubilee.macro_fit
#'
#' @importFrom stats sd
#'
#' @references Stephen H.T. Lihn, "Business Cycles, Optimal Interest Rate,
#'     and Recession Forecast From Yield Curve, Unemployment, GDP, and Payrolls."
#'     Available at SSRN: \url{https://ssrn.com/abstract=3422278}
#'
#' @examples
#' \dontrun{
#'   repo <- jubilee.repo()
#'   ju <- jubilee(repo@ie, 45, 20)
#'   N <- 4
#'   K <- 1.5
#'   rs <- jubilee.macro_fit(ju@reg.dtb, N, K, 1950, 1960, 2010, 2019)
#' }
### <======================================================================>
jubilee.macro_fit <- function(dtb, N, K, unrate.frac.start, gdp.frac.start,
                               frac.end, cv.frac.end) {

    
    dtb$gdp.logrp.N <- jubilee.backward_rtn(dtb$fraction, log(dtb$gdp), N) * 100
    dtb$real.gdp.logrp.N <- jubilee.backward_rtn(dtb$fraction, log(dtb$real.gdp), N) * 100
    dtb$deflator.logrp.N <- jubilee.backward_rtn(dtb$fraction, log(dtb$deflator), N) * 100

    dtb$gdp.logrp.K <- jubilee.backward_rtn(dtb$fraction, log(dtb$gdp), K) * 100
    dtb$real.gdp.logrp.K <- jubilee.backward_rtn(dtb$fraction, log(dtb$real.gdp), K) * 100
    dtb$deflator.logrp.K <- jubilee.backward_rtn(dtb$fraction, log(dtb$deflator), K) * 100
    dtb$payroll.logrp.K <- jubilee.backward_rtn(dtb$fraction, log(dtb$payroll), K) * 100 - dtb$deflator.logrp.K

    model.type1 = gdp.logrp.N ~ rate.gs10 + rate.gs10.modinv + rate.spread.norm + deflator.logrp.N
    model.type2 = deflator.logrp.N ~ rate.gs10 + rate.gs10.modinv + rate.spread.norm + gdp.logrp.N
    model.type3 = real.gdp.logrp.N ~ rate.gs10 + rate.gs10.modinv + rate.spread.norm
    model.type3.K = real.gdp.logrp.K ~ rate.gs10 + rate.gs10.modinv + rate.spread.norm
    model.unrate = unrate ~ rate.gs10 + rate.gs10.modinv + rate.spread.norm
    model.payroll = payroll.logrp.K ~ rate.gs10 + rate.gs10.modinv + rate.spread.norm + real.gdp.logrp.K
    model.tcu = tcu ~ rate.gs10 + real.gdp.logrp.K + rate.spread.norm + gdp.logrp.K

    cv.frac.start <- frac.end+1/12/30
    J <- which(dtb$fraction >= gdp.frac.start & dtb$fraction <= frac.end)
    J2 <- which(dtb$fraction >= unrate.frac.start & dtb$fraction <= frac.end)

    rs <- list(gdp.which = J,
               unrate.which = J2,
               gdp.frac.start = gdp.frac.start,
               unrate.frac.start = unrate.frac.start,
               frac.end = frac.end,
               N = N,
               K = K)

    calc.lm.sd <- function(lm, response) {
        if (! is.finite(cv.frac.end)) return(NaN)
        J4 <- which(dtb$fraction >= cv.frac.start & dtb$fraction <= cv.frac.end)
        if (length(J4)==0) return(NA)
        a4.err <- response[J4] - predict(lm, newdata=dtb[J4])
        sd(a4.err, na.rm=TRUE)
    }

    run_model <- function(model, I, response) {
        m <- list()
        m$model <- model
        m$lm <- lm(model, data=dtb[I])
        m$summary <- summary(m$lm)
        m$adj.r.squared <- m$summary$adj.r.squared
        m$sigma <- m$summary$sigma
        m$sigma_cv <- calc.lm.sd(m$lm, response)
        m$coef <- m$lm$coefficients

        return(m)
    }
    
    rs$type1 <- run_model(model.type1, J, dtb$gdp.logrp.N)
    rs$type2 <- run_model(model.type2, J, dtb$deflator.logrp.N)
    rs$type3 <- run_model(model.type3, J, dtb$real.gdp.logrp.N)
    rs$type3.K <- run_model(model.type3.K, J, dtb$real.gdp.logrp.K)
    rs$unrate <- run_model(model.unrate, J2, dtb$unrate)
    rs$payroll <- run_model(model.payroll, J, dtb$payroll.logrp.K)
    rs$tcu <- run_model(model.tcu, J, dtb$tcu)

    rs$lm.dtb <- jubilee.macro_predict(dtb, rs)

    return(rs)
}
### <---------------------------------------------------------------------->
