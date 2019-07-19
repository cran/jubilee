#' Prediction from UNRATE and GDP models
#'
#' This utility performs the prediction from the linear models of UNRATE and GDP.
#' The purpose of this method is to automate the prediction and to allow users
#' experimenting optimization on the natural rate of interest.
#' 
#' @param dtb data table, usually this is \code{lm.dtb} of the \code{rs} object,
#'            with GDP log-return percent \code{(logrp.N, logrp.K)} calculated.
#' @param rs the list returned from \code{jubilee.macro_fit}, which provides
#'           regression parameters for the prediction (not the data).
#' @param new.tb3ms numeric, vector of new \code{rate.tb3ms} with length equal to NROW of dtb.
#'                  Default is \code{NA}.
#' @param new.gs10 numeric, vector of new \code{rate.gs10} with length equal to NROW of dtb.
#'                 Default is \code{NA}.
#'
#' @return The data table containing the predictions and all the required input columns
#'
#' @keywords data
#'
#' @importFrom data.table setattr
#' @importFrom data.table data.table
#'
#' @author Stephen H. Lihn
#'
#' @export jubilee.macro_predict
#'
### <======================================================================>
jubilee.macro_predict <- function(dtb, rs, new.tb3ms=NA, new.gs10=NA) {

    if (class(rs$type1$lm) != "lm") stop("rs structure is wrong: type1$lm")
    if (class(rs$type1$coef) != "numeric") stop("rs structure is wrong: type1$coef")
    
    if (! all(is.na(new.tb3ms))) {
        if (NROW(dtb) != length(new.tb3ms)) stop("new.tb3ms doesn't have the required length")
        dtb$rate.tb3ms <- new.tb3ms
    }
    
    if (! all(is.na(new.gs10))) {
        if (NROW(dtb) != length(new.gs10)) stop("new.gs10 doesn't have the required length")
        dtb$rate.gs10 <- new.gs10
    }
    
    if (! ("gdp.logrp.N" %in% colnames(dtb) )) stop("gdp.logrp.N doesn't exist in dtb")
    if (! ("gdp.logrp.K" %in% colnames(dtb) )) stop("gdp.logrp.K doesn't exist in dtb")

    
    if (! all(is.na(new.tb3ms) & is.na(new.gs10))) {
        dtb$rate.spread <- dtb$rate.gs10-dtb$rate.tb3ms
        dtb$rate.spread.norm <- (dtb$rate.spread-attr(dtb, "rate.spread.mean"))/dtb$rate.gs10
        dtb$rate.gs10.modinv <- attr(dtb, "rate.spread.mean")/dtb$rate.gs10
    }
    
    calc_3factors <- function(co) {
        co[1] + co["rate.gs10"]*dtb$rate.gs10 + co["rate.gs10.modinv"]*dtb$rate.gs10.modinv
    }

    df <- data.table(
        fraction = dtb$fraction,
        rate.gs10 = dtb$rate.gs10,
        rate.gs10.modinv = dtb$rate.gs10.modinv,
        rate.spread.norm = dtb$rate.spread.norm,
        rate.spread = dtb$rate.spread,
        rate.tb3ms = dtb$rate.tb3ms,
        unrate = dtb$unrate,
        unrate.logr.1=dtb$unrate.logr.1,
        unrate.logr.6m=dtb$unrate.logr.6m,
        tcu = dtb$tcu,
        gdp.logrp.N = dtb$gdp.logrp.N,
        real.gdp.logrp.N = dtb$real.gdp.logrp.N,
        deflator.logrp.N = dtb$deflator.logrp.N,
        gdp.logrp.K = dtb$gdp.logrp.K,
        real.gdp.logrp.K = dtb$real.gdp.logrp.K,
        deflator.logrp.K = dtb$deflator.logrp.K,
        payroll.logrp.K = dtb$payroll.logrp.K,
        usrec.nber=dtb$usrec.nber,
        usrec.cp=dtb$usrec.cp)

    setattr(df, "rate.spread.mean", attr(dtb, "rate.spread.mean"))
    
    df$type1.predict <- predict(rs$type1$lm, newdata=dtb)
    df$type2.predict <- predict(rs$type2$lm, newdata=dtb)
    df$type3.predict <- predict(rs$type3$lm, newdata=dtb)
    df$type3.K.predict <- predict(rs$type3.K$lm, newdata=dtb)
    df$unrate.predict <- predict(rs$unrate$lm, newdata=dtb)
    df$payroll.predict <- predict(rs$payroll$lm, newdata=dtb)
    df$tcu.predict <- predict(rs$tcu$lm, newdata=dtb)
    
    df$unrate.predict.logr.1  <- jubilee.backward_rtn(df$fraction, log(df$unrate.predict), 1) # one-year log-return
    df$unrate.predict.logr.6m  <- jubilee.backward_rtn(df$fraction, log(df$unrate.predict), 0.5) # 6m log-return

    df$type1.epsilon <- df$gdp.logrp.N - df$type1.predict
    df$type2.epsilon <- df$deflator.logrp.N - df$type2.predict
    df$type3.epsilon <- df$real.gdp.logrp.N - df$type3.predict
    df$unrate.epsilon <- df$unrate - df$unrate.predict
    df$payroll.epsilon <- df$payroll.logrp.K - df$payroll.predict
    df$tcu.epsilon <- df$tcu - df$tcu.predict
    
    uc <- rs$unrate$coef
    df$unrate.nat <- calc_3factors(uc)
    df$unrate.min <- df$unrate.nat - abs(uc["rate.spread.norm"])*dtb$rate.gs10.modinv
    df$unrate.max <- df$unrate.nat + abs(uc["rate.spread.norm"])*dtb$rate.gs10.modinv
    df$unrate.gap <- df$unrate - df$unrate.nat

    ac <- rs$type1$coef
    bc <- rs$type2$coef
    cc <- rs$type3$coef
    cc.K <- rs$type3.K$coef
    ee <- rs$payroll$coef

    a4 <- ac["deflator.logrp.N"]
    b4 <- bc["gdp.logrp.N"]

    df$real.gdp.nat1 <- df$deflator.logrp.N*(a4-1) + calc_3factors(ac)
    df$real.gdp.nat2 <- df$gdp.logrp.N*(1-b4) - calc_3factors(bc)

    df$real.gdp.nat3 <- calc_3factors(cc)
    df$real.gdp.max3 <- df$real.gdp.nat3 + abs(cc["rate.spread.norm"])*dtb$rate.gs10.modinv
    df$real.gdp.min3 <- df$real.gdp.nat3 - abs(cc["rate.spread.norm"])*dtb$rate.gs10.modinv

    df$real.gdp.K.nat <- calc_3factors(cc.K)
    df$real.gdp.K.max <- df$real.gdp.K.nat + abs(cc.K["rate.spread.norm"])*dtb$rate.gs10.modinv
    df$real.gdp.K.min <- df$real.gdp.K.nat - abs(cc.K["rate.spread.norm"])*dtb$rate.gs10.modinv

    df$payroll.nat <- calc_3factors(ee) + df$real.gdp.K.nat * ee["real.gdp.logrp.K"]
    df$payroll.max <- calc_3factors(ee) + abs(ee["rate.spread.norm"])*dtb$rate.gs10.modinv + df$real.gdp.K.max * ee["real.gdp.logrp.K"]
    df$payroll.min <- calc_3factors(ee) - abs(ee["rate.spread.norm"])*dtb$rate.gs10.modinv + df$real.gdp.K.min * ee["real.gdp.logrp.K"]

    df$type1.real.gdp <- df$type1.predict - df$deflator.logrp.N
    df$type2.real.gdp <- df$gdp.logrp.N - df$type2.predict
    df$type3.real.gdp <- df$type3.predict
    
    # solutions for coupled equations for Type-I and Type-II, but these are somewhat problematic
    # df$type1.predict.coupled <- (df$type1.predict - a4*b4 * df$gdp.logrp.N) / (1-a4*b4)
    # df$type2.predict.coupled <- (df$type2.predict - a4*b4 * df$deflator.logrp.N) / (1-a4*b4)
    # df$type1.real.gdp.coupled <- df$type1.predict.coupled - df$deflator.logrp.N
    # df$type2.real.gdp.coupled <- df$gdp.logrp.N - df$type2.predict.coupled


    return(df)
}
### <---------------------------------------------------------------------->
