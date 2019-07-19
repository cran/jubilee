#' Calculate the optimal TB3MS
#'
#' This utility calculates the optimal TB3MS using the analytic solution.
#' 
#' @param dtb data table, usually this is \code{lm.dtb} of the \code{rs} object,
#'            with GDP log-return percent \code{(logrp.N, logrp.K)} calculated.
#' @param rs the list returned from \code{jubilee.macro_fit}.
#' @param penalty numeric, the penalty vector for the models.
#'                 Default is \code{c(1,1,1)}.
#'
#' @return The data table containing the "optimal.tb3ms" column
#'
#' @keywords data
#'
#' @author Stephen H. Lihn
#'
#' @export jubilee.optimal_tb3ms
#'
### <======================================================================>
jubilee.optimal_tb3ms <- function(dtb, rs, penalty=c(1,1,1)) {

    if (class(rs$type1$lm) != "lm") stop("rs structure is wrong: type1$lm")
    if (class(rs$type1$coef) != "numeric") stop("rs structure is wrong: type1$coef")

    sp <- c(
        rs$type1$coef["rate.spread.norm"],
        rs$type2$coef["rate.spread.norm"],
        rs$type3$coef["rate.spread.norm"],
        rs$unrate$coef["rate.spread.norm"],
        rs$payroll$coef["rate.spread.norm"],
        rs$tcu$coef["rate.spread.norm"]
        )
    
    penalty <- head(c(penalty, rep(0,6)),6)
    if (length(penalty) != length(sp)) stop("penalty has wrong length")

    new_tb3ms <- dtb$rate.gs10 - attr(dtb, "rate.spread.mean") # S_hat=0
    df <- jubilee.macro_predict(dtb, rs, new.tb3ms=new_tb3ms)
    
    f <- function(J) {
        df1 <- df[J]
        err <- c(
            df1$type1.epsilon, # type I
            df1$type2.epsilon, # type II
            df1$type3.epsilon, # type III
            df1$unrate.epsilon,
            df1$payroll.epsilon,
            df1$tcu.epsilon
            )
        unname(sum(sp*penalty*err)/sum(sp^2*penalty))
    }
    J <- which(is.finite(df$fraction))
    S <- jubilee.mcsapply(J, f)
    S <- ifelse(is.finite(S), S, NaN)
    dtb$optimal.tb3ms <- dtb$rate.gs10 - (dtb$rate.gs10 * S + attr(dtb, "rate.spread.mean"))
    
    return(dtb)
}
### <---------------------------------------------------------------------->

