#' Calculate the cost function of the macro model
#'
#' This utility calculates the cost function of the macro model according to
#' the squared error sum with penaty parameter. This utility can be used to
#' experiment more sophisticated optimization schemes.
#' 
#' @param dtb data table, usually this is the reg.dtb of the jubilee object
#' @param rs the list returned from \code{jubilee.macro_fit}
#' @param penalty numeric, the penalty vector for the 6 models.
#'                 Default is \code{c(1,1,1)}.
#' @param new.tb3ms numeric, vector of new \code{rate.tb3ms} with length equal to NROW of dtb.
#'                  Default is \code{NA}.
#' @param new.gs10 numeric, vector of new \code{rate.gs10} with length equal to NROW of dtb.
#'                 Default is \code{NA}.
#'
#' @return The data table containing the "macro.cost" column
#'
#' @keywords data
#'
#' @author Stephen H. Lihn
#'
#' @export jubilee.macro_cost
#'
### <======================================================================>
jubilee.macro_cost <- function(dtb, rs, penalty=c(1,1,1), new.tb3ms=NA, new.gs10=NA) {

    if (! is(rs$type1$lm, "lm")) stop("rs structure is wrong: type1$lm")
    if (! is(rs$type1$coef, "numeric")) stop("rs structure is wrong: type1$coef")
    
    penalty <- head(c(penalty, rep(0,6)),6)
    df <- jubilee.macro_predict(dtb, rs, new.tb3ms=new.tb3ms, new.gs10=new.gs10)
    
    cost <- function(J) {
        df1 <- df[J]
        err <- c(
            df1$type1.epsilon, # type I
            df1$type2.epsilon, # type II
            df1$type3.epsilon, # type III
            df1$unrate.epsilon,
            df1$payroll.epsilon,
            df1$tcu.epsilon
            )
        unname(sum(penalty*err^2)/2)
    }
    J <- which(is.finite(df$fraction))
    S <- jubilee.mcsapply(J, cost)
    dtb$macro.cost <- ifelse(is.finite(S), S, NaN)
    
    return(dtb)
}
### <---------------------------------------------------------------------->

