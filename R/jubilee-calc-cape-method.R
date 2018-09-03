#' Internal utility to calculate n-year CAPE
#'
#' This CAPE calculator replicates the methodology of Shiller, so that one can calculate
#' n-year CAPE, e.g. n=20.
#' This utility has been calibrated by original 10-year CAPE data from Shiller.
#'
#' @param dtb data.table
#' @param period numeric, the backward-looking regression period
#' @param tol.frac numeric, tolerance of missing data in the beginning of the time series,
#'                 expressed as fraction.
#'                 Default is 1/6, that is, two months.
#'
#' @return numeric, the same length as \code{dtb$fraction}.
#'
#' @keywords utility
#'
#' @author Stephen H. Lihn
#'
#' @export jubilee.calc_cape
#'
#' @importFrom stats lm
#'
#' @examples
#'   \dontrun{
#'     dtb <- jubilee.repo(online=FALSE)@ie
#'     cape10 <- jubilee.calc_cape(dtb, 10)
#'     cape20 <- jubilee.calc_cape(dtb, 20)
#'   }
### <======================================================================>
jubilee.calc_cape <- function(dtb, period, tol.frac=1/6) {

    jubilee.assert_column(dtb, c("fraction", "real.price", "real.earnings"))

    P <- function(frac) {
        I <- which(dtb$fraction==frac)
        dtb$real.price[I]
    }

    E <- function(frac) { # this doesn't include frac
        start.frac <- frac-period-0.0001
        I <- which(dtb$fraction < frac-0.0001 & dtb$fraction >= start.frac & !is.na(dtb$real.earnings))
        if (length(I)==0) return(NA)
        dtb2 <- dtb[I,]
        if (min(dtb2$fraction) > start.frac+tol.frac) return(NA)
        E <- dtb2$real.earnings
        sum(E)/length(E)
    }

    PE <- function(frac) P(frac)/E(frac)

    jubilee.mcsapply(dtb$fraction, PE)
}
### <---------------------------------------------------------------------->
