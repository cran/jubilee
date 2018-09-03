#' Converter from daily Date to fraction
#'
#' Utility to convert from daily Date (R's Date object) to fraction.
#'
#' @param d array of Date object, or string in ISO \code{yyyy-mm-dd} format
#' 
#' @return numeric, year in fraction convention
#'
#' @keywords utility
#'
#' @author Stephen H. Lihn
#'
#' @export daily2fraction
#'
#' @importFrom zoo yearmon
#' @importFrom zoo as.Date
#'
#' @examples
#'   daily2fraction(as.Date("2017-01-15")) # 2017.038
#'   daily2fraction(as.Date("2017-02-14")) # 2017.122
#'   daily2fraction(as.Date("2017-07-15")) # 2017.538
#'
### <======================================================================>
daily2fraction <- function(d) {
    d <- as.Date(d)
    fm <- (.jubi_get_day(d)-1)/.jubi_days_in_month(d)
    f <- .jubi_get_year(d) + (.jubi_get_month(d)-1)/12 + fm/12
    unname(f)
}
### <---------------------------------------------------------------------->
# internal utility for daily2fraction and fraction2daily
.jubi_get_day   <- function(d) as.numeric(format(d,"%d"))
.jubi_get_month <- function(d) as.numeric(format(d,"%m"))
.jubi_get_year  <- function(d) as.numeric(format(d,"%Y"))
#
.jubi_days_in_month <- function(d) {
    rigor_days_in_month <- function(d) {
        m <- zoo::as.yearmon(d)
        .jubi_get_day(zoo::as.Date(m, frac = 1))
    }
    mm <- .jubi_get_month(d)
    ifelse(mm %in% c(1,3,5,7,8,10,12), 31,
        ifelse(mm != 2, 30, 
               rigor_days_in_month(d)))
}
### <---------------------------------------------------------------------->
