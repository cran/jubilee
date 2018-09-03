#' Converter from fraction to daily Date
#'
#' Utility to convert from fraction to daily Date (R's Date object).
#'
#' @param fraction numeric, representing year in fraction convention.
#' 
#' @return array of Date object
#'
#' @keywords utility
#' @include daily2fraction-method.R
#'
#' @author Stephen H. Lihn
#'
#' @export fraction2daily
#'
#' @examples
#'   fraction2daily(2017.038) # 2017-01-15
#'   fraction2daily(2017.125) # 2017-02-15
#'
### <======================================================================>
fraction2daily <- function(fraction) {
    origin = "1970-01-01" # as.POSIXct("1970-01-01", tz="EST")
    if (length(fraction) > 1) return(as.Date(sapply(fraction, fraction2daily), origin=origin))
    yr <- floor(fraction)
    mm <- floor((fraction-yr)*12+1)
    df <- (fraction-yr-(mm-1)/12)*12
    d1 <- as.Date(sprintf("%04d-%02d-01", yr, mm))
    dd <- round(df * .jubi_days_in_month(d1))
    unname(d1 + dd)
}
### <---------------------------------------------------------------------->
