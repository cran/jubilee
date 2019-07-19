#' List of dates for yield curve inversion
#'
#' List of dates for yield curve inversion, generally compliant to the dating
#' of business cycles after WWII in the U.S.. This data is also stored in the
#' \code{yield.inversion} slot in the \code{jubilee.repo} object.
#'
#' @return numeric, in the unit of fraction.
#'
#' @keywords constructor
#'
#' @author Stephen H. Lihn
#'
#' @export jubilee.yield_inversion
#'
#' @examples
#'   jubilee.yield_inversion()
#'
### <======================================================================>
"jubilee.yield_inversion" <- function()
{
    c(
        1920.75, 1929.5 # ,1953.25
        ,1957.1, 1960, 1966.5, 1969.5, 1973.75
        ,1980, 1981, 1989.5, 2001, 2007
        ,2019.6  # precise date for this inversion is yet to be determined
    )
}
### <---------------------------------------------------------------------->
