#' Internal utility to download time series data from FRED
#' 
#' This utility downloads time series from FRED. Many time series that this package uses
#' are available on FRED. Therefore, this utility is used to provide daily or monthly updates
#' by concatenating live data to the internal static data.
#'
#' @param symbol character, the name of the time series
#' @param col_out character, the name of the output closing price column. Default is "Close"
#'
#' @return The \code{xts} object for the time series
#'
#' @keywords data
#'
#' @export
#'
#' @importFrom utils download.file
#' @importFrom utils read.csv
#' @importFrom stats na.exclude
#'
#' @examples
#' \dontrun{
#'    jubilee.fred_data("VIXCLS") # VIX
#' }
#'
### <======================================================================>
jubilee.fred_data <- function (symbol, col_out="Close")
{

    tmp <- tempfile()
    FRED.URL <- "https://fred.stlouisfed.org/series"
    URL <- paste(FRED.URL, "/", symbol, "/downloaddata/", symbol, ".csv", sep="")
    utils::download.file(URL, destfile=tmp, quiet=TRUE)
    fr <- utils::read.csv(tmp, na.strings=".")
    unlink(tmp)

    ts <- xts(as.matrix(fr[,-1]),
              as.Date(fr[,1],origin='1970-01-01'),
              src='FRED', symbol=symbol,
              updated=Sys.time())
    
    dim(ts) <- c(NROW(ts),1)
    colnames(ts) <- col_out
    ts <- stats::na.exclude(ts)
    return (ts)
}
### <---------------------------------------------------------------------->
    
