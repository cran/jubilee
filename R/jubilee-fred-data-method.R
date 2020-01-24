#' Internal utility to download time series data from FRED
#' 
#' This utility downloads time series from FRED. Many time series that this package uses
#' are available on FRED. Therefore, this utility is used to provide daily or monthly updates
#' by concatenating live data to the internal static data.
#'
#' @param symbol character, the name of the time series
#' @param col_out character, the name of the output closing price column. Default is "Close"
#' @param retry numeric, number of retries on the URL. Default is 3.
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
jubilee.fred_data <- function (symbol, col_out="Close", retry=3)
{

    tmp <- tempfile()
    FRED.URL <- "https://fred.stlouisfed.org/series"
    URL <- paste(FRED.URL, "/", symbol, "/downloaddata/", symbol, ".csv", sep="")
    fetch <- function() {
        utils::download.file(URL, destfile=tmp, quiet=TRUE)
        fr <- utils::read.csv(tmp, na.strings=c(".","NA"))
        unlink(tmp)
        return (fr)
    }
    
    fr <- NULL
    attempt <- 0
    while( is.null(fr) && attempt <= retry ) {
        attempt <- attempt + 1
        Sys.sleep(0.2) # slow down hitting FRED server
        tryCatch(
            fr <- fetch(),
            error = function(e) Sys.sleep(2)
        )
    }
    if (is.null(fr)) stop(sprintf("ERROR: fred_data API failed to fetch %s", symbol))

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
    
