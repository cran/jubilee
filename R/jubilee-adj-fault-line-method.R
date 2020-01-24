#' Adjust the time series by fault lines
#'
#' This utility is used to adjust the time series by the provided fault lines.
#'
#' @param fraction numeric, representing year in fraction convention.
#' @param ts numeric, time series to be adjusted, typically it is \code{log.cape10} or \code{log.cape20}.
#' @param fl the fault line matrix. See \code{jubilee.std_fault_line()} for more detail.
#'           If it is provided as character string,
#'           it will be looked up as the name of data set in the standard fault line library.
#'           If it is provided as numeric array, it will be converted to a matrix.
#' @param months interval in months to ramp up the fault line. Default is 1. 
#'           
#' @return numeric, ts adjusted by fault lines
#'
#' @keywords model
#'
#' @author Stephen H. Lihn
#'
#' @export 
#'
#' @examples
#' \dontrun{
#'   repo <- jubilee.repo(online=FALSE)
#'   dj <- jubilee(repo@ie, 45, 10)@reg.dtb
#'   dj$log.cape10.adj <- jubilee.adj_fault_line(dj$fraction, dj$log.cape10, "r_nom_f10_5ftr_4fl")
#' }
#'
### <======================================================================>
jubilee.adj_fault_line <- function(fraction, ts, fl, months=1) {
    if (is(fl, "character")) fl <- jubilee.std_fault_line(fl)
    if (is(fl, "numeric")) {
        make_matrix <- function(v) {
            m = matrix(v, nrow=length(v)/2, ncol=2, byrow=TRUE)
            colnames(m) <- c("fraction", "shift")
            return(m)
        }
        fl <- make_matrix(fl)
    }
    for (i in 1:NROW(fl)) {
        sht.frac <- fl[i,1]
        shift <- fl[i,2]
        # ts <- ts + ifelse(fraction >= sht.frac, shift, 0) # previous step function implementation
        eps <- 0.001
        J.start <- min(which(fraction >= sht.frac-eps))
        shift_1m <- shift/months
        ts.inc <- rep(0,length(ts))
        for (i in 1:months) {
            J <- J.start+i-1
            if (J <= length(ts.inc)) {
                ts.inc[J] <- shift_1m  
            }
        }
        ts <- ts + cumsum(ts.inc)
    }
    return(ts)
}
### <---------------------------------------------------------------------->
