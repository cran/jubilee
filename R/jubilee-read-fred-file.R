#' Internal utility to read FRED file
#'
#' This utility reads the internal static file, optionally amends with FRED online data,
#' and returns the values of a given symbol.
#'
#' @param fraction numeric, the fraction to return the value. 
#'                 The utility will lookup within a month to find value.
#'                 For debug purpose, set it to \code{NULL}, and the intermediate 
#'                 data table will be returned.
#' @param local_file character, the file name of an internal file.
#'                   For debug purpose, set it to \code{NULL}, and the process will
#'                   initiate the source data from FRED via \code{symbol}, 
#'                   instead of a local file.
#' @param symbol character, the FRED symbol.
#' @param daily_symbol character, the FRED symbol to read daily data that supplements 
#'                     the monthly data. Default is \code{NULL}.
#' @param online logical, whether to fetch online data from FRED. Default is \code{FALSE}.
#' @param period charater, length-1 string indicating the data period of the symbol.
#'               M is monthly, Q is quarterly. Default is \code{M}.
#'
#' @return The values of the symbol, numeric with the same length as \code{fraction}.
#'
#' @keywords data
#'
#' @author Stephen H. Lihn
#'
#' @export jubilee.read_fred_file
#'
#' @importFrom utils read.csv
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom stats na.omit
#' @importFrom stats approxfun
#' @importFrom data.table setkey
#' @importFrom data.table setnames
#' @importFrom data.table data.table
#' @importFrom zoo index
#'
#' @examples
#'   repo <- jubilee.repo(online=FALSE)
#'   a <- jubilee.read_fred_file(repo@ie$fraction, "BAA.csv", "BAA")
#'   tail(a)
#'
### <======================================================================>
jubilee.read_fred_file <- function(fraction, local_file, symbol, 
                                   online=FALSE, daily_symbol=NULL, period="M") {

    fetch_fred_to_dtb <- function(symbol) {
        ts <- jubilee.fred_data(symbol, col_out="Close.2") 
        df.online <- data.table(DATE=zoo::index(ts), Close.2=as.numeric(ts$Close.2))
        df.online <- jubilee.df_set_date_pk(df.online)
        return(df.online)        
    }
    
    df <- data.table() # just a placeholder

    if (!is.null(local_file)) {
        file <- jubilee.locate_file(local_file)  
        df <- data.table(utils::read.csv(file))
        df <- jubilee.df_set_date_pk(df)
    } else {
        df <- fetch_fred_to_dtb(symbol)
        colnames(df) <- c("DATE", symbol)
        df <- jubilee.df_set_date_pk(df)
    }
    
    if (online & is.null(daily_symbol)) {
        print(paste("Fetching online data from FRED for:", symbol))
        df.online <- fetch_fred_to_dtb(symbol)
        df <- merge(df, df.online, all=TRUE)
        df[, c(symbol)] <- ifelse(df[["DATE"]] < as.Date("2016-12-31"),
                                  df[[symbol]], df[["Close.2"]])
    }
    
    if (online & !is.null(daily_symbol)) {
        print(paste("Fetching online data from FRED for:", symbol, ",", daily_symbol))
        df.online <- fetch_fred_to_dtb(symbol) # this is monthly data
        df <- merge(df, df.online, all=TRUE)
        max_monthly_date <- max(df$DATE)
        
        df2.online <- fetch_fred_to_dtb(daily_symbol) # this is daily data
        DATE <- NULL
        df2.online <- df2.online[DATE >= max_monthly_date]
        colnames(df2.online) <- c("DATE", "Close.d") # to avoid merge conflict
        df <- merge(df, df2.online, all=TRUE)

        df[, c(symbol)] <- ifelse(df[["DATE"]] < as.Date("2016-12-31"), df[[symbol]], 
                                  ifelse(!is.na(df[["Close.2"]]), df[["Close.2"]], df[["Close.d"]]))
    }
    
    df <- jubilee.df_date2fraction_pk(df)
    
    # ---------------------------------------------------------------
    if (period == "Q") {
        df$fraction <- df$fraction + 1.51/12 # shift to the middle of the quarter
        min.frac <- min(df$fraction)
        max.frac <- max(df$fraction)
        symbol.val <- df[, symbol, with=FALSE][[1]]
        fn <- approxfun(df$fraction, symbol.val)

        fn2 <- function(frac) {
            if (frac >= min.frac & frac <= max.frac) return(fn(frac))
            return(NA)
        }
        return(sapply(fraction, fn2))
    }
    
    # ---------------------------------------------------------------
    if (is.null(fraction)) return(df) # this is mainly used as debug
    
    # get final result for daily and monthly data
    get_value <- function(frac) {
        one.month <- 1/12+0.001
        J <- which(df$fraction <= frac & df$fraction >= frac-one.month)
        if (length(J) > 0) return(df[[max(J), symbol]])
        return(NA)
    }
    return(sapply(fraction, get_value))
}
### <---------------------------------------------------------------------->
jubilee.df_set_date_pk <- function(df) {
    df$DATE <- as.Date(df$DATE)
    DATE <- NULL
    data.table::setkey(df, DATE)
    return(df)
}
### <---------------------------------------------------------------------->
jubilee.df_date2fraction_pk <- function(df) {
    df$fraction <- daily2fraction(df$DATE)
    fraction <- NULL
    data.table::setkey(df, fraction)
    return(df)
}
