#' Constructor of \code{jubilee.repo} class
#'
#' Construct a \code{jubilee.repo} class by combining data from that of Robert Shiller
#' since 1871, historical stock market data from 1802 to 1987 by William Schwert,
#' 3-month Treasury bill rate, gold price, and several other economic time series from FRED.
#' Optionally, this function can fetch more recent data from the website of Robert Shiller and
#' Federal Reserve FRED website if the R session has connection to the internet.
#'
#' @param online logical, indicating whether to fetch data from online resource or not. Default is \code{TRUE}.
#'
#' @return An object of \code{jubilee.repo} class
#'
#' @keywords constructor
#'
#' @author Stephen H. Lihn
#'
#' @export jubilee.repo
#' 
#' @importFrom utils download.file
#' @importFrom utils read.csv
#' @importFrom utils head
#' @importFrom utils tail
#' @importFrom stats na.omit
#' @importFrom readxl read_excel
#' @importFrom data.table setkey
#' @importFrom data.table setnames
#' @importFrom data.table data.table
#' @importFrom data.table shift
#' @importFrom zoo index
#'
#' @examples
#'   repo <- jubilee.repo(online=FALSE)
#'   dtb <- repo@ie
#'   tail(dtb,1)
#'
### <======================================================================>
"jubilee.repo" <- function(online=TRUE)
{
    call <- match.call()
    conf <- jubilee.repo.config()
    
    read_csv_to_data_table <- function(file_name) {
        file <- jubilee.locate_file(file_name) # the internal file
        data.table(utils::read.csv(file))
    }

    # -------------------------------------------------------
    # read historical data from Schwert's data file
    file <- jubilee.locate_file(conf$schwert$file)
    
    options(stringsAsFactors = FALSE)
    ws <- suppressWarnings(readxl::read_excel(file, sheet=2, col_types="numeric"))
    inflation <- suppressWarnings(readxl::read_excel(file, sheet=3, col_types="numeric"))
    # commercial interest rate that can be used to supplement the TB3MS prior to 1934
    comm.int <- suppressWarnings(readxl::read_excel(file, sheet=4, skip=9, col_types="numeric"))

    # -------------------------------------------------------
    # read Shiller's data
    file <- tempfile(fileext = ".xls")
    if (online) {
        utils::download.file(conf$ie$url, destfile=file)
    } 
    else {
        # use internal file if not online
        file <- jubilee.locate_file(conf$ie$file)
    }
    
    if (!(length(file) > 0 & file.exists(file))) stop(paste("Failed to locate file:",file))
    options(stringsAsFactors = FALSE)
    raw.ie <- suppressWarnings(readxl::read_excel(file, sheet=3, skip=7, col_types="numeric"))
    raw.ie <- data.table::data.table(raw.ie)
    data.table::setnames(raw.ie, "Rate GS10", "Rate.GS10")
    raw.ie <- stats::na.omit(raw.ie, cols="Date")

    print(sprintf("Maximum date in ie.data is %.2f", max(raw.ie$Date,na.rm=TRUE)))
    
    # -------------------------------------------------------
    # column type validation and conversion
    ws <- .enrich_ws_data(ws, inflation)
    ie <- .enrich_ie_data(raw.ie, ws)
    
    # -------------------------------------------------------
    # read TB3MS, Treasury Bill 3m interest rate
    tb3c <- conf$rate.tb3ms
    tb3ms_ts <- jubilee.read_fred_file(ie$fraction, tb3c$file, tb3c$symbol, online=online, daily_symbol=tb3c$daily_symbol)
    tb3ms <- data.table(fraction = ie$fraction, Close = tb3ms_ts)

    # add TB3MS to ie
    get_3m_rate <- function(frac) {
        one.month <- 1/12+0.001
        J <- which(tb3ms$fraction <= frac & tb3ms$fraction >= frac-one.month)
        if (length(J) > 0) return(tb3ms$Close[max(J)])
        # comm.int doesn't seem to align well with the TB3MS
        # J <- which(comm.int$Year == floor(frac))
        # if (length(J) > 0) return(comm.int$Interest[max(J)])
        return(NA)
    }
    ie[, c(tb3c$dtb_colname)] <- sapply(ie$fraction, get_3m_rate)
    
    # If TB3MS is lagging in the latest month, just copy previous month
    max_epoch <- max(ie$epoch)
    I <- which(ie$epoch==max_epoch)
    if (is.na(ie$rate.tb3ms[I])) {
        J <- which(ie$epoch==max_epoch-1)
        ie[I,]$rate.tb3ms <- ie[J,]$rate.tb3ms
    }

    # -------------------------------------------------------
    # read Gold prices from the monthly and annual files
    gc <- conf$gold
    gold2 <- read_csv_to_data_table(gc$annual_file) # annual data (Year,Close)
    # monthly data from GOLDAMGBD228NLBM (fraction,Close)
    gold_ts <- jubilee.read_fred_file(ie$fraction, gc$file, gc$symbol, online=online, daily_symbol=gc$daily_symbol)
    gold <- data.table(fraction = ie$fraction, Close = gold_ts)
    
    # add Gold to ie
    get_gold_price <- function(frac) {
        one.month <- 1/12+0.001
        J <- which(gold$fraction <= frac & gold$fraction >= frac-one.month & !is.na(gold$Close))
        if (length(J) > 0) return(gold$Close[max(J)])
        J2 <- which(gold2$Year == floor(frac))
        if (length(J2) > 0) return(gold2$Close[max(J2)])
        return(NA)
    }
    ie[, c(gc$dtb_colname)] <- sapply(ie$fraction, get_gold_price)

    # -------------------------------------------------------
    # read unemployment rate (unrate) from the monthly files
    cm <- conf$monthly.fred$unrate
    ie[, c(cm$dtb_colname)] <- jubilee.read_fred_file(ie$fraction, cm$file, cm$symbol, online=online, daily_symbol=cm$daily_symbol)

    # -------------------------------------------------------
    # read BAA from the monthly files
    cm <- conf$monthly.fred$rate.baa
    ie[, c(cm$dtb_colname)] <- jubilee.read_fred_file(ie$fraction, cm$file, cm$symbol, online=online, daily_symbol=cm$daily_symbol)

    # -------------------------------------------------------
    yield.inversion = c(
        1920.75, 1929.5, # 1953.25,
        1957.1, 1960, 1966.5, 1969.5, 1973.75,
        1980, 1981, 1989.5, 2001, 2007)

    # -------------------------------------------------------
    # store in the class object
    r <- new("jubilee.repo",
            call = call,
            ie = ie,
            ws = ws,
            yield.inversion = yield.inversion,
            raw.ie = data.table(raw.ie),
            inflation = data.table(inflation),
            comm.int = data.table(comm.int),
            tb3ms = data.table(tb3ms),
            gold = data.table(gold),
            gold2 = data.table(gold2),
            create.time = Sys.time()
    )

    invisible(r)
}
### <---------------------------------------------------------------------->
.enrich_ws_data <- function(ws, inflation) {
    
    if(NCOL(ws)!=13) stop("ws: number of columns is not 13")
    
    Year <- NULL # mask CMD check warning in subset
    get_ws_cpi <- function(yr) subset(inflation, Year==floor(yr))$Annual.Avg
    
    years <- 113-1
    df1 <- matrix(nrow=years*12, ncol=5, byrow=TRUE)
    for (i in 1:years) {
        yr <- as.numeric(ws[i,1])
        for (j in 1:13) {
            a <- ws[i,j]
            b <- as.character(suppressWarnings(as.numeric(a)))
            if (grepl("\\.", a)) a <- sub("0+$","",a)
            if (is.na(b) | a != b) stop(paste("ws: cell conversion failed", yr, colnames(ws)[j], i, j, a, b))

            if (j >= 2) {
                r <- (i-1)*12+(j-1)
                df1[r,1] <- (yr-1800)*12+(j-1)
                df1[r,2] <- yr+(j-1)/100
                df1[r,3] <- yr+(j-2)/12+1/24
                df1[r,4] <- as.numeric(a)
                df1[r,5] <- get_ws_cpi(yr)
            }
        }
    }
    
    df1 <- data.table(df1)
    colnames(df1) <- c("epoch", "date", "fraction", "tri.ws", "cpi.ws")
    # derive 
    df1$log.tri.ws <- log(df1$tri.ws)
    df1$logrtn.1m.ws <- c(0,diff(log(df1$tri.ws)))
    epoch <- NULL # for R CMD check
    data.table::setkey(df1, epoch)
    
    if (max(floor(df1$fraction)) != 1913) stop("ws: Failed to assert max year is 1913")
    return(df1) # return the new ws table
}
### <---------------------------------------------------------------------->
.enrich_ie_data <- function(raw.ie, ws) {
    
    date2epoch <- function(d) (floor(d)-1800)*12+((d*100) %% 100)-1
    
    raw.ie$epoch <- date2epoch(raw.ie$Date)
    epoch <- NULL # for R CMD check
    data.table::setkey(raw.ie, epoch)
    
    ci <- which(colnames(raw.ie)==c("Date","Fraction"))
    colnames(raw.ie)[ci] <- c("date","fraction")
    ie <- merge(raw.ie, ws, all=TRUE)

    # combine date and fraction columns
    # although raw.ie has upper case column, we want all lower cases
    data.table::setnames(ie, "date.x", "date")
    data.table::setnames(ie, "fraction.x", "fraction")
    ie$date <- ifelse(is.na(ie$date), ie$date.y, ie$date)
    ie$fraction <- ifelse(is.na(ie$fraction), ie$fraction.y, ie$fraction)
    # remove the following two columns
    ie$date.y = NULL
    ie$fraction.y = NULL

    rename.col <- function(c.from, c.to) {
        ci <- which(colnames(ie)==c.from)
        stopifnot(length(ci)==1)
        colnames(ie)[ci] <<- c.to
    }
    
    # rename a few columns to make them clear
    rename.col("P", "price")
    rename.col("D", "dividend")
    rename.col("E", "earnings")
    rename.col("Price", "real.price")
    rename.col("Dividend", "real.dividend")
    rename.col("Earnings", "real.earnings")
    rename.col("CPI", "cpi")
    rename.col("CAPE", "cape10")
    rename.col("Rate.GS10", "rate.gs10")
    
    # patch NA dividend and earning cells in the recent months so that log.tri won't be NA
    max.frac <- max(ie$fraction)
    I <- which(ie$fraction > max.frac-1 & is.na(ie$dividend))
    J <- max(which(ie$fraction > max.frac-1 & !is.na(ie$dividend)))
    ie$dividend[I] <- ie$dividend[J]
    
    I <- which(ie$fraction > max.frac-1 & is.na(ie$earnings))
    J <- max(which(ie$fraction > max.frac-1 & !is.na(ie$earnings)))
    ie$earnings[I] <- ie$earnings[J]
    
    # adds total return and CPI data before 1871
    ie$price <- ifelse(ie$date < 1871, ie$tri.ws*9.13, ie$price)
    ie$dividend <- ifelse(ie$date < 1871, 0, ie$dividend)
    ie$cpi <- ifelse(ie$fraction < 1871, ie$cpi.ws/36*12.4, ie$cpi)
    
    # derive logr for one month total return, and logTR for cumulative total return
    ie$logrtn.1m <- log(ie$price+data.table::shift(ie$dividend/12)) - log(data.table::shift(ie$price))
    ie$logrtn.1m[1] <- 0
    
    ie$log.tri <- cumsum(ie$logrtn.1m)
    if (is.na(utils::tail(ie$log.tri,1))) stop("Error: The last cell of log.tri is NA")
    
    # real log total return
    log.cpi <- log(ie$cpi)
    ie$real.log.tri <- ie$log.tri - log.cpi + utils::head(log.cpi,1)
    
    return(ie)
}
### <---------------------------------------------------------------------->
