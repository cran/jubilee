#' Constructor of \code{jubilee.repo} class
#'
#' Construct a \code{jubilee.repo} class by combining data from that of Robert Shiller
#' since 1871, historical stock market data from 1802 to 1987 by William Schwert,
#' 3-month Treasury bill rate, gold price, and several other economic time series from FRED.
#' Optionally, this function can fetch more recent data from the website of Robert Shiller and
#' Federal Reserve FRED website if the R session has connection to the internet.
#'
#' @param online logical, indicating whether to fetch data from online resource or not. Default is \code{TRUE}.
#' @param force logical, if FALSE, allowed to retrieve previous object stored in option.
#'              The FALSE setting overrides the online=TRUE setting. Default is \code{TRUE}.
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
#' @importFrom stats lm
#' @importFrom readxl read_excel
#' @importFrom data.table setkey
#' @importFrom data.table setnames
#' @importFrom data.table data.table
#' @importFrom data.table shift
#' @importFrom data.table as.data.table
#' @importFrom zoo index
#' @importFrom zoo yearmon
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom dplyr %>%
#'
#' @examples
#' \dontrun{
#'   repo <- jubilee.repo(online=FALSE)
#'   dtb <- repo@ie
#'   tail(dtb,1)
#' }
#'
### <======================================================================>
"jubilee.repo" <- function(online=TRUE, force=TRUE)
{
    if (! force) {
        r <- getOption("jubilee.repo.object")
        if (! is.null(r)) return(r)
    }
    # -------------------------------------------------------

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
    raw.ie <- suppressMessages(suppressWarnings(
        readxl::read_excel(file, sheet="Data", skip=7, col_types="numeric", progress=FALSE)
        ))
    # message(colnames(raw.ie))
    raw.ie <- data.table::data.table(raw.ie)
    # message(colnames(raw.ie))
    data.table::setnames(raw.ie, "Rate GS10", "Rate.GS10")
    raw.ie <- stats::na.omit(raw.ie, cols="Date")
    
    max_date <- tail(na.omit(raw.ie$Date),1)
    max_spx <- tail(na.omit(raw.ie$P),1)
    message(sprintf("Maximum date in raw ie.data is %.2f and SPX average at %.2f", max_date, max_spx))
    
    # -------------------------------------------------------
    # column type validation and conversion
    ws <- .enrich_ws_data(ws, inflation)

    # -------------------------------------------------------
    ie <- .enrich_ie_data(raw.ie, ws, online)

    if (online) {
        max_date2 <- tail(ie$date,1)
        max_spx2 <- tail(ie$price,1)
        message(sprintf("Maximum date in enriched ie is %.2f and SPX average at %.2f", max_date2, max_spx2))
    }
    
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
    # read GS10, Treasury Bill 10y interest rate
    gs10c <- conf$monthly.fred$rate.gs10
    gs10_ts <- jubilee.read_fred_file(ie$fraction, gs10c$file, gs10c$symbol, online=online, daily_symbol=gs10c$daily_symbol)
    gs10 <- data.table(fraction = ie$fraction, Close = gs10_ts)

    # append GS10 to ie
    get_10y_rate <- function(frac) {
        one.month <- 1/12+0.001
        J <- which(ie$fraction == frac)
        if (length(J) > 0 & is.finite(ie$rate.gs10[J])) return(ie$rate.gs10[J])
        J <- which(gs10$fraction <= frac & gs10$fraction >= frac-one.month)
        if (length(J) > 0) return(gs10$Close[max(J)])
        return(NA)
    }
    ie[, c(gs10c$dtb_colname)] <- sapply(ie$fraction, get_10y_rate)
    
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

    read_fred_by_config <- function(cm) {
        jubilee.read_fred_file(ie$fraction, cm$file, cm$symbol, online=online,
                               daily_symbol=cm$daily_symbol, period=cm$period)
    }

    cm <- conf$monthly.fred$unrate
    ie[, c(cm$dtb_colname)] <- jubilee.read_fred_file(ie$fraction, cm$file, cm$symbol, online=online, daily_symbol=cm$daily_symbol, period=cm$period)

    # -------------------------------------------------------
    # read TCU, total capacity utilization
    cm <- conf$monthly.fred$tcu
    ie[, c(cm$dtb_colname)] <- read_fred_by_config(cm)
    
    # add manufacturing capacity utilization before 01/1967, to align with GDP data
    old_tcu <- data.table(fraction=ie$fraction)
    old_tcu$tcu <- jubilee.read_fred_file(ie$fraction, "CAPUTLB00004SQ.csv", "CAPUTLB00004SQ", period="Q")
    ie$tcu <- ifelse(is.finite(ie$tcu) & ie$fraction > 1967, ie$tcu, old_tcu$tcu)
    
    # -------------------------------------------------------
    # read BAA from the monthly files
    cm <- conf$monthly.fred$rate.baa
    ie[, c(cm$dtb_colname)] <- jubilee.read_fred_file(ie$fraction, cm$file, cm$symbol, online=online, daily_symbol=cm$daily_symbol, period=cm$period)

    # -------------------------------------------------------
    # read Eurodollar 3m rate from the monthly files
    cm <- conf$monthly.fred$rate.ed3ms
    ie[, c(cm$dtb_colname)] <- jubilee.read_fred_file(ie$fraction, cm$file, cm$symbol, online=online, daily_symbol=cm$daily_symbol, period=cm$period)

    # -------------------------------------------------------
    # read FED FUNDS rate from the monthly files
    cm <- conf$monthly.fred$rate.fedfunds
    ie[, c(cm$dtb_colname)] <- jubilee.read_fred_file(ie$fraction, cm$file, cm$symbol, online=online, daily_symbol=cm$daily_symbol, period=cm$period)

    # read GDP data
    cm <- conf$monthly.fred$gdp
    ie[, c(cm$dtb_colname)] <- jubilee.read_fred_file(ie$fraction, cm$file, cm$symbol, online=online, daily_symbol=cm$daily_symbol, period=cm$period)

    cm <- conf$monthly.fred$real.gdp
    ie[, c(cm$dtb_colname)] <- jubilee.read_fred_file(ie$fraction, cm$file, cm$symbol, online=online, daily_symbol=cm$daily_symbol, period=cm$period)

    # corporate profit
    cm <- conf$monthly.fred$cp
    ie[, c(cm$dtb_colname)] <- jubilee.read_fred_file(ie$fraction, cm$file, cm$symbol, online=online, daily_symbol=cm$daily_symbol, period=cm$period)

    # to extrapolate GDP here, INACTIVE: not sure if we want to make assumption
    extrapolate_quarterly <- function(frac, df) {
        N <- 3 # data points to extrapolate
        col.val <- df[, col, with=FALSE][[1]]
        J <- which(! is.na(col.val))
        max.frac <- max(df[J]$fraction)
        df.N <- data.frame(t=tail(df[J]$fraction, N), y=tail(col.val[J], N))
        exp.lm <- lm(y ~ t, data=df.N)
        if (frac > max.frac) return(predict(exp.lm, newdata=data.frame(t=frac)))
        return(NA)
    }

    # -------------------------------------------------------
    # read payroll data
    cm <- conf$monthly.fred$payroll
    ie[, c(cm$dtb_colname)] <- read_fred_by_config(cm)

    # add manufacturing data before 01/1964, to align with GDP data
    old_payroll <- data.table(fraction=ie$fraction)
    old_payroll$payroll <- jubilee.read_fred_file(
        ie$fraction, "CES3000000035.csv", "CES3000000035", period="M")
    old_scale <- 8.5/17.4 # conversion scale, hardcoded, as of 1964/01
    ie$payroll <- ifelse(is.finite(ie$payroll) & ie$fraction > 1964,
        ie$payroll, old_payroll$payroll*old_scale)

    # -------------------------------------------------------
    # read POPTH, population
    cm <- conf$monthly.fred$popth
    ie[, c(cm$dtb_colname)] <- read_fred_by_config(cm)

    # add quarterly data before 01/1959
    old_popth <- data.table(fraction=ie$fraction)
    old_popth$popth <- jubilee.read_fred_file(ie$fraction, "B230RC0Q173SBEA.csv", "B230RC0Q173SBEA", period="Q")
    ie$popth <- ifelse(is.finite(ie$popth) & ie$fraction > 1959, ie$popth, old_popth$popth)

    # recession data
    cm <- conf$monthly.fred$usrec.nber
    ie[, c(cm$dtb_colname)] <- read_fred_by_config(cm)
    
    cm <- conf$monthly.fred$usrec.cp
    ie[, c(cm$dtb_colname)] <- read_fred_by_config(cm)

    # -------------------------------------------------------
    # calculate GS10 logr.1m and log.tri
    gs10_mth_ttl_logrtn <- function(frac) {
        J <- which(ie$fraction == frac)
        y_t1 <- ie[J-1]$rate.gs10/100
        y_t <- ie[J]$rate.gs10/100
        m_t <- 10 # 10 years
        # duration, p.148 of Tuckerman; convexity, p. 150 of Tuckerman
        duration <- 1/y_t*(1-1/(1+0.5*y_t)^(2*m_t))
        convexity <- 2/y_t^2*(1-1/(1+0.5*y_t)^(2*m_t)) - (2*m_t)/(y_t*(1+0.5*y_t)^(2*m_t+1))
        rtn.1m <- y_t1/12 -duration*(y_t-y_t1) +convexity/2*(y_t-y_t1)^2
        log(1+rtn.1m)
    }
    K <- which(is.finite(ie$rate.gs10))
    ie$rate.gs10.logr.1m <- as.numeric(jubilee.mcsapply(ie$fraction, gs10_mth_ttl_logrtn))
    ie$rate.gs10.logr.1m[K[1]] <- 0
    ie$rate.gs10.log.tri[K] <- cumsum(ie$rate.gs10.logr.1m[K]) 

    # -------------------------------------------------------
    gdp_date <- fraction2daily(max(ie[is.finite(ie$real.gdp)]$fraction))
    unrate_date <- fraction2daily(max(ie[is.finite(ie$unrate)]$fraction))

    message(sprintf("Maximum date for unrate is %s and for GDP, %s", unrate_date, gdp_date))

    # -------------------------------------------------------
    # store in the class object
    r <- new("jubilee.repo",
            call = call,
            ie = ie,
            ws = ws,
            yield.inversion = jubilee.yield_inversion(),
            raw.ie = data.table(raw.ie),
            inflation = data.table(inflation),
            comm.int = data.table(comm.int),
            tb3ms = data.table(tb3ms),
            gold = data.table(gold),
            gold2 = data.table(gold2),
            create.time = Sys.time()
    )

    options("jubilee.repo.object"=r)
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
.R_Date2date <- function(d) {
    d2 <- as.numeric(zoo::as.yearmon(d))
    floor(d2) + ((d2 %% 1) * 12 + 1)/100 # YYYY.MM
}
.date2epoch <- function(d) (floor(d)-1800)*12+((d*100) %% 100)-1  # input: YYYY.MM
.R_Date2epoch <- function(d) .date2epoch(.R_Date2date(d))
### <---------------------------------------------------------------------->
.enrich_ie_data <- function(raw.ie, ws, online) {
    
    raw.ie$epoch <- .date2epoch(raw.ie$Date)
    epoch <- NULL # for R CMD check
    data.table::setkey(raw.ie, epoch)
    
    data.table::setnames(raw.ie, "Date", "date")
    data.table::setnames(raw.ie, "Fraction", "fraction")
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

    rename.debug = FALSE
    rename.col <- function(c.from, c.to) {
        ci <- which(colnames(ie)==c.from)
        if (rename.debug) {
            message(ci)
            message(c(c.from, c.to))
            message(colnames(ie)[ci])
        }
        if (length(ci) != 1) stop(sprintf("ERROR: Unable to rename from %s to %s", c.from, c.to))
        colnames(ie)[ci] <<- c.to
    }
    
    # rename a few columns to make them clear
    rename.col("Price...8", "real.price")
    rename.col("Dividend", "real.dividend")
    rename.col("Earnings...11", "real.earnings")
    rename.col("P", "price")
    rename.col("D", "dividend")
    rename.col("E", "earnings")
    rename.col("CPI", "cpi")
    rename.col("CAPE", "cape10")
    rename.col("Rate.GS10", "rate.gs10")
    
    # if online, add SP500 from FRED to current month
    if (online) {
        ie <- .append_sp500_from_fred(ie)
    }

    # patch NA dividend, earning, CPI cells in the recent months so that log.tri won't be NA
    max.frac <- max(ie$fraction)
    
    patch_ie_col <- function(col) {
        I <- which(ie$fraction > max.frac-1 & is.na(ie[[col]]))
        J <- max(which(ie$fraction > max.frac-1 & !is.na(ie[[col]])))
        if (length(I) > 0 & length(J) > 0) ie[I, c(col)] <<- ie[[col]][J]
        # print(col)
        # print(I)
        # print(J)
    }
    
    patch_ie_col("dividend")
    patch_ie_col("earnings")
    patch_ie_col("cpi")
    patch_ie_col("cape10")

    
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
.append_sp500_from_fred <- function(ie.raw) {
    ts <- jubilee.fred_data("SP500")
    df <- data.table(R_Date=index(ts),
        close=as.numeric(ts$Close),
        epoch=.R_Date2epoch(index(ts)),
        date=.R_Date2date(index(ts))
    )
    epoch <- NULL # dummy for CMD check
    mdf <- data.table::as.data.table(
        df %>%
        dplyr::group_by(epoch) %>%
        dplyr::summarize(
            price = mean(close, na.rm=TRUE),
            date = max(date, na.rm=TRUE))
    )
    
    fr <- tail(ie.raw[,c("date", "fraction")], 12)
    fr$month <- round((fr$date %% 1)*100)
    fr$month_frac <- fr$fraction %% 1
    
    calc_fraction_from_date <- function(d) {
        y <- floor(d)
        m <- round((d %% 1)*100)
        as.numeric(y + fr[fr$month == m]$month_frac)
    }

    `%notin%` <- Negate(`%in%`)
    mdf_new <- mdf[mdf$epoch %notin% ie.raw$epoch & mdf$epoch != .R_Date2epoch(Sys.Date())]
    mdf_new$fraction <- calc_fraction_from_date(mdf_new$date)
    
    message(sprintf("Max date in SP500: %.2f", tail(mdf$date,1)))
    if (nrow(mdf_new) > 0) {
        message(mdf_new)
    
        dt2 <- merge(ie.raw, mdf_new, by="epoch", all=TRUE, suffixes=c("", ".y"))
        dt2$date <- ifelse(is.na(dt2$date), dt2$date.y, dt2$date)
        dt2$price <- ifelse(is.na(dt2$price), dt2$price.y, dt2$price)
        dt2$real.price <- ifelse(is.na(dt2$real.price), dt2$price.y, dt2$real.price)
        dt2$fraction <- ifelse(is.na(dt2$fraction), dt2$fraction.y, dt2$fraction)
    
        dt2$date.y = NULL
        dt2$price.y = NULL
        dt2$fraction.y = NULL
    }
    else {
        dt2 <- ie.raw
    }
    
    max_date2 <- tail(dt2$date,1)
    max_spx2 <- tail(dt2$price,1)
    # message(sprintf("DEBUG: Maximum date in enriched ie is %.2f and SPX average at %.2f", max_date2, max_spx2))

    return(dt2)
}
