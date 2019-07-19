#' Configuration of jubilee's data repository
#'
#' This utility stores the data configuration for the jubilee's data repository.
#' This is used internally to provide proper abstraction to the data sources,
#' such as file name, URL, FRED symbol, column name, decimal format, etc.
#'
#' @return The list of data elements and their attributes.
#'
#' @keywords data
#'
#' @author Stephen H. Lihn
#'
#' @export jubilee.repo.config
#'
#' @examples
#'   c <- jubilee.repo.config()
#'   c$ie$url
#'
### <======================================================================>
jubilee.repo.config <- function() {
    ie <- list(url="http://www.econ.yale.edu/~shiller/data/ie_data.xls",
               file="ie_data.xls")
    schwert <- list(file="schwert-w2985-data-only.xlsx")

    gold <- list(file="GOLDAMGBD228NLBM.csv",
                 annual_file="GOLD-annual-1792.csv", # this makes gold not simply FRED
                 symbol="GOLDAMGBD228NLBM",
                 daily_symbol=NULL,
                 period="M",
                 fmt="%.3f",
                 dtb_colname="gold")

    # TB3MS has some complication, I wish I can extend before 1920.
    rate.tb3ms <- list(file="TB3MS.csv",
                       symbol="TB3MS",
                       daily_symbol="DGS3MO",
                       period="M",
                       fmt="%.2f",
                       dtb_colname="rate.tb3ms")
    
    # defines simple FRED symbols, primarily monthly, but some quarterly
    rate.baa <- list(file="BAA.csv", 
                     symbol="BAA",
                     daily_symbol=NULL,
                     period="M",
                     fmt="%.2f",
                     dtb_colname="rate.baa")

    rate.ed3ms <- list(file="IR3TED01USM156N.csv",
                       symbol="IR3TED01USM156N",
                       daily_symbol=NULL,
                       period="M",
                       fmt="%.2f",
                       dtb_colname="rate.ed3ms")

    rate.fedfunds <- list(file="FEDFUNDS.csv",
                          symbol="FEDFUNDS",
                          daily_symbol=NULL,
                          period="M",
                          fmt="%.2f",
                          dtb_colname="rate.fedfunds")

    unrate <- list(file="UNRATE.csv",
                   symbol="UNRATE",
                   daily_symbol=NULL,
                   period="M",
                   fmt="%.1f",
                   dtb_colname="unrate")

    # TCU, total capacity utilization
    # to be supplemented by manufacturing capacity utilization before 01/1967, to align with GDP data
    tcu <- list(file="TCU.csv",
                symbol="TCU",
                daily_symbol=NULL,
                period="M",
                fmt="%.4f",
                dtb_colname="tcu")
    
    # nominal GDP
    gdp <- list(file="GDP.csv",
                symbol="GDP",
                daily_symbol=NULL,
                period="Q",
                fmt="%.3f",
                dtb_colname="gdp")
    
    # real GDP
    real.gdp <- list(file="GDPC1.csv",
                     symbol="GDPC1",
                     daily_symbol=NULL,
                     period="Q",
                     fmt="%.3f",
                     dtb_colname="real.gdp")

    # corp profit
    cp <- list(file="CP.csv",
                     symbol="CP",
                     daily_symbol=NULL,
                     period="Q",
                     fmt="%.3f",
                     dtb_colname="cp")

    # weekly payrolls, total private, since 01/1964
    # to be supplimented with manufacturing data since 1939
    payroll <- list(file="CES0500000035.csv",
                    symbol="CES0500000035",
                    daily_symbol=NULL,
                    period="M",
                    fmt="%.1f",
                    dtb_colname="payroll")

    # POPTHM, population (monthly)
    # to be supplemented by quaterly data before 01/1959
    popth <- list(file="POPTHM.csv",
                  symbol="POPTHM",
                  daily_symbol=NULL,
                  period="M",
                  fmt="%.1f",
                  dtb_colname="popth")

    # recession
    usrec.nber <- list(file="USREC.csv",
                       symbol="USREC",
                       daily_symbol=NULL,
                       period="M",
                       fmt="%.0f",
                       dtb_colname="usrec.nber")

    usrec.cp <- list(file="RECPROUSM156N.csv",
                     symbol="RECPROUSM156N",
                     daily_symbol=NULL,
                     period="M",
                     fmt="%.2f",
                     dtb_colname="usrec.cp")
                  
    # list collection of "simple" FRED time series
    monthly.fred = list(unrate=unrate,
                        tcu=tcu,
                        rate.baa=rate.baa,
                        rate.ed3ms=rate.ed3ms,
                        rate.fedfunds=rate.fedfunds,
                        gdp=gdp,
                        real.gdp=real.gdp,
                        cp=cp,
                        payroll=payroll,
                        popth=popth,
                        usrec.nber=usrec.nber,
                        usrec.cp=usrec.cp)

    return(list(ie=ie,
                schwert=schwert,
                gold=gold,
                rate.tb3ms=rate.tb3ms,
                monthly.fred=monthly.fred))
}
### <---------------------------------------------------------------------->
