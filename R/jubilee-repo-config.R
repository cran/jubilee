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
                 fmt="%.3f",
                 dtb_colname="gold")

    # TB3MS has some complication, I wish I can extend before 1920.
    rate.tb3ms <- list(file="TB3MS.csv",
                       symbol="TB3MS",
                       daily_symbol="DGS3MO",
                       fmt="%.2f",
                       dtb_colname="rate.tb3ms")
    
    # defines simple FRED monthly symbols
    rate.baa <- list(file="BAA.csv", 
                     symbol="BAA",
                     daily_symbol=NULL,
                     fmt="%.2f",
                     dtb_colname="rate.baa")
    
    unrate <- list(file="UNRATE.csv",
                   symbol="UNRATE",
                   daily_symbol=NULL,
                   fmt="%.1f",
                   dtb_colname="unrate")
    
    monthly.fred = list(unrate=unrate, rate.baa=rate.baa)
    
    return(list(ie=ie, schwert=schwert, 
                gold=gold,
                rate.tb3ms=rate.tb3ms,
                monthly.fred=monthly.fred))
}
### <---------------------------------------------------------------------->
