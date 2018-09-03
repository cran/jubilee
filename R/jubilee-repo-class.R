#' The jubilee repository class
#'
#' This S4 class stores the raw data for the jubilee package
#'
#' @name jubilee.repo-class
#'
#' @slot call The match.call slot
#' @slot ie        data.table, contains the combined data from \code{ie.raw}, \code{ws}, and \code{inflation}.
#' @slot yield.inversion numeric, the fractions of yield curve inversion
#' @slot raw.ie    data.table, contains the data from \code{ie_data.xls} of Robert Shiller
#' @slot ws        data.table, contains the historical market return data from William Schwert
#' @slot inflation data.table, contains the historical inflation data from Minneapolis FED
#' @slot comm.int  data.table, contains the historical commercial interest rate
#' @slot tb3ms     data.table, contains the historical 3-month Treasury bill rate
#' @slot gold      data.table, contains the historical monthly gold prices
#' @slot gold2     data.table, contains the historical annual gold prices
#' @slot create.time \code{POSIXct}, records the creation time of this object.
#'
#' @keywords class
#'
#' @include jubilee-package.R
#'
#' @exportClass jubilee.repo
#'
setClass("jubilee.repo",
         representation(call = "call",
                        ie = "data.table",
                        yield.inversion = "numeric",
                        raw.ie = "data.table",
                        ws = "data.table",
                        inflation = "data.table",
                        comm.int = "data.table",
                        tb3ms = "data.table",
                        gold = "data.table",
                        gold2 = "data.table",
                        create.time = "POSIXct"),
         prototype(call = call("jubilee.repo"))
)
### <---------------------------------------------------------------------->
