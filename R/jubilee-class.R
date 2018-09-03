#' The jubilee class
#'
#' This S4 class stores raw and derived data,
#' channel regression settings and results.
#'
#' @name jubilee-class
#'
#' @slot call the match.call slot.
#' @slot lookback.channel numeric, the look-back channel in years.
#' @slot fwd.rtn.duration numeric, the forward return duration in years.
#' @slot reg.dtb data.table, contains the regression data.
#' @slot dtb data.table, contains the consolidated market data.
#' @slot rate.spread.mean numeric, the mean of the yield spread,
#'                        used to calculate \code{rate.spread.norm} column.
#' @slot create.time \code{POSIXct}, records the creation time of this object.
#'
#' @keywords class
#'
#' @include jubilee-package.R
#'
#' @exportClass jubilee
setClass("jubilee",
         representation(
             call = "call",
             lookback.channel = "numeric",
             fwd.rtn.duration = "numeric",
             reg.dtb = "data.table",
             dtb = "data.table",
             rate.spread.mean = "numeric",
             create.time = "POSIXct"
         ),
         prototype(
             call = call("jubilee")
         )
)
### <---------------------------------------------------------------------->
