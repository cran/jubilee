#' The triangular wave model class
#'
#' This S4 class defines the parameters in the triangular wave model.
#'
#' @name tri.wave class
#'
#' @slot call the match.call slot.
#' @slot a.t numeric, the look-back channel in years
#' @slot a0 numeric, the look-back channel in years
#' @slot s1 numeric, the forward return duration in years
#' @slot s2 numeric, the start fraction of in-sample training period
#' @slot y.mean numeric, the end fraction of in-sample training period
#' @slot y.amp numeric, the end fraction of in-sample training period
#' @slot y.t numeric, the end fraction of in-sample training period
#' @slot y.p numeric, the end fraction of in-sample training period
#'
#' @keywords class
#'
#' @include jubilee-package.R
#'
#' @references See Section 4 of Stephen H.T. Lihn, "Jubilee Tectonic Model:
#'     Forecasting Long-Term Growth and Mean Reversion in the U.S. Stock Market."
#'     Available at \url{http://dx.doi.org/10.2139/ssrn.3156574}
#'
#' @exportClass jubilee
setClass("tri.wave",
         representation(
             call = "call",
             a.t = "numeric",
             a0 = "numeric",
             s1 = "numeric",
             s2 = "numeric",
             y.mean = "numeric",
             y.amp = "numeric",
             y.t = "numeric",
             y.p = "numeric"
         ),
         prototype(
             call = call("tri.wave"),
             a.t = 1939.99,
             a0 = 8.586,
             s1 = 7.13/100,
             s2 = 10.52/100,
             y.mean = 0.046,
             y.amp = 0.46,
             y.t = 1950.76,
             y.p = 39.07
         )
)
### <---------------------------------------------------------------------->
