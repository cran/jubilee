#' Methods of triangular wave model
#'
#' Methods of triangular wave model
#'
#' @param object the object of \code{tri.wave} class
#' @param t the time vector in fraction
#' @param p the period of the triangle wave
#'
#' @return numeric
#'
#' @keywords model
#'
#' @author Stephen H. Lihn
#'
#' @export tri.wave.s
#' @export tri.wave.a
#' @export tri.wave.y
#' @export tri.wave.x
#' @export tri.wave.logr
#' @export tri.wave.logr.semi
#' @export tri.wave.logr.quarter
#' @export tri.wave.logr.y
#' @export triangle
#'
#' @references See Section 4 of Stephen H.T. Lihn, "Jubilee Tectonic Model:
#'     Forecasting Long-Term Growth and Mean Reversion in the U.S. Stock Market."
#'     Available at \url{http://dx.doi.org/10.2139/ssrn.3156574}
#'
#' @examples
#'   w <- tri.wave()
#'   t <- seq(1900, 2000, by=1)
#'   tri.wave.y(w, t)
#'
### <======================================================================>
triangle <- function(t, p) {
    2/pi*asin(sin(2*pi*t/p))
}
# ------------------
#' @rdname triangle
tri.wave.s <- function(object, t) {
    t0 <- object@a.t
    ifelse(t < t0, object@s1, object@s2)
}
# ------------------
#' @rdname triangle
tri.wave.a <- function(object, t) {
    t0 <- object@a.t
    tri.wave.s(object, t)*(t-t0) + object@a0
}
# ------------------
#' @rdname triangle
tri.wave.y <- function(object, t) {
    object@y.amp * triangle(t-object@y.t, object@y.p) + object@y.mean
}
# ------------------
#' @rdname triangle
tri.wave.x <- function(object, t) tri.wave.a(object, t) + tri.wave.y(object, t)
# ------------------
#' @rdname triangle
tri.wave.logr.y <- function(object, t, p) {
    (tri.wave.y(object, t)-tri.wave.y(object, t-p))/p + tri.wave.s(object, t)
}
# ------------------
#' @rdname triangle
tri.wave.logr <- function(object, t, p) {
    (tri.wave.x(object, t)-tri.wave.x(object, t-p))/p
}
# ------------------
#' @rdname triangle
tri.wave.logr.semi <- function(object, t) {
    triangle(t-object@y.t, object@y.p) * 4*object@y.amp/object@y.p + tri.wave.s(object,t)
}
# ------------------
#' @rdname triangle
tri.wave.logr.quarter <- function(object, t) {
    mod <- function(x) ifelse(x>=1,1,ifelse(x<=-1,-1,x))
    mod(triangle(t-object@y.t+object@y.p/8, object@y.p)*2) *4*object@y.amp/object@y.p + tri.wave.s(object,t)
}
### <---------------------------------------------------------------------->
