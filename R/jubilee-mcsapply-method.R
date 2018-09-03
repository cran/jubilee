#' Wrapper to calculate \code{sapply} using multi-core
#' 
#' This utility calculates \code{sapply} using multi-core capability. 
#' It is a simple wrapper on \code{simplify2array} and \code{parallel::mclapply}. 
#' It is particularly convenient on Linux and Mac when parallelism saves significant amount of computing time.
#'
#' @param x numeric
#' @param FUN the function to be applied to each element of x
#' @param ... optional arguments to \code{FUN}
#'
#' @return numeric
#'
#' @keywords utility
#'
#' @author Stephen H. Lihn
#'
#' @export
#'
#' @importFrom parallel mclapply
#'
#' @examples
#'    a <- seq(1,100)
#'    jubilee.mcsapply(a, function(x) x^2) # use multi-core!
### <======================================================================>
"jubilee.mcsapply" <- function(x, FUN, ...) {
    simplify2array(parallel::mclapply(x, FUN, ...))
}
### <---------------------------------------------------------------------->
