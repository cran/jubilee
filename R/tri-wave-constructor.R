#' Constructor of \code{tri.wave} class
#'
#' Construct an \code{tri.wave} object to simulate the triangular wave model.
#'
#' @return an object of \code{tri.wave} class
#'
#' @keywords constructor
#'
#' @author Stephen H. Lihn
#'
#' @export
#'
#' @examples
#' w <- tri.wave()
#'
### <======================================================================>
"tri.wave" <- function()
{
    call <- match.call()
    w <- new("tri.wave",
             call = call)
    invisible(w)
}
### <---------------------------------------------------------------------->
