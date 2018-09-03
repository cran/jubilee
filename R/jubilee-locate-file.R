#' Internal utility to locate static file
#'
#' This utility returns the path to internal file
#'
#' @param local_file character, the file name of an internal file.
#' @param stop logical, whether to stop if file can't be located. Default is \code{TRUE}.
#'
#' @return The path to the file, or else, an empty string
#'
#' @keywords data
#'
#' @author Stephen H. Lihn
#'
#' @export jubilee.locate_file
#'
#' @examples
#'   jubilee.locate_file("UNRATE.csv")
#'
### <======================================================================>
jubilee.locate_file <- function(local_file, stop=TRUE)
{
    # find the sample data location inside package
    f1 <- system.file("extdata", local_file, package = "jubilee")
    if(length(f1) > 0 & file.exists(f1)) return(f1)
    
    # during development, this is where it is!
    f2 <- system.file("inst", "extdata", local_file, package = "jubilee")
    if(length(f2) > 0 & file.exists(f2)) return(f2)
    
    if (stop) {
        stop(paste("Failed to locate internal file:",local_file))
    }
    
    return("")
}
