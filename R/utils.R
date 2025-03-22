##' Get the Version of the tmbExtract Package
##'
##' This function returns the version of the `tmbExtract` package.
##'
##' @return A list containing the package version.
##' @author Jemay Salomon
##' 
##' @examples
##' tmbExtract.version()
##' @export
tmbExtract.version <- function() {
  list(package = packageVersion("tmbExtract"))
}


