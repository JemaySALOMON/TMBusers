#' Get the Version of the TMBusers Package
#'
#' This function returns the version of the `TMBusers` package.
#'
#' @return A list containing the package version.
#' @author Jemay Salomon
#' 
#' @examples
#' TMBusers.version()
#' @export
TMBusers.version <- function() {
  requireNamespace(package="utils") 
  list(package = utils::packageVersion("TMBusers"))
}


