#' Function to return version number
#'
#' @param libname library name
#' @param pkgname package name
#'
.onAttach <- function(libname, pkgname)
{
    packageStartupMessage("lsd version: 5.46
Compile date: 2018-11-06
")
}
