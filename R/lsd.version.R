#' Function to return version number
#'
.onAttach <- function(libname, pkgname)
{
    packageStartupMessage("lsd version: 5.42
Compile date: 2018-11-05
")
}
