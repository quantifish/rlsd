#' Function to return version number
#'
.onAttach <- function(libname, pkgname)
{
    packageStartupMessage("lsd version: 5.11
Compile date: 2018-10-24
")
}
