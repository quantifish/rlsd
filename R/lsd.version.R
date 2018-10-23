#' Function to return version number
#'
.onAttach <- function(libname, pkgname)
{
    packageStartupMessage("lsd version: 5.1
Compile date: 2018-10-24
")
}
