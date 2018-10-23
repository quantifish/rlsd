#' Function to return version number
#'
.onAttach <- function(libname, pkgname)
{
    packageStartupMessage("lsd version: 5.33
Compile date: 2018-10-24
")
}
