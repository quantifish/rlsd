#' Function to return version number
#' 
#' @param ... additional stuff passed
#'
.onAttach <- function(...)
{
	#lsdLib <- dirname(system.file(package = "lsd"))
	#pkgdesc <- packageDescription("lsd", lib.loc = lsdLib)
	#gitrev <- substring(git_head(), 0, 12)
	#gitrev <- 1
    packageStartupMessage("lsd version: 5.46
                           Compile date: 2018-11-06")
    #packageStartupMessage(paste("lsd (Version ", 
    #	pkgdesc$Version, 
    #	", GitRev: ", 
    #	gitrev, 
    #	")", sep = ""))
}
