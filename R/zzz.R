#' Function to return version number
#' 
#' @param ... additional stuff passed
#' @import utils
#'
.onAttach <- function(...)
{
	#gitrev <- substring(git_head(), 0, 12)
	gitrev <- 1
	#git2r::head()
	pkgdesc <- utils::packageDescription("lsd")
    packageStartupMessage(paste("lsd (Version ", 
    	pkgdesc$Version, 
    	", GitRev: ", 
    	gitrev, 
    	", Date: ", 
    	pkgdesc$Date, 
    	")", sep = ""))
}
