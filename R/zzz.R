#' Function to return version number
#' 
#' @param ... additional stuff passed
#' @import utils
#'
.onAttach <- function(...)
{
	#gitrev <- substring(git_head(), 0, 12)
	#git2r::head()
	gitrev <- 1
	pkgdesc <- utils::packageDescription("lsd")
    packageStartupMessage(paste("lsd (Version ", 
    	pkgdesc$Version, 
    	", GitRev: ", 
    	gitrev, 
    	", Date: ", 
    	pkgdesc$Date, 
    	")", sep = ""))
}
