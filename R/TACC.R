#' Calculate the TACC given a vector of CPUE
#'
#' @export
#' 
TACC <- function(par, cpue)
{
    tacc <- rep(NA, length(cpue))
    # Constant TACC rule
    if (par[1] == 1)
    {
        tacc[] <- par[2]
    }
    # constant CPUE multiplier
    if (par[1] == 2)
    {
        tacc[] <- par[2] * cpue
    }
    # Plateau rule with slope
    if (par[1] == 3) 
    {
        tacc[cpue <= par[2]] <- 0
        tacc[cpue > par[2] & cpue <= par[3]] <- par[5] * ((cpue[cpue > par[2] & cpue <= par[3]] - par[2]) / (par[3] - par[2]))
        tacc[cpue > par[3] & cpue <= par[4]] <- par[5]
        tacc[cpue > par[4]] <- (cpue[cpue > par[4]] - par[4]) * 0.5 * par[5] / (par[6] - par[4]) + par[5]
    }
    # Plateau rule with step function
    if (par[1] == 4) 
    {
        tacc[cpue <= par[2]] <- 0
        tacc[cpue > par[2] & cpue <= par[3]] <- par[5] * ((cpue[cpue > par[2] & cpue <= par[3]] - par[2]) / (par[3] - par[2]))
        tacc[cpue > par[3] & cpue <= par[4]] <- par[5]
        tacc[cpue > par[4]] <- par[5] * ( (1 + par[7])^as.integer(((cpue[cpue > par[4]] - par[4])/par[6]) + 1) )
    }
    return(tacc)
}