#' @title Plot control rules
#'
#' @author D'Arcy N. Webber, Charles Edwards
#' @param control.pars a matrix of control parameters with columns as parameters
#' and rows as different rules
#' @param obs.cpue the current CPUE
#' @param cpue a vector of the catch per unit effort (CPUE) to be plotted as the x
#' axis
#' @param target.dir a character string of the target directory to save the plot to
#' @param file.suffix a suffix to append to the end of the saved plot name
#' @return a plot
#'
#' @include TACC.R
#' @export
#' 
plot_mp_rules <- function(mp.pars, cpue.values = numeric(), tacc.values = numeric(), cpue = seq(0, 2, 0.01))
{
    # par1  rule type
    # par2  CPUE at TACC = 0
    # par3  CPUE at plateau left
    # par4  CPUE at plateau right
    # par5  plateau height
    # par6  slope
    # par6  step width
    # par7  step height
    # par8  minimum change
    # par9  maximum change
    # par10 latent year switch
    
    invisible(apply(structure(1:dim(mp.pars)[1], dim = dim(mp.pars)[1]), 1, function(i) {
        if ( mp.pars[i,2] >= mp.pars[i,3] )
            stop(paste("par2 < par3 condition not met for rule", i))
        if ( mp.pars[i,3] > mp.pars[i,4] )
            stop(paste("par3 <= par4 condition not met for rule", i))
        }))

    outs <- apply(mp.pars, 1, FUN = TACC, cpue = cpue)
    rules <- melt(outs, varnames = c("cpue", "rule"), value.name = "TACC")
    rules$cpue <- rep(cpue, nrow(mp.pars))

    # If just one rule is being plotted then do that and plot the obs.cpue on the plot too
    if (nrow(mp.pars) == 1)
    {
        d <- data.frame(cpue = cpue.values, tacc = tacc.values)
        p <- ggplot(rules, aes(x = cpue, y = TACC)) +
            geom_line(size = 2) +
            geom_point(data = d, aes(x = cpue, y = tacc), size = 5) +
            theme_bw(base_size = 20) + labs(x = "CPUE (kg/potlift)", y = "TACC (tonnes)", col = "Rule")
    } else {
        p <- ggplot(rules, aes(x = cpue, y = TACC, group = rule, col = rule)) +
            geom_line(size = 2) +
            theme_bw(base_size = 20) + labs(x = "CPUE (kg/potlift)", y = "TACC (tonnes)", col = "Rule")
    }

    p
}