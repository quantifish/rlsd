#' Plot puerulus
#'
#' @param d and LSD object
#' @param scales free or fixed
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param figure_dir the directory to save to
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#' 
plot_puerulus <- function(d, 
                          scales = "free_y", 
                          xlab = "Fishing year", 
                          ylab = "Puerulus index", 
                          figure_dir = "figure/")
{
    if (d@data$puerulus_on == 1) {
        n_iter <- nrow(d@mcmc[[1]])
    
        # Puerulus index
        opoo <- data.frame(Region = d@data$data_puerulus_area_i, Year = d@data$data_puerulus_year_i, CPUE = d@data$data_puerulus_i, Iteration = NA, Data = "Observed", Type = "Puerulus")
    
        if (length(d@map) > 0) {
            ppoo1 <- d@map$pred_puerulus_i
            dimnames(ppoo1) <- list("Iteration" = 1, "I" = 1:d@data$n_puerulus)
            ppoo1 <- reshape2::melt(ppoo1, value.name = "CPUE") %>%
                dplyr::select(Iteration, CPUE) %>%
                dplyr::mutate(Data = "Expected", Type = "Puerulus", Region = d@data$data_puerulus_area_i, Year = d@data$data_puerulus_year_i)
        } else {
            ppoo1 <- NULL
        }

        if (length(d@mcmc) > 0) {
            ppoo <- d@mcmc$pred_puerulus_i
            dimnames(ppoo) <- list("Iteration" = 1:n_iter, "I" = 1:d@data$n_puerulus)
            ppoo <- reshape2::melt(ppoo, value.name = "CPUE") %>%
                dplyr::select(Iteration, CPUE) %>%
                dplyr::mutate(Data = "Expected", Type = "Puerulus", Region = rep(d@data$data_puerulus_area_i, each = n_iter), Year = rep(d@data$data_puerulus_year_i, each = n_iter))
        } else {
            ppoo <- NULL
        }

        # Plot CPUE
        p <- ggplot(data = ppoo, aes(x = Year, y = CPUE)) +
            geom_point(data = opoo, aes(x = Year, y = CPUE), color = "red") +
            expand_limits(y = 0) +
            xlab(xlab) + ylab(ylab) +
            theme_lsd()

        if (!is.null(ppoo)) {
            p <- p + stat_summary(data = ppoo, fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
                stat_summary(data = ppoo, fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
                stat_summary(data = ppoo, fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1)
        }

        if (!is.null(ppoo1)) p <- p + geom_line(data = ppoo1, aes(x = Year, y = CPUE), linetype = 2)

        if (d@data$n_area > 1) {
            p <- p + facet_wrap(Region ~ Type, scales = scales)
        } else {
            p <- p + facet_wrap( ~ Type, scales = scales)
        }
    
        ggsave(paste0(figure_dir, "puerulus.png"), p)
    } else {
        print("No puerulus data to plot")
    }
}
