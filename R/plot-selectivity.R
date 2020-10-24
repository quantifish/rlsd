#' Plot selectivity
#'
#' @param object an S4 object
#' @param xlab the x axis label
#' @param figure_dir the directory to save the figure to
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#' 
plot_selectivity <- function(object, 
                             xlab = "Size (mm)", 
                             figure_dir = "figure/")
{
    data <- object@data
    mcmc <- object@mcmc
    map <- object@map
    
    years <- data$first_yr:data$last_proj_yr
    n_seasons <- data$n_season
    if (n_seasons == 1) seasons = "YR"
    if (n_seasons == 2) seasons <- c("AW", "SS")

    w <- data$which_sel_rsyt
    dimnames(w) <- list("Region" = object@regions, "Sex" = object@sex, "Year" = years, "Season" = seasons)
    w <- reshape2::melt(w, value.name = "Selex")

    if (length(map) > 0) {
        sel1 <- map$selectivity_ml
        dimnames(sel1) <- list("Iteration" = 1, "Selex" = 1:data$n_sel, "Size" = data$size_midpoint_l)
        sel1 <- reshape2::melt(sel1, value.name = "Selectivity") %>%
            dplyr::inner_join(w, by = "Selex") %>%
            dplyr::mutate(Year = factor(Year), Inference = "MAP") %>%
            dplyr::distinct(Iteration, Selectivity, Region, .keep_all = TRUE)
    } else {
        sel1 <- NULL
    }
    
    if (length(mcmc) > 0) {
        sel2 <- mcmc$selectivity_ml
        dimnames(sel2) <- list("Iteration" = 1:nrow(mcmc[[1]]), "Selex" = 1:data$n_sel, "Size" = data$size_midpoint_l)
        sel2 <- reshape2::melt(sel2, value.name = "Selectivity") %>%
            dplyr::inner_join(w, by = "Selex") %>%
            dplyr::mutate(Year = factor(Year), Inference = "MCMC") %>%
            dplyr::distinct(Iteration, Selectivity, Region, .keep_all = TRUE)
    } else {
        sel2 <- NULL
    }
    
    if (!is.null(sel2)) {
        if(data$n_sel > 2 & length(unique(sel2$Year)) == 1) {
            p <- ggplot(data = sel2, aes(x = Size, y = Selectivity, col = Season, fill = Season))
        } else {
            p <- ggplot(data = sel2, aes(x = Size, y = Selectivity, col = Year, fill = Year))
        }
    } else if (!is.null(sel1)) {
        if(data$n_sel > 2 & length(unique(sel2$Year)) == 1) {
            p <- ggplot(data = sel1, aes(x = Size, y = Selectivity, col = Season, fill = Season))
        } else {
            p <- ggplot(data = sel1, aes(x = Size, y = Selectivity, col = Year, fill = Year))
        }
    }
    
    if (!is.null(sel2)) {
        if(data$n_sel > 2 & length(unique(sel2$Year)) == 1) {
            p <- p + stat_summary(data = sel2, aes(x = Size, y = Selectivity, col = Season), fun.min = function(x) stats::quantile(x, 0.05), fun.max = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
                stat_summary(data = sel2, aes(x = Size, y = Selectivity, col = Season), fun.min = function(x) stats::quantile(x, 0.25), fun.max = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
                stat_summary(data = sel2, aes(x = Size, y = Selectivity, col = Season), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1)
        } else {
            p <- p + stat_summary(data = sel2, aes(x = Size, y = Selectivity, col = Year), fun.min = function(x) stats::quantile(x, 0.05), fun.max = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
                stat_summary(data = sel2, aes(x = Size, y = Selectivity, col = Year), fun.min = function(x) stats::quantile(x, 0.25), fun.max = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
                stat_summary(data = sel2, aes(x = Size, y = Selectivity, col = Year), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1)
        }
    }
    if (length(unique(sel2$Year)) == 1 & data$n_sel == 2) {
        p <- p + guides(colour = FALSE, fill = FALSE)
    }
    if (!is.null(sel1)) {
        p <- p + geom_line(data = sel1, aes(x = Size, y = Selectivity), linetype = 2)
    }
    if (data$n_area > 1) {
        p <- p + facet_grid(Region ~ Sex)
    } else {
        p <- p + facet_grid( ~ Sex)
    }
    
    p <- p + 
        #scale_x_continuous(breaks = seq(30, 90, 10)) +
        expand_limits(y = c(0, 1)) +
        xlab(xlab) +
        theme_lsd()

    ggsave(paste0(figure_dir, "selectivity.png"), p)
    
    if (!is.null(sel1)) {
        q <- ggplot(sel1, aes(x = Size, y = Year, height = Selectivity, fill = Sex)) + 
            ggridges::geom_density_ridges(stat = "identity", alpha = .6, color = "white", scale = 0.95) +
            scale_y_discrete(expand = c(0, 0), name = "Selectivity by year") +
            scale_x_continuous(expand = c(0, 0)) +
            theme_lsd() +
            theme(axis.title.x = element_text(hjust = 0.5), axis.title.y = element_text(hjust = 0.5))  
        if (data$n_area > 1) {
           q <- q + facet_wrap(~Region + Season, ncol=1) }
    ggsave(paste0(figure_dir, "selectivity_ridges.png"), q)
    }
}
