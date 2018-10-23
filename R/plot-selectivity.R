#' Plot selectivity
#'
#' @param object an S4 object
#' @param xlab the x axis label
#' @param figure_dir the directory to save the figure to
#' @export
#' 
plot_selectivity <- function(object, xlab = "Size (mm)", figure_dir = "figure/")
{
    data <- object@data
    mcmc <- object@mcmc
    map <- object@map
    
    years <- data$first_yr:data$last_proj_yr

    w <- data$which_sel_rsy
    dimnames(w) <- list("Region" = object@regions, "Sex" = object@sex, "Year" = years)
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
        p <- ggplot(data = sel2, aes(x = Size, y = Selectivity, col = Year, fill = Year))
    } else if (!is.null(sel1)) {
        p <- ggplot(data = sel1, aes(x = Size, y = Selectivity, col = Year, fill = Year))
    }
    
    if (!is.null(sel2)) {
        p <- p + stat_summary(data = sel2, aes(x = Size, y = Selectivity, col = Year), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
            stat_summary(data = sel2, aes(x = Size, y = Selectivity, col = Year), fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
            stat_summary(data = sel2, aes(x = Size, y = Selectivity, col = Year), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1)
    }
    if(length(unique(sel2$Year))==1) p <- p + guides(colour = FALSE, fill = FALSE)
    
    if (!is.null(sel1)) {
        p <- p + geom_line(data = sel1, aes(x = Size, y = Selectivity), linetype = 2)
    }
    
    if (data$n_area > 1) {
        p <- p + facet_grid(Region ~ Sex)
    } else {
        p <- p + facet_grid( ~ Sex)
    }
    
    p <- p + #scale_x_continuous(breaks = seq(30, 90, 10)) +
        expand_limits(y = c(0, 1)) +
        xlab(xlab) +
        theme_lsd()
        
    ggsave(paste0(figure_dir, "selectivity.png"), p)
}
