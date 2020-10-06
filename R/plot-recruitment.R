#' Plot recruitment deviations
#' 
#' @param object an lsd object
#' @param xlab x axis label
#' @param ylab y axis label
#' @param figure_dir the directory to save the figure to
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#' 
plot_recruitment_deviations <- function(object,
                                        xlab = "Year",
                                        ylab = "Recruitment deviations",
                                        figure_dir = "figure/")
{
    data <- object@data
    map  <- object@map
    mcmc <- object@mcmc
    
    years <- data$first_rdev_yr:data$last_rdev_yr
    regions <- 1:data$n_area

    if (length(map) > 0) {
        rdevs1 <- map$par_rec_dev_ry
        dimnames(rdevs1) <- list("Iteration" = 1, "Region" = regions, "Year" = years)
        rdevs1 <- reshape2::melt(rdevs1)
    } else {
        rdevs1 <- NULL
    }
    
    if (length(mcmc) > 0) {
        n_iter <- nrow(mcmc[[1]])
        rdevs2 <- mcmc$par_rec_dev_ry
        dimnames(rdevs2) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = years)
        rdevs2 <- reshape2::melt(rdevs2)
    } else {
        rdevs2 <- NULL
    }
    
    if(!is.null(rdevs2)) {
        p <- ggplot(data = rdevs2, aes(x = Year, y = value))
    } else if (!is.null(rdevs1)) {
        p <- ggplot(data = rdevs1, aes(x = Year, y = value))
    }
    
    if (!is.null(rdevs2)) {
        p <- p + stat_summary(data = rdevs2, aes(x = Year, y = value), fun.min = function(x) stats::quantile(x, 0.05), fun.max = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
            stat_summary(data = rdevs2, aes(x = Year, y = value), fun.min = function(x) stats::quantile(x, 0.25), fun.max = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
            stat_summary(data = rdevs2, aes(x = Year, y = value), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1)
    }
    
    if (!is.null(rdevs1)) {
        p <- p + geom_line(data = rdevs1, aes(x = Year, y = value), linetype = 2)
    }
    
    if (data$n_area > 1) p <- p + facet_wrap(~Region)
    
    p <- p + expand_limits(y = 0) +
        xlab(xlab) + ylab(ylab) +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd() +
        theme(axis.text.x = element_text(angle = 45,hjust = 1))
    
    ggsave(paste0(figure_dir, "recruitment_deviations.png"), p)
}


#' Plot recruitment
#' 
#' This plot shows MAP and posterior of recruitment, MAP and posterior
#' of R0, and a vertical dashed line indicates the final model year
#' (after the line is projected recruitment).
#' 
#' @param object an lsd object
#' @param xlab x axis label
#' @param ylab y axis label
#' @param figure_dir the directory to save the figure to
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#' 
plot_recruitment <- function(object,
                             xlab = "Year",
                             ylab = "Recruitment (millions of individuals)",
                             figure_dir = "figure/")
{
    data <- object@data
    map  <- object@map
    mcmc <- object@mcmc
    
    ny <- dim(mcmc$recruits_ry)[3]
    years <- data$first_yr:(data$first_yr + ny - 1)
    regions <- 1:data$n_area

    if (length(map) > 0) {
        recruits1 <- map$recruits_ry
        dimnames(recruits1) <- list("Iteration" = 1, "Region" = regions, "Year" = years)
        recruits1 <- reshape2::melt(recruits1)

        R01 <- map$par_R0_r
        dimnames(R01) <- list("Iteration" = 1, "Region" = regions)
        R01 <- reshape2::melt(R01) %>%
            dplyr::left_join(expand.grid(Iteration = 1, Year = years), by = "Iteration")
    } else {
        recruits1 <- NULL
        R01 <- NULL
    }
    
    if (length(mcmc) > 0) {
        n_iter <- nrow(mcmc[[1]])
        recruits2 <- mcmc$recruits_ry
        dimnames(recruits2) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = years)
        recruits2 <- reshape2::melt(recruits2)

        R02 <- mcmc$par_R0_r
        dimnames(R02) <- list("Iteration" = 1:n_iter, "Region" = regions)
        R02 <- reshape2::melt(R02) %>%
            dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = years), by = "Iteration")
    } else {
        recruits2 <- NULL
        R02 <- NULL
    }
    xmin <- min(recruits1$Year, recruits2$Year)
    xmax <- max(recruits1$Year, recruits2$Year)

    
    if (!is.null(recruits2)) {
        p <- ggplot(data = recruits2, aes(x = Year, y = value))
    } else if (!is.null(recruits1)) {
        p <- ggplot(data = recruits1, aes(x = Year, y = value))
    }
    p <- p + geom_vline(aes(xintercept = data$last_yr), linetype = "dashed")
    if (!is.null(recruits2)) {
        p <- p +
            stat_summary(data = R02, aes(x = Year, y = value/1e+6), fun.min = function(x) stats::quantile(x, 0.05), fun.max = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, fill = "green") +
            stat_summary(data = R02, aes(x = Year, y = value/1e+6), fun.min = function(x) stats::quantile(x, 0.25), fun.max = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA, fill = "green") +
            stat_summary(data = R02, aes(x = Year, y = value/1e+6), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, colour = "green") +
            stat_summary(data = recruits2, aes(x = Year, y = value/1e+6), fun.min = function(x) stats::quantile(x, 0.05), fun.max = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
            stat_summary(data = recruits2, aes(x = Year, y = value/1e+6), fun.min = function(x) stats::quantile(x, 0.25), fun.max = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
            stat_summary(data = recruits2, aes(x = Year, y = value/1e+6), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1)
    }
    if (!is.null(recruits1)) {
        p <- p + geom_line(data = R01, aes(x = Year, y = value/1e+6), linetype = 2, colour = "green") +
            geom_line(data = recruits1, aes(x = Year, y = value/1e+6), linetype = 2)
    }
    p <- p + expand_limits(y = 0) +
        xlab(xlab) + ylab(ylab) +
        #scale_x_continuous(breaks = seq(xmin, xmax, 10), minor_breaks = seq(xmin, xmax, 1)) +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd() +
        theme(axis.text.x = element_text(angle = 45,hjust = 1))
    if (data$n_area > 1) p <- p + facet_wrap(~Region)
    
    ggsave(paste0(figure_dir, "recruitment.png"), p)

    # Time-series analysis of recruitment
    #r_ts = ts(data = rep(dplyr::select(recruits1, value)[,1], each = 2),
    #             frequency = 2, start = c(min(recruits1$Year), 1), end = c(max(recruits1$Year), 1))
    #plot(rdev_ts)
    #fit <- stl(r_ts, s.window = "periodic")
    #fit <- decompose(r_ts)
    #png(paste0(figure_dir, "recruitment_ts_decomposition.png"))
    #plot(fit)
    #dev.off()
}


#' Plot recruitment size
#' 
#' @param object an lsd object
#' @param xlab x axis label
#' @param ylab y axis label
#' @param figure_dir the directory to save the figure to
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @export
#' 
plot_recruitment_size <- function(object, xlab = "Size at recruitment (mm)", ylab = "Proportion", figure_dir = "figure/")
{
    data <- object@data
    mcmc <- object@mcmc
    
    dimnames(mcmc$recruitment_size_dist_l) <- list("Iteration" = 1:nrow(mcmc[[1]]), "Size" = data$size_midpoint_l)
    mcmc$recruitment_size_dist_l <- reshape2::melt(mcmc$recruitment_size_dist_l) %>%
        dplyr::filter(Iteration == 1)
    
    p <- ggplot(data = mcmc$recruitment_size_dist_l, aes(x = Size, y = value)) +
        geom_line() +
        geom_point() +
        expand_limits(y = 0) +
        xlab(xlab) + 
        ylab(ylab) +
        theme_lsd()
    
    ggsave(paste0(figure_dir, "recruitment_size.png"), p)
}
