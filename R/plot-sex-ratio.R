#' Plot sex-ratio
#'
#' Plot the sex-ratio data and fit to the data.
#'
#' @param object an LSD object
#' @param scales the scales
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param figure_dir the directory to save the figure to
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#' 
plot_sex_ratio <- function(object, scales = "free", xlab = "Fishing year", ylab = "Proportion", figure_dir = "figure/")
{
    data <- object@data
    map <- object@map
    mcmc <- object@mcmc

    years <- data$first_yr:data$last_yr
    seasons <- c("AW", "SS")
    sex <- c("Male", "Immature female", "Mature female")
    sources <- c("LB", "CS")
    n_iter <- nrow(mcmc[[1]])
    
    w <- data.frame(LF = 1:data$n_lf, Year = data$data_lf_year_i, Season = data$data_lf_season_i,
                    Source = data$data_lf_source_i, Region = data$data_lf_area_i, 
                    Sigma = map$sigma_sex_ratio_i[1,])

    # Observed sex-ratio's
    osexr <- map$data_sex_ratio_out_is
    dimnames(osexr) <- list("Iteration" = 1, "LF" = 1:data$n_lf, "Sex" = sex)
    osexr <- reshape2::melt(osexr) %>%
        dplyr::left_join(w, by = "LF") %>%
        dplyr::mutate(EffN = 1 / Sigma, SD = sqrt(value * (1 - value) / EffN)) %>%
        dplyr::mutate(Source = factor(Source), Source = sources[Source], Season = seasons[Season]) %>%
        dplyr::filter(Iteration == 1) %>%
        dplyr::select(-Iteration)

    # Predicted sex-ratio's
    if (length(map) > 0) {
        psexr1 <- map$pred_sex_ratio_is
        dimnames(psexr1) <- list("Iteration" = 1, "LF" = 1:data$n_lf, "Sex" = sex)
        psexr1 <- reshape2::melt(psexr1) %>%
            dplyr::left_join(w, by = "LF") %>%
            dplyr::mutate(Source = sources[Source], Season = seasons[Season])
    }
    
    if (length(mcmc) > 0) {
        psexr2 <- mcmc$pred_sex_ratio_is
        dimnames(psexr2) <- list("Iteration" = 1:n_iter, "LF" = 1:data$n_lf, "Sex" = sex)
        psexr2 <- reshape2::melt(psexr2) %>%
            dplyr::left_join(w, by = "LF") %>%
            dplyr::mutate(Source = sources[Source], Season = seasons[Season])
    }
    
    if (length(mcmc) > 0) {
        p <- ggplot(data = psexr2, aes(x = Year, y = value))
    } else if (length(map) > 0) {
        p <- ggplot(data = psexr1, aes(x = Year, y = value))
    }
    
    if (length(mcmc) > 0) {
        p <- p + stat_summary(data = psexr2, aes(x = Year, y = value), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
          stat_summary(data = psexr2, aes(x = Year, y = value), fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
          stat_summary(data = psexr2, aes(x = Year, y = value), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1)
    }
    
    if (length(map) > 0) {
        p <- p + geom_line(data = psexr1, aes(x = Year, y = value), linetype = 2)
    }
    
    p <- p + geom_point(data = osexr, aes(x = Year, y = value, color = Source)) +
        geom_linerange(data = osexr, aes(x = Year, ymin = value - SD, ymax = value + SD, color = Source), alpha = 0.75) +
        scale_y_continuous(expand = c(0, 0)) +
        coord_cartesian(ylim = c(0, 1)) +
        xlab(xlab) + 
        ylab(ylab) +
        theme_lsd()
    
    if (data$n_area == 1) {
        p <- p + facet_grid(Sex ~ Season, scales = "free_x")
    } else {
        p <- p + facet_grid(Sex ~ Season + Region, scales = "free_x")
    }
    
    ggsave(paste0(figure_dir, "sex_ratio.png"), p)
}
