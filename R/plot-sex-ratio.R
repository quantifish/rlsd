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
plot_sex_ratio <- function(object, scales = "free",
                           xlab = "Fishing year", ylab = "Proportion",
                           figure_dir = "figure/")
{
    data <- object@data
    map <- object@map
    mcmc <- object@mcmc

    seasons <- c("AW", "SS")
    sex <- c("Male", "Immature female", "Mature female")
    sources <- c("LB", "CS")
    n_iter <- nrow(mcmc[[1]])

    if (length(map) > 0) {
        w <- data.frame(LF = 1:data$n_lf,
                        Year = data$data_lf_year_i,
                        Season = data$data_lf_season_i,
                        Source = data$data_lf_source_i,
                        Region = data$data_lf_area_i,
                        Sigma = map$sigma_sex_ratio_i[1,])

        # Observed sex-ratio's
        osexr <- map$data_sex_ratio_out_is
        dimnames(osexr) <- list("Iteration" = 1, "LF" = 1:data$n_lf, "Sex" = sex)
        osexr <- melt(osexr) %>%
            left_join(w, by = "LF") %>%
            mutate(EffN = 1 / Sigma, SD = sqrt(value * (1 - value) / EffN)) %>%
            mutate(Source = sources[Source], Source = factor(Source), Season = seasons[Season]) %>%
            filter(Iteration == 1) %>%
            select(-Iteration)

        # Predicted sex-ratio's
        psexr1 <- map$pred_sex_ratio_is
        dimnames(psexr1) <- list("Iteration" = 1, "LF" = 1:data$n_lf, "Sex" = sex)
        psexr1 <- melt(psexr1) %>%
            left_join(w, by = "LF") %>%
            mutate(Source = sources[Source], Season = seasons[Season])

        rsexr1 <- map$resid_sex_ratio_is
        dimnames(rsexr1) <- list("Iteration" = 1, "LF" = 1:data$n_lf, "Sex" = sex)
        rsexr1 <- melt(rsexr1) %>%
            left_join(w, by = "LF") %>%
            mutate(Source = sources[Source], Season = seasons[Season])
    }

    if (length(mcmc) > 0) {
        w <- data.frame(LF = 1:data$n_lf,
                        Year = data$data_lf_year_i,
                        Season = data$data_lf_season_i,
                        Source = data$data_lf_source_i,
                        Region = data$data_lf_area_i,
                        Sigma = mcmc$sigma_sex_ratio_i[1,])

        # Observed sex-ratio's
        osexr <- mcmc$data_sex_ratio_out_is
        dimnames(osexr) <- list("Iteration" = 1:n_iter, "LF" = 1:data$n_lf, "Sex" = sex)
        osexr <- melt(osexr) %>%
            left_join(w, by = "LF") %>%
            mutate(EffN = 1 / Sigma, SD = sqrt(value * (1 - value) / EffN)) %>%
            mutate(Source = sources[Source], Source = factor(Source), Season = seasons[Season]) %>%
            filter(Iteration == 1) %>%
            select(-Iteration)

        psexr2 <- mcmc$pred_sex_ratio_is
        dimnames(psexr2) <- list("Iteration" = 1:n_iter, "LF" = 1:data$n_lf, "Sex" = sex)
        psexr2 <- melt(psexr2) %>%
            left_join(w, by = "LF") %>%
            mutate(Source = sources[Source], Season = seasons[Season])

        rsexr2 <- mcmc$resid_sex_ratio_is
        dimnames(rsexr2) <- list("Iteration" = 1:n_iter, "LF" = 1:data$n_lf, "Sex" = sex)
        rsexr2 <- melt(rsexr2) %>%
            left_join(w, by = "LF") %>%
            mutate(Source = sources[Source], Season = seasons[Season])
    }

    # sex residuals
    p <- ggplot(rsexr2) +
        geom_hline(yintercept = 0, alpha = 0.2) +
        # expand_limits(y = 0) +
        labs(x = xlab, y = "Standardised residual") +
        theme_lsd()  +
        guides(alpha = FALSE) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

    if (n_iter > 10) {
        p <- p + geom_violin(aes(x = as.factor(.data$Year), y = .data$value, colour = .data$Source, fill = .data$Source, alpha = .data$Sigma)) +
            scale_x_discrete(breaks = seq(0, 1e6, 5)) +
            scale_alpha(range = c(1, 0.5))
    } else {
        p <- p + geom_point(aes(x = .data$Year, y = .data$value, color = .data$Source, alpha = .data$Sigma), cex = 2) +
            scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1)) +
            scale_alpha(range = c(1, 0.5))
    }

    if (data$n_area == 1) {
        p <- p + facet_grid(Sex ~ Season, scales = "free_x")
    } else {
        p <- p + facet_grid(Sex ~ Region + Season, scales = "free_x")
    }

    ggsave(paste0(figure_dir, "sex_ratio_resid.png"), p, height = 9, width = 9)


    if (length(mcmc) > 0) {
        p <- ggplot(data = psexr2, aes(x = .data$Year, y = .data$value))
    } else if (length(map) > 0) {
        p <- ggplot(data = psexr1, aes(x = .data$Year, y = .data$value))
    }

    if (length(mcmc) > 0) {
        p <- p + stat_summary(data = psexr2, aes(x = .data$Year, y = .data$value), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
            stat_summary(data = psexr2, aes(x = .data$Year, y = .data$value), fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
            stat_summary(data = psexr2, aes(x = .data$Year, y = .data$value), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1)
    }

    if (length(map) > 0) {
        p <- p + geom_line(data = psexr1, aes(x = .data$Year, y = .data$value), linetype = 2)
    }

    p <- p + geom_point(data = osexr, aes(x = .data$Year, y = .data$value, color = .data$Source)) +
        geom_linerange(data = osexr, aes(x = .data$Year, ymin = .data$value - .data$SD, ymax = .data$value + .data$SD, color = .data$Source), alpha = 0.75) +
        scale_y_continuous(expand = c(0, 0)) +
        coord_cartesian(ylim = c(0, 1.1)) +
        xlab(xlab) +
        ylab(ylab) +
        theme_lsd() +
        theme(axis.text.x = element_text(angle = 45,hjust = 1))

    if (data$n_area == 1) {
        p <- p + facet_grid(Sex ~ Season, scales = "free_x")
    } else {
        p <- p + facet_grid(Sex ~ Region + Season, scales = "free_x")
    }

    ggsave(paste0(figure_dir, "sex_ratio.png"), p)
}
