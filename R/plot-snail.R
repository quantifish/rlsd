#' Plot snail trial
#' 
#' @param object an LSD object
#' @param figure_dir the directory to save the figure to
#' @import dplyr
#' @import ggplot2
#' @importFrom utils head tail
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#' 
plot_snail <- function(object,
                       figure_dir = "figure/")
{
    data <- object@data
    mcmc <- object@mcmc

    n_iter <- nrow(mcmc[[1]])
    years <- data$first_yr:data$last_yr
    regions <- 1:data$n_area
    
    F_Fmsy <- mcmc$F_Fmsy_ry
    dimnames(F_Fmsy) <- list(Iteration = 1:n_iter, Region = regions, Year = years)
    F_Fmsy <- reshape2::melt(F_Fmsy, value.name = "F_Fmsy")
    
    ssb <- mcmc$biomass_ssb_jyr
    dimnames(ssb) <- list(Iteration = 1:n_iter, Rule = 1:dim(ssb)[2],
                          Year = data$first_yr:data$last_proj_yr, Region = regions)
    ssb <- reshape2::melt(ssb, value.name = "SSB") %>%
        dplyr::filter(Rule == 1)
    head(ssb)
    
    ssb0 <- mcmc$SSB0_r
    dimnames(ssb0) <- list(Iteration = 1:n_iter, Region = regions)
    ssb0 <- reshape2::melt(ssb0, value.name = "SSB0")
    ssb0

    d <- dplyr::left_join(F_Fmsy, ssb) %>%
        dplyr::left_join(ssb0) %>%
        dplyr::mutate(SSB_SSB0 = SSB / SSB0)
    head(d)
    tail(d)
    
    ssbmsy <- mcmc$SSBmsy_r
    dimnames(ssbmsy) <- list(Iteration = 1:n_iter, Region = regions)
    ssbmsy <- reshape2::melt(ssbmsy, value.name = "SSBmsy") %>%
        dplyr::left_join(ssb0) %>%
        dplyr::mutate(SSBmsy_SSB0 = SSBmsy / SSB0)
    head(ssbmsy)
    
    dmed <- d %>%
        dplyr::group_by(Year, Region, Year) %>%
        dplyr::summarise(F_Fmsy = median(F_Fmsy), SSB = median(SSB), SSB0 = median(SSB0), SSB_SSB0 = median(SSB_SSB0)) %>%
        dplyr::ungroup()
    df_thin <- dplyr::select(dmed, Year, SSB_SSB0, F_Fmsy) %>%
        dplyr::mutate(Year = ifelse(Year %in% c(min(Year), max(Year), seq(0, 1e6, 5)), Year, ""))
    lyr <- dplyr::filter(d, Year %in% 2016) %>%
        dplyr::filter(F_Fmsy > stats::quantile(F_Fmsy, 0.05)) %>%
        dplyr::select(Year, SSB_SSB0, F_Fmsy) %>%
        dplyr::mutate(Year = "")
    head(df_thin)

    top_left <- dplyr::filter(df_thin,
                              SSB_SSB0 < median(ssbmsy$SSBmsy_SSB0),
                              F_Fmsy > 1)
    top_right <- dplyr::filter(df_thin,
                               SSB_SSB0 >= median(ssbmsy$SSBmsy_SSB0),
                               F_Fmsy > 1)
    bottom_left <- dplyr::filter(df_thin,
                                 SSB_SSB0 < median(ssbmsy$SSBmsy_SSB0),
                                 F_Fmsy <= 1)
    bottom_right <- dplyr::filter(df_thin,
                                  SSB_SSB0 >= median(ssbmsy$SSBmsy_SSB0),
                                  F_Fmsy <= 1)
    
    p <- ggplot(data = d) +
        annotate("rect", xmin = stats::quantile(ssbmsy$SSBmsy_SSB0, 0.05), xmax = stats::quantile(ssbmsy$SSBmsy_SSB0, 0.95), ymin = -Inf, ymax = Inf, alpha = 0.125) +
        annotate("rect", xmin = stats::quantile(ssbmsy$SSBmsy_SSB0, 0.25), xmax = stats::quantile(ssbmsy$SSBmsy_SSB0, 0.75), ymin = -Inf, ymax = Inf, alpha = 0.25) +
        geom_hline(yintercept = 1) +
        geom_vline(data = ssbmsy, aes(xintercept = median(SSBmsy_SSB0))) +
        geom_density_2d(data = dplyr::filter(d, Year %in% 2016), aes(x = SSB_SSB0, y = F_Fmsy, colour = ..level..)) +
        scale_colour_gradient(low = "white", high = "red") +
        geom_segment(data = dmed, aes(x = SSB_SSB0, y = F_Fmsy, xend = dplyr::lead(SSB_SSB0), yend = dplyr::lead(F_Fmsy)), arrow = arrow(length = unit(0.2,"cm")), colour = "red") +
        #geom_path(data = dmed, aes(x = SSB_SSB0, y = F_Fmsy, colour = Year), colour = "red", arrow = arrow()) +
        #geom_point(data = dmed, aes(x = SSB_SSB0, y = F_Fmsy, colour = Year), colour = "red") +
        expand_limits(y = 0, x = c(0, 1.01)) +
        scale_y_continuous(expand = c(0, 0)) +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 1, 0.1), minor_breaks = seq(0, 1, 0.05)) +
        labs(x = expression(SSB/SSB[0]), y = expression(paste("Fishing intensity (", F/F[MSY], ")"))) +
        ggrepel::geom_text_repel(
            data = top_left,
            aes(SSB_SSB0, F_Fmsy, label = Year),
            force = 5, min.segment.length = 0, point.padding = 0.5,
            xlim = c(0, median(ssbmsy$SSBmsy_SSB0)),
            ylim = c(1, NA)
        ) +
        ggrepel::geom_text_repel(
            data = top_right,
            aes(SSB_SSB0, F_Fmsy, label = Year),
            force = 15, min.segment.length = 0, point.padding = 0.5, 
            xlim = c(median(ssbmsy$SSBmsy_SSB0), NA),
            ylim = c(1, NA)
        ) +
        ggrepel::geom_text_repel(
            data = bottom_left,
            aes(SSB_SSB0, F_Fmsy, label = Year),
            force = 15, min.segment.length = 0, max.iter = 5000,
            nudge_y = -0.1,
            xlim = c(0, median(ssbmsy$SSBmsy_SSB0)),
            ylim = c(0, 1)
        ) +
        ggrepel::geom_text_repel(
            data = bottom_right,
            aes(SSB_SSB0, F_Fmsy, label = Year),
            force = 5, min.segment.length = 0, point.padding = 0.5,
            xlim = c(median(ssbmsy$SSBmsy_SSB0), NA),
            ylim = c(0, 1)
        ) +
        theme_lsd(base_size = 16) +
        theme(legend.position = "none")
    if (data$n_area > 1) p <- p + facet_wrap(~Region)
    p
    ggsave(file.path(figure_dir, "snail_trail.png"), p)

#    ggplot(df, aes(Bmed, Fmed)) +
#        geom_rect(xmin = -Inf, xmax = 1, ymin = -Inf, ymax = 1, fill = "yellow", alpha = 0.005) +
#        geom_rect(xmin = 1, xmax = Inf, ymin = -Inf, ymax = 1, fill = "green", alpha = 0.005) +
#        geom_rect(xmin = -Inf, xmax = 1, ymin = 1, ymax = Inf, fill = "red", alpha = 0.005) +
#        geom_rect(xmin = 1, xmax = Inf, ymin = 1, ymax = Inf, fill = "yellow", alpha = 0.005) +
#        geom_errorbar(aes(ymin = F25, ymax = F75), alpha = 0.2) +
#        geom_errorbarh(aes(xmin = B25, xmax = B75), alpha = 0.2) +
#        geom_segment(aes(xend = c(tail(Bmed, n=-1), NA), yend = c(tail(Fmed, n=-1), NA), alpha = alpha), arrow = arrow(length = unit(0.1, "cm"))) +
#        #ggrepel::geom_label_repel(data = df_thin, aes(Bmed, Fmed, label = Year), box.padding = unit(1.0, "lines"), min.segment.length = unit(2.0, "lines"), colour = "grey") +
#        #ggrepel::geom_label_repel(data = df_thin, aes(Bmed, Fmed, label = Year), box.padding = unit(0.65, "lines"), min.segment.length = unit(0.0, "lines"), colour = "grey", alpha = 0.5, force = 35, max.iter = 4000) +
#        ggrepel::geom_label_repel(data = df_thin, aes(Bmed, Fmed, label = Year), box.padding = unit(0.65, "lines"), colour = "grey", alpha = 0.5, min.segment.length = unit(0.0, "lines"), force = 3) +
#        theme_bw(base_size = 16) +
#        theme(legend.position = "none") +
#        expand_limits(x = 0, y = 0) +
#        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 100, 1), minor_breaks = seq(0, 100, 0.5)) +
#        scale_y_continuous(expand = c(0, 0)) +
#        #coord_fixed() +
#        labs(x = xlab, y = ylab)
}
