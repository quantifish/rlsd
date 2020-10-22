#' Plot snail trial
#' 
#' @param object an LSD object
#' @param figure_dir the directory to save the figure to
#' @param irule if multiple rules then which rule
#' @import dplyr
#' @import ggplot2
#' @importFrom utils head tail
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @importFrom ggrepel geom_text_repel
#' @export
#' 
plot_snail <- function(object,
                       figure_dir = "figure/",
                       irule = 1)
{
    data <- object@data
    mcmc <- object@mcmc

    n_iter <- nrow(mcmc[[1]])
    years <- data$first_yr:data$last_yr
    pyears <- data$first_yr:data$last_proj_yr
    regions <- 1:data$n_area
    if (length(regions) > 1) regions2 <- c(regions, "Total")
    if (length(regions) == 1) regions2 <- regions
    rules <- 1:data$n_rules
    seasons <- c("AW", "SS")
    fleets <- c("SL", "NSL")
    
    F_Fmsy <- mcmc$F_Fmsy_ry
    dimnames(F_Fmsy) <- list(Iteration = 1:n_iter, Region = regions, Year = years)
    F_Fmsy <- reshape2::melt(F_Fmsy, value.name = "F_Fmsy")
    F_Fmsy$Region <- factor(F_Fmsy$Region)
    
    ssb <- mcmc$biomass_ssb_jyr
    dimnames(ssb) <- list(Iteration = 1:n_iter, Rule = 1:dim(ssb)[2],
                          Year = data$first_yr:data$last_proj_yr, Region = regions)
    ssb <- reshape2::melt(ssb, value.name = "SSB") %>%
        filter(Rule == 1)
    head(ssb)
    ssb$Region <- factor(ssb$Region)

    ssb0 <- mcmc$SSB0_r
    dimnames(ssb0) <- list(Iteration = 1:n_iter, Region = regions2)
    ssb0 <- reshape2::melt(ssb0, value.name = "SSB0")
    head(ssb0)
    ssb0$Region <- factor(ssb0$Region)

    ## VERSION 1: Fmsy on Y axis
    d <- left_join(F_Fmsy, ssb) %>%
        left_join(ssb0) %>%
        mutate(SSB_SSB0 = SSB / SSB0)
    head(d)
    tail(d)
    
    ssbmsy <- mcmc$SSBmsy_r
    dimnames(ssbmsy) <- list(Iteration = 1:n_iter, Region = regions2)
    ssbmsy <- reshape2::melt(ssbmsy, value.name = "SSBmsy") 
    ssbmsy$Region <- factor(ssbmsy$Region)
    ssbmsy <- left_join(ssbmsy, ssb0) %>%
        mutate(SSBmsy_SSB0 = SSBmsy / SSB0)
    head(ssbmsy)
    
    dmed <- d %>%
        group_by(Year, Region) %>%
        summarise(F_Fmsy = median(F_Fmsy), SSB = median(SSB), SSB0 = median(SSB0), SSB_SSB0 = median(SSB_SSB0)) %>%
        ungroup()
    df_thin <- select(dmed, Year, SSB_SSB0, F_Fmsy) %>%
        mutate(Year = ifelse(Year %in% c(min(Year), max(Year), seq(0, 1e6, 5)), Year, ""))
    lyr <- filter(d, Year %in% 2016) %>%
        filter(F_Fmsy > quantile(F_Fmsy, 0.05)) %>%
        select(Year, SSB_SSB0, F_Fmsy) %>%
        mutate(Year = "")
    head(df_thin)

    top_left <- df_thin %>% filter(SSB_SSB0 < median(ssbmsy$SSBmsy_SSB0), F_Fmsy > 1)
    top_right <- df_thin %>% filter(SSB_SSB0 >= median(ssbmsy$SSBmsy_SSB0), F_Fmsy > 1)
    bottom_left <- df_thin %>% filter(SSB_SSB0 < median(ssbmsy$SSBmsy_SSB0), F_Fmsy <= 1)
    bottom_right <- df_thin %>% filter(SSB_SSB0 >= median(ssbmsy$SSBmsy_SSB0), F_Fmsy <= 1)
    
    p <- ggplot(data = d %>% filter(Region %in% regions)) +
        annotate("rect", xmin = quantile(ssbmsy$SSBmsy_SSB0, 0.05), xmax = quantile(ssbmsy$SSBmsy_SSB0, 0.95), ymin = -Inf, ymax = Inf, alpha = 0.125) +
        annotate("rect", xmin = quantile(ssbmsy$SSBmsy_SSB0, 0.25), xmax = quantile(ssbmsy$SSBmsy_SSB0, 0.75), ymin = -Inf, ymax = Inf, alpha = 0.25) +
        geom_hline(yintercept = 1) +
        geom_vline(data = ssbmsy %>% filter(Region %in% regions), aes(xintercept = median(SSBmsy_SSB0))) +
        geom_density_2d(data = filter(d, Year %in% data$last_yr) %>% filter(Region %in% regions), aes(x = SSB_SSB0, y = F_Fmsy, colour = ..level..)) +
        scale_colour_gradient(low = "white", high = "red") +
        geom_segment(data = dmed %>% filter(Region %in% regions), aes(x = SSB_SSB0, y = F_Fmsy, xend = lead(SSB_SSB0), yend = lead(F_Fmsy)), arrow = arrow(length = unit(0.2,"cm")), colour = "red") +
        #geom_path(data = dmed, aes(x = SSB_SSB0, y = F_Fmsy, colour = Year), colour = "red", arrow = arrow()) +
        #geom_point(data = dmed, aes(x = SSB_SSB0, y = F_Fmsy, colour = Year), colour = "red") +
        expand_limits(y = 0, x = c(0, 1.01)) +
        scale_y_continuous(expand = c(0, 0)) +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, 1, 0.1), minor_breaks = seq(0, 1, 0.05)) +
        labs(x = expression(SSB/SSB[0]), y = expression(paste("Fishing intensity (", F/F[MSY], ")"))) +
        geom_text_repel(
            data = top_left,
            aes(SSB_SSB0, F_Fmsy, label = Year),
            force = 5, min.segment.length = 0, point.padding = 0.5,
            xlim = c(0, median(ssbmsy$SSBmsy_SSB0)),
            ylim = c(1, NA)
        ) +
        geom_text_repel(
            data = top_right,
            aes(SSB_SSB0, F_Fmsy, label = Year),
            force = 15, min.segment.length = 0, point.padding = 0.5, 
            xlim = c(median(ssbmsy$SSBmsy_SSB0), NA),
            ylim = c(1, NA)
        ) +
        geom_text_repel(
            data = bottom_left,
            aes(SSB_SSB0, F_Fmsy, label = Year),
            force = 15, min.segment.length = 0, max.iter = 5000,
            nudge_y = -0.1,
            xlim = c(0, median(ssbmsy$SSBmsy_SSB0)),
            ylim = c(0, 1)
        ) +
        geom_text_repel(
            data = bottom_right,
            aes(SSB_SSB0, F_Fmsy, label = Year),
            force = 5, min.segment.length = 0, point.padding = 0.5,
            xlim = c(median(ssbmsy$SSBmsy_SSB0), NA),
            ylim = c(0, 1)
        ) +
        theme_lsd(base_size = 16) +
        theme(legend.position = "none")
    if (data$n_area > 1) p <- p + facet_wrap(~Region)
    ggsave(file.path(figure_dir, "snail_trail.png"), p)



    # VERSION 2: F on Y axis
    F_jytrf <- mcmc$proj_F_jytrf
    dimnames(F_jytrf) <- list(Iteration = 1:n_iter, Rule = rules, Year = pyears, Season = seasons, Region = regions, Fishery = fleets)
    F_jytrf <- reshape2::melt(F_jytrf) %>% 
        rename(F_val = value) %>%
        filter(Season == "AW") %>%
        group_by(Iteration, Rule, Year, Region) %>%
        summarise(F_aw = sum(F_val)) %>%
        filter(Year %in% years, Rule %in% irule)
    F_jytrf$Region <- factor(F_jytrf$Region)

    d <- left_join(F_jytrf, ssb) %>%
        left_join(ssb0) %>%
        mutate(SSB_SSB0 = SSB / SSB0) %>%
        ungroup()
    
    ssbmsy <- mcmc$SSBmsy_r
    dimnames(ssbmsy) <- list(Iteration = 1:n_iter, Region = regions2)
    ssbmsy <- reshape2::melt(ssbmsy, value.name = "SSBmsy") 
    ssbmsy$Region <- factor(ssbmsy$Region)
    ssbmsy <- left_join(ssbmsy, ssb0) %>%
        mutate(SSBmsy_SSB0 = SSBmsy / SSB0)
    
    dmed <- d %>%
        group_by(Year, Region) %>%
        summarise(F_aw = median(F_aw), SSB = median(SSB), SSB0 = median(SSB0), SSB_SSB0 = median(SSB_SSB0)) %>%
        ungroup()
    df_thin <- select(dmed, Region, Year, SSB_SSB0, F_aw) %>%
        mutate(Year = ifelse(Year %in% c(min(Year), max(Year), seq(0, 1e6, 10)), Year, ""))
    
    xmax <- round(max(dmed$SSB_SSB0), 2)

    p2 <- ggplot(data = d) +
        #annotate("rect", xmin = quantile(ssbmsy$SSBmsy_SSB0, 0.05), xmax = quantile(ssbmsy$SSBmsy_SSB0, 0.95), ymin = -Inf, ymax = Inf, alpha = 0.125) +
        #annotate("rect", xmin = quantile(ssbmsy$SSBmsy_SSB0, 0.25), xmax = quantile(ssbmsy$SSBmsy_SSB0, 0.75), ymin = -Inf, ymax = Inf, alpha = 0.25) +
        #geom_vline(data = ssbmsy %>% filter(Region %in% regions), aes(xintercept = median(SSBmsy_SSB0))) +
        geom_density_2d(data = filter(d, Year %in% data$last_yr) %>% filter(Region %in% regions), aes(x = SSB_SSB0, y = F_aw, colour = ..level..)) +
        scale_colour_gradient(low = "white", high = "red") +
        geom_path(data = dmed, aes(x = SSB_SSB0, y = F_aw), colour = "red") +
        geom_point(data = dmed, aes(x = SSB_SSB0, y = F_aw), colour = "red") +
        geom_text_repel(
            data = df_thin, 
            aes(x = SSB_SSB0, y = F_aw, label = Year), 
            force = 5, min.segment.length = 0, point.padding = 0.5) +
        expand_limits(y = 0, x = c(0, 1.01)) +
        scale_y_continuous(expand = c(0, 0)) +
        scale_x_continuous(expand = c(0, 0), breaks = seq(0, xmax, 0.1), minor_breaks = seq(0, xmax, 0.05)) +
        labs(x = expression(SSB/SSB[0]), y = "Fishing mortality (F)") +
        theme_lsd(base_size = 16) +
        theme(legend.position = "none")
    if (data$n_area > 1) p2 <- p2 + facet_wrap(~Region)
    ggsave(file.path(figure_dir, "snail_trail_v2.png"), p2, width = 16, height = 8)
}
