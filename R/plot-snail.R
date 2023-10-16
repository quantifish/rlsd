#' Plot snail trial
#'
#' Creates snail trail plots for MPI plenary report.
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
#' @importFrom ggforce facet_zoom
#' @export
#'
plot_snail <- function(object, figure_dir = "figure/", irule = 1) {
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

  Bref <- data$Bref_r
  # Uref <- data$Uref_rt
  # U <- mcmc$U_ytrf
  # dimnames(U) <- list(Iteration = 1:n_iter, Year = years, Season = seasons, Region = regions, Fishery = fleets)
  # U <- melt(U, value.name = "U") %>%
  #   filter(Season == "AW", Year %in% years) %>%
  #   group_by(Iteration, Year, Season, Region) %>%
  #   summarise(U = sum(U))
  # U$Region <- factor(U$Region)

  U_Uref <- mcmc$U_Uref_ry
  dimnames(U_Uref) <- list(Iteration = 1:n_iter, Region = regions, Year = years)
  U_Uref <- melt(U_Uref, value.name = "U_Uref") %>%
    filter(Year %in% years)
  U_Uref$Region <- factor(U_Uref$Region)

  B_Bref <- mcmc$biomass_vulnref_jytr / as.numeric(Bref)
  dimnames(B_Bref) <- list(Iteration = 1:n_iter, Rule = rules, Year = pyears, Season = seasons, Region = regions)
  B_Bref <- melt(B_Bref, value.name = "B_Bref") %>%
    filter(Season == "AW", Rule == 1, Year %in% years)
  B_Bref$Region <- factor(B_Bref$Region)

  df <- full_join(U_Uref, B_Bref)

  dfmed <- df %>%
    group_by(Region, Year) %>%
    summarise(ymedian = median(U_Uref), ylower = quantile(U_Uref, probs = 0.025), yupper = quantile(U_Uref, probs = 0.975),
              xmedian = median(B_Bref), xlower = quantile(B_Bref, probs = 0.025), xupper = quantile(B_Bref, probs = 0.975))

  # dferr <- dfmed %>% filter(Year %in% c(data$first_yr, data$last_yr))
  dferr <- dfmed %>%
    mutate(Decade = case_when(Year < 1980 ~ "< 1980",
                              Year %in% 1980:1989 ~ "1980s",
                              Year %in% 1990:1999 ~ "1990s",
                              Year %in% 2000:2009 ~ "2000s",
                              Year %in% 2010:2019 ~ "2010s",
                              Year > 2019 ~ "> 2019"))
  dferr$Decade <- factor(dferr$Decade, levels = c("< 1980", "1980s", "1990s", "2000s", "2010s", "> 2019"))
  df_label <- dfmed %>% filter(Year %in% c(data$first_yr, data$last_yr, seq(1980, 2021, 10)))

  p <- ggplot(data = dfmed, aes(x = xmedian, y = ymedian)) +
    geom_segment(aes(xend = lead(xmedian), yend = lead(ymedian)), arrow = arrow(length = unit(0.2, "cm"))) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_linerange(data = dferr %>% filter(Year == max(Year)), aes(ymin = ylower, ymax = yupper), color = "tomato", alpha = 0.8, size = 1) +
    geom_errorbarh(data = dferr %>% filter(Year == max(Year)), aes(xmin = xlower, xmax = xupper), color = "tomato", height = 0, alpha = 0.8, size = 1) +
    geom_point(data = dfmed %>% filter(Year == max(Year)), aes(x = xmedian, y = ymedian), color = "tomato", pch = 19, size = 3, alpha = 0.8) +
    geom_text(data = df_label, aes(label = Year)) +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(x = expression(B/B[R]), y = expression(paste("Fishing intensity (", U/U[R], ")"))) +
    # labs(x = expression(B/B[REF]), y = expression(paste("Exploitation rate (U)"))) +
    theme_lsd(base_size = 16) #+
    # theme(legend.position = "none")

  p
  ggsave(file.path(figure_dir, "snail_trail.png"), p, width = 8, height = 7)

  p <- ggplot(data = dfmed, aes(x = xmedian, y = ymedian)) +
    # geom_text(data = df_label, aes(label = Year)) +
    # geom_linerange(data = dferr, aes(ymin = ylower, ymax = yupper, color = Decade), alpha = 0.4, size = 1) +
    # geom_errorbarh(data = dferr, aes(xmin = xlower, xmax = xupper, color = Decade), height = 0, alpha = 0.25, size = 1) +
    geom_segment(aes(xend = lead(xmedian), yend = lead(ymedian)), arrow = arrow(length = unit(0.2, "cm"))) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_linerange(data = dferr %>% filter(Year == max(Year)), aes(ymin = ylower, ymax = yupper), color = "tomato", alpha = 0.8, size = 1) +
    geom_errorbarh(data = dferr %>% filter(Year == max(Year)), aes(xmin = xlower, xmax = xupper), color = "tomato", height = 0, alpha = 0.8, size = 1) +
    geom_point(data = dfmed %>% filter(Year == max(Year)), aes(x = xmedian, y = ymedian), color = "tomato", pch = 19, size = 3, alpha = 0.8) +
    geom_text(data = df_label, aes(label = Year)) +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    labs(x = expression(B/B[R]), y = expression(paste("Fishing intensity (", U/U[R], ")"))) +
    # labs(x = expression(B/B[REF]), y = expression(paste("Exploitation rate (U)"))) +
    theme_lsd(base_size = 16) +
    facet_zoom(xlim = c(0,2), ylim = c(0,2), zoom.size = 1)#+
    # theme(legend.position = "none")

  p
  ggsave(file.path(figure_dir, "snail_trail_zoom.png"), p, width = 14, height = 7)

    p <- ggplot(data = dfmed, aes(x = xmedian, y = ymedian)) +
    geom_segment(aes(xend = lead(xmedian), yend = lead(ymedian)), arrow = arrow(length = unit(0.2, "cm"))) +
    geom_vline(xintercept = 1, linetype = "dashed") +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_linerange(data = dferr %>% filter(Year == max(Year)), aes(ymin = ylower, ymax = yupper), color = "tomato", alpha = 0.8, size = 1) +
    geom_errorbarh(data = dferr %>% filter(Year == max(Year)), aes(xmin = xlower, xmax = xupper), color = "tomato", height = 0, alpha = 0.8, size = 1) +
    geom_point(data = dfmed %>% filter(Year == max(Year)), aes(x = xmedian, y = ymedian), color = "tomato", pch = 19, size = 3, alpha = 0.8) +
    geom_text(data = df_label, aes(label = Year)) +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0))) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0))) +
    labs(x = expression(B/B[R]), y = expression(paste("Fishing intensity (", U/U[R], ")"))) +
    # labs(x = expression(B/B[REF]), y = expression(paste("Exploitation rate (U)"))) +
    coord_cartesian(xlim = c(0,2), ylim = c(0,2)) +
    theme_lsd(base_size = 16) #+
    # theme(legend.position = "none")

  p
  ggsave(file.path(figure_dir, "snail_trail_lim2.png"), p, width = 8, height = 7)

  # F_Fmsy <- mcmc$F_Fmsy_ry
  # dimnames(F_Fmsy) <- list(Iteration = 1:n_iter, Region = regions, Year = years)
  # F_Fmsy <- melt(F_Fmsy, value.name = "F_Fmsy")
  # F_Fmsy$Region <- factor(F_Fmsy$Region)

  # ssb <- mcmc$biomass_ssb_jyr
  # dimnames(ssb) <- list(Iteration = 1:n_iter, Rule = 1:dim(ssb)[2], Year = data$first_yr:data$last_proj_yr, Region = regions)
  # ssb <- melt(ssb, value.name = "SSB") %>% filter(.data$Rule == 1)
  # ssb$Region <- factor(ssb$Region)

  # ssb0 <- mcmc$SSB0_r
  # dimnames(ssb0) <- list(Iteration = 1:n_iter, Region = regions2)
  # ssb0 <- melt(ssb0, value.name = "SSB0")
  # ssb0$Region <- factor(ssb0$Region)

  ## VERSION 1: Fmsy on Y axis
  # d <- left_join(F_Fmsy, ssb) %>%
  #   left_join(ssb0) %>%
  #   mutate(SSB_SSB0 = .data$SSB / .data$SSB0)
  #
  # ssbmsy <- mcmc$SSBmsy_r
  # dimnames(ssbmsy) <- list(Iteration = 1:n_iter, Region = regions2)
  # ssbmsy <- melt(ssbmsy, value.name = "SSBmsy")
  # ssbmsy$Region <- factor(ssbmsy$Region)
  # ssbmsy <- left_join(ssbmsy, ssb0) %>%
  #   mutate(SSBmsy_SSB0 = .data$SSBmsy / .data$SSB0)
  #
  # dmed <- d %>%
  #   group_by(.data$Year, .data$Region) %>%
  #   summarise(F_Fmsy = median(.data$F_Fmsy), SSB = median(.data$SSB), SSB0 = median(.data$SSB0), SSB_SSB0 = median(.data$SSB_SSB0)) %>%
  #   ungroup()
  #
  # df_thin <- dmed %>%
  #   select(.data$Year, .data$SSB_SSB0, .data$F_Fmsy) %>%
  #   mutate(Year = ifelse(.data$Year %in% c(min(.data$Year), max(.data$Year), seq(0, 1e6, 5)), .data$Year, ""))
  #
  # lyr <- d %>%
  #   filter(.data$Year %in% 2016) %>%
  #   filter(.data$F_Fmsy > quantile(.data$F_Fmsy, 0.05)) %>%
  #   select(.data$Year, .data$SSB_SSB0, .data$F_Fmsy) %>%
  #   mutate(Year = "")
  #
  # top_left <- df_thin %>% filter(.data$SSB_SSB0 < median(ssbmsy$SSBmsy_SSB0), .data$F_Fmsy > 1)
  # top_right <- df_thin %>% filter(.data$SSB_SSB0 >= median(ssbmsy$SSBmsy_SSB0), .data$F_Fmsy > 1)
  # bottom_left <- df_thin %>% filter(.data$SSB_SSB0 < median(ssbmsy$SSBmsy_SSB0), .data$F_Fmsy <= 1)
  # bottom_right <- df_thin %>% filter(.data$SSB_SSB0 >= median(ssbmsy$SSBmsy_SSB0), .data$F_Fmsy <= 1)
  #
  # p <- ggplot(data = d %>% filter(.data$Region %in% regions)) +
  #   annotate("rect", xmin = quantile(ssbmsy$SSBmsy_SSB0, 0.05), xmax = quantile(ssbmsy$SSBmsy_SSB0, 0.95), ymin = -Inf, ymax = Inf, alpha = 0.125) +
  #   annotate("rect", xmin = quantile(ssbmsy$SSBmsy_SSB0, 0.25), xmax = quantile(ssbmsy$SSBmsy_SSB0, 0.75), ymin = -Inf, ymax = Inf, alpha = 0.25) +
  #   geom_hline(yintercept = 1) +
  #   geom_vline(data = ssbmsy %>% filter(.data$Region %in% regions), aes(xintercept = median(.data$SSBmsy_SSB0))) +
  #   geom_density_2d(data = d %>% filter(.data$Year %in% data$last_yr, .data$Region %in% regions), aes(x = .data$SSB_SSB0, y = .data$F_Fmsy, colour = ..level..)) +
  #   scale_colour_gradient(low = "white", high = "red") +
  #   geom_segment(data = dmed %>% filter(.data$Region %in% regions), aes(x = .data$SSB_SSB0, y = F_Fmsy, xend = lead(.data$SSB_SSB0), yend = lead(F_Fmsy)), arrow = arrow(length = unit(0.2,"cm")), colour = "red") +
  #   #geom_path(data = dmed, aes(x = SSB_SSB0, y = F_Fmsy, colour = Year), colour = "red", arrow = arrow()) +
  #   #geom_point(data = dmed, aes(x = SSB_SSB0, y = F_Fmsy, colour = Year), colour = "red") +
  #   expand_limits(y = 0, x = c(0, 1.01)) +
  #   scale_y_continuous(expand = c(0, 0)) +
  #   scale_x_continuous(expand = c(0, 0), breaks = seq(0, 1, 0.1), minor_breaks = seq(0, 1, 0.05)) +
  #   labs(x = expression(SSB/SSB[0]), y = expression(paste("Fishing intensity (", F/F[MSY], ")"))) +
  #   geom_text_repel(
  #     data = top_left,
  #     aes(.data$SSB_SSB0, .data$F_Fmsy, label = .data$Year),
  #     force = 5, min.segment.length = 0, point.padding = 0.5,
  #     xlim = c(0, median(ssbmsy$SSBmsy_SSB0)),
  #     ylim = c(1, NA)
  #   ) +
  #   geom_text_repel(
  #     data = top_right,
  #     aes(.data$SSB_SSB0, .data$F_Fmsy, label = .data$Year),
  #     force = 15, min.segment.length = 0, point.padding = 0.5,
  #     xlim = c(median(ssbmsy$SSBmsy_SSB0), NA),
  #     ylim = c(1, NA)
  #   ) +
  #   geom_text_repel(
  #     data = bottom_left,
  #     aes(.data$SSB_SSB0, .data$F_Fmsy, label = .data$Year),
  #     force = 15, min.segment.length = 0, max.iter = 5000,
  #     nudge_y = -0.1,
  #     xlim = c(0, median(ssbmsy$SSBmsy_SSB0)),
  #     ylim = c(0, 1)
  #   ) +
  #   geom_text_repel(
  #     data = bottom_right,
  #     aes(.data$SSB_SSB0, .data$F_Fmsy, label = .data$Year),
  #     force = 5, min.segment.length = 0, point.padding = 0.5,
  #     xlim = c(median(ssbmsy$SSBmsy_SSB0), NA),
  #     ylim = c(0, 1)
  #   ) +
  #   theme_lsd(base_size = 16) +
  #   theme(legend.position = "none")
  #
  # if (data$n_area > 1) p <- p + facet_wrap(~ .data$Region)
  #
  # ggsave(file.path(figure_dir, "snail_trail.png"), p, width = 7, height = 7)
  #
  #
  # # VERSION 2: F on Y axis
  # F_jytrf <- mcmc$proj_F_jytrf
  # dimnames(F_jytrf) <- list(Iteration = 1:n_iter, Rule = rules, Year = pyears, Season = seasons, Region = regions, Fishery = fleets)
  # F_jytrf <- melt(F_jytrf) %>%
  #   rename(F_val = .data$value) %>%
  #   filter(.data$Season == "AW") %>%
  #   group_by(.data$Iteration, .data$Rule, .data$Year, .data$Region) %>%
  #   summarise(F_aw = sum(.data$F_val)) %>%
  #   filter(.data$Year %in% years, .data$Rule %in% irule) %>%
  #   mutate(Region = factor(Region))
  #
  # d <- left_join(F_jytrf, ssb) %>%
  #   left_join(ssb0) %>%
  #   mutate(SSB_SSB0 = .data$SSB / .data$SSB0) %>%
  #   ungroup()
  #
  # ssbmsy <- mcmc$SSBmsy_r
  # dimnames(ssbmsy) <- list(Iteration = 1:n_iter, Region = regions2)
  # ssbmsy <- melt(ssbmsy, value.name = "SSBmsy")
  # ssbmsy$Region <- factor(ssbmsy$Region)
  # ssbmsy <- left_join(ssbmsy, ssb0) %>% mutate(SSBmsy_SSB0 = .data$SSBmsy / .data$SSB0)
  #
  # dmed <- d %>%
  #   group_by(.data$Year, .data$Region) %>%
  #   summarise(F_aw = median(.data$F_aw), SSB = median(.data$SSB), SSB0 = median(.data$SSB0), SSB_SSB0 = median(.data$SSB_SSB0)) %>%
  #   ungroup()
  #
  # df_thin <- dmed %>%
  #   select(.data$Region, .data$Year, .data$SSB_SSB0, .data$F_aw) %>%
  #   mutate(Year = ifelse(.data$Year %in% c(min(.data$Year), max(.data$Year), seq(0, 1e6, 10)), .data$Year, ""))
  #
  # xmax <- round(max(dmed$SSB_SSB0), 2)
  #
  # p2 <- ggplot(data = d) +
  #   #annotate("rect", xmin = quantile(ssbmsy$SSBmsy_SSB0, 0.05), xmax = quantile(ssbmsy$SSBmsy_SSB0, 0.95), ymin = -Inf, ymax = Inf, alpha = 0.125) +
  #   #annotate("rect", xmin = quantile(ssbmsy$SSBmsy_SSB0, 0.25), xmax = quantile(ssbmsy$SSBmsy_SSB0, 0.75), ymin = -Inf, ymax = Inf, alpha = 0.25) +
  #   #geom_vline(data = ssbmsy %>% filter(Region %in% regions), aes(xintercept = median(SSBmsy_SSB0))) +
  #   geom_vline(aes(xintercept = 0.1), linetype = "dashed") +
  #   geom_vline(aes(xintercept = 0.2), linetype = "dashed") +
  #   # geom_density_2d(data = d %>% filter(.data$Year %in% data$last_yr, .data$Region %in% regions), aes(x = .data$SSB_SSB0, y = .data$F_aw, colour = ..level..)) +
  #   scale_colour_gradient(low = "white", high = "red") +
  #   geom_path(data = dmed, aes(x = .data$SSB_SSB0, y = .data$F_aw), colour = "red") +
  #   geom_point(data = dmed, aes(x = .data$SSB_SSB0, y = .data$F_aw), colour = "red") +
  #   geom_text_repel(
  #     data = df_thin,
  #     aes(x = .data$SSB_SSB0, y = .data$F_aw, label = .data$Year),
  #     force = 5, min.segment.length = 0, point.padding = 0.5) +
  #   expand_limits(y = 0, x = c(0, 1)) +
  #   scale_y_continuous(expand = c(0, 0)) +
  #   # scale_x_continuous(expand = c(0, 0), breaks = seq(0, xmax, 0.1), minor_breaks = seq(0, xmax, 0.05)) +
  #   scale_x_continuous(expand = c(0, 0), breaks = seq(0, 1, 0.1), minor_breaks = seq(0, 1, 0.05)) +
  #   labs(x = expression(SSB/SSB[0]), y = "Fishing mortality (F)") +
  #   theme_lsd(base_size = 16) +
  #   theme(legend.position = "none")
  #
  # if (data$n_area > 1) {
  #   p2 <- p2 + facet_wrap(~ .data$Region)
  #   ggsave(file.path(figure_dir, "snail_trail_v2.png"), p2, width = 14, height = 7)
  # } else {
  #   ggsave(file.path(figure_dir, "snail_trail_v2.png"), p2, width = 7, height = 7)
  # }
}
