#' Length-frequency OSA residual plots
#'
#' @param object and LSD object
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param figure_dir the directory to save to
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @importFrom compResidual resMulti
#' @export
#'
plot_lfs_resid_OSA <- function(object, figure_dir = "figure/") {

  data <- object@data
  mcmc <- object@mcmc
  n_iter <- nrow(mcmc[[1]])
  years <- data$first_yr:data$last_yr
  sex <- c("Male", "Immature female", "Mature female")
  seasons <- c("AW", "SS")
  bins <- data$size_midpoint_l
  regions <- 1:data$n_area

  w <- data.frame(LF = 1:data$n_lf,
                  Year = data$data_lf_year_i, Season = data$data_lf_season_i,
                  Region = data$data_lf_area_i, Sex = data$data_lf_sex_i)

  lim <- data$data_lf_bin_limits_i
  colnames(lim) <- c("Min", "Max")
  lim <- data.frame(lim) %>%
    mutate(LF = 1:data$n_lf) %>%
    mutate(lower = bins[Min], upper = bins[Max]) %>%
    select("LF", "lower", "upper")

  # Observed LF
  dlf <- mcmc$data_lf_obs2_il
  dimnames(dlf) <- list("Iteration" = 1:n_iter, "LF" = 1:data$n_lf, "Size" = bins)
  dlf <- melt(dlf) %>%
    left_join(w, by = "LF") %>%
    filter(Iteration == 1) %>%
    select(-Iteration) %>%
    left_join(lim, by = c("LF")) %>%
    mutate(Season = seasons[Season], Sex = sex[Sex]) %>%
    pivot_wider(names_from = Size, values_from = value) %>%
    ungroup() %>%
    arrange(LF, Year, Season, Region, Sex)

  # Predicted LF
  plf <- mcmc$pred_lf_il
  dimnames(plf) <- list("Iteration" = 1:n_iter, "LF" = 1:data$n_lf, "Size" = bins)
  plf <- melt(plf) %>%
    left_join(w, by = "LF") %>%
    left_join(lim, by = c("LF")) %>%
    mutate(Season = seasons[Season], Sex = sex[Sex]) %>%
    pivot_wider(names_from = Size, values_from = value) %>%
    select(-Iteration) %>%
    group_by(LF, Year, Season, Region, Sex, lower, upper) %>%
    summarise(across(everything(), mean, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(LF, Year, Season, Region, Sex)

  res <- df <- list()
  for (i in 1:length(sex)) {
    X0 <- dlf %>% filter(Sex == sex[i])
    P0 <- plf %>% filter(Sex == sex[i])
    ilim <- X0 %>% select(lower, upper) %>% distinct()
    lb <- P0 %>% select(LF, Year, Season, Region, Sex, lower, upper)
    X <- X0 %>%
      select(as.character(ilim$lower[1]):as.character(ilim$upper[1])) %>%
      as.matrix()
    P <- P0 %>%
      select(as.character(ilim$lower[1]):as.character(ilim$upper[1])) %>%
      as.matrix()
    if (data$like_lf == 1) res[[i]] <- resMulti(obs = t(X), pred = t(P)) %>% t()
    if (data$like_lf == 2) res[[i]] <- resDir(obs = t(X), alpha = t(P)) %>% t()
    colnames(res[[i]]) <- colnames(P)[1:(ncol(P) - 1)]
    df[[i]] <- bind_cols(lb, res[[i]]) %>% pivot_longer(cols = !LF:upper, names_to = "Size")
  }

  resid <- bind_rows(df) %>%
    mutate(Sex = factor(Sex, levels = sex), pos = ifelse(value < 0, 1, 0), abs = abs(value))

  plot(res[[1]])
  ggsave(filename = paste0(figure_dir, "lf_residuals_MM.png"), width = 7, height = 10)
  plot(res[[2]])
  ggsave(filename = paste0(figure_dir, "lf_residuals_IF.png"), width = 7, height = 10)
  plot(res[[3]])
  ggsave(filename = paste0(figure_dir, "lf_residuals_MF.png"), width = 7, height = 10)

  p <- ggplot(data = resid, aes(x = factor(Year), y = value, alpha = 0.75, fill = Region, color = Region)) +
    geom_hline(aes(yintercept = -2), linetype = 2) +
    geom_hline(aes(yintercept = 2), linetype = 2) +
    geom_hline(yintercept = 0, alpha = 0.8) +
    geom_violin(scale = "width", draw_quantiles = 0.5) +
    scale_x_discrete(breaks = rev(seq(max(years), by = -2))) +
    labs(x = "Year", y = "OSA residuals") +
    guides(fill = 'none', color = "none") +
    scale_alpha(guide = "none")
  if (length(regions) > 1) {
    p <- p + facet_grid(Sex ~ Region)
  } else {
    p <- p + facet_grid(Region ~ .)
  }
  ggsave(filename = paste0(figure_dir, "lf_residuals_year.png"), plot = p, width = 7, height = 10)

  p <- ggplot(data = resid, aes(x = factor(Size), y = value, alpha = 0.75, fill = Region, color = Region)) +
    geom_hline(aes(yintercept = -2), linetype = 2) +
    geom_hline(aes(yintercept = 2), linetype = 2) +
    geom_hline(yintercept = 0, alpha = 0.8) +
    geom_violin(scale = "width", draw_quantiles = 0.5) +
    scale_x_discrete(breaks = rev(seq(max(years), by = -2))) +
    labs(x = "Midpoint of size-class (mm)", y = "OSA residuals") +
    guides(fill = 'none', color = "none") +
    scale_alpha(guide = "none")
  if (length(regions) > 1) {
    p <- p + facet_grid(Sex ~ Region)
  } else {
    p <- p + facet_grid(Sex ~ .)
  }
  ggsave(filename = paste0(figure_dir, "lf_residuals_size.png"), plot = p, width = 16)

  p <- ggplot(data = resid, aes(x = factor(Year), y = value, alpha = 0.75, fill = Region, color = Region)) +
    geom_hline(aes(yintercept = -2), linetype = 2) +
    geom_hline(aes(yintercept = 2), linetype = 2) +
    geom_hline(yintercept = 0, alpha = 0.8) +
    geom_violin(scale = "width", draw_quantiles = 0.5) +
    scale_x_discrete(breaks = rev(seq(max(years), by = -2))) +
    labs(x = "Midpoint of size-class (mm)", y = "OSA residuals") +
    guides(fill = 'none', color = "none") +
    scale_alpha(guide = "none")
  if (length(regions) > 1) {
    p <- p + facet_grid(Sex ~ Region + Season)
  } else {
    p <- p + facet_grid(Sex ~ Season)
  }
  ggsave(filename = paste0(figure_dir, "lf_residuals_year_season.png"), plot = p, width = 16)

  p <- ggplot(data = resid, aes(x = factor(Size), y = factor(Year))) +
    geom_point(aes(size = value, color = value), alpha = 0.85) +
    # scale_color_continuous(limits = c(min(resid$value), max(resid$value)), breaks = seq(-2, 2, by = 1), type = "viridis") +
    scale_color_gradient2(midpoint = 0, mid = "#eee8d5", high = "#dc322f", low = "#268bd2", limits = c(min(resid$value), max(resid$value)), breaks = seq(-2, 2, by = 1)) +
    # scale_size_continuous(limits = c(min(resid$value), max(resid$value)), breaks = seq(-2, 2, by = 1)) +
    scale_size_area(max_size = 4, limits = c(min(resid$value), max(resid$value)), breaks = seq(-2, 2, by = 1)) +
    scale_y_discrete(breaks = rev(seq(max(years), by = -2))) +
    labs(x = "Midpoint of size-class (mm)", y = "Year",
         size = "OSA residual", color = "OSA residual", fill = "OSA residual") +
    guides(color = guide_legend(), size = guide_legend())
  if (length(regions) > 1) {
    p <- p + facet_grid(Sex ~ Region + Season)
  } else {
    p <- p + facet_grid(Sex ~ Season)
  }
  ggsave(filename = paste0(figure_dir, "lf_residuals_bubble.png"), plot = p, width = 16)
}
