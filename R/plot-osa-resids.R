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
    # filter(Iteration == 1) %>%
    # select(-Iteration) %>%
    left_join(lim, by = c("LF")) %>%
    mutate(Season = seasons[Season], Sex = sex[Sex]) %>%
    pivot_wider(names_from = Size, values_from = value) %>%
    arrange(Iteration, LF, Year, Season, Region, Sex)
  tail(dlf)

  # Predicted LF
  plf <- mcmc$pred_lf_il
  dimnames(plf) <- list("Iteration" = 1:n_iter, "LF" = 1:data$n_lf, "Size" = bins)
  plf <- melt(plf) %>%
    left_join(w, by = "LF") %>%
    left_join(lim, by = c("LF")) %>%
    mutate(Season = seasons[Season], Sex = sex[Sex]) %>%
    pivot_wider(names_from = Size, values_from = value) %>%
    arrange(Iteration, LF, Year, Season, Region, Sex)
  tail(plf)

  # library(compResidual)
  res <- df <- list()
  for (i in 1:3) {
    X0 <- dlf %>% filter(Sex == sex[i])
    P0 <- plf %>% filter(Sex == sex[i])
    ilim <- X0 %>% select(lower, upper) %>% distinct()
    lb <- P0 %>% select(Iteration, LF, Year, Season, Region, Sex, lower, upper)
    X <- X0 %>%
      select(as.character(ilim$lower[1]):as.character(ilim$upper[1])) %>%
      as.matrix()
    P <- P0 %>%
      # filter(Iteration == 1) %>%
      select(as.character(ilim$lower[1]):as.character(ilim$upper[1])) %>%
      as.matrix()
    res[[i]] <- resMulti(t(X), t(P)) %>% t()
    colnames(res[[i]]) <- colnames(P)[1:(ncol(P) - 1)]
    df[[i]] <- bind_cols(lb, res[[i]]) %>% pivot_longer(cols = !Iteration:upper, names_to = "Size")
  }

  # plot(res[[1]])
  # plot(res[[2]])
  # plot(res[[3]])
  resid <- bind_rows(df) %>%
    mutate(Sex = factor(Sex, levels = sex))

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
    p <- p + facet_grid(Region ~ .)
  } else {
    p <- p + facet_grid(Sex ~ Region)
  }
  ggsave(paste0(figure_dir, "lf_residuals_year.png"), p, width = 7, height = 10)

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
    p <- p + facet_grid(Region ~ .)
  } else {
    p <- p + facet_grid(Sex ~ Region)
  }
  ggsave(paste0(figure_dir, "lf_residuals_size.png"), p, width = 16)
}
