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
#' @importFrom compResidual resMulti
#' @export
#'
plot_sex_ratio <- function(object, scales = "free",
                           xlab = "Fishing year", ylab = "Proportion",
                           figure_dir = "figure/") {

  data <- object@data
  map <- object@map
  mcmc <- object@mcmc
  seasons <- c("AW", "SS")
  sex <- c("Male", "Immature female", "Mature female")
  n_iter <- nrow(mcmc[[1]])

  w <- data.frame(SexR = 1:data$n_sexr,
                  Year = data$data_sexr_year_i,
                  Season = data$data_sexr_season_i,
                  Region = data$data_sexr_area_i,
                  Sigma = map$sigma_sex_ratio_i[1,])

  # Observed sex-ratio's
  # osexr <- map$data_sex_ratio_out_is
  osexr <- data$data_sexr_obs_is
  dimnames(osexr) <- list("SexR" = 1:data$n_sexr, "Sex" = sex)
  osexr <- melt(osexr) %>%
    left_join(w, by = "SexR") %>%
    mutate(EffN = 1 / Sigma, SD = sqrt(value * (1 - value) / EffN)) %>%
    mutate(Season = seasons[Season])

  if (length(map) > 0) {
    psexr1 <- map$pred_sex_ratio_is
    dimnames(psexr1) <- list("Iteration" = 1, "SexR" = 1:data$n_sexr, "Sex" = sex)
    psexr1 <- melt(psexr1) %>%
      left_join(w, by = "SexR") %>%
      mutate(Season = seasons[Season])
  }

  if (length(mcmc) > 0) {
    # Posterior distribution
    psexr2 <- mcmc$pred_sex_ratio_is
    dimnames(psexr2) <- list("Iteration" = 1:n_iter, "SexR" = 1:data$n_sexr, "Sex" = sex)
    psexr2 <- melt(psexr2) %>%
      left_join(w, by = "SexR") %>%
      mutate(Season = seasons[Season])

    # Posterior predictive distribution
    psexr3 <- mcmc$pred_sex_ratio_is
    for (i in 1:n_iter) {
      if (data$like_lf == 1) {
        prob <- mcmc$pred_sex_ratio_is[i,,]
        N <- ceiling(1 / mcmc$sigma_sex_ratio_i[i,])
        df <- t(mapply(rmultinom, n = 1, size = N, prob = split(x = prob, f = c(row(prob)))))
      } else {
        alpha <- mcmc$pred_sex_ratio_is[i,,] * 1 / mcmc$sigma_sex_ratio_i[i,]
        df <- t(mapply(rdirichlet, n = 1, alpha = split(x = alpha, f = c(row(alpha)))))
      }
      df <- df / rowSums(df)
      psexr3[i,,] <- df
    }
    dimnames(psexr3) <- list("Iteration" = 1:n_iter, "SexR" = 1:data$n_sexr, "Sex" = sex)
    psexr3 <- melt(psexr3) %>%
      left_join(w, by = "SexR") %>%
      mutate(Season = seasons[Season])
  }

  if (length(mcmc) > 0) {
    p <- ggplot(data = psexr2, aes(x = .data$Year, y = .data$value))
  } else if (length(map) > 0) {
    p <- ggplot(data = psexr1, aes(x = .data$Year, y = .data$value))
  }

  if (length(mcmc) > 0) {
    p <- p + stat_summary(data = psexr3, aes(x = .data$Year, y = .data$value), fun.min = function(x) quantile(x, 0.025), fun.max = function(x) quantile(x, 0.975), geom = "ribbon", alpha = 0.25, colour = NA) +
      stat_summary(data = psexr2, aes(x = .data$Year, y = .data$value), fun.min = function(x) quantile(x, 0.025), fun.max = function(x) quantile(x, 0.975), geom = "ribbon", alpha = 0.5, colour = NA) +
      stat_summary(data = psexr2, aes(x = .data$Year, y = .data$value), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1)
  }

  if (length(map) > 0) {
    p <- p + geom_line(data = psexr1, aes(x = .data$Year, y = .data$value), linetype = 2)
  }

  p <- p + geom_point(data = osexr, aes(x = .data$Year, y = .data$value), color = "tomato") +
    geom_linerange(data = osexr, aes(x = .data$Year, ymin = .data$value - .data$SD, ymax = .data$value + .data$SD), color = "tomato", alpha = 0.75) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.spacing.y = unit(2, "lines")) +
    scale_x_continuous(breaks = pretty_breaks())

  if (data$n_area == 1) {
    p <- p + facet_grid(Sex ~ Season, scales = "free_x")
  } else {
    p <- p + facet_grid(Sex ~ Region + Season, scales = "free_x")
  }

  ggsave(paste0(figure_dir, "sex_ratio.png"), p)

  # Residual plot
  X <- data$data_sexr_obs_is
  psexr2 <- mcmc$pred_sex_ratio_is
  if (n_iter > 1) {
    P1 <- colMeans(psexr2[,,1])
    P2 <- colMeans(psexr2[,,2])
    P3 <- colMeans(psexr2[,,3])
  } else {
    P1 <- psexr2[,,1]
    P2 <- psexr2[,,2]
    P3 <- psexr2[,,3]
  }
  P <- bind_cols(P1, P2, P3) %>% as.matrix()
  if (data$like_lf == 1) res <- resMulti(obs = t(X), pred = t(P)) %>% t()
  if (data$like_lf == 2) res <- resDir(obs = t(X), alpha = t(P)) %>% t()
  colnames(res) <- sex[1:2]

  resid <- melt(res[1:nrow(X),]) %>%
    rename(SexR = Var1, Sex = Var2) %>%
    left_join(w, by = "SexR") %>%
    mutate(Season = seasons[Season]) %>%
    mutate(pos = ifelse(value < 0, 1, 0), abs = abs(value))

  p <- ggplot(data  = resid, aes(x = .data$Year, y = .data$value, size = .data$Sigma)) +
    geom_hline(yintercept = 0, alpha = 0.2) +
    labs(x = xlab, y = "OSA residual") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_hline(aes(yintercept = -2), linetype = 2) +
    geom_hline(aes(yintercept = 2), linetype = 2) +
    scale_x_continuous(breaks = pretty_breaks()) +
    geom_point(aes(color = Season), alpha = 0.75) +
    scale_color_hue(direction = -1) +
    scale_size_area()

  if (data$n_area == 1) {
    p <- p + facet_grid(Sex ~ Season, scales = "free_x")
  } else {
    p <- p + facet_grid(Sex ~ Region + Season, scales = "free_x")
  }

  ggsave(paste0(figure_dir, "sex_ratio_resid.png"), p, height = 9, width = 9)
}
