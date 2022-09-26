#' Plot model fits
#'
#' This is a wrapper that generates most of the LSD model plots.
#'
#' @param object the lsd object
#' @param map plot MAP if a .map output is available
#' @param mcmc plot MCMC if a .mcmc output is available
#' @param figure_dir the directory to save figures to
#' @import dplyr
#' @import ggplot2
#' @export
#'
do_plot <- function(object, map = FALSE, mcmc = FALSE, figure_dir = "figure/") {

  # Data only plots
  plot_data_extent(object, figure_dir = figure_dir)

  # MCMC diagnostic plots
  if (mcmc && nrow(object@mcmc[[1]]) > 10 && dim(object@mcmc$Bcurr_jr)[2] < 10) {

    n_panel <- 12
    n_col <- 3

    rm_post <- object@mcmc_pars %>%
      group_by(.data$par) %>%
      summarise(s = sum(.data$value)) %>%
      filter(.data$s == 0)
    rm_prior <- object@mcmc_priors %>%
      group_by(.data$par) %>%
      summarise(s = sum(.data$value)) %>%
      filter(.data$s == 0)

    posteriors <- object@mcmc_pars %>%
      filter(!.data$par %in% rm_post$par) %>%
      mutate(par = as.character(.data$par), type = "Posterior")
    priors <- object@mcmc_priors %>%
      filter(!.data$par %in% rm_prior$par) %>%
      mutate(par = as.character(gsub(pattern = "prior_", replacement = "par_", x = par)), type = "Prior")

    posteriors_trace <- posteriors %>% filter(grepl('par', par) | grepl('lp', par))

    sq <- seq(1, length(unique(posteriors_trace$par)), n_panel)

    # MCMC trace plot
    print("plotting traces")
    for (i in 1:length(sq)) {
      pq <- sq[i]:(sq[i] + n_panel - 1)
      d <- posteriors_trace %>% filter(.data$par %in% unique(posteriors_trace$par)[pq])
      npar <- length(unique(d$par))
      p <- ggplot(d) +
        geom_line(aes(x = as.integer(.data$iteration), y = .data$value, col = .data$chain)) +
        facet_wrap(~ .data$par, scales = "free_y", ncol = n_col) +
        labs(x = "Iteration", y = NULL, col = "Chain") +
        theme_lsd()
      ggsave(paste0(figure_dir, "par_trace_", i, ".png"), p, width = ifelse(npar > 1, 8, 4), height = 10) #npar + (npar %% 2)
    }

    sq <- seq(1, length(unique(posteriors$par)), n_panel)

    # MCMC histogram
    print("plotting histograms")
    for (i in 1:length(sq)) {
      pq <- sq[i]:(sq[i] + n_panel - 1)
      d <- posteriors %>% filter(.data$par %in% unique(posteriors$par)[pq])
      npar <- length(unique(d$par))
      p <- ggplot(data = d, aes(x = .data$value, fill = .data$chain)) +
        geom_histogram(aes(x = value), bins = 50) +
        facet_wrap(~ .data$par, scales = "free", ncol = n_col) +
        labs(x = "Value", y = NULL, fill = "Chain") +
        theme_lsd()
      ggsave(paste0(figure_dir, "par_histogram_", i, ".png"), p, width = ifelse(npar > 1, 8, 4), height = 10) #npar + (npar %% 2)
    }

    # MCMC density
    print("plotting density")
    for (i in 1:length(sq)) {
      pq <- sq[i]:(sq[i] + n_panel - 1)
      d <- rbind(posteriors, priors) %>%
        filter(.data$par %in% unique(posteriors$par)[pq])
      d$type <- factor(d$type, levels = c("Prior", "Posterior"))
      npar <- length(unique(d$par))
      p <- ggplot(d) +
        geom_density(aes(x = .data$value, fill = .data$type, colour = .data$type), alpha = 0.5, trim = TRUE) +
        facet_wrap(~ .data$par, scales = "free", ncol = n_col, nrow = 6) +
        labs(x = NULL, y = NULL, colour = NULL, fill = NULL) +
        scale_colour_discrete(drop = TRUE, limits = c("Prior", "Posterior")) +
        scale_fill_discrete(drop = TRUE, limits = c("Prior", "Posterior")) +
        theme_lsd() +
        theme(legend.position = "top")
      ggsave(paste0(figure_dir, "par_density_", i, ".png"), p, width = ifelse(npar > 1, 8, 4), height = 10)
      #ggsave(paste0(figure_dir, "par_density_", i, ".png"), p, width = ifelse(npar > 1, 7, 3.5), height = npar + (npar %% 2))
    }

    # MCMC cumulative density
    print("plotting cumulative density")
    for (i in 1:length(sq)) {
      pq <- sq[i]:(sq[i] + n_panel - 1)
      d <- posteriors %>% filter(.data$par %in% unique(posteriors$par)[pq])
      npar <- length(unique(d$par))
      p <- ggplot(d, aes(x = .data$value, colour = .data$chain)) +
        stat_ecdf() +
        facet_wrap(~ .data$par, scales = "free_x", ncol = n_col) +
        labs(x = "Value", y = NULL, colour = "Chain") +
        theme_lsd()
      ggsave(paste0(figure_dir, "par_cdf_", i, ".png"), p, width = ifelse(npar > 1, 7, 3.5), height = npar + (npar %% 2))
    }

    if (object@data$snail_on == 1) {
      print("plotting snail")
      plot_snail(object, figure_dir = figure_dir)
    }
  }

  # tres <- table_residuals(object, figure_dir = figure_dir)
  print("plotting CPUE")
  plot_cpue(object, figure_dir = figure_dir)
  plot_puerulus(object, figure_dir = figure_dir)
  print("plotting sex-ratios")
  plot_sex_ratio(object, figure_dir = figure_dir)
  print("plotting catch")
  # plot_catch(object, figure_dir = figure_dir, show_proj = FALSE)
  plot_catch_save(object, figure_dir = figure_dir)
  plot_catch_rule(object, figure_dir = figure_dir)
  print("plotting fishing mortality")
  plot_F(object, figure_dir = figure_dir, ref = NULL)
  print("plotting recruitment")
  plot_recruitment_deviations(object, figure_dir = figure_dir)
  plot_recruitment(object, figure_dir = figure_dir)
  plot_recruitment_size(object, figure_dir = figure_dir)
  print("plotting growth")
  plot_growth_increment(object, figure_dir = figure_dir, empirical = FALSE)
  plot_growth_matrix(object, figure_dir = figure_dir)
  plot_tag_residuals(object, figure_dir = figure_dir, ylim = c(-5, 5))
  print("plotting LFs")
  plot_lfs(object, figure_dir = figure_dir)
  plot_lfs_resid(object, figure_dir = figure_dir, ylim = c(-5, 5))
  plot_lfs_resid2(object, figure_dir = figure_dir)
  print("plotting MLS and handling mortality")
  plot_mls(object, figure_dir = figure_dir)
  plot_handling_mortality(object, figure_dir = figure_dir)
  print("plotting biomass")
  plot_biomass(object, figure_dir = figure_dir)
  plot_ssb_recruitment(object, figure_dir = figure_dir)
  plot_q(object, figure_dir = figure_dir)
  plot_maturation(object, figure_dir = figure_dir, empirical = FALSE)
  plot_selectivity(object, figure_dir = figure_dir)
  plot_initial_numbers(object, figure_dir = figure_dir)
  plot_numbers(object, figure_dir = figure_dir)
  if (object@data$move_on > 0) {
    plot_movement(object, figure_dir = figure_dir)
  }
  # plot_offset_cpue(object, figure_dir = figure_dir)
  # plot_offset_cpue_lm(object, figure_dir = figure_dir)
  # plot_aw_cpue_lm(object, figure_dir = figure_dir)
  # plot_surplus_production(object, figure_dir = figure_dir)
  # plot_retro(object, figure_dir = figure_dir)
}
