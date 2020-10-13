#' Plot model fits
#'
#' @param object the lsd object
#' @param map plot MAP if a .map output is available
#' @param mcmc plot MCMC if a .mcmc output is available
#' @param variational plot variational if a .var output is available
#' @param figure_dir the directory to save figures to
#' @import dplyr
#' @import ggplot2
#' @export
#'
do_plot <- function(object,
                    map = FALSE, mcmc = FALSE, variational = FALSE,
                    figure_dir = "figure/") {

    # Data only plots
    plot_data_extent(object, figure_dir = figure_dir)

    # MCMC diagnostic plots
    if (mcmc && nrow(object@mcmc[[1]]) > 10 && dim(object@mcmc$Bcurr_jr)[2] < 10) {

        n_panel <- 12
        n_col <- 3

        rm_post <- object@mcmc_pars %>%
            dplyr::group_by(par) %>%
            dplyr::summarise(s = sum(value)) %>%
            dplyr::filter(s == 0)
        rm_prior <- object@mcmc_priors %>%
            dplyr::group_by(par) %>%
            dplyr::summarise(s = sum(value)) %>%
            dplyr::filter(s == 0)

        posteriors <- object@mcmc_pars %>%
            dplyr::filter(!par %in% rm_post$par) %>%
            dplyr::mutate(par = as.character(par), type = "Posterior")
        priors <- object@mcmc_priors %>%
            dplyr::filter(!par %in% rm_prior$par) %>%
            dplyr::mutate(par = as.character(gsub(pattern = "prior_", replacement = "par_", x = par)), type = "Prior")

        sq <- seq(1, length(unique(posteriors$par)), n_panel)

        # MCMC trace plot
        print("plotting traces")
        for (i in 1:length(sq)) {
            pq <- sq[i]:(sq[i] + n_panel - 1)
            d <- dplyr::filter(posteriors, par %in% unique(posteriors$par)[pq])
            npar <- length(unique(d$par))
            p <- ggplot(d) +
                geom_line(aes(x = as.integer(iteration), y = value, col = chain)) +
                facet_wrap(~par, scales = "free_y", ncol = n_col) +
                labs(x = "Iteration", y = NULL, col = "Chain") +
                theme_lsd()
            ggsave(paste0(figure_dir, "par_trace_", i, ".png"), p, width = ifelse(npar > 1, 8, 4), height = 10) #npar + (npar %% 2)
        }

        # MCMC histogram
        print("plotting histograms")
        for (i in 1:length(sq)) {
            pq <- sq[i]:(sq[i] + n_panel - 1)
            d <- dplyr::filter(posteriors, par %in% unique(posteriors$par)[pq])
            npar <- length(unique(d$par))
            p <- ggplot(data = d, aes(x = value, fill = chain)) +
                geom_histogram(aes(x = value), bins = 50) +
                facet_wrap(~par, scales = "free", ncol = n_col) +
                labs(x = "Value", y = NULL, fill = "Chain") +
                theme_lsd()
            ggsave(paste0(figure_dir, "par_histogram_", i, ".png"), p, width = ifelse(npar > 1, 8, 4), height = 10) #npar + (npar %% 2)
        }

        # MCMC density
        print("plotting density")
        for (i in 1:length(sq)) {
            pq <- sq[i]:(sq[i] + n_panel - 1)
            d <- rbind(posteriors, priors) %>%
                dplyr::filter(par %in% unique(posteriors$par)[pq])
            d$type <- factor(d$type, levels = c("Prior", "Posterior"))
            npar <- length(unique(d$par))
            p <- ggplot(d) +
                geom_density(aes(x = value, fill = type, colour = type), alpha = 0.5, trim = TRUE) +
                facet_wrap(~par, scales = "free", ncol = n_col, nrow = 6) +
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
            d <- dplyr::filter(posteriors, par %in% unique(posteriors$par)[pq])
            npar <- length(unique(d$par))
            p <- ggplot(d, aes(x = value, colour = chain)) +
                stat_ecdf() +
                facet_wrap(~par, scales = "free_x", ncol = n_col) +
                labs(x = "Value", y = NULL, colour = "Chain") +
                theme_lsd()
            ggsave(paste0(figure_dir, "par_cdf_", i, ".png"), p, width = ifelse(npar > 1, 7, 3.5), height = npar + (npar %% 2))
        }

        print("plotting snail")
        #plot_snail(object, figure_dir = figure_dir)
    }

    # tres <- table_residuals(object, figure_dir = figure_dir)
    print("plotting biomass")
    plot_biomass(object, figure_dir = figure_dir)
    print("plotting catch and Fs")
    plot_F(object, figure_dir = figure_dir, ref = NULL)
    # plot_catch(object, figure_dir = figure_dir, show_proj = FALSE)
    plot_catch_save(figure_dir = "figure/")
    plot_selectivity(object, figure_dir = figure_dir)
    print("plotting CPUE")
    plot_cpue(object, figure_dir = figure_dir)
    plot_puerulus(object, figure_dir = figure_dir)
    plot_maturation(object, figure_dir = figure_dir, empirical = FALSE)
    print("plotting recruitment")
    plot_recruitment_deviations(object, figure_dir = figure_dir)
    plot_recruitment(object, figure_dir = figure_dir)
    plot_recruitment_size(object, figure_dir = figure_dir)
    print("plotting growth")
    plot_growth_increment(object, figure_dir = figure_dir, empirical = FALSE)
    plot_growth_matrix(object, figure_dir = figure_dir)
    plot_initial_numbers(object, figure_dir = figure_dir)
    print("plotting sex-ratios")
    plot_sex_ratio(object, figure_dir = figure_dir)
    plot_ssb_recruitment(object, figure_dir = figure_dir)
    plot_tag_residuals(object, figure_dir = figure_dir, ylim = c(-5, 5))
    plot_q(object, figure_dir = figure_dir)
    print("plotting LFs")
    plot_lfs(object, figure_dir = figure_dir)
    plot_lfs_resid(object, figure_dir = figure_dir, ylim = c(-5, 5))
    plot_lfs_resid2(object, figure_dir = figure_dir)
    print("plotting MLS and handling mortality")
    plot_mls(object, figure_dir = figure_dir)
    plot_handling_mortality(object, figure_dir = figure_dir)

    #plot_offset_cpue(object, figure_dir = figure_dir)
    #plot_offset_cpue_lm(object, figure_dir = figure_dir)
    plot_aw_cpue_lm(object, figure_dir = figure_dir)
    #plot_surplus_production(object, figure_dir = figure_dir)
    #plot_numbers(object, figure_dir = figure_dir)
    #plot_retro(object, figure_dir = figure_dir)
}
