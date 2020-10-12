#' Extract model fits and load into lsdOutput object class for plotting
#'
#' @param dir the directory to extract from
#' @param data extract a dat file
#' @param map extract an MAP file
#' @param mcmc extract one or more MCMC files
#' @param variational extract a var file
#' @param model a name or label for the model being extracted
#' @import rstan
#' @import methods
#' @importFrom plyr adply
#' @importFrom tidyr spread
#' @importFrom utils write.csv write.table
#' @export
#'
do_extract <- function(dir = ".", data = TRUE,
                       map = FALSE, mcmc = FALSE, variational = FALSE,
                       model = "lsd")
{
    # model outputs that we don't want
    dont_extract <- c("proj_numbers_ytrsl","data_lf_all_isl","resid_lf_i","par_M_r",
                      "vuln_selectivity_ytrsl",
                      "pred_catch_lf_ytrsl","F_ytrf",
                      "par_grow_ip","par_grow_beta_alpha_ip","par_sel_ip","recruits_estimated_ry")

    # pars we want for diagnostics
    do_extract <- c("lp__",
                    "lp_total","lp_tag","lp_prior","lp_sexr","lp_lf","lp_cpue","lp_puerulus",
                    "par_R0_r", "par_M_i", "par_mat_50_i", "par_mat_95_i",
                    "par_grow_alpha_i", "par_grow_beta_i", "par_grow_shape_i", "par_grow_cv_i", "par_grow_sd_i", "par_grow_dd",
                    "par_sel_1_i", "par_sel_2_i", "par_sel_3_i", "par_vuln_i",
                    "par_q_cpue_i", "par_q_puerulus_i", "par_q_cpue_drift_i", "par_cpue_pow", "par_init_erate_i", "par_move_i",
                    "B0_r", "Bmin_jr", "Bcurr_jr", "Bproj_jr", "Bmsy_r", "B0now_r", "Bcurr_B0_jr",
                    "SSB0_r", "SSBmsy_r", "SSBcurr_jr", "SSBproj_jr", "SSB0now_r", "SSBcurr_SSB0_jr", "SSBproj_SSB0_jr",
                    "Btot0_r", "Btot_curr_jr", "Btot0now_r", "Btot_proj_jr",
                    "Ntot0_r", "Ntot_curr_jr",
                    "SSBref_jr", "Bref_jr", "n_SSBcurr_g_SSBref_r", "SSBref_SSB0_jr", "Bref_B0_jr", "n_Bcurr_g_Bref_r", "n_Bref_g_Bmsy_r",
                    "MSY_r", "Fmult_r", "Fmsy_r",
                    "Hcurr_r", "Hproj_r",
                    "Rmean_r", "B0male_B0female_r", "Bmale_Bfemale_jr",
                    "CPUEcurr_jr", "CPUEproj_jr",
                    "n_Bcurr_g_Bmin_r", "n_Bcurr_g_Bmsy_r", "n_SSBcurr_g_SSBmsy_r", "n_SSBcurr_l_20SSB0_r", "n_SSBcurr_l_10SSB0_r")

    # Priors that we want to compare with posteriors
    do_extract_priors <- c("prior_R0_r","prior_M_i", "prior_q_cpue_drift_i",
                           "prior_mat_50_i","prior_mat_95_i",
                           "prior_grow_alpha_i", "prior_grow_diff_i", "prior_grow_shape_i", "prior_grow_cv_i", "prior_grow_sd_i",
                           "prior_sel_1_i", "prior_sel_2_i", "prior_sel_3_i",
                           "prior_vuln_i", "prior_init_erate_i")

    # initialise object
    dS4 <- new("lsdOutput", model.name = model)

    if (data) {
        datfile <- list.files(path = dir, pattern = "[.]dat")[grepl(model, list.files(path = dir, pattern = "[.]dat"))]
        if (length(datfile) > 0) {
            message("Reading ", datfile)
            dS4@data <- rstan::read_rdump(file.path(dir, datfile))
            dS4@regions <- as.character(1:dS4@data$n_area)
        } else warning("no 'dat' file")
    }

    if (map) {
        mapfile <- list.files(path = dir, pattern = "[.]map")[grepl(model, list.files(path = dir, pattern = "[.]map"))]
        if (length(mapfile) > 0) {
            message("Reading ", mapfile)
            map <- lsd::read_stan_map(file.path(dir, mapfile))
            dS4@map <- rstan::extract(map, pars = dont_extract, permuted = TRUE, inc_warmup = FALSE, include = FALSE)

            # trim estimated pars
            i <- apply(as.matrix(do_extract), 1, FUN = function(x) {any(grepl(x, names(map)))})
            do_extract_map <- do_extract[i]
            do_extract_map <- c(do_extract_map,
                                "sdnr_cpue", "sdnr_tag", "sdnr_sexr", "sdnr_lfs", "sdnr_puerulus",
                                "MAR_puerulus", "MAR_cpue", "MAR_tag", "MAR_sexr", "MAR_lfs", "Francis_lf_new_wt_r")

            # write to txt file
            map_tmp <- rstan::extract(map, pars = do_extract_map, permuted = FALSE, inc_warmup = FALSE, include = TRUE)
            map_tmp <- map_tmp
            map_tmp <- plyr::adply(map_tmp, 3)
            colnames(map_tmp) <- c("par", "value")
            #head(map_tmp)
            #x <- dS4@data[c("lf_like_wt_rs","sexr_like_wt","cpue_like_wt","cr_like_wt","puerulus_like_wt","tag_like_wt")]
            #unlist(x)
            #plyr::adply(x, 4)
            #map_tmp <- rbind(map_tmp)
            # write.csv(map_tmp, file = "key_parameters_map.csv", quote = FALSE, row.names = FALSE)
            write.table(map_tmp, file = "key_parameters_map.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
        } else warning("no 'map' file")
    }

    if (mcmc) {
        if (file.exists("mpe.csv")) {
            mcmcfiles <- "mpe.csv"
        } else {
            mcmcfiles <- list.files(path = dir, pattern = "[.]mcmc")[grepl(model, list.files(path = dir, pattern = "[.]mcmc"))]
        }
        if (length(mcmcfiles) > 0) {
            message("Reading ", mcmcfiles)

            # create stanfit object from mcmc outputs
            #mcmc_raw_stan <- rstan::read_stan_csv(mcmcfiles)
            mcmc_raw <- read_stan_mcmc(file.path(dir, mcmcfiles))

            # create list object containing all model outputs
            dS4@mcmc <- lsd::extract(mcmc_raw, pars = dont_extract, permuted = TRUE, include = FALSE)

            # trim estimated pars
            i <- vapply(do_extract, FUN = function(x) { x %in% mcmc_raw$pars_oi}, logical(1))
            do_extract_mcmc <- do_extract[i]
            i <- vapply(do_extract_priors, FUN = function(x) { x %in% mcmc_raw$pars_oi}, logical(1))
            do_extract_priors <- do_extract_priors[i]

            # extract pars (with chains) for diagnostic plots
            mcmc_tmp <- lsd::extract(mcmc_raw, pars = do_extract_mcmc, permuted = FALSE, include = TRUE)
            dimnames(mcmc_tmp)[[1]] <- 1:dim(mcmc_tmp)[1]
            dimnames(mcmc_tmp)[[2]] <- 1:dim(mcmc_tmp)[2]
            mcmc_tmp <- plyr::adply(mcmc_tmp, 1:3)
            colnames(mcmc_tmp) <- c('iteration', 'chain', 'par', 'value')
            dS4@mcmc_pars <- mcmc_tmp

            # write to csv
            mcmc_out <- mcmc_tmp %>% tidyr::spread(par, value)
            write.table(mcmc_out, file = "key_parameters_mcmc.txt", sep = "\t", row.names = FALSE, col.names = TRUE)

            mcmc_tmp <- lsd::extract(mcmc_raw, pars = do_extract_priors, permuted = FALSE, include = TRUE)
            dimnames(mcmc_tmp)[[1]] <- 1:dim(mcmc_tmp)[1]
            dimnames(mcmc_tmp)[[2]] <- 1:dim(mcmc_tmp)[2]
            mcmc_tmp <- plyr::adply(mcmc_tmp, 1:3)
            colnames(mcmc_tmp) <- c('iteration', 'chain', 'par', 'value')
            dS4@mcmc_priors <- mcmc_tmp
        } else {
            warning("no 'mcmc' files")
        }
    }

    return(dS4)
}
