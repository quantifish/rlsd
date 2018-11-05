#' Extract model fits and load into lsdOutput object class for plotting
#' 
#' @param dir the directory to extract from
#' @param data extract a dat file
#' @param map extract an MAP file
#' @param mcmc extract one or more MCMC files
#' @param variational extract a var file
#' @param model a name or label for the model being extracted
#' @include lsdOutput-class.R
#' @export
#'
do_extract_old <- function(dir = ".", data = TRUE,
                           map = FALSE, mcmc = FALSE, variational = FALSE,
                           model = character())
{
    # model outputs that we don't want
    dont_extract <- c("proj_numbers_ytrsl","data_lf_all_isl","resid_lf_i","par_M_r",
                      "vuln_selectivity_ytrsl",
                      "pred_catch_lf_ytrsl",
                      "par_grow_ip","par_grow_beta_alpha_ip")
    
    # pars we want for diagnostics
    do_extract <- c("lp__",
                    "lp_total","lp_tag","lp_prior","lp_sexr","lp_lf","lp_cr","lp_cpue","lp_puerulus",
                    "par_R0_r","par_M_i",
                    "par_grow_alpha_i","par_grow_beta_i","par_grow_shape_i","par_grow_cv_i","par_grow_sd_i",
                    "par_mat_50_i","par_mat_95_i",
                    "par_sel_1_i","par_sel_2_i","par_sel_3_i","par_vuln_i",
                    "par_q_cpue_i","par_q_catch_rate_i","par_q_puerulus_i","par_q_cpue_drift_i",
                    "par_init_erate_i", "par_move_i",
                    "SSB0_r",
                    "Bmin_r","Bref_r","Bcurr_r","Bproj_r","Bmsy_r","MSY_r","Fmult_r",
                    "Hproj_r")

    # Priors that we want to compare with posteriors
    do_extract_priors <- c("prior_R0_r","prior_M_i", "prior_q_cpue_drift_i",
                           "prior_mat_50_i","prior_mat_95_i",
                           "prior_grow_alpha_i","prior_grow_shape_i","prior_grow_cv_i","prior_grow_sd_i",
                           "prior_sel_1_i","prior_sel_3_i",
                           "prior_init_erate_i")

    # initialise object
    dS4 <- new("lsdOutput", model.name = model)
    
    if (data) {
        datfile <- list.files(pattern = "[.]dat")[grepl("lsd*", list.files(pattern = "[.]dat"))]
        if (length(datfile) > 0) {
            dS4@data <- rstan::read_rdump(datfile)
            dS4@regions <- as.character(1:dS4@data$n_area)
        } else warning("no 'dat' file")
    }

    if (map) {
        mapfile <- list.files(pattern = "[.]map")[grepl("lsd*", list.files(pattern = "[.]map"))]
        if (length(mapfile) > 0) {
            map <- lsd::read_stan_map(mapfile)
            dS4@map <- rstan::extract(map, pars = dont_extract, permuted = TRUE, inc_warmup = FALSE, include = FALSE)
            
            # trim estimated pars
            i <- apply(as.matrix(do_extract), 1, FUN = function(x) {any(grepl(x, names(map)))})
            do_extract_map <- do_extract[i]
            do_extract_map <- c(do_extract_map,
                                "SSBmsy_r", "SSBcurr_r", "SSBproj_r",
                                "Bmsy_Bref_r", "Fmult_r", 
                                "sdnr_cpue", "sdnr_tag", "sdnr_sexr", "sdnr_lfs", "sdnr_puerulus",
                                "MAR_puerulus", "MAR_cpue", "MAR_tag", "MAR_sexr", "MAR_lfs", "Francis_lf_new_wt_r")
            
            # write to csv
            map_tmp <- rstan::extract(map, pars = do_extract_map, permuted = FALSE, inc_warmup = FALSE, include = TRUE)
            map_tmp <- map_tmp
            map_tmp <- plyr::adply(map_tmp, 3)
            colnames(map_tmp) <- c("par", "value")
            #head(map_tmp)
            #x <- dS4@data[c("lf_like_wt_rs","sexr_like_wt","cpue_like_wt","cr_like_wt","puerulus_like_wt","tag_like_wt")]
            #unlist(x)
            #plyr::adply(x, 4)
            #map_tmp <- rbind(map_tmp)
            write.csv(map_tmp, file = "key_parameters_map.csv", quote = FALSE, row.names = FALSE)
        } else warning("no 'map' file")
    }
    
    if (mcmc) {
        # if(file.exists("mpe.mcmc")){
        #     mcmcfiles <- "mpe.mcmc"
        # } else {
            mcmcfiles <- list.files(pattern = "[.]mcmc")[grepl("lsd*", list.files(pattern = "[.]mcmc"))]
        # }
        if (length(mcmcfiles) > 0) {
            # create stanfit object from mcmc outputs
            mcmc_raw <- rstan::read_stan_csv(mcmcfiles)
            
            # create list object containing all model outputs
            dS4@mcmc <- rstan::extract(mcmc_raw, pars = dont_extract, permuted = TRUE, inc_warmup = FALSE, include = FALSE)
            
            # trim estimated pars
            i <- apply(as.matrix(do_extract), 1, FUN = function(x) {any(grepl(x, names(mcmc_raw)))})
            do_extract_mcmc <- do_extract[i]
            i <- apply(as.matrix(do_extract_priors), 1, FUN = function(x) {any(grepl(x, names(mcmc_raw)))})
            do_extract_priors <- do_extract_priors[i]
            
            # extract pars (with chains) for diagnostic plots
            mcmc_tmp <- rstan::extract(mcmc_raw, pars = do_extract_mcmc, permuted = FALSE, inc_warmup = FALSE, include = TRUE)
            dimnames(mcmc_tmp)[[1]] <- 1:dim(mcmc_tmp)[1]
            dimnames(mcmc_tmp)[[2]] <- 1:dim(mcmc_tmp)[2]
            mcmc_tmp <- plyr::adply(mcmc_tmp, 1:3)
            colnames(mcmc_tmp) <- c('iteration', 'chain', 'par', 'value')
            dS4@mcmc_pars <- mcmc_tmp

            # write to csv
            mcmc_out <- mcmc_tmp %>% tidyr::spread(par, value)
            write.csv(mcmc_out, file = "key_parameters_mcmc.csv", quote = FALSE, row.names = FALSE)
            
            mcmc_tmp <- rstan::extract(mcmc_raw, pars = do_extract_priors, permuted = FALSE, inc_warmup = FALSE, include = TRUE)
            dimnames(mcmc_tmp)[[1]] <- 1:dim(mcmc_tmp)[1]
            dimnames(mcmc_tmp)[[2]] <- 1:dim(mcmc_tmp)[2]
            mcmc_tmp <- plyr::adply(mcmc_tmp, 1:3)
            colnames(mcmc_tmp) <- c('iteration', 'chain', 'par', 'value')
            dS4@mcmc_priors <- mcmc_tmp
        } else warning("no 'mcmc' files")
    }
    
    if (variational) {
        varfiles <- list.files(pattern = "[.]var")[grepl("lsd*", list.files(pattern = "[.]var"))]
        if (length(varfiles) > 0) {
            warning("'var' files cannot be read in yet!")
            #vari <- lsd::read_stan_var(varfiles) # not yet working
            #dS4@variational <- rstan::extract(vari, pars = dont_extract, permuted = TRUE, inc_warmup = FALSE, include = FALSE)
        } else warning("no 'var' files")
    }
        
    return(dS4)
}
