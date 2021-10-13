#' Extract model fits and load into lsdOutput object class for plotting
#' 
#' @param dir the directory to extract from
#' @param data extract a dat file
#' @param mcmc extract one or more MCMC files
#' @param variational extract a var file
#' @param model a name or label for the model being extracted
#' @import rstan
#' @import methods
#' @importFrom plyr adply
#' @importFrom utils write.csv write.table
#' @export
#'
do_extract_ref <- function(dir = ".", model = "ref_lsd")
{
    # pars we want for diagnostics
    do_extract_items <- c("pred_catch_sl_jryt", 
                          "pred_catch_nsl_jryt",
                          # "proj_catch_commercial_jryt",
                          "recruits_ry", 
                          "mp_offset_cpue_jry",
                          "biomass_vulnref_AW_jyr", 
                          "biomass_total_jytrs",
                          "Btot0_r",
                          "Btot0now_r",
                          "B0_r", 
                          "B0now_r",
                          "biomass_ssb_jyr",
                          "SSB0_r",
                          "SSB0now_r",
                          # "MSY_r",
                          # "Bmsy_r",
                          "proj_F_jytrf"#,
                          # "par_q_cpue_qy"
                          )

    # "proj_catch_commercial_jryt", 
    # "proj_catch_recreational_jryt",
    # "proj_catch_sl_jryt", 
    # "proj_catch_nsl_jryt",
    # "biomass_vuln_jytrs", 
    # "par_q_cpue_qy"

    
  # initialise object
  dS4 <- new("lsdOutput", model.name = model)
    
  datfile <- "lsd.dat"
  if (file.exists(datfile)) {
    message("Reading ", datfile)
    dS4@data <- rstan::read_rdump(datfile)
    dS4@regions <- as.character(1:dS4@data$n_area)
  } else {
    warning("no 'dat' file")
  }

  mcmcfiles <- "mpe.csv"
  if (file.exists(mcmcfiles)) {
    message("Reading ", mcmcfiles)
    # create stanfit object from mcmc outputs
    #mcmc_raw_stan <- rstan::read_stan_csv(mcmcfiles)
    mcmc_raw <- read_stan_mcmc(mcmcfiles)
    message("Extracting ", mcmcfiles)
    # create list object containing all model outputs
    dS4@mcmc <- lsd::extract(mcmc_raw, pars = do_extract_items, permuted = TRUE, include = TRUE)
  } else {
    warning("no 'mpe' file")
  }
  return(dS4)
}
