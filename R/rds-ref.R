#' Read in specific MCMC outputs required for calculating reference points
#'
#' @export
#'
rds_ref <- function(){

	read <- do_extract(data = TRUE, mcmc = TRUE, map = FALSE, model = "lsd")	

	ref_info <- list()
	ref_info$data <- read@data
	ref_info$mcmc <- list()
	# ref_info$mcmc$proj_catch_commercial_jryt <- read@mcmc$proj_catch_commercial_jryt
	# ref_info$mcmc$proj_catch_recreational_jryt <- read@mcmc$proj_catch_recreational_jryt
	# ref_info$mcmc$proj_catch_sl_jryt <- read@mcmc$proj_catch_sl_jryt
	# ref_info$mcmc$proj_catch_nsl_jryt <- read@mcmc$proj_catch_nsl_jryt
	ref_info$mcmc$pred_catch_sl_jryt <- read@mcmc$pred_catch_sl_jryt
	ref_info$mcmc$pred_catch_nsl_jryt <- read@mcmc$pred_catch_nsl_jryt
	ref_info$mcmc$resid_catch_sl_jryt <- read@mcmc$resid_catch_sl_jryt
	ref_info$mcmc$resid_catch_nsl_jryt <- read@mcmc$resid_catch_nsl_jryt
	ref_info$mcmc$recruits_ry <- read@mcmc$recruits_ry
	ref_info$mcmc$mp_offset_cpue_jry <- read@mcmc$mp_offset_cpue_jry
	ref_info$mcmc$biomass_vuln_jytrs <- read@mcmc$biomass_vuln_jytrs
	ref_info$mcmc$biomass_vulnref_jytr <- read@mcmc$biomass_vulnref_jytr
	ref_info$mcmc$biomass_vulnref_AW_jyr <- read@mcmc$biomass_vulnref_AW_jyr
	ref_info$mcmc$B0_r <- read@mcmc$B0_r
	ref_info$mcmc$biomass_ssb_jyr <- read@mcmc$biomass_ssb_jyr
	ref_info$mcmc$SSB0_r <- read@mcmc$SSB0_r
	ref_info$mcmc$MSY_r <- read@mcmc$MSY_r
	ref_info$mcmc$Bmsy_r <- read@mcmc$Bmsy_r

	saveRDS(ref_info, "ref_lsd.rds")

}