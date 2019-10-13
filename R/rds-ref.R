#' Read in specific MCMC outputs required for calculating reference points
#'
#' @export
#'
rds_ref <- function(){

	object <- do_extract(data = TRUE, mcmc = TRUE, map = FALSE, model = "lsd")	

	ref_info <- list()
	ref_info$data <- object@data
	ref_info$mcmc <- list()
	ref_info$mcmc$proj_catch_commercial_jryt <- object@mcmc$proj_catch_commercial_jryt
	ref_info$mcmc$pred_catch_sl_jryt <- object@mcmc$pred_catch_sl_jryt
	ref_info$mcmc$resid_catch_sl_jryt <- object@mcmc$resid_catch_sl_jryt
	ref_info$mcmc$mp_offset_cpue_jry <- object@mcmc$mp_offset_cpue_jry
	ref_info$mcmc$biomass_vuln_jytrs <- object@mcmc$biomass_vuln_jytrs
	ref_info$mcmc$B0_r <- object@mcmc$B0_r
	ref_info$mcmc$biomass_ssb_jyr <- object@mcmc$biomass_ssb_jyr
	ref_info$mcmc$SSB0_r <- object@mcmc$SSB0_r
	ref_info$mcmc$MSY_r <- object@mcmc$MSY_r

	saveRDS(ref_info, "ref_lsd.rds")

}