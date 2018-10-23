#
# Utility functions that are not exported (should not be)
#
#  quantile helper function
#' @export
.quantile <- function(x, dp = 2) {
    
    y <- round(quantile(x, c(0.5, 0.025, 0.975)), dp)
    
    paste0(y[1], " (", y[2], "-", y[3], ")")
}

#' @export
.dpselect <- function(x) {
    
    y <- unlist(strsplit(x, split = "[", fixed = TRUE))[1]
    
    switch(y,
           "lp__" = 2,
           "lp_total" = 2,
           "lp_tag" = 2,
           "lp_prior" = 2,
           "lp_sexr" = 2,
           "lp_lf" = 2,
           "lp_cr" = 2,
           "lp_cpue" = 2,
           "lp_puerulus" = 2,
           "par_R0_r" = 0,
           "par_M_i" = 2,
           "par_grow_alpha_i" = 2,
           "par_grow_beta_i" = 2,
           "par_grow_diff_i" = 2,
           "par_grow_shape_i" = 2,
           "par_grow_cv_i" = 2,
           "par_grow_sd_i" = 2,
           "par_mat_50_i" = 2,
           "par_mat_95_i" = 2,
           "par_sel_1_i" = 2,
           "par_sel_2_i" = 2,
           "par_sel_3_i" = 2,
           "par_vuln_i" = 2,
           "par_q_cpue_i" = 2,
           "par_q_catch_rate_i" = 2,
           "par_q_puerulus_i" = 2,
           "SSB0_r" = 0,
           "Bmin_r" = 0,
           "Bref_r" = 0,
           "Bcurr_r" = 0,
           "Bproj_r" = 0,
           "Bmsy_r" = 0,
           "MSY_r" = 0,
           "Fmult_r" = 1,
           "Hcurr_r" = 1,
           "Hproj_r" = 1,
           2
           )
}
