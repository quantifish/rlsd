#' Parse model code
#'
#' This reads the core model code and combines it with the functions used by the model.
#'
#' @param base the core model code
#' @param funcs the functions that are called by the core model code
#' @param dat the .dat file that will be input to the model. This is needed for switching parameters on/off.
#' @param save should the full model be written to file or not
#' @import rstan
#' @export
#' 
parse_model_code <- function(base = "base_lsd",
                             funcs = c("fun_curves", "fun_growth_increment", "fun_growth_matrix", "fun_length_weight", "fun_tail_compression", "fun_equilibrium_numbers", "fun_recruitment_size", "fun_fishing_mortality", "fun_MAR", "fun_MSY"),
                             ctl = "lsd.ctl",
                             save = FALSE)
{
    d <- rstan::read_rdump(ctl)
    
    pars <- as.logical(c(d$ctl_R0,
                         d$ctl_rec_dev,
                         d$ctl_M,
                         d$ctl_grow_alpha,
                         d$ctl_grow_diff,
                         d$ctl_grow_shape,
                         d$ctl_grow_cv,
                         d$ctl_grow_sd,
                         d$ctl_grow_dd,
                         d$ctl_mat_50,
                         d$ctl_mat_95,
                         d$ctl_sel_1,
                         d$ctl_sel_2,
                         d$ctl_sel_3,
                         d$ctl_vuln,
                         d$ctl_q_cpue,
                         d$ctl_q_puerulus,
                         d$ctl_q_drift,
                         d$ctl_init_erate,
                         d$ctl_F,
                         d$move_on)) # BUG - needs its own ctl
    
    if (!grepl(".stan", base)) base <- paste0(base, ".stan")
    for (i in 1:length(funcs)) if (!grepl(".stan", funcs[i])) funcs[i] <- paste0(funcs[i], ".stan")
    
    code_base <- readLines(base)
    code_param <- readLines("parameters.stan")
    code_fun <- unlist(lapply(funcs, FUN = function(x){c("", paste0("  ", readLines(x)))}), use.names = FALSE)

    # Get the positions where the functions, fixed parameters and estimated parameters should be inserted
    i <- grep("__FUN__", code_base)
    j <- grep("__FIX__", code_base)
    k <- grep("__PAR__", code_base)
    
    code_full <- c(code_base[1:(i-1)],
                   code_fun,
                   code_base[(i+1):(j-1)],
                   code_param[!pars],
                   code_base[(j+1):(k-1)],
                   code_param[pars],
                   code_base[(k+1):length(code_base)])
    
    if (save) {
        writeLines(code_full, "lsd.stan")
    } else {
        return(code_full)
    }
}
