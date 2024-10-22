#' Parse model code
#'
#' This reads the core model code and combines it with the functions used by the model.
#'
#' @param base the core model code
#' @param funcs the model functions
#' @param params the model parameters
#' @param ctl the .ctl file that will be input to the model. This is needed for switching parameters on/off.
#' @param save should the full model be written to file or not
#' @importFrom rstan read_rdump
#' @export
#'
parse_model_code <- function(base = "base_lsd.stan",
                             funcs = "functions.stan", params = "parameters.stan",
                             ctl = "lsd.ctl", save = FALSE) {

  d <- read_rdump(ctl)

  pars <- as.logical(c(d$ctl_R0,
                       d$ctl_rec_dev,
                       d$ctl_M,
                       d$ctl_M_dev,
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
                       d$ctl_move,
                       d$ctl_cpue_pow))

  code_base <- readLines(con = base)
  code_funcs <- readLines(con = funcs)
  code_param <- readLines(con = params)
  code_param <- code_param[2:(length(code_param) - 1)]

  # Get the positions where the functions, fixed parameters, and estimated parameters should be inserted
  i <- grep("__FUN__", code_base)
  j <- grep("__FIX__", code_base)
  k <- grep("__PAR__", code_base)

  # Fixed parameters
  param_fix <- code_param[!pars]
  #param_fix <- code_param[grep("par_R0_r", code_param)]

  # Estimated parameters
  param_est <- code_param[pars]

  # Assemble the code
  code_full <- c(code_base[1:(i - 1)],
                 code_funcs,
                 code_base[(i + 1):(j - 1)],
                 param_fix,
                 code_base[(j + 1):(k - 1)],
                 "parameters {",
                 "",
                 param_est,
                 "",
                 "}",
                 code_base[(k + 1):length(code_base)])

  if (save) {
    writeLines(code_full, "lsd.stan")
  } else {
    return(code_full)
  }
}
