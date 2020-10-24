#' Create residuals table
#'
#' @param object and LSD object
#' @param figure_dir the directory to save to
#' @param save_table TRUE or FALSE
#' @import dplyr
#' @export
#'
table_parameters <- function(object, figure_dir = "figure/", save_table = TRUE)
{
  mcmc <- object@mcmc_pars
  names(mcmc) <- c("Iteration", "Chain", "Parameter", "Estimate")

  pars <- mcmc %>%
    group_by(.data$Parameter) %>%
    summarise(Estimate = median(.data$Estimate))

  if (length(object@regions) == 1) {
    pars <- pars %>% filter(.data$Parameter != "Bmsy_r[1]",
                            .data$Parameter != "MSY_r[1]",
                            .data$Parameter != "Fmult_r[1]",
                            .data$Parameter != "Fmsy_r[1]",
                            .data$Parameter != "SSBmsy_r[1]",
                            .data$Parameter != "n_Bcurr_g_Bmsy_r[1]",
                            .data$Parameter != "n_Bref_g_Bmsy_r[1]",
                            .data$Parameter != "n_SSBcurr_g_SSBmsy_r[1]")
  } else {
    for (i in 1:(length(object@regions) + 1)) {
      #remove Fmult and MSY derived quants
      pars <- pars %>% filter(.data$Parameter != paste0("Bmsy_r", "[", i, "]"),
                              .data$Parameter != paste0("MSY_r","[", i, "]"),
                              .data$Parameter != paste0("Fmult_r","[", i, "]"),
                              .data$Parameter != paste0("Fmsy_r","[", i, "]"),
                              .data$Parameter != paste0("SSBmsy_r","[", i, "]"),
                              .data$Parameter != paste0("n_Bcurr_g_Bmsy_r","[", i, "]"),
                              .data$Parameter != paste0("n_Bref_g_Bmsy_r","[", i, "]"),
                              .data$Parameter != paste0("n_SSBcurr_g_SSBmsy_r","[", i, "]"))
    }
  }

  if (save_table == TRUE) {
    write.csv(pars, file.path(figure_dir, "Parameters_summary.csv"), row.names = FALSE)
  } else {
    return(pars)
  }
}
