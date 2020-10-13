#' Create residuals table
#'
#' @param object and LSD object
#' @param figure_dir the directory to save to
#' @param save_table TRUE or FALSE
#' @importFrom reshape2 melt
#' @export
#'
table_parameters <- function(object,
                            figure_dir = "figure/",
                            save_table)
{

  mcmc <- object@mcmc_pars

  names(mcmc) <- c("Iteration","Chain", "Parameter", "Estimate")

  pars <- mcmc %>%
    dplyr::group_by(Parameter) %>%
    dplyr::summarise(Estimate = median(Estimate))

  if(length(object@regions)==1) {
    pars <- dplyr::filter(pars, Parameter != "Bmsy_r[1]", Parameter != "MSY_r[1]", Parameter != "Fmult_r[1]" ,
                          Parameter != "Fmsy_r[1]" , Parameter != "n_Bcurr_g_Bmsy_r[1]", Parameter != "SSBmsy_r[1]",
                          Parameter != "n_Bref_g_Bmsy_r[1]" , Parameter != "n_SSBcurr_g_SSBmsy_r[1]")
    } else {
  for(i in 1:c(length(object@regions)+1)){
  #remove Fmult and MSY derived quants
  pars <- dplyr::filter(pars, Parameter != paste0("Bmsy_r", "[", i, "]"), Parameter != paste0("MSY_r","[", i, "]"),
                        Parameter != paste0("Fmult_r","[", i, "]"), Parameter != paste0("Fmsy_r","[", i, "]"),
                        Parameter != paste0("n_Bcurr_g_Bmsy_r","[", i, "]"), Parameter != paste0("SSBmsy_r","[", i, "]"),
                        Parameter != paste0("n_Bref_g_Bmsy_r","[", i, "]"), Parameter != paste0("n_SSBcurr_g_SSBmsy_r","[", i, "]"))
    }
  }
  if(save_table == TRUE){
    write.csv(pars, file.path(figure_dir, "Parameters_summary.csv"), row.names=FALSE)
  } else { return(pars) }

}
