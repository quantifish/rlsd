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
  
  #remove Fmult and MSY derived quants
  pars <- dplyr::filter(pars, Parameter != "Bmsy_r[1]" , Parameter != "MSY_r[1]", Parameter != "Fmult_r[1]" , 
                          Parameter != "Fmsy_r[1]" , Parameter != "n_Bcurr_g_Bmsy_r[1]", Parameter != "SSBmsy_r[1]") 
  
  
  if(save_table == TRUE){
    write.csv(pars, file.path(figure_dir, "Parameters_summary.csv"), row.names=FALSE)
  } else { return(pars) }
  
}
