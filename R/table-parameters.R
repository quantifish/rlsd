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
  
  mcmc <- object@mcmc
  
  pars <- reshape2::melt(mcmc) %>%
    dplyr::select(L1 , Var2 , value ) 
  
  pars <- dplyr::filter(pars, grepl("par_", L1) & !grepl("par_rec", L1) & !grepl("par_q_cpue_qy", L1))
  
  names(pars) <- c("Parameter", "Subscript", "Estimate")
  
  pars$Parameter <- gsub(patter = "par_", replacement = "", x = pars$Parameter) 
  pars$Parameter <- gsub(patter = "_i", replacement = "", x = pars$Parameter) 
  pars$Parameter <- gsub(patter = "_r", replacement = "", x = pars$Parameter) 
  pars$Parameter <- paste(pars$Parameter," [",pars$Subscript,"]",sep="")
  pars <- pars[,-2]
  
  if(save_table == TRUE){
    write.csv(pars, file.path(figure_dir, "Parameters_summary.csv"), row.names=FALSE)
  } else { return(pars) }
  
}
