#' Plot movement
#'
#' @param object and LSD object
#' @param scales free or fixed
#' @param show_map show MAP or not
#' @param xlab the x axis label
#' @param figure_dir the directory to save to
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats median
#' @importFrom stats quantile
#' @export
#'
plot_movement <- function(object,
                                 scales = "free",
                                 show_map = TRUE,
                                 xlab = "Year",
                                 ylab = "Movement proportion",
                                 figure_dir = "figure/")
{
  data <- object@data
  map <- object@map
  mcmc <- object@mcmc
  
  regions <- 1:data$n_area
  n_rules <- data$n_rules
  
  data$move_yrs
  
  if (length(mcmc) > 0) {
    n_iter <- nrow(mcmc[[1]])
    mov <- mcmc$movement_iy
    dimnames(mov) <- list(Iteration = 1:n_iter, Rules = 1:n_rules,
                          Year = data$first_yr:data$last_proj_yr)
    mov <- melt(mov, value.name = "Mov") %>%
      filter(Year %in% data$first_yr:data$last_yr)
    
    mov_est <- mov %>% filter(Year >= min(data$move_yrs) | Year >= max(data$move_yrs))
                        
    p <- ggplot(mov, aes(x = Year, y = Mov)) +
      geom_path() +
      geom_point(data = mov_est, 
                 mapping = aes(x = Year, y = Mov)) + 
      labs(x = xlab) +
      labs(y = ylab) +
      theme_lsd() +
      scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1)))
  }
  ggsave(paste0(figure_dir, "Movement.png"), p, width = 10)
}
