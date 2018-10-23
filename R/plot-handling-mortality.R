#' Plot handling mortality by year
#' 
#' @export
#' 
plot_handling_mortality <- function(object, 
                                    xlab = "Year", 
                                    ylab = "Handling mortality", 
                                    figure_dir = "figure/")
{
    data <- object@data
    years <- data$first_yr:data$last_yr
    pyears <- data$first_yr:data$last_proj_yr
   
    fpar_handling_mortality <- data.frame(data$fpar_handling_mortality)
    fpar_handling_mortality$Year <- pyears
    names(fpar_handling_mortality)[1] <- "value"
    fpar_handling_mortality <- dplyr::filter(fpar_handling_mortality, Year %in% years)

    p <- ggplot(data =  fpar_handling_mortality, aes(x = Year)) +
        geom_step(aes(x = Year, y = value), linetype = 1, size = 1.5) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
        xlab(xlab) + 
        ylab(ylab) +
        theme_lsd()
       
    ggsave(paste0(figure_dir, "handling_mortality.png"), p, width = 6, height = 6)
}
