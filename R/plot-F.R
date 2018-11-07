#' Plot fishing mortality
#'
#' @param object and LSD object
#' @param scales free or fixed
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param figure_dir the directory to save to
#' @param ref which ref to plot
#' @param show_proj show projection or not
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#' 
plot_F <- function(object, scales = "free_y",
                   xlab = "Fishing year", 
                   ylab = "Fishing mortality (F)", 
                   figure_dir = "figure/", 
                   ref = "Fmsy", 
                   show_proj = FALSE)
{
    data <- object@data
    map <- object@map
    mcmc <- object@mcmc
    
    years <- data$first_yr:data$last_yr
    pyears <- data$first_yr:data$last_proj_yr
    seasons <- c("AW", "SS")
    regions <- 1:data$n_area

    if (length(map) > 0) {
        F_ytrf1 <- map$proj_F_ytrf
        dimnames(F_ytrf1) <- list("Iteration" = 1, "Year" = pyears, "Season" = seasons, "Region" = regions, "Fishery" = c("SL","NSL"))
        F_ytrf1 <- reshape2::melt(F_ytrf1)
        if(show_proj == FALSE){
            F_ytrf1 <- dplyr::filter(F_ytrf1, Year %in% years)
        } 

        if("Fmsy" %in% ref){
            Fmsy1 <- map$Fmsy_r
            dimnames(Fmsy1) <- list("Iteration" = 1, "Region" = regions)
            Fmsy1 <- reshape2::melt(Fmsy1) %>%
                        dplyr::rename("Fmsy" = value) %>%
                        dplyr::group_by(Iteration, Region, Fmsy)
            F_ytrf1 <- dplyr::left_join(F_ytrf1, Fmsy1, by=c("Iteration", "Region"))
            F_ytrf1$Fmsy[which(F_ytrf1$Fishery == "NSL")] <- NA

        }


    }
    
    if (length(mcmc) > 0) {
        n_iter <- nrow(mcmc[[1]])
        #F_ytrf2 <- mcmc$F_ytrf
        F_ytrf2 <- mcmc$proj_F_ytrf
        dimnames(F_ytrf2) <- list("Iteration" = 1:n_iter, "Year" = pyears, "Season" = seasons, "Region" = regions, "Fishery" = c("SL","NSL"))
        F_ytrf2 <- reshape2::melt(F_ytrf2)
        if(show_proj == FALSE){
            F_ytrf2 <- dplyr::filter(F_ytrf2, Year %in% years)
        } 

        if("Fmsy" %in% ref){
            Fmsy <- mcmc$Fmsy_r
            dimnames(Fmsy) <- list("Iteration" = 1:n_iter, "Region" = regions)
            Fmsy <- reshape2::melt(Fmsy) %>%
                        dplyr::rename("Fmsy" = value) %>%
                        dplyr::group_by(Iteration, Region, Fmsy)
            F_ytrf2 <- dplyr::left_join(F_ytrf2, Fmsy, by=c("Iteration", "Region"))
            F_ytrf2$Fmsy[which(F_ytrf2$Fishery == "NSL")] <- NA
        }
    }
    
    if (length(mcmc) > 0) {
        p <- ggplot(data = F_ytrf2, aes(x = Year)) 
    } else if (length(map) > 0) {
        p <- ggplot(data = F_ytrf1, aes(x = Year))
    }

    if (length(mcmc) > 0) {
        # F_ytrf2 <- dplyr::mutate(F_ytrf2, Label = ifelse(Year == max(F_ytrf2$Year) & Iteration == 1, "F", ""))

        p <- p + stat_summary(aes(y = value), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
            stat_summary(aes(y = value),fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
            stat_summary(aes(y = value),fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1)# + 
            # ggrepel::geom_label_repel(data = F_ytrf2, aes(label = Label, y = value), fill = "black", size = 5, color = 'white', force = 10, segment.color = '#bbbbbb', min.segment.length = unit(0, "lines"))

        if("Fmsy" %in% ref){
            F_ytrf2 <- dplyr::mutate(F_ytrf2, Label = ifelse(Year == max(F_ytrf2$Year) & Iteration == 1, "Fmsy", ""))

            p <- p + stat_summary(aes(y = Fmsy), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, fill = "tomato") +
                stat_summary(aes(y = Fmsy), fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA, fill = "tomato") +
                stat_summary(aes(y = Fmsy), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, colour = "tomato") #+ 
                # ggrepel::geom_label_repel(data = F_ytrf2, aes(label = Label, y = Fmsy), fill = "tomato", size = 5, color = 'white', force = 10, segment.color = '#bbbbbb', min.segment.length = unit(0, "lines"))
        }
    }

    
    if (length(map) > 0) {
        p <- p + geom_line(data = F_ytrf1, aes(x = Year, y = value), linetype = 2, colour = "black")
        if("Fmsy" %in% ref) p <- p + geom_line(data = F_ytrf1, aes(x = Year, y = Fmsy), linetype = 2, colour = "tomato")
    }
            
    p <- p + expand_limits(y = 0) +
        xlab(xlab) + ylab(ylab) +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd()
    
    if (data$n_area > 1) {
        p <- p + facet_grid(Region + Fishery ~ Season, scales = scales)
    } else {
        p <- p + facet_grid(Fishery ~ Season, scales = scales)
    }
    
    ggsave(paste0(figure_dir, "fishing_mortality.png"), p)
}
