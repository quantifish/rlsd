#' Plot maturation
#' 
#' @export
#' 
plot_maturation <- function(object, xlab = "Size (mm)", ylab = "Maturation", figure_dir = "figure/", empirical = FALSE)
{
    data <- object@data
    mcmc <- object@mcmc
    
    n_iter <- nrow(mcmc[[1]])
    bins <- data$size_midpoint_l

    colnames(mcmc$maturation_l) <- bins
    names(attributes(mcmc$maturation_l)$dimnames) <- c("Iteration","Size")
    mcmc$maturation_l <- reshape2::melt(mcmc$maturation_l)
    mcmc$maturation_l$Type <- "Maturation"
    pmat <- mcmc$maturation_l
    
    dmat1 <- data$data_lf_in[,(length(bins)+1):(2*length(bins))]
    dmat1 <- data.frame(dmat1)
    names(dmat1) <- bins
    dmat1$Year <- data$data_lf_year_i
    dmat1$Season <- data$data_lf_season_i
    dmat1$Source <- data$data_lf_source_i
    dmat1 <- reshape2::melt(dmat1, id.vars = list("Year", "Season", "Source")) %>%
        dplyr::mutate(Sex = 2)
    dmat2 <- data$data_lf_in[,(2*length(bins)+1):ncol(data$data_lf_in)]
    dmat2 <- data.frame(dmat2)
    names(dmat2) <- bins
    dmat2$Year   <- data$data_lf_year_i
    dmat2$Season <- data$data_lf_season_i
    dmat2$Source <- data$data_lf_source_i
    dmat2 <- reshape2::melt(dmat2, id.vars = list("Year", "Season", "Source")) %>%
        dplyr::mutate(Sex = 3)
    dmat3 <- rbind(dmat1, dmat2) %>%
        dplyr::group_by(Year, Season, Source, variable) %>%
        dplyr::summarize(sum = sum(value))
    dmat4 <- rbind(dmat1, dmat2) %>%
        dplyr::filter(Sex == 3) %>%
        dplyr::group_by(Year, Season, Source, variable) %>%
        dplyr::summarize(value = sum(value)) %>%
        dplyr::select(Year, Season, Source, variable, value)
    dmat5 <- dplyr::full_join(dmat3, dmat4, by = c("Year", "Season", "Source", "variable")) %>%
        dplyr::mutate(value = value/sum, Size = as.numeric(as.character(variable)))
    
#    p <- ggplot() +
#        stat_summary(data = dmat5, aes(x = Size, y = value), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.05, fill = "red", colour = NA) +
#        stat_summary(data = dmat5, aes(x = Size, y = value), fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.1, fill = "red", colour = NA) +
#        stat_summary(data = dmat5, aes(x = Size, y = value), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.2, colour = "red") +
#        stat_summary(data = pmat, aes(x = Size, y = value), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
#        stat_summary(data = pmat, aes(x = Size, y = value), fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
#        stat_summary(data = pmat, aes(x = Size, y = value), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
#        expand_limits(y = c(0,1)) +
#        xlab(xlab) + ylab(ylab) +
#        theme_lsd()

   p <- ggplot() +
        stat_summary(data = pmat, aes(x = Size, y = value), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(data = pmat, aes(x = Size, y = value), fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(data = pmat, aes(x = Size, y = value), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
        expand_limits(y = c(0,1)) +
        xlab(xlab) + ylab(ylab) +
        theme_lsd()

if (empirical == T) {
    p = p +
        stat_summary(data = dmat5, aes(x = Size, y = value), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.05, fill = "red", colour = NA) +
        stat_summary(data = dmat5, aes(x = Size, y = value), fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.1, fill = "red", colour = NA) +
        stat_summary(data = dmat5, aes(x = Size, y = value), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.2, colour = "red") 
    }
    
    ggsave(paste0(figure_dir, "maturation.png"), p)
}
