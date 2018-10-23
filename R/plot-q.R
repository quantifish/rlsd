#' Compare catchability coefficient q from multiple models
#' 
#' @param object as LSD output object
#' @export
#' 
plot_q <- function(object, figure_dir = "figure/")
{
    data <- object@data
    mcmc <- object@mcmc

    n_iter <- nrow(mcmc[[1]])

    years <- data$first_yr:data$last_yr
    pyears <- data$first_yr:data$last_proj_yr
    nq <- data$n_q
    q_info <- data.frame("qtype"=data$data_cpue_q_i, "Season" = data$data_cpue_season_i, "Year" = data$data_cpue_year_i, "Region" = data$data_cpue_area_i)
    q_info <- q_info %>%
        dplyr::filter(Season == 1) %>%
        dplyr::mutate(QY = paste(Year, qtype))

    q <- mcmc$par_q_cpue_qy
    dimnames(q) <- list("Iteration" = 1:n_iter, "qtype" = 1:nq, "Year" = pyears)
    q <- reshape2::melt(q) %>%
        dplyr::filter(Year <= max(years)) %>%
        dplyr::filter(Year %in% unique(q_info$Year)) %>%
        dplyr::mutate(QY = paste(Year, qtype)) %>%
        dplyr::filter(QY %in% q_info$QY)

       p <- ggplot(data = q, aes(x = Year, y = value)) +
            stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
            stat_summary(fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
            stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha=0.75) + 
            expand_limits(y = 0) +
            xlab("Fishing year") + ylab("Catchability coefficient (q)") +
            # scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
            theme_lsd() +
            theme(legend.position = "none") +
            facet_wrap(~qtype, scales="free")

    # if (data$n_area > 1 & "Region" %in% colnames(q)) {
    #     p <- p + facet_wrap(Region~qtype, scales="free")
    # }
    ggsave(paste0(figure_dir, "q_over_time.png"), p, width = 10)
}
