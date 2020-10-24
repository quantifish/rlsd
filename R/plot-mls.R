#' Plot miminum legal size (mm) by sex and season
#'
#' @param object and LSD object
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param figure_dir the directory to save to
#' @param do_save save to file or return
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @export
#'
plot_mls <- function(object,
                     xlab = "Year",
                     ylab = "Minimum legal size (mm)",
                     figure_dir = "figure/", do_save = TRUE) {
    data <- object@data

    years <- data$first_yr:data$last_yr
    pyears <- data$first_yr:data$last_proj_yr
    seasons <- c("AW","SS")
    regions <- 1:data$n_area
    sex <- c("Males", "Immature Females", "Females")

    mls_ytrs <- data$cov_mls_ytrs
        dimnames(mls_ytrs) <- list("Year" = pyears, "Season" = seasons, "Region" = regions, "Sex" = sex)
    mls_ytrs2 <- reshape2::melt(mls_ytrs) %>%
      dplyr::filter(Year %in% years, Sex %in% c("Males", "Females"))

    mls_ytrs2$Region <- sapply(1:nrow(mls_ytrs2), function(x) paste0("Region ", mls_ytrs2$Region[x]))

    p <- ggplot(data = mls_ytrs2) +
        geom_step(aes(x = Year, y = value, color = Sex), linetype = 1, size = 1.5) +
        xlab(xlab) +
        ylab(ylab) +
        theme_lsd()
    if (data$n_area > 1) {
      p <- p + facet_wrap(Season~Region)
    } else {
      p <- p + facet_wrap(Season ~ .)
    }
    if (do_save) {
      ggsave(paste0(figure_dir, "mls.png"), p, width = 12)
    } else {
      return(p)
    }
}
