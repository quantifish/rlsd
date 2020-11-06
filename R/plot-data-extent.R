#' Plot data extent
#'
#' Plot the data extent for tag, LF, CPUE and catch data. Plots data
#' by year as points where the point size represents the relative
#' amount of data.
#'
#' @param object and LSD object
#' @param xlab the x axis label
#' @param figure_dir the directory to save to
#' @param save_plot to save the plot to file or not
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @export
#'
plot_data_extent <- function(object,
                             xlab = "Fishing year (1 April - 31 March)",
                             figure_dir = "figure/",
                             save_plot = TRUE) {
  d <- object@data
  mcmc <- object@mcmc

  years <- d$first_yr:d$last_yr
  seasons <- c("AW", "SS")
  regions <- 1:d$n_area
  bins <- d$size_midpoint_l
  sex <- c("Male", "Immature female", "Mature female")
  sources <- c("LB", "CS")
  n_iter <- nrow(mcmc[[1]])

  scalar <- 5

  # Catch
  dsl <- d$data_catch_commercial_ryt
  dimnames(dsl) <- list(Region = regions, Year = years, Season = seasons)
  dsl <- melt(dsl,  value.name = "Catch") %>%
    mutate(Type = "SL")

  dnsl <- d$data_catch_nsl_ryt
  dimnames(dnsl) <- list(Region = regions, Year = years, Season = seasons)
  dnsl <- melt(dnsl, value.name = "Catch") %>%
    mutate(Type = "NSL")

  dcatch <- bind_rows(dnsl, dnsl) %>%
    filter(Catch > 0) %>%
    mutate(DataType = "Catch", DataSource = paste(DataType, Season, Type)) %>%
    select(-Catch) %>%
    mutate(N = scalar / 2)

  # Abundance Index
  ocpue <- data.frame(Region = d$data_cpue_area_i, Year = d$data_cpue_year_i, Source = d$data_cpue_q_i, Season = seasons[d$data_cpue_season_i], CPUE = d$data_cpue_i, Type = "CPUE", N = d$cov_cpue_sd_i) %>%
    select(-CPUE) %>%
    mutate(DataType = "CPUE", DataSource = paste(DataType, Season, Type)) %>%
    mutate(DataType = paste(DataType, Source)) %>%
    select(-Source) %>%
    mutate(N = N / max(N) * scalar)

  # Observed LF
  w <- data.frame(LF = 1:d$n_lf,
                  Year = d$data_lf_year_i,
                  Season = d$data_lf_season_i,
                  Source = d$data_lf_source_i,
                  Region = d$data_lf_area_i,
                  N = rowSums(d$data_lf_N_is))
  dlf <- mcmc$data_lf_out_isl
  dimnames(dlf) <- list("Iteration" = 1:n_iter, "LF" = 1:d$n_lf, "Sex" = sex, "Size" = bins)
  dlf <- melt(dlf) %>%
    left_join(w, by = "LF") %>%
    mutate(Type = factor(Source), Type = sources[Source]) %>%
    mutate(Season = factor(Season), Season = seasons[Season]) %>%
    filter(Iteration == 1, value >= 0) %>%
    select(-c(Iteration, LF, Size, value)) %>%
    mutate(DataType = "LF", DataSource = paste(DataType, Season, Type)) %>%
    select(-Sex, -Source) %>%
    group_by(Year, Season, Region, Type, DataType, DataSource) %>%
    summarise(N = sum(N)) %>%
    ungroup() %>%
    mutate(N = N / max(N) * scalar)

  # Tags
  tags <- data.frame("Area" = d$cov_grow_release_area_g, "Year" = d$cov_grow_release_yr_g, "Season" = "AW", "Type" = 1, DataType = "Tags") %>%
    mutate(DataSource = paste(DataType, Area), Type = as.character(Type)) %>%
    group_by(Year, Season, Area, Type, DataType, DataSource) %>%
    summarise(N = n()) %>%
    ungroup() %>%
    mutate(N = N / max(N) * scalar) %>%
    select(-Area) %>%
    full_join(data.frame("DataType" = "Tags", "Region" = regions), by = "DataType")

  #ggd <- rbind(dcatch, ocpue, dlf, tags)
  ggd <- bind_rows(dcatch, ocpue, dlf, tags)

  p <- ggplot(data = ggd) +
    geom_point(aes(x = Year, y = DataSource, colour = DataType, size = N), alpha = 0.6) +
    scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
    labs(x = xlab, y = NULL) +
    theme_lsd() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

  if (d$n_area > 1) {
    p <- p + facet_wrap(~Region)
  }

  if (save_plot) {
    if (d$n_area > 1) {
      ggsave(paste0(figure_dir, "data_extent.png"), p, width = 14)
    } else {
      ggsave(paste0(figure_dir, "data_extent.png"), p)
    }
  } else {
    return(p)
  }
}
