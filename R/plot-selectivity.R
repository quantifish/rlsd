#' Plot selectivity
#'
#' Plot selectivity by size class for each sex, region, and year.
#'
#' @param object an S4 object
#' @param xlab the x axis label
#' @param figure_dir the directory to save the figure to
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @importFrom ggridges geom_density_ridges
#' @importFrom ggokabeito scale_color_okabe_ito scale_fill_okabe_ito
#' @export
#'
plot_selectivity <- function(object,
                             xlab = "Size (mm)",
                             figure_dir = "figure/") {
  data <- object@data
  mcmc <- object@mcmc
  map <- object@map

  years <- data$first_yr:data$last_proj_yr
  n_seasons <- data$n_season
  if (n_seasons == 1) seasons = "YR"
  if (n_seasons == 2) seasons <- c("AW", "SS")
  sex <- c("Male", "Immature female", "Mature female")

  w <- data$which_sel_rsyt
  dimnames(w) <- list("Region" = object@regions, "Sex" = sex, "Year" = years, "Season" = seasons)
  w <- melt(w, value.name = "Selex")
  # w2 <- w %>%
  #   filter(grepl("female", Sex)) %>%
  #   mutate(Sex = case_when(Sex == "Immature female" ~ "IF",
  #                          Sex == "Mature female" ~ "MF")) %>%
  #   pivot_wider(names_from = Sex, values_from = Selex) %>%
  #   mutate(Check = ifelse(IF == MF, 1, 0)) %>%
  #   select(Region, Year, Season, Check)
  # w_simple <- w %>%
  #   left_join(w2) %>%
  #   mutate(Sex = case_when(Sex == "Male" ~ "Male",
  #                       Sex == "Immature female" & Check == 1 ~ "Female",
  #                       Sex == "Mature female" & Check == 1 ~ "Female",
  #                       Sex ==  "Immature female" & Check == 0 ~ "Immature female",
  #                       Sex == "Mature female" & Check == 0 ~ "Mature female")) %>%
  #   select(-Check) %>%
  #   unique()
  # sex_selex <- unique(w_simple$Sex)

  sel2 <- mcmc$selectivity_ml
  dimnames(sel2) <- list("Iteration" = 1:nrow(mcmc[[1]]), "Selex" = 1:data$n_sel, "Size" = data$size_midpoint_l)
  sel2 <- melt(sel2, value.name = "Selectivity") %>%
    inner_join(w, by = "Selex", relationship = 'many-to-many') %>%
    mutate(Year = factor(.data$Year)) %>%
    distinct(.data$Iteration, .data$Sex, .data$Selectivity, .data$Region, .data$Size, .keep_all = TRUE)
  # sel2$Sex = factor(sel2$Sex, labels = unique(sel2$Sex))

  p <- ggplot(data = sel2, aes(x = .data$Size, y = .data$Selectivity, col = .data$Year, fill = .data$Year))
  # if(data$n_sel > 3 & length(unique(sel2$Year)) == 1) {
  #     p <- ggplot(data = sel2, aes(x = .data$Size, y = .data$Selectivity, col = .data$Season, fill = .data$Season))
  # } else {
  #     p <- ggplot(data = sel2, aes(x = .data$Size, y = .data$Selectivity, col = .data$Year, fill = .data$Year))
  # }
  # if(data$n_sel > 3 & length(unique(sel2$Year)) == 1) {
  #     p <- p + stat_summary(data = sel2, aes(x = .data$Size, y = .data$Selectivity, col = .data$Season), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
  #         # stat_summary(data = sel2, aes(x = .data$Size, y = .data$Selectivity, col = .data$Season), fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
  #         stat_summary(data = sel2, aes(x = .data$Size, y = .data$Selectivity, col = .data$Season), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1)
  # } else {
  p <- p + stat_summary(data = sel2, aes(x = .data$Size, y = .data$Selectivity, col = .data$Year), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    # stat_summary(data = sel2, aes(x = .data$Size, y = .data$Selectivity, col = .data$Year), fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
    stat_summary(data = sel2, aes(x = .data$Size, y = .data$Selectivity, col = .data$Year), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1)

  if (data$n_area > 1) {
    p <- p + facet_grid(.data$Region + .data$Season ~ .data$Sex)
  } else {
    p <- p + facet_grid(.data$Season ~ .data$Sex)
  }

  p <- p +
    # scale_color_okabe_ito() +
    # scale_fill_okabe_ito() +
    #scale_x_continuous(breaks = seq(30, 90, 10)) +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(ylim = c(0, 1)) +
    xlab(xlab)

  ggsave(paste0(figure_dir, "selectivity.png"), p, width = 8, height = 8)

  ridge <- sel2 %>% group_by(Selex, Size, Region, Sex, Year, Season) %>%
    mutate(Selectivity = median(Selectivity))

  q <- ggplot(data = ridge, aes(x = .data$Size, y = .data$Year, height = .data$Selectivity, fill = .data$Sex)) +
    geom_density_ridges(stat = "identity", alpha = .6, color = "white", scale = 0.95) +
    scale_y_discrete(expand = c(0, 0), name = "Selectivity by year") +
    scale_x_continuous(expand = c(0, 0)) +
    # scale_color_okabe_ito() +
    # scale_fill_okabe_ito() +
    theme(axis.title.x = element_text(hjust = 0.5), axis.title.y = element_text(hjust = 0.5))

  if (data$n_area > 1) {
    q <- q + facet_wrap(.data$Season ~ .data$Region, ncol = 1)
  } else {
    q <- q + facet_wrap(~ .data$Season)
  }

  ggsave(paste0(figure_dir, "selectivity_ridges.png"), q)

  ################################
  ## vulnerability & selectivity
  ################################
  n_iter <- nrow(mcmc[[1]])
  pyears <- data$first_yr:data$last_proj_yr
  seasons <- c("AW", "SS")
  regions <- 1:data$n_area
  if (length(regions) > 1) regions2 <- c(regions, max(regions) + 1)
  if (length(regions) == 1) regions2 <- regions
  YR <- "YR" # label for the season before the season change year
  n_rules <- data$n_rules
  sex <- c("Male", "Immature female", "Mature female")
  bins <- data$size_midpoint_l

  which_vuln <- data$which_vuln_rsyt
  dimnames(which_vuln) <- list("Region" = regions, "Sex" = sex, "Year" = pyears, "Season" = seasons)
  wv <- melt(which_vuln) %>%
    rename(Vuln = value) %>%
    full_join(w)

  vs <- mcmc$vuln_selectivity_ytrsl
  dimnames(vs) <- list("Iteration" = 1:n_iter, "Year" = pyears, "Season" = seasons, "Region" = regions, "Sex" = sex, "Size" = bins)
  vs2 <- melt(vs) %>%
    rename(SelVuln = value) %>%
    inner_join(wv) %>%
    mutate(Year = factor(.data$Year)) %>%
    distinct(.data$Iteration, .data$Sex, .data$SelVuln, .data$Region, .data$Size, .keep_all = TRUE)


  # if(length(unique(vs2$Sex)[which(grepl("female", unique(vs2$Sex)))]) == 1){
  #   vs2 <- vs2 %>%
  #     mutate(Sex = ifelse(Sex == "Male", "Male", "Female"))
  # }

  p <- ggplot(data = vs2, aes(x = .data$Size, y = .data$SelVuln, col = .data$Season, fill = .data$Season)) +
    stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    stat_summary(fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
    stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
    scale_y_continuous(expand = c(0,0)) +
    coord_cartesian(ylim = c(0, 1)) +
    xlab(xlab) + ylab("Selectivity * Vulnerability") +
    theme_lsd()
  if (data$n_area > 1) {
    p <- p + facet_grid(.data$Region + .data$Year ~ .data$Sex)
  } else {
    p <- p + facet_grid(.data$Year ~ .data$Sex)
  }
  ggsave(paste0(figure_dir, "selectivity_vulnerability.png"), p, width = 12, height = 9)


}
