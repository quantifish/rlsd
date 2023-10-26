#' Plot initial nubers
#'
#' @param object and LSD object
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param figure_dir the directory to save to
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#'
plot_initial_numbers <- function(object,
                                 xlab = "Size (mm)",
                                 ylab = "Number of individuals (thousands)",
                                 figure_dir = "figure/")
{
  data <- object@data
  mcmc <- object@mcmc

  numbers_initial_rsl_v2 <- mcmc$numbers_initial_rsl
  numbers_initial_rsl_v2[,,2,] <- mcmc$numbers_initial_rsl[,,2,] + mcmc$numbers_initial_rsl[,,3,]
  dimnames(numbers_initial_rsl_v2) <- list("Iteration" = 1:nrow(mcmc[[1]]), "Region" = 1:data$n_area, "Sex" = c("Male","Female","Mature female"), "Size" = data$size_midpoint_l)
  numbers_initial_rsl_v2 <- melt(numbers_initial_rsl_v2, value.name = "N") %>%
    filter(Sex != "Mature female", N > 0.001)

  numbers_initial_rsl_v1 <- mcmc$numbers_initial_rsl
  dimnames(numbers_initial_rsl_v1) <- list("Iteration" = 1:nrow(mcmc[[1]]), "Region" = 1:data$n_area, "Sex" = c("Male","Immature female","Mature female"), "Size" = data$size_midpoint_l)
  numbers_initial_rsl_v1 <- melt(numbers_initial_rsl_v1, value.name = "N") %>%
    filter(N > 0.001)

  p <- ggplot(data = numbers_initial_rsl_v1, aes(x = Size, y = N/1000, color = Sex, fill = Sex)) +
    #stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    #stat_summary(fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", linewidth = 1) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
    xlab(xlab) + ylab(ylab) +
    theme_lsd()
  if (data$n_area > 1) p <- p + facet_wrap(~Region, scales = "free_y")
  ggsave(paste0(figure_dir, "numbers_initial_v1.png"), p)

  p <- ggplot(data = numbers_initial_rsl_v2, aes(x = Size, y = N/1000, color = Sex, fill = Sex)) +
    #stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    #stat_summary(fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", linewidth = 1) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)))  +
    xlab(xlab) + ylab(ylab) +
    theme_lsd()
  if (data$n_area > 1) p <- p + facet_wrap(~Region, scales = "free_y")
  ggsave(paste0(figure_dir, "numbers_initial_v2.png"), p)
}


#' Plot nubers
#'
#' @param object and LSD object
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param figure_dir the directory to save to
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#'
plot_numbers <- function(object,
                         xlab = "Size (mm)",
                         ylab = "Number of individuals (thousands)",
                         figure_dir = "figure/")
{
  data <- object@data
  mcmc <- object@mcmc

  n_iter <- nrow(object@mcmc[[1]])
  years <- data$first_yr:data$last_yr
  pyears <- data$first_yr:data$last_proj_yr
  sex <- c("Male", "Immature female", "Mature female")
  seasons <- c("AW", "SS")
  bins <- object@data$size_midpoint_l
  regions <- 1:object@data$n_area

  # Numbers
  numbers <- mcmc$numbers_ytrsl
  dimnames(numbers) <- list("Iteration" = 1:n_iter, "Year" = years, "Season" = c(seasons, "EOY"), "Region" = regions, "Sex" = sex, "Size" = bins)
  numbers2 <- melt(numbers, value.name = "N") %>%
    mutate(Region = as.factor(Region)) %>%
    filter(Year == max(years)) %>%
    filter(Season == "AW")

  p <- ggplot(data = numbers2, aes(x = Size, y = N/1000, color = Sex, fill = Sex)) +
    #stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    # stat_summary(fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
    xlab(xlab) + ylab(ylab) +
    theme_lsd()
  ggsave(paste0(figure_dir, "numbers_AW_finalyear.png"), p)

  if (data$n_area > 1) {
    p <- p + facet_wrap( ~ .data$Region, ncol = 2, scales="free_y")
    ggsave(paste0(figure_dir, "numbers_AW_finalyear_byArea.png"), p)
  }
  # for (r in 1:object@data$n_area)
  # {
  #   for (t in c("AW","SS"))
  #   {
  #     for (s in c("Male","Immature female","Mature female"))
  #     {
  #       p <- ggplot(data = filter(object@mcmc$numbers_ytrsl, Season == t, Sex == s, Region == r)) +
  #         geom_line(aes(x = Size, y = N/1000, group = Iteration)) +
  #         facet_wrap(~Year) +
  #         expand_limits(y = 0) +
  #         xlab("Size (mm)") +
  #         theme_lsd()
  #       ggsave(paste0(figure_dir, "numbers_",t,r,s,".png"), p)
  #     }
  #   }
  # }
}


#' Plot projected numbers at length in the population
#'
#' @param object and LSD object
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param figure_dir the directory to save to
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#'
plot_proj_numbers <- function(object,
                         xlab = "Size (mm)",
                         ylab = "Number of individuals (thousands)",
                         figure_dir = "figure/")
{
  data <- object@data
  mcmc <- object@mcmc

  n_iter <- nrow(object@mcmc[[1]])
  n_rules <- data$n_rules
  years <- data$first_yr:data$last_yr
  pyears <- data$first_yr:data$last_proj_yr
  # sex <- c("Male", "Female", "Female")
  sex <- c("Male", "Immature female", "Mature female")
  seasons <- c("AW", "SS")
  bins <- object@data$size_midpoint_l
  regions <- 1:object@data$n_area

  # Numbers
  numbers <- mcmc$proj_numbers_jytrsl
  dimnames(numbers) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Year" = pyears, "Season" = c(seasons, "EOY"), "Region" = regions, "Sex" = sex, "Size" = bins)
  numbers2 <- melt(numbers, value.name = "N") %>%
    mutate(Region = as.factor(Region)) %>%
    mutate(Sex = ifelse(Sex == "Male", "Male", "Female")) %>%
    filter(Year %in% c(max(years), 2026, max(pyears))) %>%
    # filter(Year %in% c(min(years), max(years), max(pyears))) %>%
    filter(Season == "AW") %>%
    group_by(across(-N)) %>%
    summarise(N = sum(N))
  numbers2$Sex <- factor(numbers2$Sex, levels = c("Male", "Female"))

  p <- ggplot(data = numbers2, aes(x = Size, y = N/1000, color = factor(Rule), fill = factor(Rule))) +
    stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", linewidth = 1) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
    labs(x = xlab, y = ylab, color = "Options", fill = "Options") +
    theme_lsd() +
    facet_grid(Year ~ Sex)
  p



  # object2 <- readRDS("/home/darcy/Projects/CRA/lsd-auto/CRA1/2022/rec18_30yr_2019sq/lsd.rds")
  # data2 <- object2@data
  # mcmc2 <- object2@mcmc

  # numbers <- mcmc2$proj_numbers_jytrsl
  # dimnames(numbers) <- list("Iteration" = 1:n_iter, "Rule" = 1, "Year" = 1945:2051, "Season" = c(seasons, "EOY"), "Region" = regions, "Sex" = sex, "Size" = bins)
  # numbers3 <- melt(numbers, value.name = "N") %>%
  #   mutate(Region = as.factor(Region)) %>%
  #   mutate(Sex = case_when(
  #     Sex == "Male" ~ "Male",
  #     .default = "Female"
  #   )) %>%
  #   filter(Year %in% c(max(years), 2026, max(pyears))) %>%
  #   # filter(Year %in% c(min(years), max(years), max(pyears))) %>%
  #   filter(Season == "AW") %>%
  #   group_by(across(-N)) %>%
  #   summarise(N = sum(N))
  # numbers3$Sex <- factor(numbers3$Sex, levels = c("Male", "Female"))

  # p2 <- p +
  #   stat_summary(data = numbers3, fun = function(x) quantile(x, 0.5), geom = "line", linetype = "dashed", linewidth = 1, colour = "black")
  # p2
  # figure_dir <- ""
  # ggsave(paste0(figure_dir, "NNN.png"), p2, width = 7, height = 8)




  if (data$n_area == 1) {
    p <- p + facet_wrap( ~ .data$Sex, ncol = 2, scales = "free_y")
    ggsave(paste0(figure_dir, "numbers_AW_proj.png"), p, width = 7, height = 8)
  } else {
    p <- p + facet_wrap(.data$Region ~ .data$Sex, ncol = 2, scales = "free_y")
    ggsave(paste0(figure_dir, "numbers_AW_proj_byArea.png"), p)
  }
}
