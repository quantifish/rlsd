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
    numbers_initial_rsl_v2 <- reshape2::melt(numbers_initial_rsl_v2, value.name = "N") %>%
        dplyr::filter(Sex != "Mature female", N > 0.001)

    numbers_initial_rsl_v1 <- mcmc$numbers_initial_rsl
    dimnames(numbers_initial_rsl_v1) <- list("Iteration" = 1:nrow(mcmc[[1]]), "Region" = 1:data$n_area, "Sex" = c("Male","Immature female","Mature female"), "Size" = data$size_midpoint_l)
    numbers_initial_rsl_v1 <- reshape2::melt(numbers_initial_rsl_v1, value.name = "N") %>%
    dplyr::filter(N > 0.001)

    p <- ggplot(data = numbers_initial_rsl_v1, aes(x = Size, y = N/1000, color = Sex, fill = Sex)) +
        #stat_summary(fun.min = function(x) stats::quantile(x, 0.05), fun.max = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        #stat_summary(fun.min = function(x) stats::quantile(x, 0.25), fun.max = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(fun = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
        scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
        xlab(xlab) + ylab(ylab) +
        theme_lsd()
    if (data$n_area > 1) p <- p + facet_wrap(~Region, scales="free_y")

    ggsave(paste0(figure_dir, "numbers_initial_v1.png"), p)

    p <- ggplot(data = numbers_initial_rsl_v2, aes(x = Size, y = N/1000, color = Sex, fill = Sex)) +
        #stat_summary(fun.min = function(x) stats::quantile(x, 0.05), fun.max = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        #stat_summary(fun.min = function(x) stats::quantile(x, 0.25), fun.max = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(fun = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
        scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1)))  +
        xlab(xlab) + ylab(ylab) +
        theme_lsd()
    if (data$n_area > 1) p <- p + facet_wrap(~Region, scales="free_y")

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
    sex <- c("Male","Immature female","Mature female")
    seasons <- c("AW", "SS")
    bins <- object@data$size_midpoint_l
    regions <- 1:object@data$n_area
    n_rules <- data$n_rules

    # Numbers in final model year
    # numbers <- mcmc$numbers_ytrsl
    # dimnames(numbers) <- list("Iteration" = 1:n_iter, "Year" = years, "Season" = c(seasons, "EOY"), "Region" = regions, "Sex" = sex, "Size" = bins)
    # numbers2 <- reshape2::melt(numbers, value.name = "N") %>%
    #     dplyr::mutate(Region = as.factor(Region)) %>%
    #     dplyr::filter(Year == max(years)) %>%
    #     dplyr::filter(Season == "AW")

    # p <- ggplot(data = numbers2, aes(x = Size, y = N/1000, color = Sex, fill = Sex)) +
    #     #stat_summary(fun.min = function(x) stats::quantile(x, 0.05), fun.max = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    #     # stat_summary(fun.min = function(x) stats::quantile(x, 0.25), fun.max = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
    #     stat_summary(fun = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
    #     scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    #     xlab(xlab) + ylab(ylab) +
    #     theme_lsd()

    ## numbers in final model year and final projection year
lf <- mcmc$proj_numbers_jytrsl
dimnames(lf) <- list("Iteration" = 1:n_iter, "RuleNum" = 1:n_rules, "Year" = pyears, "Season" = c(seasons, "EOY"), "Region" = regions, "Sex" = sex, "Size" = bins)
numbers_proj <- reshape2::melt(lf, value.name = "N") %>%
  filter(Year %in% c(max(years), max(pyears))) %>%
  mutate(Region = as.character(Region)) %>%
  filter(Season == "AW") %>%
  select(-Season)

  ymax_df <- numbers_proj %>%
  group_by(Size) %>%
  summarise(Median = quantile(N, 0.5))
  ymax <- max(ymax_df$Median)

    p <- ggplot(data = numbers_proj %>% filter(Year == max(years)), aes(x = Size, y = N/1000, color = Sex, fill = Sex)) +
        stat_summary(fun = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
        scale_y_continuous(limits = c(0, ymax/1000), expand = expansion(mult = c(0, 0.1))) +
        xlab(xlab) + ylab(ylab) +
        theme_lsd()
    ggsave(paste0(figure_dir, "numbers_AW_finalyear.png"), p)

    if (data$n_area > 1) {
      p <- p + facet_wrap( ~ .data$Region, ncol = 2, scales="free_y")
      ggsave(paste0(figure_dir, "numbers_AW_finalyear_byArea.png"), p)
    }

  p <- ggplot(data = numbers_proj %>% filter(Year == max(pyears)), aes(x = Size, y = N/1000, color = Sex, fill = Sex)) +
  stat_summary(fun = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
        scale_y_continuous(limits = c(0, ymax/1000), expand = expansion(mult = c(0, 0.1))) +
        xlab(xlab) + ylab(ylab) +
        theme_lsd()

if(n_rules > 1) {
    p <- p + facet_wrap(~RuleNum)
 }
    ggsave(paste0(figure_dir, "numbers_AW_finalprojyear.png"), p)

    if (data$n_area > 1) {
      p <- p + facet_wrap( RuleNum ~ .data$Region, ncol = 2, scales="free_y")
      ggsave(paste0(figure_dir, "numbers_AW_finalprojyear_byArea.png"), p)
    }

    p <- ggplot(data = numbers_proj, aes(x = Size, y = N/1000, color = factor(Year), fill = factor(Year))) +
      stat_summary(fun = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
        scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
        guides(color=guide_legend(title="Year")) +
        xlab(xlab) + ylab(ylab) +
        facet_wrap(RuleNum ~ Sex) +
        theme_lsd()
    ggsave(paste0(figure_dir, "numbers_AW_compare_current_proj.png"), p, width = 12)





    # for (r in 1:object@data$n_area)
    # {
    #     for (t in c("AW","SS"))
    #     {
    #         for (s in c("Male","Immature female","Mature female"))
    #         {
    #             p <- ggplot(data = dplyr::filter(object@mcmc$numbers_ytrsl, Season == t, Sex == s, Region == r)) +
    #                 geom_line(aes(x = Size, y = N/1000, group = Iteration)) +
    #                 facet_wrap(~Year) +
    #                 expand_limits(y = 0) +
    #                 xlab("Size (mm)") +
    #                 theme_lsd()
    #             ggsave(paste0(figure_dir, "numbers_",t,r,s,".png"), p)
    #         }
    #     }
    # }
}
