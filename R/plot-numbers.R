#' Plot initial nubers
#' 
#' @export
#' 
plot_initial_numbers <- function(object, xlab = "Size (mm)", ylab = "Number of individuals (thousands)", figure_dir = "figure/")
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
        stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
        expand_limits(y = 0) +
        xlab(xlab) + ylab(ylab) +
        theme_lsd()
    if (data$n_area > 1) p <- p + facet_wrap(~Region)

    ggsave(paste0(figure_dir, "numbers_initial_v1.png"), p)

    p <- ggplot(data = numbers_initial_rsl_v2, aes(x = Size, y = N/1000, color = Sex, fill = Sex)) +
        stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
        expand_limits(y = 0) +
        xlab(xlab) + ylab(ylab) +
        theme_lsd()
    if (data$n_area > 1) p <- p + facet_wrap(~Region)
    
    ggsave(paste0(figure_dir, "numbers_initial_v2.png"), p)
}



#' Plot initial nubers
#' 
#' @export
#' 
plot_numbers <- function(object, xlab = "Size (mm)", ylab = "Number of individuals (thousands)", figure_dir = "figure/")
{
    data <- object@data
    mcmc <- object@mcmc

    n_iter <- nrow(object@mcmc[[1]])
    years <- object@data$first_yr:object@data$last_yr
    pyears <- object@data$first_yr:object@data$proj_yr
    sex <- c("Male","Immature female","Mature female")
    seasons <- c("AW","SS")
    bins <- object@data$size_midpoint_l
    regions <- 1:object@data$n_area
    
    # Numbers
    dimnames(object@mcmc$numbers_ytrsl) <- list("Iteration" = 1:n_iter, "Year" = years, "Season" = c(seasons, "EOY"), "Region" = regions, "Sex" = sex, "Size" = bins)
    object@mcmc$numbers_ytrsl <- reshape2::melt(object@mcmc$numbers_ytrsl, value.name = "N") %>%
        dplyr::mutate(Region = as.factor(Region))
    
    for (r in 1:object@data$n_area)
    {
        for (t in c("AW","SS"))
        {
            for (s in c("Male","Immature female","Mature female"))
            {
                p <- ggplot(data = dplyr::filter(object@mcmc$numbers_ytrsl, Season == t, Sex == s, Region == r)) +
                    geom_line(aes(x = Size, y = N/1000, group = Iteration)) +
                    facet_wrap(~Year) +
                    expand_limits(y = 0) +
                    xlab("Size (mm)") +
                    theme_lsd()
                ggsave(paste0(figure_dir, "numbers_",t,r,s,".png"), p)
            }
        }
    }
}
