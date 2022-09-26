#' Plot fishing mortality
#'
#' @param object and LSD object
#' @param scales free or fixed
#' @param xlab the x axis label
#' @param figure_dir the directory to save to
#' @param show_proj show projection or not
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#'
plot_F <- function(object, scales = "free",
                   xlab = "Fishing year",
                   figure_dir = "figure/",
                   show_proj = FALSE)
{
  data <- object@data
  map <- object@map
  mcmc <- object@mcmc
  year_change <- c( data$first_yr,data$season_change_yr)
  file_names <- c("", "_V2")

  type_catch <- data$type_catch
  if(type_catch == 1) ylab <- "Exploitation Rate (U)"
  if(type_catch == 2) ylab <- "Fishing Mortality Rate (F)"

  for (i in 1:length(year_change)) {
    years <- year_change[i]:data$last_yr
    pyears <- year_change[i]:data$last_proj_yr

    seasons <- c("AW", "SS")
    regions <- 1:data$n_area
    rules <- 1:data$n_rules

    if (length(map) > 0) {
      if(type_catch == 2){
        F_jytrf1 <-   map$proj_F_jytrf
        dimnames(F_jytrf1) <-   list("Iteration" =  1, "Rule" =  rules, "Year" =  data$first_yr:data$last_proj_yr, "Season" =  seasons, "Region" =  regions, "Fishery" =   c("SL", " NSL"))
        F_jytrf1 <-   melt( F_jytrf1 )
        F_jytrf1 <-   F_jytrf1 %>%  filter (! (Season == "SS" &  Year < 1979))
        F_jytrf1 <-   F_jytrf1 %>%  filter(  Year %  in%   pyears)

        if (  show_proj ==  FALSE) {
          F_jytrf1 <-   F_jytrf1 %>%  filter(  .data$  Year %  in%   years)
        }
      }
      if(type_catch == 1){
        F_jytrf1 <-   map$proj_U_jytrf
        dimnames(F_jytrf1) <-   list("Iteration" =  1, "Rule" =  rules, "Year" =  data$first_yr:data$last_proj_yr, "Season" =  seasons, "Region" =  regions, "Fishery" =   c("SL", "NSL"))
        F_jytrf1 <-   melt(F_jytrf1) %>%
          filter(Year %in%   pyears)

        if (show_proj ==  FALSE) {
          F_jytrf1 <-   F_jytrf1 %>%  filter(.data$Year %in% years)
        }      
      }
    }

    if (length(mcmc) > 0) {
      n_iter <- nrow(mcmc[[1]])

      if(type_catch == 2){
        F_jytrf2 <- mcmc$proj_F_jytrf
        dimnames(F_jytrf2) <- list("Iteration" = 1:n_iter, "Rule" = rules, "Year" = data$first_yr:data$last_proj_yr, "Season" = seasons, "Region" = regions, "Fishery" = c("SL", "NSL"))
        F_jytrf2 <- melt(F_jytrf2)
        F_jytrf2 <- F_jytrf2 %>% filter (! (Season=="SS" & Year<1979))
        F_jytrf2 <- F_jytrf2 %>% filter(.data$Year %in% pyears) 

        if (show_proj == FALSE) {
          F_jytrf2 <- F_jytrf2 %>% filter(.data$Year %in% years)
        }
      }
      if(type_catch == 1){
        F_jytrf2 <- mcmc$proj_U_jytrf
        dimnames(F_jytrf2) <- list("Iteration" = 1:n_iter, "Rule" = rules, "Year" = data$first_yr:data$last_proj_yr, "Season" = seasons, "Region" = regions, "Fishery" = c("SL", "NSL"))
        F_jytrf2 <- melt(F_jytrf2)
        F_jytrf2 <- F_jytrf2 %>% filter(.data$Year %in% pyears) 

        if (show_proj == FALSE) {
          F_jytrf2 <- F_jytrf2 %>% filter(.data$Year %in% years)
        }        
      }
    }

    if (length(mcmc) > 0) {
      p <- ggplot(data = F_jytrf2, aes(x = .data$Year))
    } else if (length(map) > 0) {
      p <- ggplot(data = F_jytrf1, aes(x = .data$Year))
    }

    if (length(mcmc) > 0) {
      p <- p + stat_summary(aes(y = .data$value), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(aes(y = .data$value),fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(aes(y = .data$value),fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1)

      # if ("Fmsy" %in% ref) {
      #   F_jytrf2 <- mutate(F_jytrf2, Label = ifelse(Year == max(F_jytrf2$Year) & Iteration == 1, "Fmsy", ""))
      #   p <- p + stat_summary(aes(y = Fmsy), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, fill = "tomato") +
      #     stat_summary(aes(y = Fmsy), fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA, fill = "tomato") +
      #     stat_summary(aes(y = Fmsy), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, colour = "tomato") #+
      # }
    }

    if (length(map) > 0) {
      p <- p + geom_line(data = F_jytrf1, aes(x = .data$Year, y = .data$value), linetype = 2, colour = "black")
      # if ("Fmsy" %in% ref) {
      #   p <- p + geom_line(data = F_jytrf1, aes(x = .data$Year, y = .data$Fmsy), linetype = 2, colour = "tomato")
      # }
    }

    p <- p +
      expand_limits(y = 0) +
      xlab(xlab) + ylab(ylab) +
      scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
      scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
      theme_lsd() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    if (data$n_area > 1) {
      if (data$n_rules == 1) {
        p <- p + facet_grid(.data$Region + .data$Fishery ~ .data$Season, scales = scales)
      } else {
        p <- p + facet_grid(.data$Region + .data$Fishery ~ .data$Season + .data$Rule, scales = scales)
      }
    } else {
      if (data$n_rules == 1) {
        p <- p + facet_grid(.data$Fishery ~ .data$Season, scales = scales)
      } else {
        p <- p + facet_grid(.data$Fishery ~ .data$Season + .data$Rule, scales = scales)
      }
    }

    ggsave(paste0(figure_dir, "fishing_mortality",file_names[i] , ".png"), p)
  }

}
