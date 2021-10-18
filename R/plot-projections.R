#' Plot AW adjusted vulnerable biomass with projections
#'
#' @param object and LSD object
#' @param scales free or fixed
#' @param show_map show MAP or not
#' @param show_mcmc show MCMC or not
#' @param show_proj show projection or not
#' @param show_ref show reference level or not
#' @param xlab the x axis label
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#'
plot_vulnref_AW_proj <- function(object,
                                 scales = "free",
                                 show_map =  FALSE,
                                 show_mcmc = TRUE,
                                 show_proj = TRUE,
                                 show_ref = TRUE,
                                 xlab = "Fishing year (1 April - 31 March)")
{
  data <- object@data
  map <- object@map
  mcmc <- object@mcmc

  cpal <- c("#56B4E9", "#009E73", "#E69F00", "tomato")

  regions <- 1:data$n_area
  n_rules <- data$n_rules

  if (length(regions) > 1) {
    regions2 <- c(regions, "Total")
  } else {
    regions2 <- regions
  }

  if (length(map) > 0 & show_map) {
    vb1 <- map$biomass_vulnref_AW_jyr
    dimnames(vb1) <- list(Iteration = 1, Rule = 1:n_rules, Year = data$first_yr:data$last_proj_yr, Region = regions)
    vb1 <- melt(vb1, value.name = "VB")
  }

  if (length(mcmc) > 0 & show_mcmc) {
    n_iter <- nrow(mcmc[[1]])

    vb2 <- mcmc$biomass_vulnref_AW_jyr
    dimnames(vb2) <- list(Iteration = 1:n_iter, Rule = 1:n_rules, Year = data$first_yr:data$last_proj_yr, Region = regions)
    vb2 <- melt(vb2, value.name = "VB") %>%
      mutate(Region = as.character(Region))

    vb3 <- vb2 %>%
      group_by(Iteration, Rule, Year) %>%
      summarise(VB = sum(VB, na.rm = TRUE)) %>%
      mutate(Region = "Total")

    vb <- bind_rows(vb2, vb3)

    Bref <- mcmc$Bref_jr
    dimnames(Bref) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Region" = regions2)
    Bref <- melt(Bref)
    Bref <- unique(Bref %>% select(.data$Region, .data$value)) %>%
      filter(.data$Region == "Total")
  }

  if (!show_proj) {
    vb <- vb %>% filter(.data$Year <= data$last_yr)
    if (length(map) > 0 & show_map) vb1 <- vb1 %>% filter(.data$Year <= data$last_yr)
  }

  vb <- vb %>% filter(.data$Year > data$season_change_yr)

  p <- ggplot(data = vb, aes(x = .data$Year, y = .data$VB, colour = factor(.data$Rule), fill = factor(.data$Rule)))

  if (show_ref) {
    p <- p +
      geom_hline(data = Bref, aes(yintercept = .data$value), colour = cpal[2], fill = NA) +
      geom_label(data = Bref, label = "Reference", aes(x = min(vb$Year) + 15, y = .data$value), colour = cpal[2], size = 5, fill = "white")
  }

  if (show_proj) {
    p <- p + geom_vline(aes(xintercept = data$last_yr), linetype = "dashed")
  }

  p <- p +
    stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.125, colour = NA) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
    scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1), expand = c(0, 1)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
    labs(x = xlab, y = "AW adjusted vulnerable biomass (tonnes)", colour = NULL, fill = NULL) +
    theme_lsd()

  if (length(map) > 0 & show_map) {
    p <- p + geom_line(data = vb1 %>% filter(.data$Region %in% regions), aes(x = .data$Year, y = .data$VB), linetype = 2, colour = cpal[1])
  }

  if (data$n_area > 1) {
    p <- p + facet_wrap(~ .data$Region, scales = scales)
  }

  return(p)
}
