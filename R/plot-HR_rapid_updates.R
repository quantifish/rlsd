#' Plot harvest rate for rapid updates 2023
#'
#' @param object and LSD object
#' @param scales free or fixed
#' @param show_mcmc show MCMC or not
#' @param show_quants the quantiles to plot
#' @param xlab the x axis label
#' @param ref which reference biomass to plot
#' @import dplyr
#' @import ggplot2
#' @import ggrepel
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#'
#'
plot_HR_rapid_updates <- function(object, scales = "free",
                                              #show_map = TRUE,
                                              show_mcmc = TRUE,
                                              #show_proj = FALSE,
                                              xlab = "Fishing year (1 April - 31 March)",
                                              show_quants = c(0.05, 0.25))
{
  data <- object@data
  #map <- object@map
  mcmc <- object@mcmc
  
  cpal <- c("#56B4E9", "#009E73", "#E69F00", "tomato")
  
  years <- data$first_yr:data$last_yr
  pyears <- data$first_yr:data$last_proj_yr
  sex <- c("Male", "Immature female", "Mature female")
  seasons <- c("AW", "SS")
  regions <- 1:data$n_area
  if (length(regions) > 1) regions2 <- c(regions, "Total")
  if (length(regions) == 1) regions2 <- regions
  YR <- "YR" # label for the season before the season change year
  n_rules <- data$n_rules
  
  # if (length(map) > 0 & show_map) {
  #   if ("biomass_vulnref_jytr" %in% names(map)) {
  #     vbref1 <- map$biomass_vulnref_jytr
  #     dimnames(vbref1) <- list("Iteration" = 1, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions)
  #     vbref1 <- melt(vbref1) %>%
  #       filter(.data$value > 0) %>%
  #       mutate(Season = as.character(.data$Season), Season = ifelse(.data$Year >= data$season_change_yr, .data$Season, YR)) %>%
  #       group_by(.data$Iteration, .data$Year, .data$Season, .data$Region) %>%
  #       summarise(value = sum(.data$value))
  #   } else {
    #   vbref1 <- map$biomass_vulnref_ytr
    #   dimnames(vbref1) <- list("Iteration" = 1,"Year" = pyears, "Season" = seasons, "Region" = regions)
    #   vbref1 <- melt(vbref1) %>%
    #     filter(.data$value > 0) %>%
    #     mutate(Season = as.character(.data$Season), Season = ifelse(.data$Year >= data$season_change_yr, .data$Season, YR)) %>%
    #     group_by(.data$Iteration, .data$Year, .data$Season, .data$Region) %>%
    #     summarise(value = sum(.data$value))
    # #}
    # Bmsy1 <- map$Bmsy_r
    # dimnames(Bmsy1) <- list("Iteration" = 1, "Region" = regions)
    # Bmsy1 <- melt(Bmsy1) %>%
    #   left_join(expand.grid(Iteration = 1, Year = years), by = "Iteration") %>%
    #   mutate(Season = "AW") %>%
    #   group_by(Iteration, Region, value, Year, Season)
    # Bref1 <- map$Bref_jr
    # dimnames(Bref1) <- list("Iteration" = 1, "Rule" = 1:n_rules, "Region" = regions)
    # Bref1 <- melt(Bref1) %>%
    #   left_join(expand.grid(Iteration = 1, Year = years), by = "Iteration") %>%
    #   mutate(Season = "AW") %>%
    #   group_by(Iteration, Region, value, Year, Season)
  #}
  
  if (length(mcmc) > 0 & show_mcmc) {
    n_iter <- nrow(mcmc[[1]])
    
    if ("biomass_vulnref_jytr" %in% names(mcmc)) {
      vbref <- mcmc$biomass_vulnref_jytr
      dimnames(vbref) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions)
      vbref <- melt(vbref) %>%
        filter(.data$value > 0) %>%
        mutate(Season = as.character(.data$Season), Season = ifelse(.data$Year >= data$season_change_yr, .data$Season, YR)) %>%
        group_by(.data$Iteration, .data$Year, .data$Season, .data$Region) %>%
        summarise(value = sum(.data$value))
    } else {
      vbref <- mcmc$biomass_vulnref_ytr
      dimnames(vbref) <- list("Iteration" = 1:n_iter, "Year" = pyears, "Season" = seasons, "Region" = regions)
      vbref <- melt(vbref) %>%
        filter(.data$value > 0) %>%
        mutate(Season = as.character(.data$Season), Season = ifelse(.data$Year >= data$season_change_yr, .data$Season, YR)) %>%
        group_by(.data$Iteration, .data$Year, .data$Season, .data$Region) %>%
        summarise(value = sum(.data$value))
    }
    
    # Reference biomass - version 1
  # if (show_proj) {
  #   if (length(map) > 0 & show_map) vbref_in1 <- vbref1
  #   vbref_in2 <- vbref
  # } else {
  #   if (length(map) > 0 & show_map) {
  #     vbref_in1 <- vbref1 %>%
  #       filter(.data$Year <= data$last_yr)
  #   }
     vbref_in2 <- vbref %>%
      filter(.data$Year <= data$last_yr+1)
  # }
  din <- vbref_in2 %>%
    mutate("Label" = "") %>%
    group_by(.data$Iteration, .data$Year, .data$Season, .data$Region, .data$value, .data$Label)
  
  
  # Catch
  slcatch <- mcmc$pred_catch_sl_jryt
  dimnames(slcatch) <- list("Iteration"=1:n_iter, "Rule" = 1, "Region"= 1:data$n_area , "Year"=pyears, "Season"=seasons)
  slcatch2_t <- reshape2::melt(slcatch, value.name = "Catch") %>% 
    dplyr::group_by(Iteration, Year, Region, Season) %>%
    dplyr::summarise(Catch = sum(Catch)) %>%
    dplyr::mutate("CatchType" = "SL")
  
  nslcatch <- mcmc$pred_catch_nsl_jryt
  dimnames(nslcatch) <- list("Iteration"=1:n_iter, "Rule"=1, "Region"=1:data$n_area, "Year"=pyears, "Season"=seasons)
  nslcatch2_t <- reshape2::melt(nslcatch, value.name = "Catch") %>% 
    dplyr::group_by(Iteration, Year, Region, Season) %>%
    dplyr::summarise(Catch = sum(Catch)) %>%
    dplyr::mutate("CatchType" = "NSL")   
  
  pcatch_t <- rbind.data.frame(slcatch2_t, nslcatch2_t) %>%
    tidyr::pivot_wider(names_from = CatchType, values_from = Catch) %>%
    dplyr::mutate(Catch = SL + NSL) %>%
   subset(Year %in% 1979:c(data$last_yr+1))
  
    din_f <- din %>% filter (Year>1978)
    h_rate <- left_join(pcatch_t, din_f) %>% 
    mutate("HR" = Catch/value) 
    
  # din$Region <- sapply(1:nrow(din), function(x) paste0("Region ", din$Region[x]))
  # vbref_in1$Region <- sapply(1:nrow(vbref_in1), function(x) paste0("Region ", vbref_in1$Region[x]))
  # vbref_in2$Region <- sapply(1:nrow(vbref_in2), function(x) paste0("Region ", vbref_in2$Region[x]))
  
  p <- ggplot(data = h_rate, aes(x = .data$Year, y = .data$HR, colour = .data$Season, fill = .data$Season))
  
  if (0.05 %in% show_quants) {
    p <- p +
      stat_summary(data =h_rate, geom = "ribbon", alpha = 0.125, colour = NA, aes(x = .data$Year, y = .data$HR, fill = .data$Season),
                   fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95))
  }
  
  if (0.25 %in% show_quants) {
    p <- p +
      stat_summary(data = h_rate, geom = "ribbon", alpha = 0.25, colour = NA, aes(x = .data$Year, y = .data$HR, fill = .data$Season),
                   fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75))
  }
  
  p <- p + stat_summary(data = h_rate, aes(x = .data$Year, y = .data$HR, color = .data$Season), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
    expand_limits(y = 0) +
    labs(x = xlab, y = "Harvest rate (total catch/adjusted vulnerable biomass)") +
    scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1), expand = c(0, 1)) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    theme_lsd()
  
  # if (length(map) > 0 & show_map) {
  #   p <- p + geom_line(data = vbref_in1 %>% filter (Year>1978), aes(x = .data$Year, y = .data$value), linetype = 2)
  # }
  
  if (data$n_area > 1) {
    p <- p + facet_wrap(~ .data$Region, scales = scales)
    }
  }
  return(p)
}
