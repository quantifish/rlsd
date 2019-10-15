#' Plot stock recruit relationship
#' 
#' @param object and LSD object
#' @param scales free or fixed
#' @param show_map show MAP or not
#' @param xlab the x axis label
#' @param figure_dir the directory to save to
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats median
#' @importFrom stats quantile
#' @export
#' 
plot_ssb_recruitment <- function(object,
                                 scales = "free_x",
                                 show_map = TRUE,
                                 xlab = "Spawning stock biomass (tonnes)", 
                                 figure_dir = "figure/")
{
    data <- object@data
    map <- object@map
    mcmc <- object@mcmc
    
    regions <- 1:data$n_area
    n_rules <- data$n_rules
    
    # if (length(map) > 0) {

    # }
    
    if (length(mcmc) > 0) {
        n_iter <- nrow(mcmc[[1]])
        ssb <- mcmc$biomass_ssb_jyr
        dimnames(ssb) <- list(Iteration = 1:n_iter, Rules = 1:n_rules,
                              Year = data$first_yr:data$last_proj_yr, 
                              Region = regions)
        ssb <- reshape2::melt(ssb, value.name = "SSB") %>%
            dplyr::filter(Year %in% data$first_yr:data$last_yr)
        
        rec <- mcmc$recruits_ry
        dimnames(rec) <- list(Iteration = 1:n_iter, Region = regions, 
                              Year = data$first_yr:data$last_proj_yr)
        rec <- reshape2::melt(rec, value.name = "Recruitment") %>%
            dplyr::filter(Year %in% data$first_yr:data$last_yr)
        
        d <- inner_join(ssb, rec) %>%
          dplyr::group_by(Rules, Year, Region) %>%
          dplyr::summarise(SSB = median(SSB), Recruitment = median(Recruitment)) %>%
          dplyr::ungroup()
        d$Region <- sapply(1:nrow(d), function(x) paste0("Region ", d$Region[x]))

      p <- ggplot(d, aes(x = SSB, y = Recruitment)) +
        geom_path() +
        geom_text(aes(label = Year, colour = Year)) +
        expand_limits(x = 0, y = 0) +
        labs(x = xlab) +
        facet_wrap(~Region) +
        theme_lsd()

    }
        #stat_smooth(method = "loess") +
    # B0 <- 179280
    # (SSB / B0)/(1-((5*h-1)/(4*h))*(1-(SSB / B0)));
    ggsave(paste0(figure_dir, "biomass_ssb_recruitment.png"), p, width = 12)
}


#' Plot SSB
#' 
#' @param object and LSD object
#' @param scales free or fixed
#' @param show_map show MAP or not
#' @param show_mcmc show MCMC or not
#' @param show_proj show projection or not
#' @param show_target show target or not
#' @param xlab the x axis label
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#' 
plot_ssb <- function(object,
                     scales = "free_x",
                     show_map = TRUE,
                     show_mcmc = TRUE,
                     show_proj = FALSE,
                     show_target = FALSE,
                     xlab = "Fishing year (1 April - 31 March)")
{
  data <- object@data
  map <- object@map
  mcmc <- object@mcmc
  
  cpal <- c("#56B4E9", "#009E73", "#E69F00", "tomato")
  
  years <- data$first_yr:data$last_yr
  pyears <- data$first_yr:data$last_proj_yr
  regions <- 1:data$n_area
  n_rules <- data$n_rules
  
  if (length(map) > 0 & show_map) {
    ssb1 <- map$biomass_ssb_jyr
    dimnames(ssb1) <- list(Iteration = 1, Rule = 1:n_rules, Year = data$first_yr:data$last_proj_yr, Region = regions)
    ssb1 <- reshape2::melt(ssb1, value.name = "SSB")
  }
  
  if (length(mcmc) > 0 & show_mcmc) {
    n_iter <- nrow(mcmc[[1]])
    ssb <- mcmc$biomass_ssb_jyr
    dimnames(ssb) <- list(Iteration = 1:n_iter, Rule = 1:n_rules, Year = data$first_yr:data$last_proj_yr, Region = regions)
    ssb <- reshape2::melt(ssb, value.name = "SSB")
    
    SSB0 <- mcmc$SSB0_r
    dimnames(SSB0) <- list("Iteration" = 1:n_iter, "Region" = regions)
    hard_limit <- reshape2::melt(SSB0) %>%
      dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = years), by = "Iteration") %>%
      dplyr::group_by(Iteration, Region, value, Year) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Rule = 1, type = "Hard limit", value = value * 0.1)
    soft_limit <- reshape2::melt(SSB0) %>%
      dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = years), by = "Iteration") %>%
      dplyr::group_by(Iteration, Region, value, Year) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Rule = 1, type = "Soft limit", value = value * 0.2)
    
    SSBref <- mcmc$SSBref_jr
    dimnames(SSBref) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Region" = regions)
    SSBref <- reshape2::melt(SSBref) %>%
      dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = years), by = "Iteration") %>%
      dplyr::group_by(Iteration, Region, Rule, value, Year) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(type = "Target")
  }
  
  # spawning stock biomass
  if (show_proj) {
    ssb_in <- ssb %>%
      dplyr::mutate(type = "SSB") %>%
      dplyr::rename(value = SSB) %>%
      dplyr::bind_rows(soft_limit, hard_limit, SSBref)
    if (length(map) > 0 & show_map) ssb1_in <- ssb1 %>% dplyr::mutate(type = "SSB")
  } else {
    ssb_in <- dplyr::filter(ssb, Year <= data$last_yr) %>%
      dplyr::mutate(type = "SSB") %>%
      dplyr::rename(value = SSB) %>%
      dplyr::bind_rows(soft_limit, hard_limit, SSBref)
    if (length(map) > 0 & show_map) ssb1_in <- dplyr::filter(ssb1, Year <= data$last_yr) %>% dplyr::mutate(type = "SSB")
  }
  ssb_in$type <- factor(ssb_in$type, levels = c("SSB","Target","Soft limit","Hard limit"))

  if (!show_target) {
      ssb_in <- dplyr::filter(ssb_in, type != "Target")
  }
  ssb_in$Region <- sapply(1:nrow(ssb_in), function(x) paste0("Region ", ssb_in$Region[x]))
  ssb1_in$Region <- sapply(1:nrow(ssb1_in), function(x) paste0("Region ", ssb1_in$Region[x]))
  
  p <- ggplot(data = ssb_in, aes(x = Year, y = value))
  if (show_target) {
      p <- p + geom_vline(aes(xintercept = data$first_ref_yr), linetype = "dashed", colour = cpal[2]) +
          geom_vline(aes(xintercept = data$last_ref_yr), linetype = "dashed", colour = cpal[2])
  }
  if (show_proj) p <- p + geom_vline(aes(xintercept = data$last_yr), linetype = "dashed")
  p <- p +
    stat_summary(aes(fill = type), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.125) +
    stat_summary(aes(fill = type), fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.25) +
    stat_summary(aes(colour = type), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
    expand_limits(y = 0) +
    labs(x = xlab, y = "Spawning stock biomass (tonnes)", colour = NULL, fill = NULL) +
    scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1), expand = c(0, 1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(ssb_in$value)*1.05)) +
    scale_fill_manual(values = cpal) +
    scale_colour_manual(values = cpal) +
    theme_lsd()
  if (length(map) > 0 & show_map) {
    p <- p + geom_line(data = ssb1_in, aes(x = Year, y = SSB, colour = type), linetype = 2)
  }
  if (data$n_area > 1) {
    p <- p + facet_wrap(~Region)
  }
  return(p)
}


#' Plot vulnerable reference biomass
#'
#' @param object and LSD object
#' @param scales free or fixed
#' @param show_map show MAP or not
#' @param show_mcmc show MCMC or not
#' @param show_proj show projection or not
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
plot_vulnerable_reference_biomass <- function(object,
                                              scales = "free_x",
                                              show_map = TRUE,
                                              show_mcmc = TRUE,
                                              show_proj = FALSE,
                                              xlab = "Fishing year (1 April - 31 March)",
                                              show_quants = c(0.05, 0.25),
                                              ref = c("Bmsy", "Bref"))
{
  data <- object@data
  map <- object@map
  mcmc <- object@mcmc
  
  cpal <- c("#56B4E9", "#009E73", "#E69F00", "tomato")
  
  years <- data$first_yr:data$last_yr
  pyears <- data$first_yr:data$last_proj_yr
  sex <- c("Male","Immature female","Mature female")
  seasons <- c("AW","SS")
  regions <- 1:data$n_area
  YR <- "YR" # label for the season before the season change year
  n_rules <- data$n_rules
  
  if (length(map) > 0 & show_map) {
    vbref1 <- map$biomass_vulnref_ytr
    dimnames(vbref1) <- list("Iteration" = 1,"Year" = pyears, "Season" = seasons, "Region" = regions)
    vbref1 <- reshape2::melt(vbref1) %>%
      dplyr::filter(value > 0) %>%
      dplyr::mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR))
    
    vbref1 <- vbref1 %>%
      dplyr::group_by(Iteration, Year, Season, Region) %>%
      dplyr::summarise(value = sum(value))
    
    # Bmsy1 <- map$Bmsy_r
    # dimnames(Bmsy1) <- list("Iteration" = 1, "Region" = regions)
    # Bmsy1 <- reshape2::melt(Bmsy1) %>%
    #   dplyr::left_join(expand.grid(Iteration = 1, Year = years), by = "Iteration") %>%
    #   dplyr::mutate(Season = "AW") %>%
    #   dplyr::group_by(Iteration, Region, value, Year, Season)
    
    
    # Bref1 <- map$Bref_jr
    # dimnames(Bref1) <- list("Iteration" = 1, "Rule" = 1:n_rules, "Region" = regions)
    # Bref1 <- reshape2::melt(Bref1) %>%
    #   dplyr::left_join(expand.grid(Iteration = 1, Year = years), by = "Iteration") %>%
    #   dplyr::mutate(Season = "AW") %>%
    #   dplyr::group_by(Iteration, Region, value, Year, Season)
  }
  
  if (length(mcmc) > 0 & show_mcmc) {
    n_iter <- nrow(mcmc[[1]])
    
    vbref <- mcmc$biomass_vulnref_ytr
    dimnames(vbref) <- list("Iteration" = 1:n_iter,"Year" = pyears, "Season" = seasons, "Region" = regions)
    vbref <- reshape2::melt(vbref) %>%
      dplyr::filter(value > 0) %>%
      dplyr::mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR))
    
    vbref <- vbref %>%
      dplyr::group_by(Iteration, Year, Season, Region) %>%
      dplyr::summarise(value = sum(value))
    
    Bmsy <- mcmc$Bmsy_r
    dimnames(Bmsy) <- list("Iteration" = 1:n_iter, "Region" = regions)
    Bmsy <- reshape2::melt(Bmsy) %>%
      dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = years), by = "Iteration") %>%
      dplyr::mutate(Season = "AW") %>%
      dplyr::group_by(Iteration, Region, value, Year, Season)
    
    Bref <- mcmc$Bref_jr
    dimnames(Bref) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Region" = regions)
    Bref <- reshape2::melt(Bref) %>%
      dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = years), by = "Iteration") %>%
      dplyr::mutate(Season = "AW") %>%
      dplyr::group_by(Iteration, Region, value, Year, Season)
  }
  
  # Reference biomass - version 1
  if (show_proj) {
    if (length(map) > 0 & show_map) vbref_in1 <- vbref1
    vbref_in2 <- vbref
  } else {
    if (length(map) > 0 & show_map) {
      vbref_in1 <- vbref1 %>%
        dplyr::filter(Year <= data$last_yr)
    }
    vbref_in2 <- vbref %>%
      dplyr::filter(Year <= data$last_yr)
  }
  din <- vbref_in2 %>% dplyr::mutate("Label" = "") %>%
    dplyr::group_by(Iteration, Year, Season, Region, value, Label)

  din$Region <- sapply(1:nrow(din), function(x) paste0("Region ", din$Region[x]))
  vbref_in1$Region <- sapply(1:nrow(vbref_in1), function(x) paste0("Region ", vbref_in1$Region[x]))
  vbref_in2$Region <- sapply(1:nrow(vbref_in2), function(x) paste0("Region ", vbref_in2$Region[x]))
  
  p <- ggplot(data = vbref_in2, aes(x = Year, y = value, colour = Season, fill = Season))
  if (show_proj) p <- p + geom_vline(aes(xintercept = data$last_yr), linetype = "dashed")
  
  if ("Bref" %in% ref) {
    Bref <- dplyr::mutate(Bref, Label = ifelse(Year == max(Bref$Year) & Iteration == 1 & Rule == 1, "Bref", ""))
    Bmsy <- dplyr::mutate(Bmsy, Label = "")
    dl <- rbind(din, Bmsy, Bref) %>% dplyr::filter(Iteration %in% seq(1, n_iter, length.out = 500))
    
    p <- p +
      geom_vline(aes(xintercept = data$first_ref_yr), linetype = "dashed", colour = cpal[2]) + 
      geom_vline(aes(xintercept = data$last_ref_yr), linetype = "dashed", colour = cpal[2]) +
      stat_summary(data = Bref, aes(x = Year, y = value), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, fill = cpal[2]) +
      stat_summary(data = Bref, aes(x = Year, y = value), fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.25, colour = NA, fill = cpal[2]) +
      stat_summary(data = Bref, aes(x = Year, y = value), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, colour = cpal[2]) +
      ggrepel::geom_label_repel(data = dl, aes(label = Label), fill = cpal[2], size = 5, color = 'white', force = 10, segment.color = '#bbbbbb', min.segment.length = unit(0, "lines"))
    # if(length(map) > 0 & show_map) p <- p + geom_line(data = Bref1, aes(x = Year, y = value), linetype = 2, colour = cpal[2])
  }
  
  if ("Bmsy" %in% ref) {
    Bmsy <- dplyr::mutate(Bmsy, Label = ifelse(Year == max(Bmsy$Year) & Iteration == 1, "Bmsy", ""))
    Bref <- dplyr::mutate(Bref, Label = "")
    dl <- rbind(din, Bmsy, Bref) %>% dplyr::filter(Iteration %in% seq(1, n_iter, length.out = 500))
    
    p <- p +
      stat_summary(data = Bmsy, aes(x = Year, y = value), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.125, colour = NA, fill = "#BCBDDC") +
      stat_summary(data = Bmsy, aes(x = Year, y = value), fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.25, colour = NA, fill = "#BCBDDC") +
      stat_summary(data = Bmsy, aes(x = Year, y = value), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, colour = "#BCBDDC") +
      ggrepel::geom_label_repel(data = dl, aes(label = Label), fill = "#BCBDDC", size = 5, color = 'white', force = 10, segment.color = '#bbbbbb', min.segment.length = unit(0, "lines"))
    # if(length(map) > 0 & show_map) p <- p + geom_line(data=Bmsy1, aes(x = Year, y = value), linetype = 2, colour = "#BCBDDC")
  }
  
  if (0.05 %in% show_quants) {
    p <- p + 
      stat_summary(data = din, geom = "ribbon", alpha = 0.125, colour = NA, aes(x = Year, y = value, fill = Season), 
                   fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95))
  }
  if (0.25 %in% show_quants) {
    p <- p + 
      stat_summary(data = din, geom = "ribbon", alpha = 0.25, colour = NA, aes(x = Year, y = value, fill = Season), 
                   fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75))
  }
  p <- p + stat_summary(data = din, aes(x = Year, y = value, color = Season), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
    expand_limits(y = 0) +
    labs(x = xlab, y = "Vulnerable reference biomass (tonnes)") +
    scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1), expand = c(0, 1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(din$value)*1.05)) +
    theme_lsd()
  if (length(map) > 0 & show_map) {
    p <- p + geom_line(data = vbref_in1, aes(x = Year, y = value), linetype = 2)
  }
  if (data$n_area > 1) {
    p <- p + facet_wrap(~Region)
  }
  return(p)
}


#' Plot vulnerable reference biomass
#'
#' @param object and LSD object
#' @param scales free or fixed
#' @param show_map show MAP or not
#' @param show_mcmc show MCMC or not
#' @param show_proj show projection or not
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
plot_vulnerable_biomass <- function(object,
                                    scales = "free_x",
                                    show_map = TRUE,
                                    show_mcmc = TRUE,
                                    show_proj = FALSE,
                                    xlab = "Fishing year (1 April - 31 March)",
                                    show_quants = c(0.05, 0.25),
                                    ref = c("Bmsy", "Bref"))
{
  data <- object@data
  map <- object@map
  mcmc <- object@mcmc
  
  cpal <- c("#56B4E9", "#009E73", "#E69F00", "tomato")
  
  years <- data$first_yr:data$last_yr
  pyears <- data$first_yr:data$last_proj_yr
  sex <- c("Male","Immature female","Mature female")
  seasons <- c("AW","SS")
  regions <- 1:data$n_area
  YR <- "YR" # label for the season before the season change year
  n_rules <- data$n_rules
  
  if (length(map) > 0 & show_map) {
    vb1 <- map$biomass_vuln_jytrs
    dimnames(vb1) <- list("Iteration" = 1, "Rule"=1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions, Sex = sex)
    vb1 <- reshape2::melt(vb1) %>%
      dplyr::filter(value > 0) %>%
      dplyr::mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR))

    vb1 <- vb1 %>%
      dplyr::group_by(Iteration, Rule, Year, Season, Region) %>%
      dplyr::summarise(value = sum(value))

    # Bmsy1 <- map$Bmsy_r
    # dimnames(Bmsy1) <- list("Iteration" = 1, "Region" = regions)
    # Bmsy1 <- reshape2::melt(Bmsy1) %>%
    #   dplyr::left_join(expand.grid(Iteration = 1, Year = years), by = "Iteration") %>%
    #   dplyr::mutate(Season = "AW") %>%
    #   dplyr::group_by(Iteration, Region, value, Year, Season)
    
    # Bref1 <- map$Bref_jr
    # dimnames(Bref1) <- list("Iteration" = 1, "Rule" = 1:n_rules, "Region" = regions)
    # Bref1 <- reshape2::melt(Bref1) %>%
    #   dplyr::left_join(expand.grid(Iteration = 1, Year = years), by = "Iteration") %>%
    #   dplyr::mutate(Season = "AW") %>%
    #   dplyr::group_by(Iteration, Region, value, Year, Season)
  }
  
  if (length(mcmc) > 0 & show_mcmc) {
    n_iter <- nrow(mcmc[[1]])
    
    vb2 <- mcmc$biomass_vuln_jytrs
    dimnames(vb2) <- list("Iteration" = 1:n_iter, "Rule"=1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions, Sex = sex)
    vb2 <- reshape2::melt(vb2) %>%
      dplyr::filter(value > 0) %>%
      dplyr::mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR)) %>%
      dplyr::group_by(Iteration, Rule, Year, Season, Region) %>%
      dplyr::summarise(value = sum(value))
    
    Bmsy <- mcmc$Bmsy_r
    dimnames(Bmsy) <- list("Iteration" = 1:n_iter, "Region" = regions)
    Bmsy <- reshape2::melt(Bmsy) %>%
      dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = years), by = "Iteration") %>%
      dplyr::mutate(Season = "AW") %>%
      dplyr::group_by(Iteration, Region, value, Year, Season)
    
    Bref <- mcmc$Bref_jr
    dimnames(Bref) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Region" = regions)
    Bref <- reshape2::melt(Bref) %>%
      dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = years), by = "Iteration") %>%
      dplyr::mutate(Season = "AW") %>%
      dplyr::group_by(Iteration, Region, value, Year, Season)
  }
  
  # vulnerable biomass
  if (show_proj) {
    if (length(map) > 0 & show_map) vb_in1 <- vb1
    vb_in2 <- vb2
  } else {
    if (length(map) > 0 & show_map) {
      vb_in1 <- vb1 %>% dplyr::filter(Year <= data$last_yr)
    }
    vb_in2 <- vb2 %>% dplyr::filter(Year <= data$last_yr)
  }

  vb_in <- vb_in2 %>% dplyr::mutate("Label" = "") %>%
    dplyr::group_by(Iteration, Rule, Year, Season, Region, value, Label)

  vb_in$Region <- sapply(1:nrow(vb_in), function(x) paste0("Region ", vb_in$Region[x]))
  vb_in1$Region <- sapply(1:nrow(vb_in1), function(x) paste0("Region ", vb_in1$Region[x]))

  
  p <- ggplot(data = vb_in, aes(x = Year, y = value, colour = Season, fill = Season))
  if (show_proj) p <- p + geom_vline(aes(xintercept = data$last_yr), linetype = "dashed")
  
  if ("Bref" %in% ref) {
    Bref <- dplyr::mutate(Bref, Label = ifelse(Year == max(Bref$Year) & Iteration == 1 & Rule == 1, "Bref", ""))
    Bmsy <- dplyr::mutate(Bmsy, Label = "")
    dl <- rbind(vb_in, Bmsy, Bref) %>% dplyr::filter(Iteration %in% seq(1, n_iter, length.out = 500))
    
    p <- p +
      geom_vline(aes(xintercept = data$first_ref_yr), linetype = "dashed", colour = cpal[2]) + 
      geom_vline(aes(xintercept = data$last_ref_yr), linetype = "dashed", colour = cpal[2]) +
      stat_summary(data = Bref, aes(x = Year, y = value), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, fill = cpal[2]) +
      stat_summary(data = Bref, aes(x = Year, y = value), fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.25, colour = NA, fill = cpal[2]) +
      stat_summary(data = Bref, aes(x = Year, y = value), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, colour = cpal[2]) +
      ggrepel::geom_label_repel(data = dl, aes(label = Label), fill = cpal[2], size = 5, color = 'white', force = 10, segment.color = '#bbbbbb', min.segment.length = unit(0, "lines"))
    # if(length(map) > 0 & show_map) p <- p + geom_line(data = Bref1, aes(x = Year, y = value), linetype = 2, colour = cpal[2])
  }
  
  if ("Bmsy" %in% ref) {
    Bmsy <- dplyr::mutate(Bmsy, Label = ifelse(Year == max(Bmsy$Year) & Iteration == 1, "Bmsy", ""))
    Bref <- dplyr::mutate(Bref, Label = "")
    dl <- rbind(vb_in, Bmsy, Bref) %>% dplyr::filter(Iteration %in% seq(1, n_iter, length.out = 500))
    
    p <- p +
      stat_summary(data = Bmsy, aes(x = Year, y = value), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.125, colour = NA, fill = "#BCBDDC") +
      stat_summary(data = Bmsy, aes(x = Year, y = value), fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.25, colour = NA, fill = "#BCBDDC") +
      stat_summary(data = Bmsy, aes(x = Year, y = value), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, colour = "#BCBDDC") +
      ggrepel::geom_label_repel(data = dl, aes(label = Label), fill = "#BCBDDC", size = 5, color = 'white', force = 10, segment.color = '#bbbbbb', min.segment.length = unit(0, "lines"))
    # if(length(map) > 0 & show_map) p <- p + geom_line(data=Bmsy1, aes(x = Year, y = value), linetype = 2, colour = "#BCBDDC")
  }
  
  if (0.05 %in% show_quants) {
    p <- p + 
      stat_summary(data = vb_in, geom = "ribbon", alpha = 0.125, colour = NA, aes(x = Year, y = value, fill = Season), 
                   fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95))
  }
  if (0.25 %in% show_quants) {
    p <- p + 
      stat_summary(data = vb_in, geom = "ribbon", alpha = 0.25, colour = NA, aes(x = Year, y = value, fill = Season), 
                   fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75))
  }
  p <- p + stat_summary(data = vb_in, aes(x = Year, y = value, color = Season), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
    expand_limits(y = 0) +
    labs(x = xlab, y = "Vulnerable biomass (tonnes)") +
    scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1), expand = c(0, 1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(vb_in$value)*1.05)) +
    theme_lsd()
  if (length(map) > 0 & show_map) {
    p <- p + geom_line(data = vb_in1, aes(x = Year, y = value), linetype = 2)
  }
  if (data$n_area > 1) {
    if(data$n_rules == 1){
      p <- p + facet_wrap(~Region)
    } else {
      p <- p + facet_wrap(Rule~Region)
    }
  }
  return(p)
}


#' Plot biomass measures
#' 
#' Plots three types of biomass.
#' 
#' @param object and LSD object
#' @param scales free or fixed
#' @param show_map show MAP or not
#' @param show_mcmc show MCMC or not
#' @param xlab the x axis label
#' @param figure_dir the directory to save to
#' @param ref specify Bmsy, Bref, or both
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#' 
plot_biomass <- function(object,
                         scales = "free_x",
                         show_map = TRUE,
                         show_mcmc = TRUE,
                         xlab = "Fishing year",
                         figure_dir = "figure/",
                         ref = c("Bmsy", "Bref"))
{
    data <- object@data
    map <- object@map
    mcmc <- object@mcmc
    
    years <- data$first_yr:data$last_yr
    pyears <- data$first_yr:data$last_proj_yr
    sex <- c("Male","Immature female","Mature female")
    seasons <- c("AW","SS")
    regions <- 1:data$n_area
    YR <- "YR" # label for the season before the season change year
    n_rules <- data$n_rules

    if (length(map) > 0 & show_map) {
        biomass_recruited_jytrs1 <- map$biomass_recruited_jytrs
        dimnames(biomass_recruited_jytrs1) <- list("Iteration" = 1, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions, Sex = c(sex, "Total"))
        biomass_recruited_jytrs1 <- reshape2::melt(biomass_recruited_jytrs1) %>%
            filter(value > 0)

       biomass_vuln_jytrs1 <- map$biomass_vuln_jytrs
       dimnames(biomass_vuln_jytrs1) <- list("Iteration" = 1, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions, Sex = sex)
       biomass_vuln_ytr1 <- reshape2::melt(biomass_vuln_jytrs1) %>%
            filter(value > 0) %>%
            mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR)) %>%
            group_by(Iteration, Rule, Year, Season, Region) %>%
            summarise(value = sum(value))
        
        biomass_vulnref_jytr1 <- map$biomass_vulnref_jytr
        dimnames(biomass_vulnref_jytr1) <- list("Iteration" = 1, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions)
        biomass_vulnref_jytr1 <- reshape2::melt(biomass_vulnref_jytr1) %>%
            filter(value > 0) %>%
            mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR))

        biomass_vulnref_yt1 <- biomass_vulnref_jytr1 %>%
            group_by(Iteration, Year, Season) %>%
            summarise(value = sum(value))
        
        biomass_cpue_ryt1 <- map$biomass_cpue_ryt
        dimnames(biomass_cpue_ryt1) <- list("Iteration" = 1, "Region" = regions, "Year" = years, "Season" = seasons)
        biomass_cpue_ryt1 <- reshape2::melt(biomass_cpue_ryt1)
        
        biomass_total_jytrs1 <- map$biomass_total_jytrs
        dimnames(biomass_total_jytrs1) <- list("Iteration" = 1, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions, "Sex" = c(sex,"Total"))
        biomass_total_jytrs1 <- reshape2::melt(biomass_total_jytrs1) %>%
            filter(value > 0)
        
        biomass_total_yts1 <- biomass_total_jytrs1 %>%
            group_by(Iteration, Year, Season, Sex) %>%
            summarise(value = sum(value))
    } else {
      biomass_recruited_ytrs1 <- NULL
      biomass_vuln_jytrs1 <- NULL
      biomass_vulnref_ytr1 <- NULL
      biomass_vulnref_yt1 <- NULL
      biomass_cpue_ryt1 <- NULL
      biomass_total_ytrs1 <- NULL
      biomass_total_yts1 <- NULL
    }
    
    if (length(mcmc) > 0 & show_mcmc) {
        n_iter <- nrow(mcmc[[1]])
        biomass_recruited_ytrs2 <- mcmc$biomass_recruited_ytrs
        dimnames(biomass_recruited_ytrs2) <- list("Iteration" = 1:n_iter,"Year" = pyears, "Season" = seasons, "Region" = regions, Sex = c(sex,"Total"))
        biomass_recruited_ytrs2 <- reshape2::melt(biomass_recruited_ytrs2) %>%
            dplyr::filter(value > 0)

        biomass_vuln_jytrs2 <- mcmc$biomass_vuln_jytrs
        dimnames(biomass_vuln_jytrs2) <- list("Iteration" = 1:n_iter, "Rule"=1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions, Sex = sex)
        biomass_vuln_jytr2 <- reshape2::melt(biomass_vuln_jytrs2) %>%
            dplyr::filter(value > 0) %>%
            dplyr::mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR)) %>%
            dplyr::group_by(Iteration, Rule, Year, Season, Region) %>%
            dplyr::summarise(value = sum(value))
        
        biomass_vulnref_ytr2 <- mcmc$biomass_vulnref_ytr
        dimnames(biomass_vulnref_ytr2) <- list("Iteration" = 1:n_iter,"Year" = pyears, "Season" = seasons, "Region" = regions)
        biomass_vulnref_ytr2 <- reshape2::melt(biomass_vulnref_ytr2) %>%
            dplyr::filter(value > 0) %>%
            dplyr::mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR))
        
        biomass_vulnref_yt2 <- biomass_vulnref_ytr2 %>%
            dplyr::group_by(Iteration, Year, Season) %>%
            dplyr::summarise(value = sum(value))
        
        biomass_cpue_ryt2 <- mcmc$biomass_cpue_ryt
        dimnames(biomass_cpue_ryt2) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = years, "Season" = seasons)
        biomass_cpue_ryt2 <- reshape2::melt(biomass_cpue_ryt2)
        
        biomass_total_ytrs2 <- mcmc$biomass_total_ytrs
        dimnames(biomass_total_ytrs2) <- list("Iteration" = 1:n_iter, "Year" = pyears, "Season" = seasons, "Region" = regions, "Sex" = c(sex,"Total"))
        biomass_total_ytrs2 <- reshape2::melt(biomass_total_ytrs2) %>%
            dplyr::filter(value > 0)
        
        biomass_total_yts2 <- biomass_total_ytrs2 %>%
            dplyr::group_by(Iteration, Year, Season, Sex) %>%
            dplyr::summarise(value = sum(value))

        Bmsy <- mcmc$Bmsy_r
        dimnames(Bmsy) <- list("Iteration" = 1:n_iter, "Region" = regions)
        Bmsy <- reshape2::melt(Bmsy) %>%
            dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = years), by = "Iteration") %>%
            dplyr::mutate(Season = "AW") %>%
            dplyr::group_by(Iteration, Region, value, Year, Season)
        
        Bref <- mcmc$Bref_jr
        dimnames(Bref) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Region" = regions)
        Bref <- reshape2::melt(Bref) %>%
            dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = years), by = "Iteration") %>%
            dplyr::mutate(Season = "AW") %>%
            dplyr::group_by(Iteration, Region, value, Year, Season)
    } else {
      biomass_recruited_ytrs2 <- NULL
      biomass_vuln_ytrs2 <- NULL
      biomass_vulnref_ytr2 <- NULL
      biomass_vulnref_yt2 <- NULL
      biomass_cpue_ryt2 <- NULL
      biomass_total_ytrs2 <- NULL
      biomass_total_yts2 <- NULL
    }
    

    # spawning stock biomass
    p <- plot_ssb(object)
    ggsave(paste0(figure_dir, "biomass_spawning.png"), p, width = 12)

    p <- plot_ssb(object, show_proj = TRUE)
    ggsave(paste0(figure_dir, "biomass_spawning_v2.png"), p, width=15)

    
    # Plot recruited biomass - no projection
    biomass_recruited_ytrs2_in <- dplyr::filter(biomass_recruited_ytrs2, Year <= data$last_yr)
    biomass_recruited_ytrs2_in$Region <- sapply(1:nrow(biomass_recruited_ytrs2_in), function(x) paste0("Region ", biomass_recruited_ytrs2_in$Region[x]))
    if (length(map) > 0 & show_map){
      biomass_recruited_ytrs1_in <- dplyr::filter(biomass_recruited_ytrs1, Year <= data$last_yr)
      biomass_recruited_ytrs1_in$Region <- sapply(1:nrow(biomass_recruited_ytrs1_in), function(x) paste0("Region ", biomass_recruited_ytrs1_in$Region[x]))
    }


    p <- ggplot(data = biomass_recruited_ytrs2_in, aes(x = Year, y = value, color = Sex, fill = Sex))
    p <- p + stat_summary(fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
        expand_limits(y = 0) +
        xlab(xlab) + ylab("Recruited biomass (tonnes)") +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd()
    if (length(map) > 0 & show_map) {
        p <- p + geom_line(data = biomass_recruited_ytrs1_in, aes(x = Year, y = value, colour = Sex), linetype = 2)
    }
    if (data$n_area > 1) {
        p <- p + facet_wrap(Region ~ Season)
    } else {
        p <- p + facet_wrap( ~ Season)
    }
    ggsave(paste0(figure_dir, "biomass_recruited.png"), p, width = 12)

    biomass_recruited_ytrs2_in <- biomass_recruited_ytrs2
    biomass_recruited_ytrs2_in$Region <- sapply(1:nrow(biomass_recruited_ytrs2_in), function(x) paste0("Region ", biomass_recruited_ytrs2_in$Region[x]))
    if (length(map) > 0 & show_map){
      biomass_recruited_ytrs1_in <- biomass_recruited_ytrs1
      biomass_recruited_ytrs1_in$Region <- sapply(1:nrow(biomass_recruited_ytrs1_in), function(x) paste0("Region ", biomass_recruited_ytrs1_in$Region[x]))
    }

    p <- ggplot(data = biomass_recruited_ytrs2_in, aes(x = Year, y = value, color = Sex, fill = Sex))
    p <- p + geom_vline(aes(xintercept = data$last_yr), linetype = "dashed")
    p <- p + stat_summary(fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
        expand_limits(y = 0) +
        xlab(xlab) + ylab("Recruited biomass (tonnes)") +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd()
    if (length(map) > 0 & show_map) {
        p <- p + geom_line(data = biomass_recruited_ytrs1_in, aes(x = Year, y = value, colour = Sex), linetype = 2)
    }
    if (data$n_area > 1) {
        p <- p + facet_wrap(Region ~ Season)
    } else {
        p <- p + facet_wrap( ~ Season)
    }
    ggsave(paste0(figure_dir, "biomass_recruited_v2.png"), p, width = 12)

    
    # Total biomass
    if (length(map) > 0 & show_map){
      biomass_total_ytrs1_in <- dplyr::filter(biomass_total_ytrs1, Year <= data$last_yr)
      biomass_total_ytrs1_in$Region <- sapply(1:nrow(biomass_total_ytrs1_in), function(x) paste0("Region ", biomass_total_ytrs1_in$Region[x]))
    }
    biomass_total_ytrs2_in <- dplyr::filter(biomass_total_ytrs2, Year <= data$last_yr)
    biomass_total_ytrs2_in$Region <- sapply(1:nrow(biomass_total_ytrs2_in), function(x) paste0("Region ", biomass_total_ytrs2_in$Region[x]))

    p <- ggplot(data = biomass_total_ytrs2_in, aes(x = Year, y = value, color = Sex, fill = Sex))
    p <- p + stat_summary(fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
        expand_limits(y = 0) +
        xlab(xlab) + ylab("Total biomass (tonnes)") +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd()
    if (length(map) > 0 & show_map) {
        p <- p + geom_line(data = biomass_total_ytrs1_in, aes(x = Year, y = value, colour = Sex), linetype = 2)
    }
    if (data$n_area > 1) {
        p <- p + facet_wrap(Region ~ Season)
    } else {
        p <- p + facet_wrap( ~ Season)
    }
    ggsave(paste0(figure_dir, "biomass_total.png"), p, width = 12)


    p <- ggplot(data = biomass_total_yts2, aes(x = Year, y = value, color = Sex, fill = Sex)) +
        geom_vline(aes(xintercept = data$last_yr), linetype = "dashed") + 
        stat_summary(fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
        expand_limits(y = 0) +
        xlab(xlab) + ylab("Total biomass (tonnes)") +
        facet_wrap( ~ Season) +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd()
    if (length(map) > 0 & show_map) {
        biomass_total_yts1$Region <- sapply(1:nrow(biomass_total_yts1), function(x) paste0("Region ", biomass_total_yts1$Region[x]))
        p <- p + geom_line(data = biomass_total_yts1, aes(x = Year, y = value, colour = Sex), linetype = 2)
    }
    ggsave(paste0(figure_dir, "biomass_total_v2.png"), p, width = 15)

    
    # Vulnerable biomass  no projection
    p <- plot_vulnerable_biomass(object, show_proj = FALSE, ref = ref)
    ggsave(paste0(figure_dir, "biomass_vuln.png"), p, width = 10)

    ## vulnerable biomass with projection
    p <- plot_vulnerable_biomass(object, show_proj = TRUE, ref = ref)
    ggsave(paste0(figure_dir, "biomass_vuln_v2.png"), p, width = 12)


    
    # Reference biomass - version 1
    p <- plot_vulnerable_reference_biomass(object, ref = ref)
    ggsave(paste0(figure_dir, "biomass_vulnref.png"), p, width = 10)

    
    # # Reference biomass - version 2
    # p <- ggplot(data = biomass_vulnref_yt2, aes(x = Year, y = value/1000, color = Season, fill = Season)) +
    #     geom_vline(aes(xintercept = data$last_yr), linetype = "dashed") + 
    #     geom_vline(aes(xintercept = data$first_ref_yr), linetype = "dashed") + 
    #     geom_vline(aes(xintercept = data$last_ref_yr), linetype = "dashed") + 
    #     stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    #     stat_summary(fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
    #     stat_summary(fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
    #     expand_limits(y = 0) +
    #     xlab(xlab) + ylab("Reference biomass (tonnes)") +
    #     scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
    #     theme_lsd()    
    # if (length(map) > 0 & show_map) {
    #     p <- p + geom_line(data = biomass_vulnref_yt1, aes(x = Year, y = value/1000), linetype = 2)
    # }
    p <- plot_vulnerable_reference_biomass(object, show_proj = TRUE, ref = ref)
    ggsave(paste0(figure_dir, "biomass_vulnref_v2.png"), p, width = 12)


    # Reference biomass - version 3 - no projection
    if (length(map) > 0 & show_map) {
      bvref_in1 <- biomass_vulnref_ytr1 %>%
        dplyr::filter(Year <= data$last_yr)
    }
    bvref_in2 <- biomass_vulnref_ytr2 %>%
      dplyr::filter(Year <= data$last_yr)
    din <- bvref_in2 %>% 
      dplyr::group_by(Iteration, Year, Season, Region, value)

    dinaw <- din %>% dplyr::filter(Season == "AW")
    dinaw1 <- dinaw %>% dplyr::filter(Year == 1979) %>%
                        dplyr::rename("B1" = value)
    dinaw2 <- full_join(dinaw, dinaw1, by = c("Iteration", "Season", "Region")) %>%
                dplyr::select(-Year.y) %>%
                dplyr::rename("Year" = Year.x) %>%
                dplyr::mutate(BB1 = value/B1)

    dinss <- din %>% dplyr::filter(Season == "SS")
    dinss1 <- dinss %>% dplyr::filter(Year == 1979) %>%
                        dplyr::rename("B1" = value)
    dinss2 <- full_join(dinss, dinss1, by = c("Iteration", "Season", "Region")) %>%
                dplyr::select(-Year.y) %>%
                dplyr::rename("Year" = Year.x) %>%
                dplyr::mutate(BB1 = value/B1)

    din2 <- rbind.data.frame(dinaw2, dinss2) %>%
      dplyr::ungroup()

    if(length(map)>0){
      dinx <- bvref_in1 %>% dplyr::group_by(Iteration, Year, Season, Region, value)
    } else{
      dinx <- NULL
    }
    if(length(mcmc)>0){
      dinx <- bvref_in2 %>% dplyr::group_by(Iteration, Year, Season, Region, value)
    } else{
      dinx <- NULL

    }

    dinawx <- dinx %>% dplyr::filter(Season == "AW")
    dinaw1x <- dinawx %>% dplyr::filter(Year == 1979) %>%
                        dplyr::rename("B1" = value)
    dinaw2x <- full_join(dinawx, dinaw1x, by = c("Iteration", "Season", "Region")) %>%
                dplyr::select(-Year.y) %>%
                dplyr::rename("Year" = Year.x) %>%
                dplyr::mutate(BB1 = value/B1)

    dinssx <- dinx %>% dplyr::filter(Season == "SS")
    dinss1x <- dinssx %>% dplyr::filter(Year == 1979) %>%
                        dplyr::rename("B1" = value)
    dinss2x <- full_join(dinssx, dinss1x, by = c("Iteration", "Season", "Region")) %>%
                dplyr::select(-Year.y) %>%
                dplyr::rename("Year" = Year.x) %>%
                dplyr::mutate(BB1 = value/B1)

    din2x <- rbind.data.frame(dinaw2x, dinss2x) %>%
      dplyr::ungroup()

    din2$Region <- sapply(1:nrow(din2), function(x) paste0("Region ", din2$Region[x]))    
    din2x$Region <- sapply(1:nrow(din2x), function(x) paste0("Region ", din2x$Region[x]))    
    p <- ggplot(data = din2, aes(x = Year, y = BB1, colour = Season, fill = Season)) +
        geom_hline(aes(yintercept = 0.5), linetype = "dashed", colour = "purple") + 
        geom_hline(aes(yintercept = 0.3), linetype = "dashed", colour = "purple") + 
        geom_hline(aes(yintercept = 0.1), linetype = "dashed", colour = "purple")
    p <- p + stat_summary(data = din2, aes(x = Year, y = BB1, color = Season, fill = Season), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(data = din2, aes(x = Year, y = BB1, color = Season, fill = Season), fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(data = din2, aes(x = Year, y = BB1, color = Season), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
        expand_limits(y = 0) +
        xlab(xlab) + ylab("Reference biomass (tonnes)") +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd()
    if (length(map) > 0 & show_map) {
       p <- p + geom_line(data = din2x, aes(x = Year, y = BB1), linetype = 2)
    }
    if (data$n_area > 1) {
        p <- p + facet_wrap(~Region)
    } 
    ggsave(paste0(figure_dir, "biomass_vulnref_relyr1.png"), p, width = 10)
    
    if (length(map) > 0 & show_map) bvref_in1 <- biomass_vulnref_ytr1
    bvref_in2 <- biomass_vulnref_ytr2
    din <- bvref_in2 %>% dplyr::group_by(Iteration, Year, Season, Region, value)

    dinaw <- din %>% dplyr::filter(Season == "AW")
    dinaw1 <- dinaw %>% dplyr::filter(Year == 1979) %>%
                        dplyr::rename("B1" = value)
    dinaw2 <- full_join(dinaw, dinaw1, by = c("Iteration", "Season", "Region")) %>%
                dplyr::select(-Year.y) %>%
                dplyr::rename("Year" = Year.x) %>%
                dplyr::mutate(BB1 = value/B1)

    dinss <- din %>% dplyr::filter(Season == "SS")
    dinss1 <- dinss %>% dplyr::filter(Year == 1979) %>%
                        dplyr::rename("B1" = value)
    dinss2 <- full_join(dinss, dinss1, by = c("Iteration", "Season", "Region")) %>%
                dplyr::select(-Year.y) %>%
                dplyr::rename("Year" = Year.x) %>%
                dplyr::mutate(BB1 = value/B1)

    din2 <- rbind.data.frame(dinaw2, dinss2) %>% dplyr::ungroup()

    if (length(map) > 0) {
      dinx <- bvref_in1 %>% dplyr::group_by(Iteration, Year, Season, Region, value)
    } else{
      dinx <- NULL
    }
    if (length(mcmc) > 0) {
      dinx <- bvref_in2 %>% dplyr::group_by(Iteration, Year, Season, Region, value)
    } else{
      dinx <- NULL
    }

    dinawx <- dinx %>% dplyr::filter(Season == "AW")
    dinaw1x <- dinawx %>% dplyr::filter(Year == 1979) %>%
                        dplyr::rename("B1" = value)
    dinaw2x <- full_join(dinawx, dinaw1x, by = c("Iteration", "Season", "Region")) %>%
                dplyr::select(-Year.y) %>%
                dplyr::rename("Year" = Year.x) %>%
                dplyr::mutate(BB1 = value/B1)

    dinssx <- dinx %>% dplyr::filter(Season == "SS")
    dinss1x <- dinssx %>% dplyr::filter(Year == 1979) %>%
                        dplyr::rename("B1" = value)
    dinss2x <- full_join(dinssx, dinss1x, by = c("Iteration", "Season", "Region")) %>%
                dplyr::select(-Year.y) %>%
                dplyr::rename("Year" = Year.x) %>%
                dplyr::mutate(BB1 = value/B1)

    din2x <- rbind.data.frame(dinaw2x, dinss2x) %>% dplyr::ungroup()

    din2$Region <- sapply(1:nrow(din2), function(x) paste0("Region ", din2$Region[x]))    
    din2x$Region <- sapply(1:nrow(din2x), function(x) paste0("Region ", din2x$Region[x]))      
    p <- ggplot(data = din2, aes(x = Year, y = BB1, colour = Season, fill = Season)) +
        geom_hline(aes(yintercept = 0.5), linetype = "dashed", colour = "purple") + 
        geom_hline(aes(yintercept = 0.3), linetype = "dashed", colour = "purple") + 
        geom_hline(aes(yintercept = 0.1), linetype = "dashed", colour = "purple")
    p <- p + geom_vline(aes(xintercept = data$last_yr), linetype = "dashed")
    p <- p + stat_summary(data = din2, aes(x = Year, y = BB1, color = Season, fill = Season), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(data = din2, aes(x = Year, y = BB1, color = Season, fill = Season), fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(data = din2, aes(x = Year, y = BB1, color = Season), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
        expand_limits(y = 0) +
        xlab(xlab) + ylab("Reference biomass (tonnes)") +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd()
    if (length(map) > 0 & show_map) {
       p <- p + geom_line(data = din2x, aes(x = Year, y = BB1), linetype = 2)
    }
    if (data$n_area > 1) {
        p <- p + facet_wrap(~Region)
    } 
    ggsave(paste0(figure_dir, "biomass_vulnref_relyr1_v2.png"), p, width = 10)
}
