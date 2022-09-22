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

  if (length(mcmc) > 0) {
    n_iter <- nrow(mcmc[[1]])
    ssb <- mcmc$biomass_ssb_jyr
    dimnames(ssb) <- list(Iteration = 1:n_iter, Rules = 1:n_rules, Year = data$first_yr:data$last_proj_yr, Region = regions)
    ssb <- melt(ssb, value.name = "SSB") %>%
      filter(Year %in% data$first_yr:data$last_yr)

    rec <- mcmc$recruits_ry
    dimnames(rec) <- list(Iteration = 1:n_iter, Region = regions, Year = data$first_yr:data$last_proj_yr)
    rec <- melt(rec, value.name = "Recruitment") %>%
      filter(Year %in% data$first_yr:data$last_yr)

    d <- inner_join(ssb, rec) %>%
      group_by(Rules, Year, Region) %>%
      summarise(SSB = median(SSB), Recruitment = median(Recruitment)) %>%
      ungroup()
    d$Region <- sapply(1:nrow(d), function(x) paste0("Region ", d$Region[x]))

    p <- ggplot(d, aes(x = SSB, y = Recruitment)) +
      geom_path() +
      geom_text(aes(label = Year, colour = Year)) +
      expand_limits(x = 0, y = 0) +
      labs(x = xlab) +
      facet_wrap(~Region) +
      theme_lsd() +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)))
  }
  #stat_smooth(method = "loess") +
  # B0 <- 179280
  # (SSB / B0)/(1-((5*h-1)/(4*h))*(1-(SSB / B0)));
  ggsave(paste0(figure_dir, "biomass_ssb_recruitment.png"), p, width = 12)
}


#' Plot spawning stock biomass (SSB)
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
plot_ssb <- function(object,
                     scales = "free",
                     show_map = TRUE,
                     show_mcmc = TRUE,
                     show_proj = FALSE,
                     show_ref = FALSE,
                     xlab = "Fishing year (1 April - 31 March)")
{
  data <- object@data
  map <- object@map
  mcmc <- object@mcmc

  cpal <- c("#56B4E9", "#009E73", "#E69F00", "tomato")

  years <- data$first_yr:data$last_yr
  pyears <- data$first_yr:data$last_proj_yr
  regions <- 1:data$n_area
  if (length(regions) > 1) regions2 <- c(regions, "Total")
  if (length(regions) == 1) regions2 <- regions
  n_rules <- data$n_rules

  if (length(map) > 0 & show_map) {
    ssb1 <- map$biomass_ssb_jyr
    dimnames(ssb1) <- list(Iteration = 1, Rule = 1:n_rules, Year = data$first_yr:data$last_proj_yr, Region = regions)
    ssb1 <- melt(ssb1, value.name = "SSB")
  }

  if (length(mcmc) > 0 & show_mcmc) {
    n_iter <- nrow(mcmc[[1]])
    ssb <- mcmc$biomass_ssb_jyr
    dimnames(ssb) <- list(Iteration = 1:n_iter, Rule = 1:n_rules, Year = data$first_yr:data$last_proj_yr, Region = regions)
    ssb <- melt(ssb, value.name = "SSB")

    SSB0 <- mcmc$SSB0_r
    dimnames(SSB0) <- list("Iteration" = 1:n_iter, "Region" = regions2)
    hard_limit <- melt(SSB0) %>%
      filter(Region != "Total") %>%
      left_join(expand.grid(Iteration = 1:n_iter, Year = pyears), by = "Iteration") %>%
      group_by(Iteration, Region, value, Year) %>%
      ungroup() %>%
      mutate(Rule = 1, type = "Hard limit", value = value * 0.1)
    soft_limit <- melt(SSB0) %>%
      filter(Region != "Total") %>%
      left_join(expand.grid(Iteration = 1:n_iter, Year = pyears), by = "Iteration") %>%
      group_by(Iteration, Region, value, Year) %>%
      ungroup() %>%
      mutate(Rule = 1, type = "Soft limit", value = value * 0.2)
  }

  # spawning stock biomass
  if (show_proj) {
    ssb_in <- ssb %>%
      mutate(type = "SSB") %>%
      rename(value = SSB) %>%
      rbind(soft_limit, hard_limit)
    if (length(map) > 0 & show_map) ssb1_in <- ssb1 %>% mutate(type = "SSB")
  } else {
    ssb_in <- ssb %>%
      mutate(type = "SSB") %>%
      rename(value = SSB) %>%
      rbind(soft_limit, hard_limit) %>%
      filter(Year <= data$last_yr)
    if (length(map) > 0 & show_map) ssb1_in <- filter(ssb1, Year <= data$last_yr) %>% mutate(type = "SSB")
  }
  ssb_in$type <- factor(ssb_in$type, levels = c("SSB", "Reference", "Soft limit", "Hard limit"))

  if (!show_ref) {
    ssb_in <- filter(ssb_in, type != "Reference")
  }
  # ssb_in$Region <- sapply(1:nrow(ssb_in), function(x) paste0("Region ", ssb_in$Region[x]))
  # ssb1_in$Region <- sapply(1:nrow(ssb1_in), function(x) paste0("Region ", ssb1_in$Region[x]))

  p <- ggplot(data = ssb_in %>% filter(Region %in% regions), aes(x = Year, y = value))
  # if (show_ref) {
  #     # p <- p + geom_vline(aes(xintercept = data$first_ref_yr), linetype = "dashed", colour = cpal[2]) +
  #     #     geom_vline(aes(xintercept = data$last_ref_yr), linetype = "dashed", colour = cpal[2])
  #   p <- p + geom_hline(aes(yintercept = unique(ssb_in %>% filter(type == "Reference") %>% select(value)), colour = cpal[2])
  # }

  if (show_proj) p <- p + geom_vline(aes(xintercept = data$last_yr), linetype = "dashed")

  p <- p +
    stat_summary(aes(fill = type), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.125) +
    stat_summary(aes(fill = type), fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.25) +
    stat_summary(aes(colour = type), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
    expand_limits(y = 0) +
    labs(x = xlab, y = "Spawning stock biomass (tonnes)", colour = NULL, fill = NULL) +
    scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1), expand = c(0, 1)) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    scale_fill_manual(values = cpal) +
    scale_colour_manual(values = cpal) +
    theme_lsd()

  if (length(map) > 0 & show_map) {
    p <- p + geom_line(data = ssb1_in %>% filter(Region %in% regions), aes(x = Year, y = SSB, colour = type), linetype = 2)
  }

  if (data$n_area > 1) {
    p <- p + facet_wrap(~Region, scales = "free")
  }

  return(p)
}


#' Plot AW adjusted vulnerable biomass
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
plot_vulnref_AW <- function(object,
                            scales = "free",
                            show_map = TRUE,
                            show_mcmc = TRUE,
                            show_proj = FALSE,
                            show_ref = FALSE,
                            xlab = "Fishing year (1 April - 31 March)")
{
  data <- object@data
  map <- object@map
  mcmc <- object@mcmc

  cpal <- c("#56B4E9", "#009E73", "#E69F00", "tomato")

  regions <- 1:data$n_area
  n_rules <- data$n_rules

  if (length(regions) > 1) regions2 <- c(regions, "Total")
  if (length(regions) == 1) regions2 <- regions

  if (length(map) > 0 & show_map) {
    vb1 <- map$biomass_vulnref_AW_jyr
    dimnames(vb1) <- list(Iteration = 1, Rule = 1:n_rules, Year = data$first_yr:data$last_proj_yr, Region = regions)
    vb1 <- melt(vb1, value.name = "VB")
  }

  if (length(mcmc) > 0 & show_mcmc) {
    n_iter <- nrow(mcmc[[1]])
    vb <- mcmc$biomass_vulnref_AW_jyr
    dimnames(vb) <- list(Iteration = 1:n_iter, Rule = 1:n_rules, Year = data$first_yr:data$last_proj_yr, Region = regions)
    vb <- melt(vb, value.name = "VB")

    # VB0 <- mcmc$B0_r
    # dimnames(VB0) <- list("Iteration" = 1:n_iter, "Region" = regions2)
    Bref <- mcmc$Bref_jr
    dimnames(Bref) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Region" = regions2)
    Bref <- melt(Bref)
    Bref <- unique(Bref %>% select(.data$Region, .data$value)) %>% filter(Region %in% regions)
  }

  if (show_proj == FALSE) {
    vb <- vb %>% filter(.data$Year <= data$last_yr)
    if (length(map) > 0 & show_map) vb1 <- vb1 %>% filter(.data$Year <= data$last_yr)
  }

  p <- ggplot(data = vb %>% filter(.data$Region %in% regions), aes(x = .data$Year, y = .data$VB))

  if (show_ref) {
    # p <- p + geom_vline(aes(xintercept = data$first_ref_yr), linetype = "dashed", colour = cpal[2]) +
    #     geom_vline(aes(xintercept = data$last_ref_yr), linetype = "dashed", colour = cpal[2])
    p <- p +
      geom_hline(data = Bref, aes(yintercept = .data$value), colour = cpal[2]) +
      geom_label(data = Bref %>% filter(.data$Region == 1), label = "Reference", aes(x = min(vb$Year) + 10, y = .data$value), color = cpal[2], size = 5)
  }

  if (show_proj) p <- p + geom_vline(aes(xintercept = data$last_yr), linetype = "dashed")

  p <- p +
    stat_summary(fill = cpal[1], fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.125) +
    stat_summary(fill = cpal[1], fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.25) +
    stat_summary(colour = cpal[1], fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
    expand_limits(y = 0) +
    labs(x = xlab, y = "AW adjusted vulnerable biomass (tonnes)", colour = NULL, fill = NULL) +
    scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1), expand = c(0, 1)) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    # scale_fill_manual(values = cpal) +
    # scale_colour_manual(values = cpal) +
    guides(color = FALSE, fill = FALSE) +
    theme_lsd()

  if (length(map) > 0 & show_map) {
    p <- p + geom_line(data = vb1 %>% filter(.data$Region %in% regions), aes(x = .data$Year, y = .data$VB), linetype = 2, colour = cpal[1])
  }

  if (data$n_area > 1) {
    p <- p + facet_wrap(~ .data$Region, scales = scales)
  }

  return(p)
}


#' Plot adjusted vulnerable biomass
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
                                              scales = "free",
                                              show_map = TRUE,
                                              show_mcmc = TRUE,
                                              show_proj = FALSE,
                                              xlab = "Fishing year (1 April - 31 March)",
                                              show_quants = c(0.05, 0.25))
{
  data <- object@data
  map <- object@map
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

  if (length(map) > 0 & show_map) {
    if ("biomass_vulnref_jytr" %in% names(map)) {
      vbref1 <- map$biomass_vulnref_jytr
      dimnames(vbref1) <- list("Iteration" = 1, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions)
      vbref1 <- melt(vbref1) %>%
        filter(.data$value > 0) %>%
        mutate(Season = as.character(.data$Season), Season = ifelse(.data$Year >= data$season_change_yr, .data$Season, YR)) %>%
        group_by(.data$Iteration, .data$Year, .data$Season, .data$Region) %>%
        summarise(value = sum(.data$value))
    } else {
      vbref1 <- map$biomass_vulnref_ytr
      dimnames(vbref1) <- list("Iteration" = 1,"Year" = pyears, "Season" = seasons, "Region" = regions)
      vbref1 <- melt(vbref1) %>%
        filter(.data$value > 0) %>%
        mutate(Season = as.character(.data$Season), Season = ifelse(.data$Year >= data$season_change_yr, .data$Season, YR)) %>%
        group_by(.data$Iteration, .data$Year, .data$Season, .data$Region) %>%
        summarise(value = sum(.data$value))
    }
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
  }

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

    Bmsy <- mcmc$Bmsy_r
    dimnames(Bmsy) <- list("Iteration" = 1:n_iter, "Region" = regions2)
    Bmsy <- melt(Bmsy) %>%
      left_join(expand.grid(Iteration = 1:n_iter, Year = years), by = "Iteration") %>%
      mutate(Season = "AW") %>%
      group_by(.data$Iteration, .data$Region, .data$value, .data$Year, .data$Season)

    Bref <- mcmc$Bref_jr
    dimnames(Bref) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Region" = regions2)
    Bref <- melt(Bref) %>%
      left_join(expand.grid(Iteration = 1:n_iter, Year = years), by = "Iteration") %>%
      mutate(Season = "AW") %>%
      group_by(.data$Iteration, .data$Region, .data$value, .data$Year, .data$Season)
  }

  # Reference biomass - version 1
  if (show_proj) {
    if (length(map) > 0 & show_map) vbref_in1 <- vbref1
    vbref_in2 <- vbref
  } else {
    if (length(map) > 0 & show_map) {
      vbref_in1 <- vbref1 %>%
        filter(.data$Year <= data$last_yr)
    }
    vbref_in2 <- vbref %>%
      filter(.data$Year <= data$last_yr)
  }
  din <- vbref_in2 %>%
    mutate("Label" = "") %>%
    group_by(.data$Iteration, .data$Year, .data$Season, .data$Region, .data$value, .data$Label)

  # din$Region <- sapply(1:nrow(din), function(x) paste0("Region ", din$Region[x]))
  # vbref_in1$Region <- sapply(1:nrow(vbref_in1), function(x) paste0("Region ", vbref_in1$Region[x]))
  # vbref_in2$Region <- sapply(1:nrow(vbref_in2), function(x) paste0("Region ", vbref_in2$Region[x]))

  p <- ggplot(data = vbref_in2, aes(x = .data$Year, y = .data$value, colour = .data$Season, fill = .data$Season))

  if (show_proj) p <- p + geom_vline(aes(xintercept = data$last_yr), linetype = "dashed")

  # if ("Bref" %in% ref) {
  #   Bref <- mutate(Bref, Label = ifelse(Year == max(Bref$Year) & Iteration == 1 & Rule == 1, "Bref", ""))
  #   Bmsy <- mutate(Bmsy, Label = "")
  #   dl <- rbind(din, Bmsy, Bref) %>% filter(Iteration %in% seq(1, n_iter, length.out = 500))
  #   p <- p +
  #     # geom_vline(aes(xintercept = data$first_ref_yr), linetype = "dashed", colour = cpal[2]) +
  #     # geom_vline(aes(xintercept = data$last_ref_yr), linetype = "dashed", colour = cpal[2]) +
  #     stat_summary(data = Bref %>% filter(Region %in% regions), aes(x = Year, y = value), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, fill = cpal[2]) +
  #     stat_summary(data = Bref %>% filter(Region %in% regions), aes(x = Year, y = value), fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.25, colour = NA, fill = cpal[2]) +
  #     stat_summary(data = Bref %>% filter(Region %in% regions), aes(x = Year, y = value), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, colour = cpal[2]) +
  #     ggrepel::geom_label_repel(data = dl, aes(label = Label), fill = cpal[2], size = 5, color = 'white', force = 10, segment.color = '#bbbbbb', min.segment.length = unit(0, "lines"))
  #   # if(length(map) > 0 & show_map) p <- p + geom_line(data = Bref1, aes(x = Year, y = value), linetype = 2, colour = cpal[2])
  # }
  # if ("Bmsy" %in% ref) {
  #   Bmsy <- mutate(Bmsy, Label = ifelse(Year == max(Bmsy$Year) & Iteration == 1, "Bmsy", ""))
  #   Bref <- mutate(Bref, Label = "")
  #   dl <- rbind(din, Bmsy, Bref) %>% filter(Iteration %in% seq(1, n_iter, length.out = 500))
  #   p <- p +
  #     stat_summary(data = Bmsy %>% filter(Region %in% regions), aes(x = Year, y = value), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.125, colour = NA, fill = "#BCBDDC") +
  #     stat_summary(data = Bmsy %>% filter(Region %in% regions), aes(x = Year, y = value), fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.25, colour = NA, fill = "#BCBDDC") +
  #     stat_summary(data = Bmsy %>% filter(Region %in% regions), aes(x = Year, y = value), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, colour = "#BCBDDC") +
  #     ggrepel::geom_label_repel(data = dl, aes(label = Label), fill = "#BCBDDC", size = 5, color = 'white', force = 10, segment.color = '#bbbbbb', min.segment.length = unit(0, "lines"))
  #   # if(length(map) > 0 & show_map) p <- p + geom_line(data=Bmsy1, aes(x = Year, y = value), linetype = 2, colour = "#BCBDDC")
  # }

  if (0.05 %in% show_quants) {
    p <- p +
      stat_summary(data = din, geom = "ribbon", alpha = 0.125, colour = NA, aes(x = .data$Year, y = .data$value, fill = .data$Season),
                   fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95))
  }

  if (0.25 %in% show_quants) {
    p <- p +
      stat_summary(data = din, geom = "ribbon", alpha = 0.25, colour = NA, aes(x = .data$Year, y = .data$value, fill = .data$Season),
                   fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75))
  }

  p <- p + stat_summary(data = din, aes(x = .data$Year, y = .data$value, color = .data$Season), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
    expand_limits(y = 0) +
    labs(x = xlab, y = "Adjusted vulnerable biomass (tonnes)") +
    scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1), expand = c(0, 1)) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    theme_lsd()

  if (length(map) > 0 & show_map) {
    p <- p + geom_line(data = vbref_in1, aes(x = .data$Year, y = .data$value), linetype = 2)
  }

  if (data$n_area > 1) {
    p <- p + facet_wrap(~ .data$Region, scales = scales)
  }

  return(p)
}


#' Plot adjusted vulnerable biomass
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
                                    scales = "free",
                                    show_map = TRUE,
                                    show_mcmc = TRUE,
                                    show_proj = FALSE,
                                    xlab = "Fishing year (1 April - 31 March)",
                                    show_quants = c(0.05, 0.25))
{
  data <- object@data
  map <- object@map
  mcmc <- object@mcmc

  cpal <- c("#56B4E9", "#009E73", "#E69F00", "tomato")

  years <- data$first_yr:data$last_yr
  pyears <- data$first_yr:data$last_proj_yr
  sex <- c("Male", "Immature female", "Mature female")
  seasons <- c("AW", "SS")
  regions <- 1:data$n_area
  if (length(regions) > 1) regions2 <- c(regions, max(regions) + 1)
  if (length(regions) == 1) regions2 <- regions
  YR <- "YR" # label for the season before the season change year
  n_rules <- data$n_rules

  if (length(map) > 0 & show_map) {
    vb1 <- map$biomass_vuln_jytrs
    dimnames(vb1) <- list("Iteration" = 1, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions, Sex = sex)
    vb1 <- melt(vb1) %>%
      filter(.data$value > 0) %>%
      mutate(Season = as.character(.data$Season), Season = ifelse(.data$Year >= data$season_change_yr, .data$Season, YR)) %>%
      group_by(.data$Iteration, .data$Rule, .data$Year, .data$Season, .data$Region) %>%
      summarise(value = sum(.data$value))

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
  }

  if (length(mcmc) > 0 & show_mcmc) {
    n_iter <- nrow(mcmc[[1]])

    vb2 <- mcmc$biomass_vuln_jytrs
    dimnames(vb2) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions, Sex = sex)
    vb2 <- melt(vb2) %>%
      filter(.data$value > 0) %>%
      mutate(Season = as.character(.data$Season), Season = ifelse(.data$Year >= data$season_change_yr, .data$Season, YR)) %>%
      group_by(.data$Iteration, .data$Rule, .data$Year, .data$Season, .data$Region) %>%
      summarise(value = sum(.data$value))

    # Bmsy <- mcmc$Bmsy_r
    # dimnames(Bmsy) <- list("Iteration" = 1:n_iter, "Region" = regions2)
    # Bmsy <- melt(Bmsy) %>%
    #   left_join(expand.grid(Iteration = 1:n_iter, Year = years), by = "Iteration") %>%
    #   mutate(Season = "AW") %>%
    #   group_by(Iteration, Region, value, Year, Season)
    # Bref <- mcmc$Bref_jr
    # dimnames(Bref) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Region" = regions2)
    # Bref <- melt(Bref) %>%
    #   left_join(expand.grid(Iteration = 1:n_iter, Year = years), by = "Iteration") %>%
    #   mutate(Season = "AW") %>%
    #   group_by(Iteration, Region, value, Year, Season)
  }

  # vulnerable biomass
  if (show_proj) {
    if (length(map) > 0 & show_map) vb_in1 <- vb1
    vb_in2 <- vb2
  } else {
    if (length(map) > 0 & show_map) {
      vb_in1 <- vb1 %>% filter(.data$Year <= data$last_yr)
    }
    vb_in2 <- vb2 %>% filter(.data$Year <= data$last_yr)
  }

  vb_in <- vb_in2 %>% mutate("Label" = "") %>%
    group_by(.data$Iteration, .data$Rule, .data$Year, .data$Season, .data$Region, .data$value, .data$Label)

  p <- ggplot(data = vb_in, aes(x = .data$Year, y = .data$value, colour = .data$Season, fill = .data$Season))

  if (show_proj) p <- p + geom_vline(aes(xintercept = data$last_yr), linetype = "dashed")

  # if ("Bref" %in% ref) {
  #   Bref <- mutate(Bref, Label = ifelse(Year == max(Bref$Year) & Iteration == 1 & Rule == 1, "Bref", ""))
  #   Bmsy <- mutate(Bmsy, Label = "")
  #   dl <- rbind(vb_in, Bmsy, Bref) %>% filter(Iteration %in% seq(1, n_iter, length.out = 500))
  #   p <- p +
  #     # geom_vline(aes(xintercept = data$first_ref_yr), linetype = "dashed", colour = cpal[2]) +
  #     # geom_vline(aes(xintercept = data$last_ref_yr), linetype = "dashed", colour = cpal[2]) +
  #     stat_summary(data = Bref %>% filter(Region %in% regions), aes(x = Year, y = value), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, fill = cpal[2]) +
  #     stat_summary(data = Bref %>% filter(Region %in% regions), aes(x = Year, y = value), fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.25, colour = NA, fill = cpal[2]) +
  #     stat_summary(data = Bref %>% filter(Region %in% regions), aes(x = Year, y = value), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, colour = cpal[2]) +
  #     ggrepel::geom_label_repel(data = dl %>% filter(Region %in% regions), aes(label = Label), fill = cpal[2], size = 5, color = 'white', force = 10, segment.color = '#bbbbbb', min.segment.length = unit(0, "lines"))
  #   # if(length(map) > 0 & show_map) p <- p + geom_line(data = Bref1, aes(x = Year, y = value), linetype = 2, colour = cpal[2])
  # }
  # if ("Bmsy" %in% ref) {
  #   Bmsy <- mutate(Bmsy, Label = ifelse(Year == max(Bmsy$Year) & Iteration == 1, "Bmsy", ""))
  #   Bref <- mutate(Bref, Label = "")
  #   dl <- rbind(vb_in, Bmsy, Bref) %>% filter(Iteration %in% seq(1, n_iter, length.out = 500))
  #   p <- p +
  #     stat_summary(data = Bmsy %>% filter(Region %in% regions), aes(x = Year, y = value), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.125, colour = NA, fill = "#BCBDDC") +
  #     stat_summary(data = Bmsy %>% filter(Region %in% regions), aes(x = Year, y = value), fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.25, colour = NA, fill = "#BCBDDC") +
  #     stat_summary(data = Bmsy %>% filter(Region %in% regions), aes(x = Year, y = value), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, colour = "#BCBDDC") +
  #     ggrepel::geom_label_repel(data = dl %>% filter(Region %in% regions), aes(label = Label), fill = "#BCBDDC", size = 5, color = 'white', force = 10, segment.color = '#bbbbbb', min.segment.length = unit(0, "lines"))
  #   # if(length(map) > 0 & show_map) p <- p + geom_line(data=Bmsy1, aes(x = Year, y = value), linetype = 2, colour = "#BCBDDC")
  # }

  if (0.05 %in% show_quants) {
    p <- p +
      stat_summary(data = vb_in, geom = "ribbon", alpha = 0.125, colour = NA, aes(x = .data$Year, y = .data$value, fill = .data$Season),
                   fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95))
  }

  if (0.25 %in% show_quants) {
    p <- p +
      stat_summary(data = vb_in, geom = "ribbon", alpha = 0.25, colour = NA, aes(x = .data$Year, y = .data$value, fill = .data$Season),
                   fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75))
  }

  p <- p + stat_summary(data = vb_in, aes(x = .data$Year, y = .data$value, color = .data$Season), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
    expand_limits(y = 0) +
    labs(x = xlab, y = "Vulnerable biomass (tonnes)") +
    scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1), expand = c(0, 1)) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    theme_lsd()

  if (length(map) > 0 & show_map) {
    p <- p + geom_line(data = vb_in1, aes(x = .data$Year, y = .data$value), linetype = 2)
  }

  if (data$n_area > 1) {
    if (data$n_rules == 1) {
      p <- p + facet_wrap(~ .data$Region, scales = scales)
    } else {
      p <- p + facet_wrap(.data$Rule ~ .data$Region)
    }
  }

  return(p)
}


#' Plot total biomass
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
plot_total_biomass <- function(object,
                               scales = "free",
                               show_proj = TRUE,
                               show_map = TRUE,
                               show_mcmc = TRUE,
                               xlab = "Fishing year",
                               figure_dir = "figure/")
{
  data <- object@data
  map <- object@map
  mcmc <- object@mcmc

  years <- data$first_yr:data$last_yr
  pyears <- data$first_yr:data$last_proj_yr
  sex <- c("Male","Immature female","Mature female")

  seasons <- c("AW","SS")
  regions <- 1:data$n_area
  if (length(regions) > 1) regions2 <- c(regions, max(regions + 1))
  if (length(regions) == 1) regions2 <- regions
  YR <- "YR" # label for the season before the season change year
  n_rules <- data$n_rules

  if (length(map) > 0 & show_map) {
    # if ("biomass_vuln_jytrs" %in% names(map)) {
      biomass_vuln_jytrs1 <- map$biomass_vuln_jytrs
      dimnames(biomass_vuln_jytrs1) <- list("Iteration" = 1, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions, Sex = sex)
      biomass_vuln_ytr1 <- melt(biomass_vuln_jytrs1) %>%
        filter(value > 0) %>%
        mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR)) %>%
        group_by(Iteration, Rule, Year, Season, Region) %>%
        summarise(value = sum(value))

      biomass_vulnref_jytr1 <- map$biomass_vulnref_jytr
      dimnames(biomass_vulnref_jytr1) <- list("Iteration" = 1, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions)
      biomass_vulnref_jytr1 <- melt(biomass_vulnref_jytr1) %>%
        filter(value > 0) %>%
        mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR))

      biomass_vulnref_yt1 <- biomass_vulnref_jytr1 %>%
        group_by(Iteration, Year, Season) %>%
        summarise(value = sum(value))

      biomass_cpue_ryt1 <- map$biomass_cpue_ryt
      dimnames(biomass_cpue_ryt1) <- list("Iteration" = 1, "Region" = regions, "Year" = years, "Season" = seasons)
      biomass_cpue_ryt1 <- melt(biomass_cpue_ryt1)

      biomass_total_jytrs1 <- map$biomass_total_jytrs
      dimnames(biomass_total_jytrs1) <- list("Iteration" = 1, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions, "Sex" = c(sex,"Total"))
      biomass_total_jytrs1 <- melt(biomass_total_jytrs1) %>%
        filter(value > 0)

      biomass_total_yts1 <- biomass_total_jytrs1 %>%
        group_by(Iteration, Year, Season, Sex) %>%
        summarise(value = sum(value))
    # } else {

    #   biomass_vuln_jytrs1 <- map$biomass_vuln_jytrs
    #   dimnames(biomass_vuln_jytrs1) <- list("Iteration" = 1, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions, Sex = sex)
    #   biomass_vuln_ytr1 <- melt(biomass_vuln_jytrs1) %>%
    #     filter(value > 0) %>%
    #     mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR)) %>%
    #     group_by(Iteration, Rule, Year, Season, Region) %>%
    #     summarise(value = sum(value))

    #   biomass_vulnref_jytr1 <- map$biomass_vulnref_ytr
    #   dimnames(biomass_vulnref_jytr1) <- list("Iteration" = 1, "Year" = pyears, "Season" = seasons, "Region" = regions)
    #   biomass_vulnref_jytr1 <- melt(biomass_vulnref_jytr1) %>%
    #     filter(value > 0) %>%
    #     mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR))

    #   biomass_vulnref_yt1 <- biomass_vulnref_jytr1 %>%
    #     group_by(Iteration, Year, Season) %>%
    #     summarise(value = sum(value))

    #   biomass_cpue_ryt1 <- map$biomass_cpue_ryt
    #   dimnames(biomass_cpue_ryt1) <- list("Iteration" = 1, "Region" = regions, "Year" = years, "Season" = seasons)
    #   biomass_cpue_ryt1 <- melt(biomass_cpue_ryt1)

    #   biomass_total_jytrs1 <- map$biomass_total_ytrs
    #   dimnames(biomass_total_jytrs1) <- list("Iteration" = 1, "Year" = pyears, "Season" = seasons, "Region" = regions, "Sex" = c(sex,"Total"))
    #   biomass_total_jytrs1 <- melt(biomass_total_jytrs1) %>%
    #     filter(value > 0)

    #   biomass_total_yts1 <- biomass_total_jytrs1 %>%
    #     group_by(Iteration, Year, Season, Sex) %>%
    #     summarise(value = sum(value))
    # }

  } else {
    biomass_vuln_jytrs1 <- NULL
    biomass_vulnref_jytr1 <- NULL
    biomass_vulnref_yt1 <- NULL
    biomass_cpue_ryt1 <- NULL
    biomass_total_jytrs1 <- NULL
    biomass_total_yts1 <- NULL
  }

  if (length(mcmc) > 0 & show_mcmc) {
    n_iter <- nrow(mcmc[[1]])
    # if ("biomass_vuln_jytrs" %in% names(mcmc)) {
      biomass_vuln_jytrs2 <- mcmc$biomass_vuln_jytrs
      dimnames(biomass_vuln_jytrs2) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions, Sex = sex)
      biomass_vuln_jytr2 <- melt(biomass_vuln_jytrs2) %>%
        filter(value > 0) %>%
        mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR)) %>%
        group_by(Iteration, Rule, Year, Season, Region) %>%
        summarise(value = sum(value))

      biomass_vulnref_jytr2 <- mcmc$biomass_vulnref_jytr
      dimnames(biomass_vulnref_jytr2) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions)
      biomass_vulnref_jytr2 <- melt(biomass_vulnref_jytr2) %>%
        filter(value > 0) %>%
        mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR))

      biomass_vulnref_yt2 <- biomass_vulnref_jytr2 %>%
        group_by(Iteration, Year, Season) %>%
        summarise(value = sum(value))

      biomass_cpue_ryt2 <- mcmc$biomass_cpue_ryt
      dimnames(biomass_cpue_ryt2) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = years, "Season" = seasons)
      biomass_cpue_ryt2 <- melt(biomass_cpue_ryt2)

      biomass_total_jytrs2 <- mcmc$biomass_total_jytrs
      dimnames(biomass_total_jytrs2) <- list("Iteration" = 1:n_iter, "Rules" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions, "Sex" = c(sex,"Total"))
      biomass_total_jytrs2 <- melt(biomass_total_jytrs2) %>%
        filter(value > 0)

      biomass_total_yts2 <- biomass_total_jytrs2 %>%
        group_by(Iteration, Year, Season, Sex) %>%
        summarise(value = sum(value))

      Bmsy <- mcmc$Bmsy_r
      dimnames(Bmsy) <- list("Iteration" = 1:n_iter, "Region" = regions2)
      Bmsy <- melt(Bmsy) %>%
        left_join(expand.grid(Iteration = 1:n_iter, Year = years), by = "Iteration") %>%
        mutate(Season = "AW") %>%
        group_by(Iteration, Region, value, Year, Season)

      Bref <- mcmc$Bref_jr
      dimnames(Bref) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Region" = regions2)
      Bref <- melt(Bref) %>%
        left_join(expand.grid(Iteration = 1:n_iter, Year = years), by = "Iteration") %>%
        mutate(Season = "AW") %>%
        group_by(Iteration, Region, value, Year, Season)
    # } else {
    #   biomass_vuln_jytrs2 <- mcmc$biomass_vuln_jytrs
    #   dimnames(biomass_vuln_jytrs2) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions, Sex = sex)
    #   biomass_vuln_jytr2 <- melt(biomass_vuln_jytrs2) %>%
    #     filter(value > 0) %>%
    #     mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR)) %>%
    #     group_by(Iteration, Rule, Year, Season, Region) %>%
    #     summarise(value = sum(value))

    #   biomass_vulnref_jytr2 <- mcmc$biomass_vulnref_ytr
    #   dimnames(biomass_vulnref_jytr2) <- list("Iteration" = 1:n_iter, "Year" = pyears, "Season" = seasons, "Region" = regions)
    #   biomass_vulnref_jytr2 <- melt(biomass_vulnref_jytr2) %>%
    #     filter(value > 0) %>%
    #     mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR))

    #   biomass_vulnref_yt2 <- biomass_vulnref_jytr2 %>%
    #     group_by(Iteration, Year, Season) %>%
    #     summarise(value = sum(value))

    #   biomass_cpue_ryt2 <- mcmc$biomass_cpue_ryt
    #   dimnames(biomass_cpue_ryt2) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = years, "Season" = seasons)
    #   biomass_cpue_ryt2 <- melt(biomass_cpue_ryt2)

    #   biomass_total_jytrs2 <- mcmc$biomass_total_ytrs
    #   dimnames(biomass_total_jytrs2) <- list("Iteration" = 1:n_iter, "Year" = pyears, "Season" = seasons, "Region" = regions, "Sex" = c(sex,"Total"))
    #   biomass_total_jytrs2 <- melt(biomass_total_jytrs2) %>%
    #     filter(value > 0)

    #   biomass_total_yts2 <- biomass_total_jytrs2 %>%
    #     group_by(Iteration, Year, Season, Sex) %>%
    #     summarise(value = sum(value))

    #   Bmsy <- mcmc$Bmsy_r
    #   dimnames(Bmsy) <- list("Iteration" = 1:n_iter, "Region" = regions2)
    #   Bmsy <- melt(Bmsy) %>%
    #     left_join(expand.grid(Iteration = 1:n_iter, Year = years), by = "Iteration") %>%
    #     mutate(Season = "AW") %>%
    #     group_by(Iteration, Region, value, Year, Season)

    #   Bref <- mcmc$Bref_jr
    #   dimnames(Bref) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Region" = regions2)
    #   Bref <- melt(Bref) %>%
    #     left_join(expand.grid(Iteration = 1:n_iter, Year = years), by = "Iteration") %>%
    #     mutate(Season = "AW") %>%
    #     group_by(Iteration, Region, value, Year, Season)
    # }
  } else {
    biomass_vuln_ytrs2 <- NULL
    biomass_vulnref_ytr2 <- NULL
    biomass_vulnref_yt2 <- NULL
    biomass_cpue_ryt2 <- NULL
    biomass_total_ytrs2 <- NULL
    biomass_total_yts2 <- NULL
  }



  # Total biomass
  if (length(map) > 0 & show_map) {
    biomass_total_ytrs1_in <- filter(biomass_total_jytrs1, Year <= data$last_yr)
    # biomass_total_ytrs1_in$Region <- sapply(1:nrow(biomass_total_ytrs1_in), function(x) paste0("Region ", biomass_total_ytrs1_in$Region[x]))
  }
  biomass_total_ytrs2_in <- filter(biomass_total_jytrs2, Year <= data$last_yr)
  # biomass_total_ytrs2_in$Region <- sapply(1:nrow(biomass_total_ytrs2_in), function(x) paste0("Region ", biomass_total_ytrs2_in$Region[x]))

  if (show_proj == FALSE) {
    p <- ggplot(data = biomass_total_ytrs2_in %>% filter(Region %in% regions), aes(x = Year, y = value, color = Sex, fill = Sex)) +
      stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
      stat_summary(fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
      stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
      expand_limits(y = 0) +
      xlab(xlab) + ylab("Total biomass (tonnes)") +
      scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
      scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
      theme_lsd()
    if (length(map) > 0 & show_map) {
      p <- p + geom_line(data = biomass_total_ytrs1_in %>% filter(Region %in% regions), aes(x = Year, y = value, colour = Sex), linetype = 2)
    }
    if (data$n_area > 1) {
      p <- p + facet_wrap(Region ~ Season, scales = scales)
    } else {
      p <- p + facet_wrap( ~ Season)
    }
  }

  if (show_proj == TRUE) {
    p <- ggplot(data = biomass_total_yts2, aes(x = Year, y = value, color = Sex, fill = Sex)) +
      geom_vline(aes(xintercept = data$last_yr), linetype = "dashed") +
      stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
      stat_summary(fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
      stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
      expand_limits(y = 0) +
      xlab(xlab) + ylab("Total biomass (tonnes)") +
      facet_wrap( ~ Season, scales = scales) +
      scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
      theme_lsd()
    if (length(map) > 0 & show_map) {
      # biomass_total_yts1$Region <- sapply(1:nrow(biomass_total_yts1), function(x) paste0("Region ", biomass_total_yts1$Region[x]))
      p <- p + geom_line(data = biomass_total_yts1, aes(x = Year, y = value, colour = Sex), linetype = 2)
    }
  }
  return(p)
}


#' Plot relative adjusted vulnerable biomass
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
plot_vulnref_rel <- function(object,
                             scales = "free",
                             show_proj = TRUE,
                             show_map = TRUE,
                             show_mcmc = TRUE,
                             xlab = "Fishing year",
                             figure_dir = "figure/")
{
  data <- object@data
  map <- object@map
  mcmc <- object@mcmc

  years <- data$first_yr:data$last_yr
  pyears <- data$first_yr:data$last_proj_yr
  sex <- c("Male","Immature female","Mature female")

  seasons <- c("AW","SS")
  regions <- 1:data$n_area
  if (length(regions) > 1) regions2 <- c(regions, max(regions + 1))
  if (length(regions) == 1) regions2 <- regions
  YR <- "YR" # label for the season before the season change year
  n_rules <- data$n_rules

  if (length(map) > 0 & show_map) {
    # if ("biomass_vuln_jytrs" %in% names(map)) {
      biomass_vuln_jytrs1 <- map$biomass_vuln_jytrs
      dimnames(biomass_vuln_jytrs1) <- list("Iteration" = 1, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions, Sex = sex)
      biomass_vuln_ytr1 <- melt(biomass_vuln_jytrs1) %>%
        filter(value > 0) %>%
        mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR)) %>%
        group_by(Iteration, Rule, Year, Season, Region) %>%
        summarise(value = sum(value))

      biomass_vulnref_jytr1 <- map$biomass_vulnref_jytr
      dimnames(biomass_vulnref_jytr1) <- list("Iteration" = 1, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions)
      biomass_vulnref_jytr1 <- melt(biomass_vulnref_jytr1) %>%
        filter(value > 0) %>%
        mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR))

      biomass_vulnref_yt1 <- biomass_vulnref_jytr1 %>%
        group_by(Iteration, Year, Season) %>%
        summarise(value = sum(value))

      biomass_cpue_ryt1 <- map$biomass_cpue_ryt
      dimnames(biomass_cpue_ryt1) <- list("Iteration" = 1, "Region" = regions, "Year" = years, "Season" = seasons)
      biomass_cpue_ryt1 <- melt(biomass_cpue_ryt1)

      biomass_total_jytrs1 <- map$biomass_total_jytrs
      dimnames(biomass_total_jytrs1) <- list("Iteration" = 1, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions, "Sex" = c(sex,"Total"))
      biomass_total_jytrs1 <- melt(biomass_total_jytrs1) %>%
        filter(value > 0)

      biomass_total_yts1 <- biomass_total_jytrs1 %>%
        group_by(Iteration, Year, Season, Sex) %>%
        summarise(value = sum(value))
    # } else {
    #   biomass_vuln_jytrs1 <- map$biomass_vuln_jytrs
    #   dimnames(biomass_vuln_jytrs1) <- list("Iteration" = 1, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions, Sex = sex)
    #   biomass_vuln_ytr1 <- melt(biomass_vuln_jytrs1) %>%
    #     filter(value > 0) %>%
    #     mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR)) %>%
    #     group_by(Iteration, Rule, Year, Season, Region) %>%
    #     summarise(value = sum(value))

    #   biomass_vulnref_jytr1 <- map$biomass_vulnref_ytr
    #   dimnames(biomass_vulnref_jytr1) <- list("Iteration" = 1, "Year" = pyears, "Season" = seasons, "Region" = regions)
    #   biomass_vulnref_jytr1 <- melt(biomass_vulnref_jytr1) %>%
    #     filter(value > 0) %>%
    #     mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR))

    #   biomass_vulnref_yt1 <- biomass_vulnref_jytr1 %>%
    #     group_by(Iteration, Year, Season) %>%
    #     summarise(value = sum(value))

    #   biomass_cpue_ryt1 <- map$biomass_cpue_ryt
    #   dimnames(biomass_cpue_ryt1) <- list("Iteration" = 1, "Region" = regions, "Year" = years, "Season" = seasons)
    #   biomass_cpue_ryt1 <- melt(biomass_cpue_ryt1)

    #   biomass_total_jytrs1 <- map$biomass_total_ytrs
    #   dimnames(biomass_total_jytrs1) <- list("Iteration" = 1, "Year" = pyears, "Season" = seasons, "Region" = regions, "Sex" = c(sex,"Total"))
    #   biomass_total_jytrs1 <- melt(biomass_total_jytrs1) %>%
    #     filter(value > 0)

    #   biomass_total_yts1 <- biomass_total_jytrs1 %>%
    #     group_by(Iteration, Year, Season, Sex) %>%
    #     summarise(value = sum(value))
    # }
  } else {
    biomass_vuln_jytrs1 <- NULL
    biomass_vulnref_jytr1 <- NULL
    biomass_vulnref_yt1 <- NULL
    biomass_cpue_ryt1 <- NULL
    biomass_total_jytrs1 <- NULL
    biomass_total_yts1 <- NULL
  }

  if (length(mcmc) > 0 & show_mcmc) {
    n_iter <- nrow(mcmc[[1]])
    # if ("biomass_vuln_jytrs" %in% names(mcmc)) {
      biomass_vuln_jytrs2 <- mcmc$biomass_vuln_jytrs
      dimnames(biomass_vuln_jytrs2) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions, Sex = sex)
      biomass_vuln_jytr2 <- melt(biomass_vuln_jytrs2) %>%
        filter(value > 0) %>%
        mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR)) %>%
        group_by(Iteration, Rule, Year, Season, Region) %>%
        summarise(value = sum(value))

      biomass_vulnref_jytr2 <- mcmc$biomass_vulnref_jytr
      dimnames(biomass_vulnref_jytr2) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions)
      biomass_vulnref_jytr2 <- melt(biomass_vulnref_jytr2) %>%
        filter(value > 0) %>%
        mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR))

      biomass_vulnref_yt2 <- biomass_vulnref_jytr2 %>%
        group_by(Iteration, Year, Season) %>%
        summarise(value = sum(value))

      biomass_cpue_ryt2 <- mcmc$biomass_cpue_ryt
      dimnames(biomass_cpue_ryt2) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = years, "Season" = seasons)
      biomass_cpue_ryt2 <- melt(biomass_cpue_ryt2)

      biomass_total_jytrs2 <- mcmc$biomass_total_jytrs
      dimnames(biomass_total_jytrs2) <- list("Iteration" = 1:n_iter, "Rules" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions, "Sex" = c(sex,"Total"))
      biomass_total_jytrs2 <- melt(biomass_total_jytrs2) %>%
        filter(value > 0)

      biomass_total_yts2 <- biomass_total_jytrs2 %>%
        group_by(Iteration, Year, Season, Sex) %>%
        summarise(value = sum(value))

      Bmsy <- mcmc$Bmsy_r
      dimnames(Bmsy) <- list("Iteration" = 1:n_iter, "Region" = regions2)
      Bmsy <- melt(Bmsy) %>%
        left_join(expand.grid(Iteration = 1:n_iter, Year = years), by = "Iteration") %>%
        mutate(Season = "AW") %>%
        group_by(Iteration, Region, value, Year, Season)

      Bref <- mcmc$Bref_jr
      dimnames(Bref) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Region" = regions2)
      Bref <- melt(Bref) %>%
        left_join(expand.grid(Iteration = 1:n_iter, Year = years), by = "Iteration") %>%
        mutate(Season = "AW") %>%
        group_by(Iteration, Region, value, Year, Season)
    # } else {

    #   biomass_vuln_jytrs2 <- mcmc$biomass_vuln_jytrs
    #   dimnames(biomass_vuln_jytrs2) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions, Sex = sex)
    #   biomass_vuln_jytr2 <- melt(biomass_vuln_jytrs2) %>%
    #     filter(value > 0) %>%
    #     mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR)) %>%
    #     group_by(Iteration, Rule, Year, Season, Region) %>%
    #     summarise(value = sum(value))

    #   biomass_vulnref_jytr2 <- mcmc$biomass_vulnref_ytr
    #   dimnames(biomass_vulnref_jytr2) <- list("Iteration" = 1:n_iter, "Year" = pyears, "Season" = seasons, "Region" = regions)
    #   biomass_vulnref_jytr2 <- melt(biomass_vulnref_jytr2) %>%
    #     filter(value > 0) %>%
    #     mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR))

    #   biomass_vulnref_yt2 <- biomass_vulnref_jytr2 %>%
    #     group_by(Iteration, Year, Season) %>%
    #     summarise(value = sum(value))

    #   biomass_cpue_ryt2 <- mcmc$biomass_cpue_ryt
    #   dimnames(biomass_cpue_ryt2) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = years, "Season" = seasons)
    #   biomass_cpue_ryt2 <- melt(biomass_cpue_ryt2)

    #   biomass_total_jytrs2 <- mcmc$biomass_total_ytrs
    #   dimnames(biomass_total_jytrs2) <- list("Iteration" = 1:n_iter, "Year" = pyears, "Season" = seasons, "Region" = regions, "Sex" = c(sex,"Total"))
    #   biomass_total_jytrs2 <- melt(biomass_total_jytrs2) %>%
    #     filter(value > 0)

    #   biomass_total_yts2 <- biomass_total_jytrs2 %>%
    #     group_by(Iteration, Year, Season, Sex) %>%
    #     summarise(value = sum(value))

    #   Bmsy <- mcmc$Bmsy_r
    #   dimnames(Bmsy) <- list("Iteration" = 1:n_iter, "Region" = regions2)
    #   Bmsy <- melt(Bmsy) %>%
    #     left_join(expand.grid(Iteration = 1:n_iter, Year = years), by = "Iteration") %>%
    #     mutate(Season = "AW") %>%
    #     group_by(Iteration, Region, value, Year, Season)

    #   Bref <- mcmc$Bref_jr
    #   dimnames(Bref) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Region" = regions2)
    #   Bref <- melt(Bref) %>%
    #     left_join(expand.grid(Iteration = 1:n_iter, Year = years), by = "Iteration") %>%
    #     mutate(Season = "AW") %>%
    #     group_by(Iteration, Region, value, Year, Season)
    # }
  } else {
    biomass_vuln_ytrs2 <- NULL
    biomass_vulnref_ytr2 <- NULL
    biomass_vulnref_yt2 <- NULL
    biomass_cpue_ryt2 <- NULL
    biomass_total_ytrs2 <- NULL
    biomass_total_yts2 <- NULL
  }

 
  # Reference biomass - version 3 - no projection
  if (length(map) > 0 & show_map) {
    bvref_in1 <- biomass_vulnref_jytr1 %>%
      filter(Year <= data$last_yr)
  }

  bvref_in2 <- biomass_vulnref_jytr2 %>%
    filter(Year <= data$last_yr)

  din <- bvref_in2 %>%
    group_by(Iteration, Year, Season, Region, value)

  dinaw <- din %>% filter(Season == "AW")
  dinaw1 <- dinaw %>% filter(Year == 1979) %>%
    rename("B1" = value)
  dinaw2 <- full_join(dinaw, dinaw1, by = c("Iteration", "Season", "Region")) %>%
    select(-Year.y) %>%
    rename("Year" = Year.x) %>%
    mutate(BB1 = value/B1)

  dinss <- din %>% filter(Season == "SS")
  dinss1 <- dinss %>% filter(Year == 1979) %>%
    rename("B1" = value)
  dinss2 <- full_join(dinss, dinss1, by = c("Iteration", "Season", "Region")) %>%
    select(-Year.y) %>%
    rename("Year" = Year.x) %>%
    mutate(BB1 = value / B1)

  din2 <- rbind.data.frame(dinaw2, dinss2) %>%
    ungroup()

  if (length(map) > 0  & show_map) {
    dinx <- bvref_in1 %>% group_by(.data$Iteration, .data$Year, .data$Season, .data$Region, .data$value)
  } else {
    dinx <- NULL
  }

  if (length(mcmc) > 0) {
    dinx <- bvref_in2 %>% group_by(.data$Iteration, .data$Year, .data$Season, .data$Region, .data$value)
  } else{
    dinx <- NULL

  }

  dinawx <- dinx %>% filter(.data$Season == "AW")
  dinaw1x <- dinawx %>% filter(.data$Year == 1979) %>% rename("B1" = value)
  dinaw2x <- full_join(dinawx, dinaw1x, by = c("Iteration", "Season", "Region")) %>%
    select(-Year.y) %>%
    rename("Year" = Year.x) %>%
    mutate(BB1 = value / B1)

  dinssx <- dinx %>% filter(.data$Season == "SS")
  dinss1x <- dinssx %>% filter(.data$Year == 1979) %>% rename("B1" = value)
  dinss2x <- full_join(dinssx, dinss1x, by = c("Iteration", "Season", "Region")) %>%
    select(-Year.y) %>%
    rename("Year" = Year.x) %>%
    mutate(BB1 = value / B1)

  din2x <- rbind.data.frame(dinaw2x, dinss2x) %>% ungroup()

  if (!show_proj) {
    p <- ggplot(data = din2, aes(x = .data$Year, y = .data$BB1, colour = .data$Season, fill = .data$Season)) +
      geom_hline(aes(yintercept = 0.5), linetype = "dashed", colour = "purple") +
      geom_hline(aes(yintercept = 0.3), linetype = "dashed", colour = "purple") +
      geom_hline(aes(yintercept = 0.1), linetype = "dashed", colour = "purple")
    p <- p + stat_summary(data = din2, aes(x = .data$Year, y = .data$BB1, color = .data$Season, fill = .data$Season), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
      stat_summary(data = din2, aes(x = .data$Year, y = .data$BB1, color = .data$Season, fill = .data$Season), fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
      stat_summary(data = din2, aes(x = .data$Year, y = .data$BB1, color = .data$Season), fun.y = function(x) quantile(x, 0.5), geom = "path", lwd = 1) +
      expand_limits(y = 0) +
      xlab(xlab) + ylab("Reference biomass (tonnes)") +
      scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
      theme_lsd()
    if (data$n_area > 1) {
      p <- p + facet_wrap(~Region)
    }
  }

  if (length(map) > 0 & show_map) bvref_in1 <- biomass_vulnref_jytr1
  bvref_in2 <- biomass_vulnref_jytr2
  din <- bvref_in2 %>% group_by(Iteration, Year, Season, Region, value)

  dinaw <- din %>% filter(.data$Season == "AW")
  dinaw1 <- dinaw %>% filter(.data$Year == 1979) %>% rename("B1" = value)
  dinaw2 <- full_join(dinaw, dinaw1, by = c("Iteration", "Season", "Region")) %>%
    select(-Year.y) %>%
    rename("Year" = Year.x) %>%
    mutate(BB1 = value / B1)

  dinss <- din %>% filter(.data$Season == "SS")
  dinss1 <- dinss %>% filter(.data$Year == 1979) %>% rename("B1" = value)
  dinss2 <- full_join(dinss, dinss1, by = c("Iteration", "Season", "Region")) %>%
    select(-Year.y) %>%
    rename("Year" = Year.x) %>%
    mutate(BB1 = value / B1)

  din2 <- rbind.data.frame(dinaw2, dinss2) %>% ungroup()

  if (length(map) > 0 & show_map) {
    dinx <- bvref_in1 %>% group_by(Iteration, Year, Season, Region, value)
  } else{
    dinx <- NULL
  }

  if (length(mcmc) > 0) {
    dinx <- bvref_in2 %>% group_by(Iteration, Year, Season, Region, value)
  } else{
    dinx <- NULL
  }

  dinawx <- dinx %>% filter(.data$Season == "AW")
  dinaw1x <- dinawx %>% filter(.data$Year == 1979) %>% rename("B1" = value)
  dinaw2x <- full_join(dinawx, dinaw1x, by = c("Iteration", "Season", "Region")) %>%
    select(-Year.y) %>%
    rename("Year" = Year.x) %>%
    mutate(BB1 = value / B1)

  dinssx <- dinx %>% filter(.data$Season == "SS")
  dinss1x <- dinssx %>% filter(.data$Year == 1979) %>% rename("B1" = value)
  dinss2x <- full_join(dinssx, dinss1x, by = c("Iteration", "Season", "Region")) %>%
    select(-Year.y) %>%
    rename("Year" = Year.x) %>%
    mutate(BB1 = value / B1)

  din2x <- rbind.data.frame(dinaw2x, dinss2x) %>% ungroup()

  if (show_proj) {
    p <- ggplot(data = din2, aes(x = .data$Year, y = .data$BB1, colour = .data$Season, fill = .data$Season)) +
      geom_hline(aes(yintercept = 0.5), linetype = "dashed", colour = "purple") +
      geom_hline(aes(yintercept = 0.3), linetype = "dashed", colour = "purple") +
      geom_hline(aes(yintercept = 0.1), linetype = "dashed", colour = "purple") +
      geom_vline(aes(xintercept = data$last_yr), linetype = "dashed") +
      stat_summary(data = din2, aes(x = Year, y = BB1, color = Season, fill = Season), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
      stat_summary(data = din2, aes(x = Year, y = BB1, color = Season, fill = Season), fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
      stat_summary(data = din2, aes(x = Year, y = BB1, color = Season), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
      expand_limits(y = 0) +
      xlab(xlab) + ylab("Reference biomass (tonnes)") +
      scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
      scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
      theme_lsd()
    if (data$n_area > 1) {
      p <- p + facet_wrap(~Region)
    }
  }
  return(p)
}


#' Plot Money biomass
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
plot_money_biomass <-  function(object,
                                scales = "free",
                                show_map = TRUE,
                                show_mcmc = TRUE,
                                show_proj = FALSE,
                                xlab = "Fishing year (1 April - 31 March)",
                                show_quants = c(0.05, 0.25))
{
  data <- object@data
  map <- object@map
  mcmc <- object@mcmc

  pyears <- data$first_yr:data$last_proj_yr
  seasons <- c("AW", "SS")
  regions <- 1:data$n_area
  if (length(regions) > 1) regions2 <- c(regions, max(regions) + 1)
  if (length(regions) == 1) regions2 <- regions
  YR <- "YR" # label for the season before the season change year
  n_rules <- data$n_rules

  if (length(map) > 0 & show_map) {
    mb1 <- map$biomass_money_jytr
    dimnames(mb1) <- list("Iteration" = 1, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions)
    mb1 <- melt(mb1) %>%
      filter(.data$value > 0) %>%
      mutate(Season = as.character(.data$Season), Season = ifelse(.data$Year >= data$season_change_yr, .data$Season, YR)) %>%
      group_by(.data$Iteration, .data$Rule, .data$Year, .data$Season, .data$Region) %>%
      summarise(value = sum(.data$value))
  }

  if (length(mcmc) > 0 & show_mcmc) {
    n_iter <- nrow(mcmc[[1]])
    mb2 <- mcmc$biomass_money_jytr
    dimnames(mb2) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions)
    mb2 <- melt(mb2) %>%
      filter(.data$value > 0) %>%
      mutate(Season = as.character(.data$Season), Season = ifelse(.data$Year >= data$season_change_yr, .data$Season, YR)) %>%
      group_by(.data$Iteration, .data$Rule, .data$Year, .data$Season, .data$Region) %>%
      summarise(value = sum(.data$value))
  }

  if (show_proj) {
    if (length(map) > 0 & show_map) mb_in1 <- mb1
    mb_in2 <- mb2
  } else {
    if (length(map) > 0 & show_map) {
      mb_in1 <- mb1 %>% filter(.data$Year <= data$last_yr)
    }
    mb_in2 <- mb2 %>% filter(.data$Year <= data$last_yr)
  }
  mb_in <- mb_in2 %>% mutate("Label" = "") %>%
    group_by(.data$Iteration, .data$Rule, .data$Year, .data$Season, .data$Region, .data$value, .data$Label) %>%
    filter(Season != "YR")

  p <- ggplot(data = mb_in, aes(x = .data$Year, y = .data$value, colour = .data$Season, fill = .data$Season))

  if (show_proj) p <- p + geom_vline(aes(xintercept = data$last_yr), linetype = "dashed")

  if (0.05 %in% show_quants) {
    p <- p +
      stat_summary(data = mb_in, geom = "ribbon", alpha = 0.125, colour = NA, aes(x = .data$Year, y = .data$value, fill = .data$Season),
                   fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95))
  }

  if (0.25 %in% show_quants) {
    p <- p +
      stat_summary(data = mb_in, geom = "ribbon", alpha = 0.25, colour = NA, aes(x = .data$Year, y = .data$value, fill = .data$Season),
                   fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75))
  }

  p <- p + stat_summary(data = mb_in, aes(x = .data$Year, y = .data$value, color = .data$Season), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
    expand_limits(y = 0) +
    labs(x = xlab, y = "Money biomass (tonnes)") +
    scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1), expand = c(0, 1)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
    theme_lsd()

  if (length(map) > 0 & show_map) {
    p <- p + geom_line(data = mb_in1 %>% filter(Season != "YR"), aes(x = .data$Year, y = .data$value), linetype = 2)
  }

  if (data$n_area > 1) {
    if (data$n_rules == 1) {
      p <- p + facet_wrap(~ .data$Region, scales = scales)
    } else {
      p <- p + facet_wrap(.data$Rule ~ .data$Region, scales = scales)
    }
  }

  return(p)
}


#' Plot Money biomass and vulnerable biomass together
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
plot_money_vuln_biomass <-  function(object,
                                     scales = "free",
                                     show_map = TRUE,
                                     show_mcmc = TRUE,
                                     show_proj = FALSE,
                                     xlab = "Fishing year (1 April - 31 March)",
                                     show_quants = c(0.05, 0.25))
{
  data <- object@data
  map <- object@map
  mcmc <- object@mcmc

  pyears <- data$first_yr:data$last_proj_yr
  sex <- c("Male", "Immature female", "Mature female")
  seasons <- c("AW", "SS")
  regions <- 1:data$n_area
  if (length(regions) > 1) regions2 <- c(regions, max(regions) + 1)
  if (length(regions) == 1) regions2 <- regions
  YR <- "YR" # label for the season before the season change year
  n_rules <- data$n_rules

  if (length(map) > 0 & show_map) {
    mb1 <- map$biomass_money_jytr
    dimnames(mb1) <- list("Iteration" = 1, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions)
    mb1 <- melt(mb1) %>%
      filter(.data$value > 0) %>%
      mutate(Season = as.character(.data$Season), Season = ifelse(.data$Year >= data$season_change_yr, .data$Season, YR))

    mb1 <- mb1 %>%
      group_by(.data$Iteration, .data$Rule, .data$Year, .data$Season, .data$Region) %>%
      summarise(value = sum(.data$value)) %>%
      mutate(Type = "Money biomass")

    vb1 <- map$biomass_vuln_jytrs
    dimnames(vb1) <- list("Iteration" = 1, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions, Sex = sex)
    vb1 <- melt(vb1) %>%
      filter(.data$value > 0) %>%
      mutate(Season = as.character(.data$Season), Season = ifelse(.data$Year >= data$season_change_yr, .data$Season, YR))

    vb1 <- vb1 %>%
      group_by(.data$Iteration, .data$Rule, .data$Year, .data$Season, .data$Region) %>%
      summarise(value = sum(.data$value)) %>%
      mutate(Type = "Vulnerable biomass")

    all_b1 <- full_join(vb1, mb1)
  }

  if (length(mcmc) > 0 & show_mcmc) {
    n_iter <- nrow(mcmc[[1]])

    mb2 <- mcmc$biomass_money_jytr
    dimnames(mb2) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions)
    mb2 <- melt(mb2) %>%
      filter(.data$value > 0) %>%
      mutate(Season = as.character(.data$Season), Season = ifelse(.data$Year >= data$season_change_yr, .data$Season, YR)) %>%
      group_by(.data$Iteration, .data$Rule, .data$Year, .data$Season, .data$Region) %>%
      summarise(value = sum(.data$value)) %>%
      mutate(Type = "Money biomass")

    vb2 <- mcmc$biomass_vuln_jytrs
    dimnames(vb2) <- list("Iteration" = 1:n_iter, "Rule" = 1:n_rules, "Year" = pyears, "Season" = seasons, "Region" = regions, Sex = sex)
    vb2 <- melt(vb2) %>%
      filter(.data$value > 0) %>%
      mutate(Season = as.character(.data$Season), Season = ifelse(.data$Year >= data$season_change_yr, .data$Season, YR)) %>%
      group_by(.data$Iteration, .data$Rule, .data$Year, .data$Season, .data$Region) %>%
      summarise(value = sum(.data$value)) %>%
      mutate(Type = "Vulnerable biomass")

    all_b2 <- full_join(vb1, mb2)
  }

  # money biomass
  if (show_proj) {
    if (length(map) > 0 & show_map) all_b1 <- all_b1
    all_b2 <- all_b22
  } else {
    if (length(map) > 0 & show_map) {
      all_b1 <- all_b1 %>% filter(.data$Year <= data$last_yr)
    }
    all_b2 <- all_b2 %>% filter(.data$Year <= data$last_yr)
  }

  p <- ggplot(data = all_b1, aes(x = .data$Year, y = .data$value, colour = .data$Season, fill = .data$Season, linetype = Type))

  if (show_proj) p <- p + geom_vline(aes(xintercept = data$last_yr), linetype = "dashed")

  if (0.05 %in% show_quants) {
    p <- p +
      stat_summary(data = all_b1, geom = "ribbon", alpha = 0.125, colour = NA, aes(x = .data$Year, y = .data$value, fill = .data$Season, linetype = .data$Type),
                   fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95))
  }

  if (0.25 %in% show_quants) {
    p <- p +
      stat_summary(data = all_b1, geom = "ribbon", alpha = 0.25, colour = NA, aes(x = .data$Year, y = .data$value, fill = .data$Season, linetype = .data$Type),
                   fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75))
  }

  p <- p + stat_summary(data = all_b1, aes(x = .data$Year, y = .data$value, color = .data$Season, linetype = .data$Type), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
    expand_limits(y = 0) +
    labs(x = xlab, y = "Biomass (tonnes)") +
    scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1), expand = c(0, 1)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
    theme_lsd()

  if (data$n_area > 1) {
    if (data$n_rules == 1) {
      p <- p + facet_wrap(~ .data$Region, scales = scales)
    } else {
      p <- p + facet_wrap(.data$Rule ~ .data$Region, scales = scales)
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
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#'
plot_biomass <- function(object,
                         scales = "free",
                         show_map = TRUE,
                         show_mcmc = TRUE,
                         xlab = "Fishing year",
                         figure_dir = "figure/")
{
  # spawning stock biomass
  p <- plot_ssb(object)
  ggsave(paste0(figure_dir, "biomass_spawning.png"), p, width = 12)

  p <- plot_ssb(object, show_proj = TRUE, show_map = FALSE)
  ggsave(paste0(figure_dir, "biomass_spawning_v2.png"), p, width = 15)

  p <- plot_ssb(object, show_ref = TRUE)
  ggsave(paste0(figure_dir, "biomass_spawning_wRef.png"), p, width = 12)

  p <- plot_ssb(object, show_proj = TRUE, show_ref = TRUE, show_map = FALSE)
  ggsave(paste0(figure_dir, "biomass_spawning_wRef_v2.png"), p, width = 15)

  # AW adjusted vulnerable biomass
  p <- plot_vulnref_AW(object)
  ggsave(paste0(figure_dir, "biomass_AW_vulnref.png"), p, width = 12)

  p <- plot_vulnref_AW(object, show_proj = TRUE, show_map = FALSE)
  ggsave(paste0(figure_dir, "biomass_AW_vulnref_v2.png"), p, width = 15)

  p <- plot_vulnref_AW(object, show_ref = TRUE)
  ggsave(paste0(figure_dir, "biomass_AW_vulnref_wRef.png"), p, width = 12)

  p <- plot_vulnref_AW(object, show_proj = TRUE, show_ref = TRUE, show_map = FALSE)
  ggsave(paste0(figure_dir, "biomass_AW_vulnref_wRef_v2.png"), p, width = 15)


  p <- plot_total_biomass(object, show_proj = FALSE)
  ggsave(paste0(figure_dir, "biomass_total.png"), p, width = 12)

  p <- plot_total_biomass(object, show_proj = TRUE, show_map = FALSE)
  ggsave(paste0(figure_dir, "biomass_total_v2.png"), p, width = 15)

  # Vulnerable biomass  no projection
  p <- plot_vulnerable_biomass(object, show_proj = FALSE)
  ggsave(paste0(figure_dir, "biomass_vuln.png"), p, width = 12)

  # vulnerable biomass with projection
  p <- plot_vulnerable_biomass(object, show_proj = TRUE, show_map = FALSE)
  ggsave(paste0(figure_dir, "biomass_vuln_v2.png"), p, width = 12)

  # Reference biomass - version 1
  p <- plot_vulnerable_reference_biomass(object)
  ggsave(paste0(figure_dir, "biomass_vulnref.png"), p, width = 10)

  # # Reference biomass - version 2
  # p <- ggplot(data = biomass_vulnref_yt2, aes(x = Year, y = value/1000, color = Season, fill = Season)) +
  #     geom_vline(aes(xintercept = data$last_yr), linetype = "dashed") +
  #     geom_vline(aes(xintercept = data$first_ref_yr), linetype = "dashed") +
  #     geom_vline(aes(xintercept = data$last_ref_yr), linetype = "dashed") +
  #     stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
  #     stat_summary(fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
  #     stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
  #     expand_limits(y = 0) +
  #     xlab(xlab) + ylab("Reference biomass (tonnes)") +
  #     scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
  #     theme_lsd()
  # if (length(map) > 0 & show_map) {
  #     p <- p + geom_line(data = biomass_vulnref_yt1, aes(x = Year, y = value/1000), linetype = 2)
  # }
  p <- plot_vulnerable_reference_biomass(object, show_proj = TRUE, show_map = FALSE)
  ggsave(paste0(figure_dir, "biomass_vulnref_v2.png"), p, width = 12)

  p <- plot_vulnref_rel(object, show_proj = FALSE)
  ggsave(paste0(figure_dir, "biomass_vulnref_relyr1.png"), p, width = 10)

  p <- plot_vulnref_rel(object, show_proj = TRUE, show_map = FALSE)
  ggsave(paste0(figure_dir, "biomass_vulnref_relyr1_v2.png"), p, width = 10)

  # plot Money biomass
  p <- plot_money_biomass(object, show_proj = FALSE, show_map = TRUE)
  ggsave(paste0(figure_dir, "biomass_money.png"), p, width = 12)

  # plot Money and vulnerable biomass on the same plot
  p <- plot_money_vuln_biomass(object, show_proj = FALSE, show_map = TRUE)
  ggsave(paste0(figure_dir, "biomass_vuln_and_money.png"), p, width = 12)

  # Plot projected biomass
  p <- plot_vulnref_AW_proj(object)
  ggsave(paste0(figure_dir, "biomass_vulnref_proj.png"), p, width = 14)

  p <- plot_ssb_AW_proj(object)
  ggsave(paste0(figure_dir, "biomass_ssb_proj.png"), p, width = 14)
}
