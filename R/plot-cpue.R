#' Plot Offset CPUE
#'
#' Plot the offset CPUE data and fit to the data.
#'
#' @param object and LSD object
#' @param scales free or fixed
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param figure_dir the directory to save to
#' @import dplyr
#' @import ggplot2
#' @importFrom stats quantile
#' @importFrom reshape2 melt
#' @export
#'
plot_offset_cpue <- function(object,
                             scales = "fixed",
                             xlab = "Offset year (October-September)",
                             ylab = "Offset year CPUE (kg/potlift)",
                             figure_dir = "figure/") {

  data <- object@data
  map <- object@map
  mcmc <- object@mcmc

  n_iter <- nrow(mcmc[[1]])
  n_area <- data$n_area
  n_rules <- data$n_rules
  seasons <- c("AW","SS")
  years <- data$first_yr:data$last_proj_yr

  obs_offset <- data$data_offset_cpue_ry
  dimnames(obs_offset) <- list(Region = 1:n_area, Year = data$data_offset_cpue_year_i)
  obs_offset <- melt(obs_offset) %>%
    mutate(Iteration = NA, Data = "Observed") %>%
    filter(Region == 2)

    p <- ggplot(data = obs_offset) +
    geom_point(aes(x = Year, y = value), color = "red") +
    facet_wrap(~Region)

  # if (length(map) > 0) {
  #     map_offset <- map$mp_offset_cpue_jry
  #     dimnames(map_offset) <- list(Iteration = 1, Option = 1:n_rules, Region = 1:n_area, Year = years)
  #     map_offset <- melt(map_offset) %>%
  #         mutate(Data = "Expected") %>%
  #         filter(Year >= min(obs_offset$Year))
  # } else {
  #     map_offset <- NULL
  # }

  if (length(mcmc) > 0) {
    mcmc_offset <- mcmc$mp_offset_cpue_jry
    dimnames(mcmc_offset) <- list(Iteration = 1:n_iter, Option = 1:n_rules, Region = 1:n_area, Year = data$first_yr:data$last_proj_yr)
    mcmc_offset <- melt(mcmc_offset) %>%
      mutate(Data = "Expected") %>%
      filter(Year >= min(obs_offset$Year)) %>%
      filter(Region == 2)
  } else {
    mcmc_offset <- NULL
  }

  p <- ggplot(data = obs_offset) +
    expand_limits(y = 0) +
    xlab(xlab) + ylab(ylab) +
    scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
    theme_lsd()

  if (!is.null(mcmc_offset)) {
    p <- p + stat_summary(data = mcmc_offset, aes(x = Year, y = value), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
      stat_summary(data = mcmc_offset, aes(x = Year, y = value), fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
      stat_summary(data = mcmc_offset, aes(x = Year, y = value), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1)
  }
  # if (!is.null(map_offset)) {
  #     p <- p + geom_line(data = map_offset, aes(x = Year, y = value), linetype = 2)
  # }
  if (data$n_area > 1) {
    p <- p + facet_wrap(~ Region, scales = scales)
  }
  p <- p + geom_point(data = obs_offset, aes(x = Year, y = value), color = "red", alpha = 0.75)

  ggsave(paste0(figure_dir, "cpue_offset.png"), p, width = 9)
}


#' Plot CPUE
#'
#' Plot the CPUE data and fit to the data.
#'
#' @param object and LSD object
#' @param scales free or fixed
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param figure_dir the directory to save to
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#'
plot_cpue <- function(object,
                      scales = "fixed",
                      xlab = "Fishing year",
                      ylab = "CPUE",
                      figure_dir = "figure/") {

  data <- object@data
  map <- object@map
  mcmc <- object@mcmc

  n_iter <- nrow(mcmc[[1]])
  seasons <- c("AW", "SS")
  n_q <- data$n_q
  years <- data$first_yr:data$last_yr
  pyears <- data$first_yr:data$last_proj_yr
  poffset <- data$data_offset_cpue_ry

  if (length(map) > 0) {
    pq1 <- map$par_q_cpue_qy
    dimnames(pq1) <- list("Iteration" = 1, "qtype" = 1:n_q, "Year" = pyears)
    pq1 <- melt(pq1, value.name = "q")  %>%
      filter(Year <= max(years))

    pcpue1 <- map$pred_cpue_i
    dimnames(pcpue1) <- list("Iteration" = 1, "I" = 1:data$n_cpue)
    pcpue1 <- melt(pcpue1, value.name = "CPUE") %>%
      select(Iteration, CPUE) %>%
      mutate(Data = "Expected", Type = "CPUE", Region = data$data_cpue_area_i, Year = data$data_cpue_year_i, Season = seasons[data$data_cpue_season_i])
    if (any(names(data) == "data_cpue_type_i")) {
      pcpue1 <- bind_cols(pcpue1, "CPUE_type" = data$data_cpue_type_i)
    } else {
      pcpue1 <- bind_cols(pcpue1, "CPUE_type" = 1)
    }
    pcpue1 <- pcpue1 %>%
      full_join(pq1, by = c("Iteration", "Year")) %>%
      select(-q)
    pcpue1$SD <- NA

    rcpue1 <- map$resid_cpue_i
    dimnames(rcpue1) <- list("Iteration" = 1, "I" = 1:data$n_cpue)
    rcpue1 <- melt(rcpue1, value.name = "CPUE") %>%
      select(Iteration, CPUE) %>%
      mutate(Data = "Residual", Type = "CPUE", Region = data$data_cpue_area_i, Year = data$data_cpue_year_i, Season = seasons[data$data_cpue_season_i])
    if (any(names(data) == "data_cpue_type_i")) {
      rcpue1 <- bind_cols(rcpue1, "CPUE_type" = data$data_cpue_type_i)
    } else {
      rcpue1 <- bind_cols(rcpue1, "CPUE_type" = 1)
    }
    rcpue1 <- rcpue1 %>%
      full_join(pq1, by = c("Iteration", "Year")) %>%
      select(-q)
  } else {
    pcpue1 <- NULL
    rcpue1 <- NULL
  }

  if (length(mcmc) > 0) {
    pq <- mcmc$par_q_cpue_qy
    dimnames(pq) <- list("Iteration" = 1:n_iter, "qtype" = 1:n_q, "Year" = pyears)
    pq <- melt(pq, value.name = "q") %>%
      filter(Year <= max(years))

    pcpue <- mcmc$pred_cpue_i
    dimnames(pcpue) <- list("Iteration" = 1:n_iter, "I" = 1:data$n_cpue)
    pcpue <- melt(pcpue, value.name = "CPUE") %>%
      select(Iteration, CPUE) %>%
      mutate(Data = "Expected", Type = "CPUE", Region = rep(data$data_cpue_area_i, each = n_iter), Year = rep(data$data_cpue_year_i, each = n_iter), Season = seasons[rep(data$data_cpue_season_i, each = n_iter)])
    if (any(names(data) == "data_cpue_type_i")) {
      pcpue <- bind_cols(pcpue, "CPUE_type" = rep(data$data_cpue_type_i, each = n_iter))
    } else {
      pcpue <- bind_cols(pcpue, "CPUE_type" = 1)
    }
    pcpue <- pcpue %>%
      full_join(pq, by = c("Iteration", "Year")) %>%
      select(-q)
    pcpue$SD <- NA

    rcpue <- mcmc$resid_cpue_i
    dimnames(rcpue) <- list("Iteration" = 1:n_iter, "I" = 1:data$n_cpue)
    rcpue <- melt(rcpue, value.name = "CPUE") %>%
      select(Iteration, CPUE) %>%
      mutate(Data = "Residual", Type = "CPUE", Region = rep(data$data_cpue_area_i, each = n_iter), Year = rep(data$data_cpue_year_i, each = n_iter), Season = seasons[rep(data$data_cpue_season_i, each = n_iter)])
    if (any(names(data) == "data_cpue_type_i")) {
      rcpue <- bind_cols(rcpue, "CPUE_type" = rep(data$data_cpue_type_i, each = n_iter))
    } else {
      rcpue <- bind_cols(rcpue, "CPUE_type" = 1)
    }
    rcpue <- rcpue %>%
      full_join(pq, by = c("Iteration", "Year")) %>%
      select(-q)
  } else {
    pcpue <- NULL
    rcpue <- NULL
  }

  # CPUE
  ocpue <- data.frame(
    Iteration = NA,
    CPUE = data$data_cpue_i,
    Data = "Observed", Type = "CPUE",
    Region = data$data_cpue_area_i,
    Year = data$data_cpue_year_i,
    Season = seasons[data$data_cpue_season_i],
    qtype = data$data_cpue_q_i,
    SD = sqrt(data$cov_cpue_sd_i^2 + data$cov_cpue_process_error_i^2) * 1.0 / data$cpue_like_wt[data$data_cpue_q_i]
  )
  if (any(names(data) == "data_cpue_type_i")) {
    ocpue <- bind_cols(ocpue, "CPUE_type" = data$data_cpue_type_i)
  } else {
    ocpue <- bind_cols(ocpue, "CPUE_type" = 1)
  }
  ocpue <- ocpue %>%
    mutate(CPUE_name = case_when(Year < 1979 ~ "CR",
                                 Year >= 1979 & Year <= 1989 & CPUE_type == 1 ~ "FSU",
                                 Year == 1989 & CPUE_type == 1 & Season == "SS" ~ "CELR",
                                 Year <= 2019 & Year > 1989 & CPUE_type == 1 ~ "CELR",
                                 CPUE_type == 2 ~ "Logbook" )) %>%
    filter(!is.na(Type))
  ocpue$CPUE_name <- factor(ocpue$CPUE_name, levels = unique(ocpue$CPUE_name))

  pcpue <- pcpue %>%
    mutate(CPUE_name = case_when(Year < 1979 ~ "CR",
                                 Year >= 1979 & Year <= 1989 & CPUE_type == 1 ~ "FSU",
                                 Year == 1989 & CPUE_type == 1 & Season == "SS" ~ "CELR",
                                 Year <= 2019 & Year > 1989 & CPUE_type == 1 ~ "CELR",
                                 CPUE_type == 2 ~ "Logbook" )) %>%
    filter(!is.na(Type))
  pcpue$CPUE_name <- factor(pcpue$CPUE_name, levels = unique(pcpue$CPUE_name))

  rcpue <- rcpue %>%
    mutate(CPUE_name = case_when(Year < 1979 ~ "CR",
                                 Year >= 1979 & Year <= 1989 & CPUE_type == 1 ~ "FSU",
                                 Year == 1989 & CPUE_type == 1 & Season == "SS" ~ "CELR",
                                 Year <= 2019 & Year > 1989 & CPUE_type == 1 ~ "CELR",
                                 CPUE_type == 2 ~ "Logbook" )) %>%
    filter(!is.na(Type))
  rcpue$CPUE_name <- factor(rcpue$CPUE_name, levels = unique(rcpue$CPUE_name))

  pcpue1 <- pcpue1 %>%
    mutate(CPUE_name = case_when(Year < 1979 ~ "CR",
                                 Year >= 1979 & Year <= 1989 & CPUE_type == 1 ~ "FSU",
                                 Year == 1989 & CPUE_type == 1 & Season == "SS" ~ "CELR",
                                 Year <= 2019 & Year > 1989 & CPUE_type == 1 ~ "CELR",
                                 CPUE_type == 2 ~ "Logbook" )) %>%
    filter(!is.na(Type))
  pcpue1$CPUE_name <- factor(pcpue1$CPUE_name, levels = unique(pcpue1$CPUE_name))

  rcpue1 <- rcpue1 %>%
    mutate(CPUE_name = case_when(Year < 1979 ~ "CR",
                                 Year >= 1979 & Year <= 1989 & CPUE_type == 1 ~ "FSU",
                                 Year == 1989 & CPUE_type == 1 & Season == "SS" ~ "CELR",
                                 Year <= 2019 & Year > 1989 & CPUE_type == 1 ~ "CELR",
                                 CPUE_type == 2 ~ "Logbook" )) %>%
    filter(!is.na(Type))
  rcpue1$CPUE_name <- factor(rcpue1$CPUE_name, levels = unique(rcpue1$CPUE_name))

  # if (!is.null(pcpue)) {
  #     pcpue <- mutate(pcpue, QY = paste(Year, qtype))
  #     ocpue <- mutate(ocpue, QY = paste(Year, qtype))
  #     rcpue <- mutate(rcpue, QY = paste(Year, qtype))
  #     pcpue <- filter(pcpue, QY %in% ocpue$QY)
  #     rcpue <- filter(rcpue, QY %in% ocpue$QY)
  # }
  # if (!is.null(pcpue1)) {
  #     pcpue1 <- pcpue1 %>%
  #         mutate(QY = paste(Year, qtype)) %>%
  #         filter(QY %in% ocpue$QY)
  # }
  # if (!is.null(rcpue1)) {
  #     rcpue1 <- rcpue1 %>%
  #         mutate(QY = paste(Year, qtype)) %>%
  #         filter(QY %in% ocpue$QY)
  # }

  # ocpue <- ocpue %>%
  #     mutate("Region_q" = paste("Region", Region, "q", qtype)) %>%
  #     mutate(qtype = as.character(qtype))
  # ocpue$qtype <- factor(ocpue$qtype, levels = rev(unique(ocpue$qtype)))

  # pcpue <- pcpue %>% mutate("Region_q" = paste("Region", Region, "q", qtype)) %>%
  #     filter(Region_q %in% ocpue$Region_q) %>%
  #     mutate(qtype = as.character(qtype))
  # pcpue$qtype <- factor(pcpue$qtype, levels = rev(unique(pcpue$qtype)))

  # rcpue <- rcpue %>% mutate("Region_q" = paste("Region", Region, "q", qtype)) %>%
  #     filter(Region_q %in% ocpue$Region_q) %>%
  #     mutate(qtype = as.character(qtype))
  # rcpue$qtype <- factor(rcpue$qtype, levels = rev(unique(rcpue$qtype)))

  # if (!is.null(pcpue1)) {
  #     pcpue1 <- pcpue1 %>% mutate("Region_q" = paste("Region", Region, "q", qtype)) %>%
  #         filter(Region_q %in% ocpue$Region_q) %>%
  #         mutate(qtype = as.character(qtype))
  #     pcpue1$qtype <- factor(pcpue1$qtype, levels = rev(unique(pcpue1$qtype)))
  # }
  # if (!is.null(rcpue1)) {
  #     rcpue1 <- rcpue1 %>% mutate("Region_q" = paste("Region", Region, "q", qtype)) %>%
  #         filter(Region_q %in% ocpue$Region_q) %>%
  #         mutate(qtype = as.character(qtype))
  #     rcpue1$qtype <- factor(rcpue1$qtype, levels = rev(unique(rcpue1$qtype)))
  # }

  # Plot CPUE
  p <- ggplot(data = ocpue) +
    geom_point(aes(x = Year, y = CPUE), color = "red", alpha = 0.75) +
    geom_linerange(aes(x = Year, ymin = exp(log(CPUE) - SD), ymax = exp(log(CPUE) + SD)), color = "red", alpha = 0.75) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    xlab(xlab) + ylab(ylab) +
    theme_lsd()
  if (!is.null(pcpue)) {
    p <- p + stat_summary(data = pcpue, aes(x = Year, y = CPUE), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
      stat_summary(data = pcpue, aes(x = Year, y = CPUE), fun = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
      stat_summary(data = pcpue, aes(x = Year, y = CPUE), fun = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1)
  }
  if (!is.null(pcpue1)) {
    p <- p + geom_line(data = pcpue1, aes(x = Year, y = CPUE), linetype = 2)
  }
  if (data$n_area > 1) {
    p <- p + facet_wrap(Region ~ Season+CPUE_name, scales = "free", nrow = data$n_area)
    ggsave(paste0(figure_dir, "cpue.png"), p, height = 10, width = 15)
  } else {
    p <- p + facet_wrap(Season ~ CPUE_name, scales = "free", ncol = length(unique(ocpue$CPUE_name)))
    ggsave(paste0(figure_dir, "cpue.png"), p, height = 9, width = 12)
  }

  ## separate by series
  ### CR
  ocr_yrs <- ocpue %>% filter(Year < 1979) %>% mutate(Region = paste0("Region ", Region))
  if (nrow(ocr_yrs) > 0) {
    pcr_yrs <- pcpue %>% filter(Year < 1979) %>% mutate(Region = paste0("Region ", Region))
    p1cr_yrs <- pcpue1 %>%  filter(Year < 1979) %>% mutate(Region = paste0("Region ", Region))
    p <- ggplot(data = ocr_yrs) +
      geom_point(aes(x = Year, y = CPUE), color = "red", alpha = 0.75) +
      geom_linerange(aes(x = Year, ymin = exp(log(CPUE) - SD), ymax = exp(log(CPUE) + SD)), color = "red", alpha = 0.75) +
      scale_x_continuous(breaks = pretty(c(min(ocr_yrs$Year), max(ocr_yrs$Year)))) +
      scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
      expand_limits(y = 0) +
      xlab(xlab) + ylab(ylab) +
      theme_lsd()
    if (!is.null(pcpue)) {
      p <- p + stat_summary(data = pcr_yrs, aes(x = Year, y = CPUE), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(data = pcr_yrs, aes(x = Year, y = CPUE), fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(data = pcr_yrs, aes(x = Year, y = CPUE), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1)
    }
    if (!is.null(pcpue1)) {
      p <- p + geom_line(data = p1cr_yrs, aes(x = Year, y = CPUE), linetype = 2)
    }
    if (length(unique(ocr_yrs$Region)) > 1) {
      p <- p + facet_wrap(~Region, scales = "free", ncol = data$n_area)
      ggsave(paste0(figure_dir, "cpue_CR.png"), p, width = 15, height = 10)
    } else {
      ggsave(paste0(figure_dir, "cpue_CR.png"), p, height = 9, width = 12)
    }
  }

  # FSU
  # dfilter <- expand.grid("Year" = 1979:1989, "Season" = c("AW","SS"))
  # dfilter <- dfilter[-which(dfilter$Year == 1989 & dfilter$Season == "SS"),]
  ocpue_fsu <- ocpue %>%
    filter(CPUE_name == "FSU") %>%
    mutate(Region = paste0("Region ", Region))
  pcpue_fsu <- pcpue %>%
    filter(CPUE_name == "FSU") %>%
    mutate(Region = paste0("Region ", Region))
  pcpue1_fsu <- pcpue1 %>%
    filter(CPUE_name == "FSU") %>%
    mutate(Region = paste0("Region ", Region))

  if (nrow(ocpue_fsu) > 0) {
    # ocr_yrs <- ocpue %>% filter(Flag == 1) #ocpue %>% right_join(dfilter) %>% mutate(Region = paste0("Region ", Region))
    # pcr_yrs <- pcpue %>% filter(Flag == 1) #right_join(dfilter) %>% mutate(Region = paste0("Region ", Region))
    # p1cr_yrs <- pcpue1 %>%  filter(Flag == 1) #right_join(dfilter) %>% mutate(Region = paste0("Region ", Region))
    p <- ggplot(data = ocpue_fsu) +
      geom_point(aes(x = Year, y = CPUE), color = "red", alpha = 0.75) +
      geom_linerange(aes(x = Year, ymin = exp(log(CPUE) - SD), ymax = exp(log(CPUE) + SD)), color = "red", alpha = 0.75) +
      scale_x_continuous(breaks = pretty(c(min(ocpue_fsu$Year), max(ocpue_fsu$Year)))) +
      scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
      expand_limits(y = 0) +
      xlab(xlab) + ylab(ylab) +
      theme_lsd()

    if (!is.null(pcpue_fsu)) {
      p <- p + stat_summary(data = pcpue_fsu, aes(x = Year, y = CPUE), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(data = pcpue_fsu, aes(x = Year, y = CPUE), fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(data = pcpue_fsu, aes(x = Year, y = CPUE), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1)
    }
    if (!is.null(pcpue1_fsu)) {
      p <- p + geom_line(data = pcpue1_fsu, aes(x = Year, y = CPUE), linetype = 2)
    }
    if (length(unique(ocpue_fsu$Region)) > 1) {
      p <- p + facet_wrap(Region~Season, scales = "free", ncol = data$n_area)
      ggsave(paste0(figure_dir, "cpue_FSU.png"), p, width = 15, height = 10)
    } else {
      p <- p + facet_wrap(~Season, scales = "free", ncol = data$n_area)
      ggsave(paste0(figure_dir, "cpue_FSU.png"), p, height = 9, width = 12)
    }
  }

  # CELR
  # dfilter <- expand.grid("Year" = 1989:max(ocpue$Year), "Season" = c("AW","SS"))
  # dfilter <- dfilter[-which(dfilter$Year == 1989 & dfilter$Season == "AW"),]
  # dfilter <- dfilter[-which(dfilter$Year >=2019 & dfilter$Season == "SS"),]
  ocpue_celr <- ocpue %>% filter(CPUE_name == "CELR") #right_join(dfilter) %>% mutate(Region = paste0("Region ", Region))
  pcpue_celr <- pcpue %>% filter(CPUE_name == "CELR") #right_join(dfilter) %>% mutate(Region = paste0("Region ", Region))
  pcpue1_celr <- pcpue1 %>% filter(CPUE_name == "CELR") #right_join(dfilter) %>% mutate(Region = paste0("Region ", Region))

  if (nrow(ocpue_celr) > 0) {
    p <- ggplot(data = ocpue_celr) +
      geom_point(aes(x = Year, y = CPUE), color = "red", alpha = 0.75) +
      geom_linerange(aes(x = Year, ymin = exp(log(CPUE) - SD), ymax = exp(log(CPUE) + SD)), color = "red", alpha = 0.75) +
      scale_x_continuous(breaks = pretty(c(min(ocpue_celr$Year), max(ocpue_celr$Year)))) +
      scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
      expand_limits(y = 0) +
      xlab(xlab) + ylab(ylab) +
      theme_lsd()
    if (!is.null(pcpue_celr)) {
      p <- p + stat_summary(data = pcpue_celr, aes(x = Year, y = CPUE), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(data = pcpue_celr, aes(x = Year, y = CPUE), fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(data = pcpue_celr, aes(x = Year, y = CPUE), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1)
    }
    if (!is.null(pcpue1_celr)) {
      p <- p + geom_line(data = pcpue1_celr, aes(x = Year, y = CPUE), linetype = 2)
    }
    if (length(unique(ocpue_celr$Region)) > 1) {
      p <- p + facet_wrap(Region~Season, scales = "free", ncol = data$n_area)
      ggsave(paste0(figure_dir, "cpue_CELR.png"), p, height = 10, width = 15)
    } else {
      p <- p + facet_wrap(~Season, scales = "free", ncol = data$n_area)
      ggsave(paste0(figure_dir, "cpue_CELR.png"), p, height = 9, width = 12)
    }
  }

  # Logbook
  # dfilter <- expand.grid("Year" = 1989:max(ocpue$Year), "Season" = c("AW","SS"))
  # dfilter <- dfilter[-which(dfilter$Year == 1989 & dfilter$Season == "AW"),]
  # dfilter <- dfilter[-which(dfilter$Year >=2019 & dfilter$Season == "SS"),]
  ocpue_lb <- ocpue %>% filter(CPUE_name == "Logbook") #right_join(dfilter) %>% mutate(Region = paste0("Region ", Region))
  pcpue_lb <- pcpue %>% filter(CPUE_name == "Logbook") #right_join(dfilter) %>% mutate(Region = paste0("Region ", Region))
  pcpue1_lb <- pcpue1 %>% filter(CPUE_name == "Logbook") #right_join(dfilter) %>% mutate(Region = paste0("Region ", Region))

  if (nrow(ocpue_lb) > 0) {
    p <- ggplot(data = ocpue_lb) +
      geom_point(aes(x = Year, y = CPUE), color = "red", alpha = 0.75) +
      geom_linerange(aes(x = Year, ymin = exp(log(CPUE) - SD), ymax = exp(log(CPUE) + SD)), color = "red", alpha = 0.75) +
      scale_x_continuous(breaks = pretty(c(min(ocpue_lb$Year), max(ocpue_lb$Year)))) +
      scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
      expand_limits(y = 0) +
      xlab(xlab) + ylab(ylab) +
      theme_lsd()
    if (!is.null(pcpue_lb)) {
      p <- p + stat_summary(data = pcpue_lb, aes(x = Year, y = CPUE), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(data = pcpue_lb, aes(x = Year, y = CPUE), fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(data = pcpue_lb, aes(x = Year, y = CPUE), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1)
    }
    if (!is.null(pcpue1_lb)) {
      p <- p + geom_line(data = pcpue1_lb, aes(x = Year, y = CPUE), linetype = 2)
    }
    if (length(unique(ocpue_lb$Region)) > 1) {
      p <- p + facet_wrap(Region~Season, scales = "free", ncol = data$n_area)
      ggsave(paste0(figure_dir, "cpue_Logbook.png"), p, height = 10, width = 15)
    } else {
      p <- p + facet_wrap(~Season, scales = "free", ncol = data$n_area)
      ggsave(paste0(figure_dir, "cpue_Logbook.png"), p, height = 9, width = 12)
    }
  }

  # CPUE residuals
  p <- ggplot(rcpue) +
    geom_hline(yintercept = 0, alpha = 0.2) +
    xlab(xlab) + ylab("Standardised residual") +
    theme_lsd() +
    theme(legend.position = "none")

  if (n_iter > 10) {
    p <- p + geom_violin(aes(x = as.factor(Year), y = CPUE, colour = Season, fill = Season)) +
      scale_x_discrete(breaks = seq(0, 1e6, 5))
  } else {
    p <- p + geom_point(aes(x = Year, y = CPUE, color = Season)) +
      scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1))
  }
  if (data$n_area > 1) {
    p <- p + facet_wrap(Season ~ Region+CPUE_name, scales = "free", ncol = data$n_area)
  } else {
    p <- p + facet_grid(Season ~ CPUE_name, scales = "free")
  }
  ggsave(paste0(figure_dir, "cpue_resid.png"), p, height = 9, width = 12)

  ## separate by series
  # CR
  rcr_yrs <- rcpue %>% filter(Year < 1979) %>% mutate(Region = paste0("Region ", Region))

  if (nrow(rcr_yrs) > 0) {
    p <- ggplot(rcr_yrs) +
      geom_hline(yintercept = 0, alpha = 0.2) +
      # expand_limits(y = 0) +
      # scale_x_continuous(breaks = pretty(c(min(rcr_yrs$Year),max(rcr_yrs$Year)))) +
      xlab(xlab) + ylab("Standardised residual") +
      theme_lsd() +
      theme(legend.position = "none")
    if (n_iter > 10) {
      p <- p + geom_violin(aes(x = as.factor(Year), y = CPUE, colour = Season, fill = Season)) +
        scale_x_discrete(breaks = seq(0, 1e6, 5))
    } else {
      p <- p + geom_point(aes(x = Year, y = CPUE, color = Season)) +
        scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1))
    }
    if (length(unique(rcr_yrs$Region)) > 1) {
      p <- p + facet_wrap( ~ Region, scales = "free", ncol = data$n_area)
    }
    ggsave(paste0(figure_dir, "cpue_resid_CR.png"), p, height = 9, width = 12)
  }

  # FSU
  # dfilter <- expand.grid("Year" = 1979:1989, "Season" = c("AW","SS"))
  # dfilter <- dfilter[-which(dfilter$Year == 1989 & dfilter$Season == "SS"),]
  rcpue_fsu <- rcpue %>%
    filter(CPUE_name == "FSU") %>%
    mutate(Region = paste0("Region ", Region))

  if (nrow(rcpue_fsu) > 0) {
    p <- ggplot(rcpue_fsu) +
      geom_hline(yintercept = 0, alpha = 0.2) +
      labs(x = xlab, y = "Standardised residual") +
      theme_lsd() +
      theme(legend.position = "none")

    if (n_iter > 10) {
      p <- p + geom_violin(aes(x = as.factor(Year), y = CPUE, colour = Season, fill = Season)) +
        scale_x_discrete(breaks = seq(0, 1e6, 5))
    } else {
      p <- p + geom_point(aes(x = Year, y = CPUE, color = Season)) +
        scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1))
    }
    if (length(unique(rcpue_fsu$Region)) > 1) {
      p <- p + facet_wrap(Region ~ Season, scales = "free", ncol = data$n_area)
    } else {
      p <- p + facet_wrap( ~ Season, scales = "free", ncol = data$n_area)
    }
    ggsave(paste0(figure_dir, "cpue_resid_FSU.png"), p, height = 9, width = 12)
  }

  # CELR
  # dfilter <- expand.grid("Year" = 1989:max(ocpue$Year), "Season" = c("AW","SS"))
  # dfilter <- dfilter[-which(dfilter$Year==1989 & dfilter$Season == "AW"),]
  # dfilter <- dfilter[-which(dfilter$Year == max(ocpue$Year) & dfilter$Season == "SS"),]
  rcpue_celr <- rcpue %>% filter(CPUE_name == "CELR") #right_join(dfilter) %>% mutate(Region = paste0("Region ", Region))

  if (nrow(rcpue_celr) > 0) {
    p <- ggplot(rcpue_celr) +
      geom_hline(yintercept = 0, alpha = 0.2) +
      # expand_limits(y = 0) +
      # scale_x_continuous(breaks = pretty(c(min(rcpue_celr$Year),max(rcpue_celr$Year)))) +
      xlab(xlab) + ylab("Standardised residual") +
      theme_lsd() +
      theme(legend.position = "none")

    if (n_iter > 10) {
      p <- p + geom_violin(aes(x = as.factor(Year), y = CPUE, colour = Season, fill = Season)) +
        scale_x_discrete(breaks = seq(0, 1e6, 5))
    } else {
      p <- p + geom_point(aes(x = Year, y = CPUE, color = Season)) +
        scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1))
    }
    if (length(unique(rcpue_celr$Region)) > 1) {
      p <- p + facet_wrap(Region ~ Season, scales = "free", ncol = data$n_area)
    } else {
      p <- p + facet_wrap( ~ Season, scales = "free", ncol = data$n_area)
    }
    ggsave(paste0(figure_dir, "cpue_resid_CELR.png"), p, height = 9, width = 12)
  }

  # Logbook
  # dfilter <- expand.grid("Year" = 1989:max(ocpue$Year), "Season" = c("AW","SS"))
  # dfilter <- dfilter[-which(dfilter$Year==1989 & dfilter$Season == "AW"),]
  # dfilter <- dfilter[-which(dfilter$Year == max(ocpue$Year) & dfilter$Season == "SS"),]
  rcpue_lb <- rcpue %>% filter(CPUE_name == "Logbook") #right_join(dfilter) %>% mutate(Region = paste0("Region ", Region))

  if (nrow(rcpue_lb) > 0) {
    p <- ggplot(rcpue_lb) +
      geom_hline(yintercept = 0, alpha = 0.2) +
      # expand_limits(y = 0) +
      # scale_x_continuous(breaks = pretty(c(min(rcpue_lb$Year),max(rcpue_lb$Year)))) +
      xlab(xlab) + ylab("Standardised residual") +
      theme_lsd() +
      theme(legend.position = "none")
    if (n_iter > 10) {
      p <- p + geom_violin(aes(x = as.factor(Year), y = CPUE, colour = Season, fill = Season)) +
        scale_x_discrete(breaks = seq(0, 1e6, 5))
    } else {
      p <- p + geom_point(aes(x = Year, y = CPUE, color = Season)) +
        scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1))
    }
    if (length(unique(rcpue_lb$Region)) > 1) {
      p <- p + facet_wrap(Region ~ Season, scales = "free", ncol = data$n_area)
    } else {
      p <- p + facet_wrap( ~ Season, scales = "free", ncol = data$n_area)
    }
    ggsave(paste0(figure_dir, "cpue_resid_Logbook.png"), p, height = 9, width = 12)
  }

}


#' Plot offset year CPUE linear model
#'
#' Plot the offset year CPUE vs the mean of the AW CPUE during year y and SS CPUE during year y-1.
#'
#' @param object and LSD object
#' @param region the region to plot
#' @param figure_dir the directory to save to
#' @import ggplot2
#' @importFrom stats rnorm
#' @export
#'
plot_offset_cpue_lm <- function(object, region = 1, figure_dir = "figure/") {

  data <- object@data
  mcmc <- object@mcmc

  xx1 <- mcmc$mp_pred_offset_cpue_ry[1, region, 1:dim(mcmc$mp_pred_offset_cpue_ry)[3]] # y is n_offset_cpue
  yy1 <- data$data_offset_cpue_ry[region,]
  yr <- data$data_offset_cpue_year_i
  dd1 <- data.frame(yr = yr, x = xx1, y = yy1)
  xx2 <- log(seq(0, max(xx1) * 1.1, 0.0001))
  params <- mcmc$mp_offset_cpue_pars_ri[1, region,]

  dd2 <- data.frame(x = exp(xx2), y = exp(params[1] + xx2 * params[2]),
                    yerr = exp(params[1] + xx2 * params[2] + rnorm(length(xx2), 0, params[3])))

  p <- ggplot(dd1, aes(x = x, y = y)) +
    geom_point(data = dd2, aes(x = x, y = yerr), colour = "grey", alpha = 0.25) +
    geom_smooth(method = "lm", se = FALSE, colour = "green", size = 0.5) +
    geom_line(data = dd2, aes(x, y), colour = "red") +
    geom_text(aes(label = yr, colour = yr)) +
    expand_limits(x = 0, y = 0) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_lsd() +
    theme(legend.position = "none") +
    ggtitle(bquote(R^2 == .(round(params[5], 2)))) +
    labs(x = "Mean(AW CPUE[y], SS CPUE[y-1]) (kg/potlift)", y = "Offset year CPUE (kg/potlift)")

  ggsave(paste0(figure_dir, "lm_offset_cpue.png"), p, height = 9, width = 9)
}


#' Plot offset year CPUE linear model
#'
#' Plot the offset year CPUE vs the mean of the AW CPUE during year y and SS CPUE during year y-1.
#'
#' @param object and LSD object
#' @param region the region to plot
#' @param figure_dir the directory to save to
#' @import dplyr
#' @import ggplot2
#' @importFrom stats rnorm
#' @export
#'
plot_aw_cpue_lm <- function(object, region = 1, figure_dir = "figure/") {

  logit <- function() { log(x / (1 - x)) }
  inv_logit <- function(x) { 1 / (1 + exp(-x)) }

  data <- object@data
  mcmc <- object@mcmc

  yr <- data$data_aw_cpue_year_i
  yy1 <- data.frame(yr = yr, py = mcmc$mp_proportion_catch_aw_ry[1, region,])
  xx1 <- data.frame(yr = yr, px = mcmc$mp_pred_aw_cpue_ry[1, region,])
  dd1 <- inner_join(yy1, xx1)
  xx2 <- seq(0, max(xx1$px) * 1.1, 0.0001)
  params <- mcmc$mp_split_catch_pars_ri[1, region,]

  dd2 <- data.frame(x = xx2, y = inv_logit(params[1] + xx2 * params[2]),
                    yerr = inv_logit(params[1] + xx2 * params[2] + rnorm(length(xx2), 0, params[3])))

  p <- ggplot(data = dd1, aes(x = px, y = py)) +
    geom_point(data = dd2, aes(x, yerr), colour = "grey", alpha = 0.25) +
    geom_smooth(method = "lm", se = FALSE, colour = "green", size = 0.5) +
    geom_line(data = dd2, aes(x, y), colour = "red") +
    geom_text(aes(label = yr, colour = yr)) +
    expand_limits(x = 0, y = c(0, 1)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = "Standardised AW CPUE (kg/potlift)", y = "Proportion AW") +
    ggtitle(bquote(R^2 == .(round(params[5], 2)))) +
    theme_lsd() +
    theme(legend.position = "none")

  ggsave(paste0(figure_dir, "lm_aw_cpue.png"), p, height = 9, width = 9)
}
