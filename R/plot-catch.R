#' Plot catch
#'
#' Plot the catch data and fit to the data.
#'
#' @param object and LSD object
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param figure_dir the directory to save to
#' @param save_plot save the plot to file
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#'
plot_catch_rule <- function(object,
                            xlab = "Fishing year",
                            ylab = "Catch (tonnes)",
                            figure_dir = "figure/",
                            save_plot = TRUE) {

  data <- object@data
  mcmc <- object@mcmc

  n_iter <- nrow(mcmc[[1]])
  rules <- 1:data$n_rules
  regions <- 1:data$n_area
  seasons <- c("AW", "SS")
  pyears <- data$first_yr:data$last_proj_yr

  #psl <- mcmc$pred_catch_sl_jryt
  psl <- mcmc$proj_catch_commercial_jryt
  dimnames(psl) <- list("Iteration" = 1:n_iter,
                        "Rule" = rules,
                        "Region" = regions,
                        "Year" = pyears,
                        "Season" = seasons)
  psl <- melt(psl, value.name = "Catch") %>%
    mutate(Rule = factor(.data$Rule))

  psl %>% filter(Year > data$last_yr) %>%
    group_by(Iteration, Rule, Year) %>%
    summarise(Catch = sum(Catch)) %>%
    filter(Iteration == 4) %>%
    arrange(Rule) %>%
    data.frame()

  df <- psl %>%
    group_by(Iteration, Rule, Year) %>%
    summarise(Catch = sum(Catch))

  p2 <- ggplot(data = df, aes(x = .data$Year, y = .data$Catch, colour = .data$Rule, fill = .data$Rule)) +
    geom_vline(aes(xintercept = data$last_yr + 0.5), linetype = "dashed") +
    stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.5, colour = NA) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
    labs(x = xlab, y = ylab) +
    scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
    theme_lsd()

  if (save_plot) {
    ggsave(filename = paste0(figure_dir, "catch_rule.png"), plot = p2)
  } else {
    return(p2)
  }
}


#' Plot catch
#'
#' Plot the catch data and fit to the data.
#'
#' @param object and LSD object
#' @param show_proj show projection or not
#' @param scales free or fixed
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @import dplyr
#' @import ggplot2
#' @import ggrepel
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#'
plot_catch <- function(object,
                       show_proj = FALSE,
                       scales = "free",
                       xlab = "Fishing year",
                       ylab = "Catch (tonnes)") {

  data <- object@data
  mcmc <- object@mcmc

  YR <- "YR" # label for the season before the season change year
  seasons <- c("AW", "SS")
  regions <- 1:data$n_area
  n_iter <- nrow(mcmc[[1]])
  years <- data$first_yr:data$last_yr
  pyears <- data$first_yr:data$last_proj_yr
  rules <- 1:data$n_rules

  if (length(regions) == 1) regions2 <- regions
  if (length(regions) > 1) regions2 <- c(regions, "Total")

  # Fit to recreational catch
  pcatch <- mcmc$pred_catch_recreational_ryt
  dimnames(pcatch) <- list("Iteration" = 1:n_iter, Region = regions, Year = years, Season = seasons)
  d <- melt(pcatch, value.name = "Catch") %>%
    group_by(Iteration, Region, Year) %>%
    summarise(Catch = sum(Catch))

  # dcatch <- data.frame(Year = c(1994, 1996, 2011), Catch = c(95, 149, 42))
  # dcatch <- data.frame(Year = c(1994, 1996, 2011), Catch = c(142000, 223000, 60100))
  # p <- ggplot(d, aes(x = Year, y = Catch)) +
  #     stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
  #     stat_summary(fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
  #     stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
  #     geom_point(data = dcatch, colour = 'red') +
  #     expand_limits(y = 0) +
  #     theme_lsd()
  # ggsave(paste0(figure_dir, "recreational_catch.png"), p)

  # Catch
  comm <- data$data_catch_commercial_ryt
  dimnames(comm) <- list("Region" = regions, "Year" = years, "Season" = seasons)
  comm <- melt(comm, value.name = "Catch") %>% mutate(Sector = "Commercial")

  rec <- data$data_catch_recreational_ryt
  dimnames(rec) <- list("Region" = regions, "Year" = years, "Season" = seasons)
  rec <- melt(rec, value.name = "Catch") %>% mutate(Sector = "Recreational")

  dsl <- rbind(comm, rec) %>%
    mutate(Iteration = NA, Type = "SL", Data = "Observed")

  dnsl <- data$data_catch_nsl_ryt
  dimnames(dnsl) <- list("Region" = regions, "Year" = years, "Season" = seasons)
  dnsl <- melt(dnsl, value.name = "Catch") %>%
    mutate(Iteration = NA, Type = "NSL", Data = "Observed", Sector = "Customary + illegal")

  psl <- mcmc$pred_catch_sl_jryt
  dimnames(psl) <- list("Iteration" = 1:n_iter, "Rule" = rules, "Region" = regions, "Year" = pyears, "Season" = seasons)
  psl <- melt(psl, value.name = "Catch") %>%
    mutate(Type = "SL", Data = "Expected")

  pnsl <- mcmc$pred_catch_nsl_jryt
  dimnames(pnsl) <- list("Iteration" = 1:n_iter, "Rule" = rules, "Region" = regions, "Year" = pyears, "Season" = seasons)
  pnsl <- melt(pnsl, value.name = "Catch") %>%
    mutate(Type = "NSL", Data = "Expected")

  ph <- mcmc$pred_death_handling_jryt
  dimnames(ph) <- list("Iteration" = 1:n_iter, "Rule" = rules, "Region" = regions, "Year" = pyears, "Season" = seasons)
  ph <- melt(ph, value.name = "Catch") %>%
    mutate(Type = "Handling", Data = "Expected")
  phlist <- lapply(rules, function(x) {
    out <- ph %>% mutate(Rule = x)
    return(out)
  })
  ph <- do.call(rbind, phlist)

  # Observed catch
  dcatch <- rbind(dsl, dnsl) %>%
    filter(!(Year < data$season_change_yr & Season == "SS")) %>%
    mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR))

  # Predicted catch
  pcatch <- rbind(psl, pnsl, ph) %>%
    filter(!(Year < data$season_change_yr & Season == "SS")) %>%
    mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR))

  if (!show_proj) {
    pcatch <- filter(pcatch, Year <= data$last_yr)
  }

  # This sets up factor order in plot
  ord1 <- c("NSL", "SL", "Handling")
  ord2 <- c(YR, "AW", "SS")
  pcatch$Type <- factor(pcatch$Type, levels = ord1)
  pcatch$Season <- factor(pcatch$Season, levels = ord2)
  dcatch$Type <- factor(dcatch$Type, levels = ord1)
  dcatch$Season <- factor(dcatch$Season, levels = ord2)

  dcatch1 <- dcatch %>%
    group_by(Region, Year, Season, Iteration, Type, Data) %>%
    summarise(Catch = sum(Catch))
  pcatch1 <- pcatch %>%
    group_by(Region, Year, Season, Type, Data) %>%
    summarise(Catch = median(Catch))

  p1 <- ggplot(data = pcatch, aes(x = Year, y = Catch))

  if (show_proj) p1 <- p1 + geom_vline(aes(xintercept = data$last_yr), linetype = "dashed")

  p1 <- p1 +
    stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
    geom_point(data = dcatch1, color = "red") +
    geom_point(data = filter(pcatch1, Year > data$last_yr, Type != "Handling"), color = "blue") +
    labs(x = xlab, y = ylab) +
    scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
    theme_lsd()

  if (data$n_area > 1) {
    p1 <- p1 + facet_grid(Region + Type ~ Season, scales = "free")
  } else {
    p1 <- p1 + facet_grid(Type ~ Season, scales = "free")
  }

  # Plot of catch summed over seasons and drop handling mortality
  dcatch_sum <- dcatch %>%
    group_by(Iteration, Region, Year, Type, Data) %>%
    summarise(Catch = sum(Catch)) %>%
    filter(Type != "Handling")
  pcatch_sum <- pcatch %>%
    group_by(Rule, Iteration, Region, Year, Type, Data) %>%
    summarise(Catch = sum(Catch)) %>%
    filter(Type != "Handling")
  pcatch_sum$Type <- factor(pcatch_sum$Type, levels = c("SL", "NSL"))
  pcatch_sum1 <- pcatch_sum %>%
    group_by(Region, Year, Type, Data) %>%
    summarise(Catch = median(Catch))

  p2 <- ggplot(data = pcatch_sum, aes(x = .data$Year, y = .data$Catch))

  if (show_proj) p2 <- p2 + geom_vline(aes(xintercept = data$last_yr), linetype = "dashed")

  p2 <- p2 +
    stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
    geom_point(data = dcatch_sum, aes(x = Year, y = Catch), color = "red") +
    geom_point(data = filter(pcatch_sum1, Year > data$last_yr, Type != "Handling"), color = "blue") +
    xlab(xlab) + ylab(ylab) +
    scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
    theme_lsd()

  if (data$n_area > 1) {
    p2 <- p2 + facet_grid(Region ~ Type, scales = scales)
  } else {
    p2 <- p2 + facet_grid(Type ~ ., scales = scales)
  }

  # Catch residuals
  rsl <- mcmc$resid_catch_sl_jryt
  dimnames(rsl) <- list("Iteration" = 1:n_iter, "Rule" = rules, "Region" = regions, "Year" = pyears, "Season" = seasons)
  rsl <- melt(rsl, value.name = "Catch") %>%
    mutate(Type = "SL", Data = "Residual")
  rnsl <- mcmc$resid_catch_nsl_jryt
  dimnames(rnsl) <- list("Iteration" = 1:n_iter, "Rule" = rules, "Region" = regions, "Year" = pyears, "Season" = seasons)
  rnsl <- melt(rnsl, value.name = "Catch") %>%
    mutate(Type = "NSL", Data = "Residual")

  rcatch <- rbind(rsl, rnsl)
  rcatch$Type <- factor(rcatch$Type, levels = c("SL", "NSL"))

  p3 <- ggplot(data = rcatch, aes(x = Year, y = Catch)) +
    stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    stat_summary(fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
    xlab(xlab) + ylab("Residual") +
    theme_lsd()

  p3 <- p3 + facet_grid(Region + Type ~ Season, scales = "free")

  # Catch area
  msy <- mcmc$MSY_r
  dimnames(msy) <- list(Iteration = 1:n_iter, "Region" = regions2)
  msy <- melt(msy) %>% rename("MSY" = value) %>% mutate(Region = factor(Region))

  dcatch2 <- dcatch %>%
    group_by(Region, Year, Sector, Iteration) %>%
    summarise(Catch = sum(Catch)) %>%
    mutate(Region = factor(Region), Iteration = 1) %>%
    left_join(msy) %>%
    mutate(Label = "") %>%
    mutate(Label = ifelse(Iteration == 1 & Year == max(years) & Sector == "Commercial", "MSY", ""))

  p4 <- ggplot(dcatch2) +
    geom_area(data = dcatch2, aes(x = Year, y = Catch, colour = Sector, fill = Sector), position = "stack") +
    xlab(xlab) + ylab(ylab) +
    theme_lsd() +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1)))

  # add msy
  p4 <- p4 +
    stat_summary(aes(x = Year, y = MSY), fun.min = function(x) quantile(x, 0.025), fun.max = function(x) quantile(x, 0.975), geom = "ribbon", alpha = 0.125, colour = "black", fill = "black" ) +
    stat_summary(aes(x = Year, y = MSY), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
    geom_label_repel(data = dcatch2, aes(x = Year, y = MSY, label = Label), fill = "black", size = 5, color = 'white', force = 10, segment.color = '#bbbbbb', min.segment.length = unit(0, "lines"))

  p4 <- p4 + facet_grid(Region ~ ., scales = "free")

  return(list(p1, p2, p3, p4))
}


#' Save catch plots
#'
#' @param object and LSD object
#' @param figure_dir the directory to save to
#' @export
#'
plot_catch_save <- function(object, figure_dir = "figure/") {
  p <- plot_catch(object, show_proj = FALSE)
  ggsave(filename = paste0(figure_dir, "catch.png"), plot = p[[1]], width = 10, height = 8)
  ggsave(filename = paste0(figure_dir, "catch_sums.png"), plot = p[[2]])
  ggsave(filename = paste0(figure_dir, "catch_resid.png"), plot = p[[3]])
  ggsave(filename = paste0(figure_dir, "catch_type.png"), plot = p[[4]], width = 10)

  p <- plot_catch(object, show_proj = TRUE)
  ggsave(filename = paste0(figure_dir, "catch_v2.png"), plot = p[[1]], width = 10, height = 8)
  ggsave(filename = paste0(figure_dir, "catch_sums_v2.png"), plot = p[[2]])
}
