
plot_mp_biomass <- function(object,
                            figure_dir = "figure/") {

  object <- readRDS("lsd.rds")

  mcmc <- object@mcmc
  data <- object@data

  mcmc$mp_biomass_jry[1,,1,]
  mcmc$mp_tacc_jry[1,,1,]
  data$mp_rule_parameters[1,]

  n_rules <- data$n_rules
  n_iter <- nrow(mcmc[[1]])
  regions <- 1:data$n_area

  mp_biomass <- mcmc$mp_biomass_jry
  dimnames(mp_biomass) <- list(Iteration = 1:n_iter, Rules = 1:n_rules, Region = regions, Year = data$first_yr:data$last_proj_yr)
  mp_biomass <- melt(mp_biomass, value.name = "mp_input") %>%
    filter(Year %in% data$first_proj_yr:data$last_proj_yr) %>%
    mutate(Rules = factor(Rules))

  mp_tacc <- mcmc$mp_tacc_jry
  dimnames(mp_tacc) <- list(Iteration = 1:n_iter, Rules = 1:n_rules, Region = regions, Year = data$first_yr:data$last_proj_yr)
  mp_tacc <- melt(mp_tacc, value.name = "mp_output") %>%
    filter(Year %in% data$first_proj_yr:data$last_proj_yr) %>%
    mutate(Rules = factor(Rules))

  mp_output <- full_join(mp_biomass, mp_tacc, by = c("Iteration", "Rules", "Region", "Year"))

  head(mp_output)

  p1 <- ggplot(data = mp_output, aes(x = Year, y = mp_input)) +
    stat_summary(aes(fill = Rules), fun.min = function(x) quantile(x, 0.025), fun.max = function(x) quantile(x, 0.975), geom = "ribbon", alpha = 0.125) +
    stat_summary(aes(colour = Rules), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
    labs(x = "Fishing year", y = "MP biomass (tonnes)", colour = NULL, fill = NULL) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    theme_lsd()

  p2 <- ggplot(data = mp_output, aes(x = Year, y = mp_output)) +
    stat_summary(aes(fill = Rules), fun.min = function(x) quantile(x, 0.025), fun.max = function(x) quantile(x, 0.975), geom = "ribbon", alpha = 0.125) +
    stat_summary(aes(colour = Rules), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
    labs(x = "Fishing year", y = "TAC (tonnes)", colour = NULL, fill = NULL) +
    # scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1), expand = c(0, 1)) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    theme_lsd()

  p3 <- ggplot(data = mp_output, aes(x = mp_input, y = mp_output)) +
    geom_point(aes(colour = Rules)) +
    # stat_summary(aes(fill = Rules), fun.min = function(x) quantile(x, 0.025), fun.max = function(x) quantile(x, 0.975), geom = "ribbon", alpha = 0.125) +
    # stat_summary(aes(colour = Rules), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
    labs(x = "MP biomass (tonnes)", y = "TAC (tonnes)", colour = "Rule") +
    # scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    theme_lsd()

  p3

  p1 + p2 + p3
}



#' @title Plot control rules
#'
#' @param mp.pars a matrix of control parameters with columns as parameters and rows as different rules
#' @param cpue.values the current CPUE
#' @param tacc.values the current TACC
#' @param cpue a vector of the catch per unit effort (CPUE) to be plotted as the x axis
#' @import ggplot2
#' @export
#'
plot_mp_rules <- function(mp.pars,
                          cpue.values = numeric(),
                          tacc.values = numeric(),
                          cpue = seq(0, 2, 0.01)) {
  # par1  rule type
  # par2  CPUE at TACC = 0
  # par3  CPUE at plateau left
  # par4  CPUE at plateau right
  # par5  plateau height
  # par6  slope
  # par6  step width
  # par7  step height
  # par8  minimum change
  # par9  maximum change
  # par10 latent year switch

  invisible(apply(structure(1:dim(mp.pars)[1], dim = dim(mp.pars)[1]), 1, function(i) {
    if (mp.pars[i,2] >= mp.pars[i, 3])
      stop(paste("par2 < par3 condition not met for rule", i))
    if (mp.pars[i,3] > mp.pars[i, 4])
      stop(paste("par3 <= par4 condition not met for rule", i))
  }))

  outs <- apply(mp.pars, 1, FUN = TACC, cpue = cpue)
  rules <- melt(outs, varnames = c("cpue", "rule"), value.name = "TACC")
  rules$cpue <- rep(cpue, nrow(mp.pars))

  # If just one rule is being plotted then do that and plot the obs.cpue on the plot too
  if (nrow(mp.pars) == 1) {
    d <- data.frame(cpue = cpue.values, tacc = tacc.values)
    p <- ggplot(rules, aes(x = cpue, y = TACC)) +
      geom_line(size = 2) +
      geom_point(data = d, aes(x = cpue, y = tacc), size = 5) +
      theme_bw(base_size = 20) +
      labs(x = "CPUE (kg/potlift)", y = "TACC (tonnes)", col = "Rule")
  } else {
    p <- ggplot(rules, aes(x = cpue, y = TACC, group = rule, col = rule)) +
      geom_line(size = 2) +
      theme_bw(base_size = 20) +
      labs(x = "CPUE (kg/potlift)", y = "TACC (tonnes)", col = "Rule")
  }

  p
}
