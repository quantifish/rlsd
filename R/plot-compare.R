#' Compare LFs from multiple models
#'
#' @param object_list list of 'lsd.rds' files from multiple models
#' @param object_names vector of model names associated with each of the output files in object_list
#' @param yrs the years to compare
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom grDevices colorRampPalette gray
#' @importFrom stats quantile
#' @export
#'
plot_compare_lfs <- function(object_list,
                             object_names,
                             yrs = 2018:2019,
                             xlab = "Midpoint of size-class (mm)",
                             ylab = "Proportion at size (mm)")
{
  sex <- c("Male", "Immature female", "Mature female")
  seasons <- c("AW", "SS")
  #sources <- c("LB", "CS")
  nmod <- length(object_list)

  elf <- NULL
  plf <- NULL
  dlf <- NULL

  for (i in 1:nmod) {
    object <- object_list[[i]]
    data <- object@data
    mcmc <- object@mcmc

    n_iter <- nrow(mcmc[[1]])
    sex <- c("Male", "Immature female", "Mature female")
    seasons <- c("AW", "SS")
    bins <- data$size_midpoint_l
    regions <- 1:data$n_area
    
    w <- data.frame(LF = 1:data$n_lf,
                    Year = data$data_lf_year_i, Season = data$data_lf_season_i,
                    Region = data$data_lf_area_i, Sex = data$data_lf_sex_i)
    
    # 1. Minimum legal size by region, year and sex. These get plotted as vertical lines on each panel.
    # 2. Bin limits
    # 3. Effective N
    # 4. Weights
    mls <- data$cov_mls_ytrs
    dimnames(mls) <- list("Year" = data$first_yr:data$last_proj_yr, "Season" = seasons, "Region" = regions, "Sex" = sex)
    mls <- melt(mls, id.var = "Year", variable.name = "Sex", value.name = "MLS")
    
    lim <- data$data_lf_bin_limits_i
    colnames(lim) <- c("Min", "Max")
    lim <- data.frame(lim) %>%
      mutate(LF = 1:data$n_lf) %>%
      mutate(lower = bins[Min], upper = bins[Max]) %>%
      dplyr::select("LF", "lower", "upper")
    
    rawW <- 1 / mcmc$sigma_lf_i
    dimnames(rawW) <- list("Iteration" = 1:n_iter, "LF" = 1:data$n_lf)
    rawW <- melt(rawW, value.name = "rawW") %>%
      filter(Iteration == 1)
    
    elf2 <- rawW %>%
      left_join(w, by = "LF") %>%
      mutate(Season = seasons[Season], Sex = sex[Sex], Model = object_names[i]) %>%
      left_join(mls, by = c("Region", "Year", "Season", "Sex")) %>%
      left_join(lim, by = c("LF")) %>%
      dplyr::select(-Iteration) %>%
      group_by(Year, Season, Region, Sex, MLS, lower, upper) %>%
      mutate(rawW2 = max(rawW))
    elf <- rbind(elf, elf2)
    
    # Observed LF
    dlf1 <- mcmc$data_lf_obs2_il
    dimnames(dlf1) <- list("Iteration" = 1:n_iter, "LF" = 1:data$n_lf, "Size" = bins)
    dlf1 <- melt(dlf1) %>%
      left_join(w, by = "LF") %>%
      filter(Iteration == 1, value >= 0) %>%
      dplyr::select(-Iteration) %>%
      left_join(lim, by = c("LF")) %>%
      mutate(Season = seasons[Season], Sex = sex[Sex], Model = object_names[i])
    dlf <- rbind(dlf, dlf1)
    
    # Predicted LF
    plf1 <- mcmc$pred_lf_il
    dimnames(plf1) <- list("Iteration" = 1:n_iter, "LF" = 1:data$n_lf, "Size" = bins)
    plf <- melt(plf1) %>%
      left_join(w, by = "LF") %>%
      left_join(lim, by = c("LF")) %>%
      mutate(Season = seasons[Season], Sex = sex[Sex], Model = object_names[i])
    plf <- rbind(plf, plf1)
  }

  p <- ggplot() +
    geom_vline(data = elf %>% filter(Year %in% yrs), aes(xintercept = MLS), linetype = "dashed") +
    # geom_label(data = elf %>% filter(Year %in% yrs), aes(x = Inf, y = Inf, label = paste(rawN, "\n", effN)), hjust = 1, vjust = 1) +
    # stat_summary(data = plf %>% filter(Year %in% yrs, Size >= lower & Size <= upper),
    #              aes(x = as.numeric(as.character(Size)), y = value), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    # stat_summary(data = plf %>% filter(Year %in% yrs, Size >= lower & Size <= upper),
    #              aes(x = as.numeric(as.character(Size)), y = value), fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
    stat_summary(data = plf %>% filter(Year %in% yrs, Size >= lower & Size <= upper),
                 aes(x = as.numeric(as.character(Size)), y = value, colour = Model), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
    geom_point(data = dlf %>% filter(Year %in% yrs, Size >= lower & Size <= upper),
               aes(x = as.numeric(as.character(Size)), y = value, colour = Model, shape = Model)) +
    labs(x = xlab, y = ylab) +
    # guides(shape = "none", colour = "none") +
    # scale_shape_manual(values = c(0, 4)) +
    # scale_colour_manual(values = rev(ggplotColours(n = length(sources)))) +
    # scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
    scale_x_continuous(minor_breaks = seq(0, 1e6, 2), limits = c(min(elf2$lower), max(elf2$upper))) +
    theme_lsd()

    if (nmod > 6) {
      p <- p +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else{
      p <- p +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }

  if (data$n_area == 1) {
    p <- p + facet_grid(Year + Season ~ Sex, scales = "free_y")
  } else {
    p <- p + facet_grid(Region + Year + Season ~ Sex, scales = "free_y")
  }

  return(p)
}

#' Compare OLD LFs from multiple models
#'
#' @param object_list list of 'lsd.rds' files from multiple models
#' @param object_names vector of model names associated with each of the output files in object_list
#' @param yrs the years to compare
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom grDevices colorRampPalette gray
#' @importFrom stats quantile
#' @export
#'
plot_compare_lfs_old <- function(object_list,
                             object_names,
                             yrs = 2018:2019,
                             xlab = "Midpoint of size-class (mm)",
                             ylab = "Proportion at size (mm)")
{
  sex <- c("Male", "Immature female", "Mature female")
  seasons <- c("AW", "SS")
  sources <- c("LB", "CS")
  nmod <- length(object_list)
  
  elf <- NULL
  plf <- NULL
  dlf <- NULL
  
  for (i in 1:nmod) {
    object <- object_list[[i]]
    data <- object@data
    mcmc <- object@mcmc
    
    n_iter <- nrow(mcmc[[1]])
    bins <- data$size_midpoint_l
    regions <- 1:data$n_area
    
    w <- data.frame(LF = 1:data$n_lf,
                    Year = data$data_lf_year_i, Season = data$data_lf_season_i,
                    Source = data$data_lf_source_i, Region = data$data_lf_area_i)
    
    # 1. Minimum legal size by region, year and sex. These get plotted as vertical lines on each panel.
    # 2. Bin limits
    # 3. Effective N
    # 4. Weights
    mls <- data$cov_mls_ytrs
    dimnames(mls) <- list("Year" = data$first_yr:data$last_proj_yr, "Season" = seasons, "Region" = regions, "Sex" = sex)
    mls <- reshape2::melt(mls, id.var = "Year", variable.name = "Sex", value.name = "MLS")
    
    lim <- array(NA, dim = c(length(regions), length(sex), 2))
    lim[,,] <- data$data_lf_bin_limits_rsi
    dimnames(lim) <- list("Region" = regions, "Sex" = sex, "Limit" = c("lower", "upper"))
    lim <- reshape2::melt(lim, variable.name = "Sex") %>%
      mutate(value = bins[value]) %>%
      tidyr::spread(Limit, value)
    
    rawN <- data$data_lf_N_is
    dimnames(rawN) <- list("LF" = 1:data$n_lf, "Sex" = sex)
    rawN <- reshape2::melt(rawN, value.name = "rawN") %>% mutate(Iteration = 1)
    
    rawW <- data$data_lf_weight_is
    dimnames(rawW) <- list("LF" = 1:data$n_lf, "Sex" = sex)
    rawW <- reshape2::melt(rawW, value.name = "rawW") %>% mutate(Iteration = 1)
    
    effN <- mcmc$pred_lf_effN_is
    dimnames(effN) <- list("Iteration" = 1:n_iter, "LF" = 1:data$n_lf, "Sex" = sex)
    effN <- reshape2::melt(effN, value.name = "effN")
    
    elf1 <- left_join(rawN, effN) %>%
      left_join(w, by = "LF") %>%
      mutate(Source = sources[Source], Season = factor(seasons[Season]), Model = object_names[i]) %>%
      left_join(mls, by = c("Region", "Year", "Season", "Sex")) %>%
      left_join(lim, by = c("Region", "Sex")) %>%
      dplyr::select(-Iteration) %>%
      mutate(effN = paste0("n: ", sprintf("%.2f", effN))) %>%
      mutate(rawN = paste0("N: ", sprintf("%.0f", rawN)))
    elf <- rbind(elf, elf1)
    
    # Observed LF
    dlf1 <- mcmc$data_lf_out_isl
    dimnames(dlf1) <- list("Iteration" = 1:n_iter, "LF" = 1:data$n_lf, "Sex" = sex, "Bin" = 1:length(bins))
    dlf1 <- reshape2::melt(dlf1) %>%
      left_join(w, by = "LF") %>%
      mutate(Source = factor(sources[Source]), Model = object_names[i]) %>%
      mutate(Season = seasons[Season], Size = bins[Bin]) %>%
      filter(Iteration == 1, value >= 0) %>%
      dplyr::select(-Iteration) %>%
      left_join(lim, by = c("Sex", "Region"))
    dlf <- rbind(dlf, dlf1)
    
    # Predicted LF
    plf1 <- mcmc$pred_lf_isl
    dimnames(plf1) <- list("Iteration" = 1:n_iter, "LF" = 1:data$n_lf, "Sex" = sex, "Size" = bins)
    plf1 <- reshape2::melt(plf1) %>%
      left_join(w, by = "LF") %>%
      mutate(Source = sources[Source], Season = seasons[Season], Model = object_names[i]) %>%
      left_join(lim, by = c("Sex", "Region"))
    plf <- rbind(plf, plf1)
  }
  
  p <- ggplot() +
    geom_vline(data = elf %>% filter(Year %in% yrs), aes(xintercept = MLS), linetype = "dashed") +
    # geom_label(data = elf %>% filter(Year %in% yrs), aes(x = Inf, y = Inf, label = paste(rawN, "\n", effN)), hjust = 1, vjust = 1) +
    # stat_summary(data = plf %>% filter(Year %in% yrs, Size >= lower & Size <= upper),
    #              aes(x = as.numeric(as.character(Size)), y = value), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    # stat_summary(data = plf %>% filter(Year %in% yrs, Size >= lower & Size <= upper),
    #              aes(x = as.numeric(as.character(Size)), y = value), fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
    stat_summary(data = plf %>% filter(Year %in% yrs, Size >= lower & Size <= upper),
                 aes(x = as.numeric(as.character(Size)), y = value, colour = Model), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
    geom_point(data = dlf %>% filter(Year %in% yrs, Size >= lower & Size <= upper),
               aes(x = as.numeric(as.character(Size)), y = value, colour = Model, shape = Model)) +
    labs(x = xlab, y = ylab) +
    # guides(shape = "none", colour = "none") +
    # scale_shape_manual(values = c(0, 4)) +
    # scale_colour_manual(values = rev(ggplotColours(n = length(sources)))) +
    # scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
    scale_x_continuous(minor_breaks = seq(0, 1e6, 2), limits = c(min(elf$lower), max(elf$upper))) +
    theme_lsd()
  
  if (nmod > 6) {
    p <- p +
      scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
      scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
  } else{
    p <- p +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
  }
  
  if (data$n_area == 1) {
    p <- p + facet_grid(Year + Season + Source ~ Sex, scales = "free_y")
  } else {
    p <- p + facet_grid(Region + Year + Season + Source ~ Sex, scales = "free_y")
  }
  
  return(p)
}

#' Compare spawning biomass from multiple models
#'
#' @param object_list list of 'lsd.rds' files from multiple models
#' @param object_names vector of model names associated with each of the output files in object_list
#' @param figure_dir the directory to save to
#' @param save_plot to save the plot to file or not
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom grDevices colorRampPalette gray
#' @importFrom stats quantile
#' @export
#' @param show_proj for rapid updates plots to show projections only for full assessment or for all model comparisons
#'
plot_compare_ssb <- function(object_list,
                             object_names,
                             figure_dir = "compare_figure/",
                             save_plot = TRUE,
                             show_proj = TRUE) {

  data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
  mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)
  years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
  pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_proj_yr)
  regions_list <- lapply(1:length(object_list), function(x) 1:data_list[[x]]$n_area)
  regions_list2 <- lapply(1:length(object_list), function(x) {
    if (length(regions_list[[x]]) == 1) out <- regions_list[[x]]
    if (length(regions_list[[x]]) > 1) out <- c(regions_list[[x]], "Total")
    return(out)
  })

  sb_list <- lapply(1:length(object_list), function(x) {
    n_iter <- nrow(mcmc_list[[x]][[1]])
    bio <- mcmc_list[[x]]$biomass_ssb_jyr
    dimnames(bio) <- list("Iteration" = 1:n_iter, "Rule" = 1:dim(bio)[2], "Year" = pyears_list[[x]], "Region" = regions_list[[x]])
    bio2 <- melt(bio) %>%
      group_by(Iteration, Year, Rule, Region) %>%
      summarise(value = sum(value)) %>%
      mutate(YearType = ifelse(Year %in% years_list[[x]], "Assessment", "Projection")) %>%
      mutate(YearType = ifelse(Year == max(years_list[[x]])+1, "FirstProjYear", YearType))
    bio2$Model <- object_names[x]
    return(bio2)
  })
  ssb <- data.frame(do.call(rbind, sb_list)) %>%
    mutate(Model = factor(Model), type = "SSB")

  ssb0_list <- lapply(1:length(object_list), function(x) {
    n_iter <- nrow(mcmc_list[[x]][[1]])
    bio <- mcmc_list[[x]]$SSB0_r
    dimnames(bio) <- list("Iteration" = 1:n_iter, "Region" = regions_list2[[x]])
    hl <- melt(bio) %>%
      left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
      filter(Region != "Total") %>%
      group_by(Iteration, Year, Region) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(Rule = 1, type = "Hard limit", value = value * 0.1) %>%
          mutate(YearType = ifelse(Year %in% years_list[[x]], "Assessment", "Projection")) %>%
      mutate(YearType = ifelse(Year == max(years_list[[x]])+1, "FirstProjYear", YearType))
    sl <- melt(bio) %>%
      left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
      filter(Region != "Total") %>%
      group_by(Iteration, Year, Region) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(Rule = 1, type = "Soft limit", value = value * 0.2) %>%
          mutate(YearType = ifelse(Year %in% years_list[[x]], "Assessment", "Projection")) %>%
      mutate(YearType = ifelse(Year == max(years_list[[x]])+1, "FirstProjYear", YearType))
    ssb0 <- melt(bio) %>%
      left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
      filter(Region != "Total") %>%
      group_by(Iteration, Year, Region) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(Rule = 1, type = "SSB0") %>%
        mutate(YearType = ifelse(Year %in% years_list[[x]], "Assessment", "Projection")) %>%
      mutate(YearType = ifelse(Year == max(years_list[[x]])+1, "FirstProjYear", YearType))
    bio2 <- rbind(ssb0, sl, hl)
    bio2$Model <- object_names[x]
    return(bio2)
  })
  ssb0 <- data.frame(do.call(rbind, ssb0_list))

  labs <- ssb0 %>%
    filter(Year == min(Year)) %>%
    dplyr::select(-Year) %>%
    group_by(Region, type) %>%
    summarise(value = median(value))

  # labs2 <- ssb0 %>%
  #   filter(Year == min(Year))  %>%
  #   group_by(Region, type) %>%
  #   summarise(value = mean(value))
  # labs2$Year <- rep(max(ssb$Year), nrow(labs2))


  nmod <- length(unique(ssb$Model))
  years <- unique(unlist(years_list))
  pyears <- unique(unlist(pyears_list))

  mods <- unique(ssb$Model)
  # mod_num <- sapply(1:length(mods), function(m) as.numeric(strsplit(as.character(mods[m]), "_")[[1]][1]))
  ssb$Model <- factor(ssb$Model, levels = unique(mods)) #[order(mod_num)])
  ssb0$Model <- factor(ssb0$Model, levels = unique(mods)) #[order(mod_num)])

  if(save_plot){
  p1 <- ggplot(ssb %>% filter(YearType =="Assessment") %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), aes(x = Year, y = value)) +
    #stat_summary(data = ssb0 %>% filter(Year %in% years) %>% filter(type == "Soft limit") %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
    #stat_summary(data = ssb0 %>% filter(Year %in% years) %>% filter(type == "Soft limit") %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
    #stat_summary(data = ssb0 %>% filter(Year %in% years) %>% filter(type == "Hard limit") %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
    #stat_summary(data = ssb0 %>% filter(Year %in% years) %>% filter(type == "Hard limit") %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
    #stat_summary(data = ssb0 %>% filter(Year %in% years) %>% filter(type == "SSB0") %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
    #stat_summary(data = ssb0 %>% filter(Year %in% years) %>% filter(type == "SSB0") %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
    stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75, aes(color = Model)) +
    #geom_label(data = labs, aes(x = Year, y = value, label = type), nudge_x = -5) +
    labs(x = "Fishing year", y = "Spawning stock biomass (tonnes)") +
    scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1), expand = expansion(mult = c(0,0.01))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
    theme_lsd(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
    coord_cartesian(clip = "off")

  if (nmod > 6) {
    p1 <- p1 +
      scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
      scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
  } else{
    p1 <- p1 +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
  }

  by.Region <- NA
  for (i in 1:length(data_list)) {
    by.Region[i] <- data_list[[i]]$n_area > 1
  }

  if (sum(by.Region) >= 1) {
    q1 <- ggplot(ssb %>% filter(YearType == "Assessment"), aes(x = Year, y = value)) +
      #stat_summary(data = ssb0 %>% filter(Year %in% years) %>% filter(type == "Soft limit"), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
      #stat_summary(data = ssb0 %>% filter(Year %in% years) %>% filter(type == "Soft limit"), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
      #stat_summary(data = ssb0 %>% filter(Year %in% years) %>% filter(type == "Hard limit"), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
      #stat_summary(data = ssb0 %>% filter(Year %in% years) %>% filter(type == "Hard limit"), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
      #stat_summary(data = ssb0 %>% filter(Year %in% years) %>% filter(type == "SSB0"), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
      #stat_summary(data = ssb0 %>% filter(Year %in% years) %>% filter(type == "SSB0"), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
      stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
      stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
      stat_summary(fun = function(x) quantile(x, 0.5), geom = "point", lwd = 1.5, alpha = 0.75, aes(color = Model)) +
      labs(x = "Fishing year", y = "Spawning stock biomass (tonnes)") +
      # scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
      scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1), expand = expansion(mult = c(0,0.01))) +
      theme_lsd(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      facet_wrap(~ Region) +
      #geom_label(data = labs2, mapping = aes(x = Year, y = value, label = type), nudge_x = -5) +
      coord_cartesian(clip = "off")

    if (nmod > 6) {
      q1 <- q1 +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else{
      q1 <- q1 +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }
  }


    ggsave(paste0(figure_dir, "biomass_ssb_compare.png"), p1, width = 10)
    if (sum(by.Region) >= 1) {
      ggsave(paste0(figure_dir, "biomass_ssb_compare_byRegion.png"), q1, width = 10)
    }


  p1 <- ggplot(ssb %>% group_by(Iteration, Year, Model, Region, YearType) %>% summarise(value = sum(value)), aes(x = Year, y = value)) +
    geom_vline(data = ssb %>% filter(YearType == "FirstProjYear"), aes(xintercept = Year), linetype = 2) +
    #stat_summary(data = ssb0 %>% filter(type == "Soft limit") %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
    #stat_summary(data = ssb0 %>% filter(type == "Soft limit") %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
    #stat_summary(data = ssb0 %>% filter(type == "Hard limit") %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
    #stat_summary(data = ssb0 %>% filter(type == "Hard limit") %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
    #stat_summary(data = ssb0 %>% filter(type == "SSB0") %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
    #stat_summary(data = ssb0 %>% filter(type == "SSB0") %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
    stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) + #, linetype = Model)) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75, aes(color = Model)) +
    #geom_label(data = labs, aes(x = Year, y = value, label = type), nudge_x = -5) +
    labs(x = "Fishing year", y = "Spawning stock biomass (tonnes)") +
    scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1), expand = expansion(mult = c(0,0.01))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
    theme_lsd(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  if (nmod > 6) {
    p1 <- p1 +
      scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
      scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
  } else{
    p1 <- p1 +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
  }

  if (sum(by.Region) >= 1) {
    q1 <- ggplot(ssb, aes(x = Year, y = value)) +
      geom_vline(data = ssb %>% filter(YearType == "FirstProjYear"), aes(xintercept = Year), linetype = 2) +
      #stat_summary(data = ssb0 %>% filter(type == "Soft limit"), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
      #stat_summary(data = ssb0 %>% filter(type == "Soft limit"), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
      #stat_summary(data = ssb0 %>% filter(type == "Hard limit"), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
      #stat_summary(data = ssb0 %>% filter(type == "Hard limit"), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
      #stat_summary(data = ssb0 %>% filter(type == "SSB0"), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
      #stat_summary(data = ssb0 %>% filter(type == "SSB0"), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
      stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
      stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
      stat_summary(fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75, aes(color = Model)) +
      labs(x = "Fishing year", y = "Spawning stock biomass (tonnes)") +
      scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1), expand = expansion(mult = c(0,0.01))) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
      theme_lsd(base_size = 14) +
      facet_wrap(~Region) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      #geom_label(data = labs2, mapping = aes(x = Year, y = value, label = type), nudge_x = -5) +
      coord_cartesian(clip = "off")

    if (nmod > 6) {
      q1 <- q1 +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else{
      q1 <- q1 +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }
  }


    ggsave(paste0(figure_dir, "biomass_ssb_compare_v2.png"), p1, width = 10)
    if (sum(by.Region) >= 1) {
      ggsave(paste0(figure_dir, "biomass_ssb_compare_byRegion_v2.png"), q1, width = 10)
    }


  relssb <- bind_rows(ssb, ssb0 %>% filter(type == "SSB0")) %>%
    tidyr::spread(type, value) %>%
    group_by(Iteration, Year, Rule, Model, YearType ) %>%
    summarise(SSB = sum(SSB), SSB0 = sum(SSB0)) %>%
    mutate(RelSSB = SSB/SSB0)

  labs_rel <- data.frame(type = c("Hard limit", "Soft limit", "SSB0"), value = c(0.1, 0.2, 1))

  nmod <- length(unique(relssb$Model))

  relssb_next <- relssb %>% filter(YearType == "FirstProjYear")

  p <- ggplot(relssb_next) +
    theme_lsd(base_size = 14) +
    theme(axis.text.x = element_blank()) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    geom_hline(aes(yintercept = 0.2), col = "gray") +
    geom_hline(aes(yintercept = 0.1), col = "gray") +
    geom_text(data = labs_rel %>% filter(type != "SSB0"), aes(x = "base", y = value, label = type)) +
    xlab("Model") + ylab("Terminal year relative spawning biomass")

  if (nmod > 6) {
    p <- p +
      scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
      scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
  } else {
    p <- p +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
  }

  if (max(relssb_next$Iteration) == 1) {
    p <- p + geom_point(aes(x = Model, y = RelSSB, fill = Model), cex = 4, pch = 21)
    if (any(relssb_next$Model == "base")) p <- p + geom_hline(data = relssb_next %>% filter(Model == "base"), aes(yintercept = unique(RelSSB)), linetype = 2)
  } else {
    p <- p + geom_violin(aes(x = Model, y = RelSSB, fill = Model))
    if (any(relssb_next$Model == "base")) p <- p + geom_hline(data = relssb_next %>% filter(Model == "base"), aes(yintercept = median(RelSSB)), linetype = 2)
  }


    ggsave(paste0(figure_dir, "relssb_nextyear_compare.png"), p, width = 10)
  

  if (sum(by.Region) >= 1) {
    relssb_r <- rbind(ssb, ssb0 %>% filter(type == "SSB0")) %>%
      tidyr::spread(type, value) %>%
      mutate(RelSSB = SSB/SSB0)

    labs_rel_r <- labs %>%
      mutate(value = ifelse(type == "Hard limit", 0.1, ifelse(type == "Soft limit", 0.2, ifelse(type == "SSB0", 1, NA)))) %>%
      filter(type != "SSB0")

    nmod <- length(unique(relssb_r$Model))

    relssb_next_r <- relssb_r %>% filter(YearType == "FirstProjYear")

    q <- ggplot(relssb_next_r) +
      theme_lsd(base_size = 14) +
      theme(axis.text.x = element_blank()) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
      geom_hline(aes(yintercept = 0.2), col = "gray") +
      geom_hline(aes(yintercept = 0.1), col = "gray") +
      geom_text(data = labs_rel_r, aes(x = "base", y = value, label = type)) +
      xlab("Model") +
      ylab("Terminal year relative spawning biomass") +
      facet_wrap(~Region)

    if (nmod > 6) {
      q <- q +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else {
      q <- q +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }

    if (max(relssb_next$Iteration) == 1) {
      q <- q + geom_point(aes(x = Model, y = RelSSB, fill = Model), cex = 4, pch = 21)
      if (any(relssb_next_r$Model == "base")) q <- q + geom_hline(data = relssb_next %>% filter(Model == "base"), aes(yintercept = unique(RelSSB)), linetype = 2)
    } else {
      q <- q + geom_violin(aes(x = Model, y = RelSSB, fill = Model))
      if (any(relssb_next_r$Model == "base")) q <- q + geom_hline(data = relssb_next %>% filter(Model == "base"), aes(yintercept = median(RelSSB)), linetype = 2)
    }

 
      ggsave(paste0(figure_dir, "relssb_nextyear_compare_byRegion.png"), q, width = 10)

  }

  relssb_next_proj <- relssb %>% filter(YearType %in% c("FirstProjYear", "Projection")) %>% group_by(Model) %>% filter(Year %in% c(min(Year),max(Year)))
  relssb_next_proj$Year <- factor(relssb_next_proj$Year)

  p <- ggplot(relssb_next_proj) +
    theme_lsd(base_size = 14) +
    theme(axis.text.x = element_blank()) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    geom_hline(aes(yintercept = 0.2), col = "gray") +
    geom_hline(aes(yintercept = 0.1), col = "gray") +
    geom_text(data = labs_rel, aes(x = "base", y = value, label = type)) +
    labs(x = "Model", y = "Terminal year relative spawning biomass") +
    scale_alpha_manual(values = rep(c(1, 0.5), length(unique(relssb_next_proj$Year))/2), guide = FALSE)

  if (nmod > 6) {
    p <- p +
      scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
      scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
  } else {
    p <- p +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
  }
  if (max(relssb_next_proj$Iteration) == 1) {
    p <- p + geom_point(aes(x = Model, y = RelSSB, fill = Model, alpha = Year), cex = 4, pch = 21)
    if (any(relssb_next_proj$Model == "base")) p <- p + geom_hline(data = relssb_next_proj %>% filter(Model == "base") %>% filter(Year == max(years) + 1), aes(yintercept = unique(RelSSB)), linetype = 2)

  } else {
    p <- p + geom_violin(aes(x = Model, y = RelSSB, fill = Model, alpha = Year))
    if (any(relssb_next_proj$Model == "base")) {
      p <- p + geom_hline(data = relssb_next_proj %>% filter(Model == "base") %>% filter(Year == max(years) + 1), aes(yintercept = median(RelSSB)), linetype = 2)
    }
  }


    ggsave(paste0(figure_dir, "relssb_nextyear_projyear_compare.png"), p, width = 10)
  

  if (sum(by.Region) >= 1) {
    relssb_next_proj_r <- relssb_r %>% filter(YearType %in% c("FirstProjYear", "Projection")) %>% group_by(Model) %>% filter(Year %in% c(min(Year),max(Year)))
    relssb_next_proj_r$Year <- factor(relssb_next_proj_r$Year)

    q <- ggplot(relssb_next_proj_r) +
      theme_lsd(base_size = 14) +
      theme(axis.text.x = element_blank()) +
      scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
      geom_hline(aes(yintercept = 0.2), col = "gray") +
      geom_hline(aes(yintercept = 0.1), col = "gray") +
      geom_text(data = labs_rel, aes(x = "base", y = value, label = type)) +
      labs(x = "Model", y = "Terminal year relative spawning biomass") +
      facet_wrap(~Region) +
      scale_alpha_manual(values = c(1, 0.5), guide = FALSE)

    if (nmod > 6) {
      q <- q +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else {
      q <- q +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }
    if (max(relssb_next_proj_r$Iteration) == 1) {
      q <- q + geom_point(aes(x = Model, y = RelSSB, fill = Model, alpha = Year), cex = 4, pch = 21)
      if (any(relssb_next_proj_r$Model == "base")) q <- q + geom_hline(data = relssb_next_proj_r %>% filter(Model == "base") %>% filter(Year == max(years) + 1), aes(yintercept = unique(RelSSB)), linetype = 2)
    } else {
      q <- q + geom_violin(aes(x = Model, y = RelSSB, fill = Model, alpha = Year))
      if (any(relssb_next_proj_r$Model == "base")) q <- q + geom_hline(data = relssb_next_proj_r %>% filter(Model == "base") %>% filter(Year == max(years) + 1), aes(yintercept = median(RelSSB)), linetype = 2)
    }

      ggsave(paste0(figure_dir, "relssb_nextyear_projyear_compare_byRegion.png"), q, width = 10)
    
  }

  p <- ggplot(relssb %>% filter(YearType == "Assessment")) +
    theme_lsd(base_size = 14) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    geom_hline(aes(yintercept = 0.2), col = "gray") +
    geom_hline(aes(yintercept = 0.1), col = "gray") +
    geom_text(data = labs_rel %>% filter(type != "SSB0"), aes(x = (min(relssb$Year)+ 10), y = value, label = type)) +
    stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(x = Year, y = RelSSB, fill = Model)) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(x = Year, y = RelSSB, color = Model)) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75, aes(x = Year, y = RelSSB, color = Model)) +
    labs(x = "Fishing year", y = "Relative spawning biomass") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  if (nmod > 6) {
    p <- p +
      scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
      scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
  } else {
    p <- p +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
  }


    ggsave(paste0(figure_dir, "relssb_compare.png"), p, width = 10)
  

  p <- ggplot(relssb) +
    theme_lsd(base_size = 14) +
    geom_vline(data = ssb %>% filter(YearType == "FirstProjYear"), aes(xintercept = Year), linetype = 2) +
    expand_limits(y = 0) +
    geom_hline(aes(yintercept = 0.2), col = "gray") +
    geom_hline(aes(yintercept = 0.1), col = "gray") +
    geom_text(data = labs_rel %>% filter(type != "SSB0"), aes(x = min(relssb$Year) + 10, y = value, label = type)) +
    stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(x = Year, y = RelSSB, fill = Model)) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(x = Year, y = RelSSB, color = Model)) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75, aes(x = Year, y = RelSSB, color = Model)) +
    labs(x = "Year", y = "Relative spawning biomass") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  if (nmod > 6) {
    p <- p +
      scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
      scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
  } else {
    p <- p +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
  }


    ggsave(paste0(figure_dir, "relssb_compare_v2.png"), p, width = 10)
  

  if (sum(by.Region) >= 1) {

    relssb <- rbind(ssb, ssb0 %>% filter(type == "SSB0")) %>%
      tidyr::spread(type, value) %>%
      group_by(Iteration, Year, Rule, Model, Region, YearType) %>%
      summarise(SSB = sum(SSB), SSB0 = sum(SSB0)) %>%
      mutate(RelSSB = SSB/SSB0)

    q <- ggplot(relssb %>% filter(YearType == "Assessment")) +
      theme_lsd(base_size = 14) +
      geom_hline(aes(yintercept = 0.2), col = "gray") +
      geom_hline(aes(yintercept = 0.1), col = "gray") +
      geom_text(data = labs_rel %>% filter(type != "SSB0"), aes(x = (min(relssb$Year) + 10), y = value, label = type)) +
      stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(x = Year, y = RelSSB, fill = Model)) +
      stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(x = Year, y = RelSSB, color = Model)) +
      stat_summary(fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75, aes(x = Year, y = RelSSB, color = Model)) +
      xlab("Year") + scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
      ylab("Relative spawning biomass") +
      facet_grid(~Region) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    if (nmod > 6) {
      q <- q +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else {
      q <- q +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }

   
      ggsave(paste0(figure_dir, "relssb_compare_byRegion.png"), q, width = 10)
    

    q <- ggplot(relssb) +
      theme_lsd(base_size = 14) +
      geom_vline(data = ssb %>% filter(YearType == "FirstProjYear"), aes(xintercept = Year), linetype = 2) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
      geom_hline(aes(yintercept = 0.2), col = "gray") +
      geom_hline(aes(yintercept = 0.1), col = "gray") +
      geom_text(data = labs_rel %>% filter(type != "SSB0"), aes(x = min(relssb$Year) + 10, y = value, label = type)) +
      stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(x = Year, y = RelSSB, fill = Model)) +
      stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(x = Year, y = RelSSB, color = Model)) +
      stat_summary(fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75, aes(x = Year, y = RelSSB, color = Model)) +
      labs(x = "Year", y = "Relative spawning biomass") +
      scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
      facet_wrap(~Region)

    if (nmod > 6) {
      q <- q +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else {
      q <- q +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }


      ggsave(paste0(figure_dir, "relssb_compare_v2_byRegion.png"), q, width = 10)


    }
  } else {
     
    Line <- ssb %>% filter(YearType == "FirstProjYear")
      
    if (show_proj == FALSE) {
        ssb1 <- ssb[ssb$Model == object_names[1],]
        ssb01 <- ssb0[ssb0$Model == object_names[1],]
        Line <- ssb[ssb$Model == object_names[1],] %>% filter(YearType == "FirstProjYear")
        for (i in 2:nmod){
          tmp <- ssb[ssb$Model == object_names[i] & ssb$YearType != "Projection", ]
          tmp2 <-ssb0[ssb0$Model == object_names[i] & ssb0$YearType != "Projection", ]
          ssb1 <- rbind(ssb1, tmp)
          ssb01 <- rbind(ssb01, tmp2)
        }
        ssb <- ssb1
        ssb0 <- ssb01 
      }
    
      p1 <- ggplot(ssb %>% group_by(Iteration, Year, Model, YearType) %>% summarise(value = sum(value)), aes(x = Year, y = value)) +
         geom_vline(data = Line, aes(xintercept = Year - 0.5), linetype = 2) +
        stat_summary(data = ssb0 %>% filter(type == "Soft limit") %>% group_by(Iteration, Year, Model, YearType) %>% summarise(value = sum(value)), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        stat_summary(data = ssb0 %>% filter(type == "Soft limit") %>% group_by(Iteration, Year, Model, YearType) %>% summarise(value = sum(value)), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
        stat_summary(data = ssb0 %>% filter(type == "Hard limit") %>% group_by(Iteration, Year, Model, YearType) %>% summarise(value = sum(value)), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        stat_summary(data = ssb0 %>% filter(type == "Hard limit") %>% group_by(Iteration, Year, Model, YearType) %>% summarise(value = sum(value)), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
        #stat_summary(data = ssb0 %>% filter(type == "SSB0") %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        #stat_summary(data = ssb0 %>% filter(type == "SSB0") %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
        stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) + #, linetype = Model)) +
        stat_summary(fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75, aes(color = Model)) +
        geom_label(data = labs %>% filter(type != "SSB0") %>% group_by(type) %>% summarise(value = sum(value)), aes(x = min(ssb$Year)+10, y = value, label = type)) +
        labs(x = "Fishing year", y = "Spawning stock biomass (tonnes)") +
        scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1), expand = expansion(mult = c(0,0.01))) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
        theme_lsd(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      if (nmod > 6) {
        p1 <- p1 +
          scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
          scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
      } else{
        p1 <- p1 +
          scale_fill_brewer(palette = "Set1") +
          scale_color_brewer(palette = "Set1")
      }
      
      # by.Region <- NA
      # for (i in 1:length(data_list)) {
      #   by.Region[i] <- data_list[[i]]$n_area > 1
      # }
      
      # if (sum(by.Region) >= 1) {
      #   q1 <- ggplot(ssb, aes(x = Year, y = value)) +
      #     geom_vline(aes(xintercept = max(years) + 0.5), linetype = 2) +
      #     stat_summary(data = ssb0 %>% filter(type == "Soft limit"), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
      #     stat_summary(data = ssb0 %>% filter(type == "Soft limit"), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
      #     stat_summary(data = ssb0 %>% filter(type == "Hard limit"), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
      #     stat_summary(data = ssb0 %>% filter(type == "Hard limit"), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
      #     geom_label(data = labs %>% filter(type != "SSB0"), aes(x = min(ssb$Year)+10, y = value, label = type)) +
      #     #stat_summary(data = ssb0 %>% filter(type == "SSB0"), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
      #     #stat_summary(data = ssb0 %>% filter(type == "SSB0"), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
      #     stat_summary(data = ssb, fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
      #     stat_summary(data = ssb, fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
      #     stat_summary(data = ssb, fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75, aes(color = Model)) +
      #     labs(x = "Fishing year", y = "Spawning stock biomass (tonnes)") +
      #     scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1), expand = expansion(mult = c(0, 0.01))) +
      #     scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
      #     theme_lsd(base_size = 14) +
      #     facet_wrap(~Region) +
      #     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      #     geom_label(data = labs %>% filter(type != "SSB0"), mapping = aes(x = Year, y = value, label = type), nudge_x = -5) +
      #     coord_cartesian(clip = "off")
      #   
      #   if (nmod > 6) {
      #     q1 <- q1 +
      #       scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
      #       scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
      #   } else{
      #     q1 <- q1 +
      #       scale_fill_brewer(palette = "Set1") +
      #       scale_color_brewer(palette = "Set1")
      #   }
      return(p1)
    }
      # if(sum(by.Region) >= 1){
      #   return(q1)
      # } else{ return(p1 )}
  
}


#' Compare vulnerable biomass from multiple models
#'
#' @param object_list list of 'lsd.rds' files from multiple models
#' @param object_names vector of model names associated with each of the output files in object_list
#' @param figure_dir the directory to save to
#' @param save_plot to save the plot to file or not
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#' @param show_proj for rapid updates plots to show projections only for full assessment or for all model comparisons
#'
plot_compare_vb <- function(object_list, object_names, figure_dir = "compare_figure/", save_plot = TRUE, show_proj = TRUE)
{
  data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
  mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)
  years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
  pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_proj_yr)
  regions_list <- lapply(1:length(object_list), function(x) 1:data_list[[x]]$n_area)
  regions_list2 <- lapply(1:length(object_list), function(x) {
    if (length(regions_list[[x]]) == 1) out <- regions_list[[x]]
    if (length(regions_list[[x]]) > 1) out <- c(regions_list[[x]], "Total")
    return(out)
  })
  rules_list <- lapply(1:length(object_list), function(x) 1:data_list[[x]]$n_rules)

  sex <- c("Male", "Immature female", "Mature female")
  seasons <- c("AW", "SS")
  YR <- "YR" # label for the season before the season change year

  # Bref from most recent model
  n_iter <- nrow(mcmc_list[[length(object_list)]][[1]])
  bref <- mcmc_list[[length(object_list)]]$Bref_jr
  dimnames(bref) <- list("Iteration" = 1:n_iter, "Rules" = 1:rules_list[[1]], "Region" = regions_list2[[1]])
  Bref <- melt(bref) %>%
    filter(Iteration == 1) %>%
    dplyr::select(-c(Iteration, Rules))

  vb_list <- lapply(1:length(object_list), function(x) {
    n_iter <- nrow(mcmc_list[[x]][[1]])

    if ("biomass_vulnref_jytr" %in% names(mcmc_list[[x]])) {
      bvuln_ytr <- mcmc_list[[x]]$biomass_vulnref_jytr
      dimnames(bvuln_ytr) <- list("Iteration" = 1:n_iter, "Rules" = 1:rules_list[[x]], "Year" = pyears_list[[x]], "Season" = seasons, "Region" = regions_list[[x]])
      bvuln_ytr2 <- melt(bvuln_ytr) %>%
        filter(value > 0) %>%
        mutate(Season = as.character(Season), Season = ifelse(Year >= data_list[[x]]$season_change_yr, Season, YR)) %>%
        filter(Season %in% c("YR","AW")) %>%
        group_by(Iteration, Year, Season, Region) %>%
        summarise(value = sum(value))%>%
      mutate(YearType = ifelse(Year %in% years_list[[x]], "Assessment", "Projection")) %>%
      mutate(YearType = ifelse(Year == max(years_list[[x]])+1, "FirstProjYear", YearType))
    } else {
      bvuln_ytr <- mcmc_list[[x]]$biomass_vulnref_ytr
      dimnames(bvuln_ytr) <- list("Iteration" = 1:n_iter, "Year" = pyears_list[[x]], "Season" = seasons, "Region" = regions_list[[x]])
      bvuln_ytr2 <- melt(bvuln_ytr) %>%
        filter(value > 0) %>%
        mutate(Season = as.character(Season), Season = ifelse(Year >= data_list[[x]]$season_change_yr, Season, YR)) %>%
        filter(Season %in% c("YR","AW")) %>%
        filter(Year <= max(years_list[[x]])) %>%
        group_by(Iteration, Year, Season, Region) %>%
        summarise(value = sum(value))%>%
      mutate(YearType = ifelse(Year %in% years_list[[x]], "Assessment", "Projection")) %>%
      mutate(YearType = ifelse(Year == max(years_list[[x]])+1, "FirstProjYear", YearType))
    }

    bvuln_ytr2$Model <- object_names[x]
    bvuln_ytr2$qconstant <- as.character(ifelse(grepl("qconstant", object_names[[x]]), 1, 0))
    return(bvuln_ytr2)
  })
  vb <- data.frame(do.call(rbind, vb_list))
  vb$Model <- factor(vb$Model)
  vb$qconstant <- factor(vb$qconstant)

  vb0_list <- lapply(1:length(object_list), function(x) {
    n_iter <- nrow(mcmc_list[[x]][[1]])
    bio <- mcmc_list[[x]]$B0_r
    dimnames(bio) <- list("Iteration" = 1:n_iter, "Region" = regions_list2[[x]])
    vb0 <- melt(bio) %>%
      left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
      filter(Region != "Total") %>%
      group_by(Iteration, Year, Region) %>%
      summarise(value = sum(value)) %>%
      ungroup() %>%
      mutate(Rule = 1, type = "VB0")%>%
      mutate(YearType = ifelse(Year %in% years_list[[x]], "Assessment", "Projection")) %>%
      mutate(YearType = ifelse(Year == max(years_list[[x]])+1, "FirstProjYear", YearType))
    # bio <- mcmc_list[[x]]$SSBref_jr
    # dimnames(bio) <- list("Iteration" = 1:n_iter, "Rule" = 1, "Region" = regions_list[[x]])
    # ref <- melt(bio) %>%
    #     left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
    #     group_by(Iteration, Region, Rule, value, Year) %>%
    #     ungroup() #%>%
    # mutate(type = "Target")
    bio2 <- vb0
    bio2$Model <- object_names[x]
    return(bio2)
  })

  vb0 <- data.frame(do.call(rbind, vb0_list))
  vb0$Model <- factor(vb0$Model)

  mods <- unique(vb$Model)
  # mod_num <- sapply(1:length(mods), function(m) as.numeric(strsplit(as.character(mods[m]), "_")[[1]][1]))
  vb$Model <- factor(vb$Model, levels = unique(mods)) #[order(mod_num)])

  nmod <- length(unique(vb$Model))
  years <- unique(unlist(years_list))
  pyears <- unique(unlist(pyears_list))

  if(save_plot){
  # Vulnerable biomass
  p <- ggplot(data = vb %>% filter(YearType == "Assessment") %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)),
              aes(x = Year, y = value, color = Model, fill = Model)) +
    stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    #stat_summary(data=vb, fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    xlab("Fishing year") + ylab("Adjusted vulnerable biomass (tonnes)") +
    scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1), expand = expansion(mult = c(0,0.01))) +
    theme_lsd(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  if (nmod > 6) {
    p <- p +
      scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
      scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
  } else {
    p <- p +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
  }

  by.Region <- NA
  for (i in 1:length(data_list)) {
    by.Region[i] <- data_list[[i]]$n_area > 1
  }

  if (sum(by.Region) >= 1) {
    q <- ggplot(data = vb %>% filter(YearType == "Assessment"),
                aes(x = Year, y = value, color = Model, fill = Model)) +
      stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
      #stat_summary(data=vb, fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
      stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
      stat_summary(fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75) +
      # scale_fill_manual(values = cols_all, labels = object_names) +
      # scale_colour_manual(values = cols_all, labels = object_names) +
      # guides(colour = guide_legend(override.aes = list(colour = cols_all, linetype = lty_all))) +
      scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
      labs(x = "Fishing year", y = "Adjusted vulnerable biomass (tonnes)") +
      scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1), expand = expansion(mult = c(0,0.01))) +
      theme_lsd(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      facet_wrap(~Region)

    if (nmod > 6) {
      q <- q +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else {
      q <- q +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }
  }


    ggsave(paste0(figure_dir, "biomass_vulnref_compare.png"), p, width = 10)
    if (sum(by.Region) >= 1) {
      ggsave(paste0(figure_dir, "biomass_vulnref_compare_byRegion.png"), q, width = 10)
    }


  if (any(Bref$value > 0)) {
    # Vulnerable biomass
    p <- ggplot(data = vb %>% filter(YearType == "Assessment") %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)),
                aes(x = Year, y = value, color = Model, fill = Model)) +
      stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
      #stat_summary(data=vb, fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
      stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
      stat_summary(fun = function(x) quantile(x, 0.5), geom = "point", lwd = 1.5, alpha = 0.75) +
      geom_hline(data = Bref, aes(yintercept = value), lwd = 1.1, color = "forestgreen") +
      geom_label(data = Bref %>% filter(Region == 1), label = "Reference", aes(x = min(vb$Year) + 10, y = value), size = 5, color = "forestgreen", fill = "white") +
      # scale_fill_manual(values = cols_all, labels = object_names) +
      # scale_colour_manual(values = cols_all, labels = object_names) +
      # guides(colour = guide_legend(override.aes = list(colour = cols_all, linetype = lty_all))) +
      # scale_linetype(guide=FALSE) +
      expand_limits(y = 0) +
      xlab("Fishing year") + ylab("Adjusted vulnerable biomass (tonnes)") +
      scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1), expand = expansion(mult = c(0, 0.01))) +
      scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
      theme_lsd(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    if (nmod > 6) {
      p <- p +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else {
      p <- p +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }

    by.Region <- NA
    for (i in 1:length(data_list)) {
      by.Region[i] <- data_list[[i]]$n_area > 1
    }

    if (sum(by.Region) >= 1) {
      q <- ggplot(data = vb %>% filter(YearType == "Assessment"),
                  aes(x = Year, y = value, color = Model, fill = Model)) +
        stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        #stat_summary(data=vb, fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
        stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
        stat_summary(fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75) +
        geom_hline(data = Bref %>% filter(Region == 1), aes(yintercept = value), lwd = 1.1, color = "forestgreen") +
        geom_label(data = Bref %>% filter(Region == 1), label = "Reference", aes(x = min(vb$Year) + 10, y = value), size = 5, color = "forestgreen", fill = "white") +
        scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
        xlab("Fishing year") + ylab("Adjusted vulnerable biomass (tonnes)") +
        scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1), expand = expansion(mult = c(0,0.01))) +
        theme_lsd(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        facet_wrap(~Region)

      if (nmod > 6) {
        q <- q +
          scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
          scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
      } else {
        q <- q +
          scale_fill_brewer(palette = "Set1") +
          scale_color_brewer(palette = "Set1")
      }
    }


      ggsave(paste0(figure_dir, "biomass_vulnref_compare_wRef.png"), p, width = 10)
      if (sum(by.Region) >= 1) {
        ggsave(paste0(figure_dir, "biomass_vulnref_compare_byRegion_wRef.png"), q, width = 10)
      }

  }

  vb$Region <- factor(vb$Region)
  vb0$Region <- factor(vb0$Region)
  relvb <- full_join(vb %>% rename(VB = value), vb0 %>% rename(VB0 = value)) %>%
    mutate(RelVB = VB / VB0)

  # Relative Vulnerable biomass
  p <- ggplot(data = relvb %>% filter(YearType == "Assessment") %>% group_by(Iteration, Year, Model) %>% summarise(value = median(RelVB)),
              aes(x = Year, y = value, color = Model, fill = Model)) +
    stat_summary(un.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    #stat_summary(data=vb, fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
    xlab("Fishing year") + ylab("Relative adjusted vulnerable biomass (tonnes)") +
    scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1), expand = expansion(mult = c(0,0.01))) +
    theme_lsd(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  if (nmod > 6) {
    p <- p +
      scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
      scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
  } else {
    p <- p +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
  }

  if (sum(by.Region) >= 1) {
    q <- ggplot(data = relvb %>% filter(YearType == "Assessment"), aes(x = Year, y = RelVB, color = Model, fill = Model)) +
      stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
      #stat_summary(data=vb, fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
      stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
      stat_summary(fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75) +
      # scale_fill_manual(values = cols_all, labels = object_names) +
      # scale_colour_manual(values = cols_all, labels = object_names) +
      # guides(colour = guide_legend(override.aes = list(colour = cols_all, linetype = lty_all))) +
      # scale_linetype(guide=FALSE) +
      #scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
      xlab("Fishing year") + ylab("Relative adjusted vulnerable biomass (tonnes)") +
      scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1), expand = expansion(mult = c(0,0.01))) +
      scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
      theme_lsd(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      facet_wrap(~Region)

    if (nmod > 6) {
      q <- q +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else {
      q <- q +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }
  }


    ggsave(paste0(figure_dir, "biomass_relvulnref_compare.png"), p, width = 10)
    if (sum(by.Region) >= 1) {
      ggsave(paste0(figure_dir, "biomass_relvulnref_compare_byRegion.png"), q, width = 10)
    }


  # Vulnerable biomass
  p <- ggplot(data = vb %>% group_by(.data$Iteration, .data$Year, .data$Model) %>% summarise(value = sum(.data$value)),
              aes(x = .data$Year, y = .data$value, color = .data$Model, fill = .data$Model)) +
    geom_vline(data = vb %>% filter(YearType == "FirstProjYear"), aes(xintercept = Year), linetype = 2) +
    stat_summary(data = vb %>% group_by(Iteration, Year, Model,) %>% summarise(value = sum(value)), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    #stat_summary(data = vb %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
    stat_summary(data = vb %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
    stat_summary(data = vb %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
    xlab("Fishing year") + ylab("Adjusted vulnerable biomass (tonnes)") +
    scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1), expand = expansion(mult = c(0, 0.01))) +
    theme_lsd(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  if (nmod > 6) {
    p <- p +
      scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
      scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
  } else {
    p <- p +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
  }

  if (sum(by.Region) >= 1) {
    q <- ggplot(data = vb, aes(x = .data$Year, y = .data$value, color = .data$Model, fill = .data$Model)) +
    geom_vline(data = vb %>% filter(YearType == "FirstProjYear"), aes(xintercept = Year), linetype = 2) +
      stat_summary(data = vb, fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
      #stat_summary(data = vb, fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
      stat_summary(data = vb, fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
      stat_summary(data = vb, fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
      labs(x = "Fishing year", y = "Adjusted vulnerable biomass (tonnes)") +
      scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1), expand = expansion(mult = c(0, 0.01))) +
      theme_lsd(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      facet_wrap(~Region)

    if (nmod > 6) {
      q <- q +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else {
      q <- q +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }


      ggsave(paste0(figure_dir, "biomass_vulnref_compare_v2_byRegion.png"), q, width = 10)

  }


    ggsave(paste0(figure_dir, "biomass_vulnref_compare_v2.png"), p, width = 10)
    vb_summary <- vb %>%
      group_by(Year, Season, Model) %>%
      summarise(p05 = quantile(value, probs = 0.05), p50 = median(value), p95 = quantile(value, probs = 0.95))
    write.csv(vb_summary, file = paste0(figure_dir, "biomass_vulnref_compare_v2.csv"))

  if (any(Bref$value > 0)) {
    # Vulnerable biomass
    p <- ggplot(data = vb %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), aes(x = Year, y = value, color = Model, fill = Model)) +
    geom_vline(data = vb %>% filter(YearType == "FirstProjYear"), aes(xintercept = Year), linetype = 2) +
      stat_summary(data = vb %>% group_by(Iteration, Year, Model,) %>% summarise(value = sum(value)), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
      geom_hline(data = Bref, aes(yintercept = value), lwd = 1.2, color = "forestgreen") +
      #stat_summary(data = vb %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
      stat_summary(data = vb %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
      stat_summary(data = vb %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75) +
      xlab("Fishing year") + ylab("Adjusted vulnerable biomass (tonnes)") +
      scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1), expand = expansion(mult = c(0,0.01))) +
      scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
      theme_lsd(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45,hjust = 1))
    
    if (nmod > 6) {
      p <- p +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else {
      p <- p +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }
    
    if (sum(by.Region) >= 1) {
      q <- ggplot(data = vb %>% filter(Year %in% years), aes(x = Year, y = value, color = Model, fill = Model)) +
    geom_vline(data = vb %>% filter(YearType == "FirstProjYear"), aes(xintercept = Year), linetype = 2) +
        stat_summary(data = vb, fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        #stat_summary(data = vb, fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
        stat_summary(data = vb %>% filter(Year %in% years), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
        stat_summary(data = vb %>% filter(Year %in% years), fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75) +
        geom_hline(data = Bref %>% filter(Region == 1), aes(yintercept = value), lwd = 1.2, color = "forestgreen") +
        geom_label(data = Bref %>% filter(Region == 1), label = "Reference", aes(x = min(vb$Year) + 10, y = value), size = 5, color = "forestgreen", fill = "white") +
        #scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
        xlab("Fishing year") + ylab("Adjusted vulnerable biomass (tonnes)") +
        scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1), expand = expansion(mult = c(0, 0.01))) +
        scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
        theme_lsd(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
        facet_wrap(~Region)
      
      if (nmod > 6) {
        q <- q +
          scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
          scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
      } else {
        q <- q +
          scale_fill_brewer(palette = "Set1") +
          scale_color_brewer(palette = "Set1")
      }
      
        ggsave(paste0(figure_dir, "biomass_vulnref_compare_wRef_v2_byRegion.png"), q, width = 10)
 
    }

      ggsave(paste0(figure_dir, "biomass_vulnref_compare_wRef_v2.png"), p, width = 10)

  }
  } else {

    
    if (any(Bref$value > 0)) {
      
      Line <- vb %>% filter(YearType == "FirstProjYear")
      
      if (show_proj == FALSE) {
        vb1 <- vb[vb$Model == object_names[1],]
        Line <- vb[vb$Model == object_names[1],] %>% filter(YearType == "FirstProjYear")
        for (i in 2:nmod){
          tmp <- vb[vb$Model == object_names[i] & vb$YearType != "Projection", ]
          vb1 <- rbind(vb1, tmp)
          }
        vb <- vb1
       }
      
      # Vulnerable biomass
      p <- ggplot(data = vb %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), aes(x = Year, y = value, color = Model, fill = Model)) +
    geom_vline(data = Line, aes(xintercept = Year - 0.5), linetype = 2) +
        stat_summary(data = vb %>% group_by(Iteration, Year, Model,) %>% summarise(value = sum(value)), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        #stat_summary(data = vb %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
        stat_summary(data = vb %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
        stat_summary(data = vb %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75) +
        xlab("Fishing year") + ylab("Adjusted vulnerable biomass (tonnes)") +
        scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1), expand = expansion(mult = c(0,0.01))) +
        scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
        theme_lsd(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45,hjust = 1))
      
      if(any(Bref$Region == "Total")){
        p <- p + 
          geom_hline(data = Bref %>% filter(Region == "Total"), aes(yintercept = value), lwd = 1.2, color = "forestgreen") +
          geom_label(data = Bref %>% filter(Region == "Total"), label = "Reference", aes(x = min(vb$Year) + 10, y = value), size = 5, color = "forestgreen", fill = "white") 
      } else{
        p <- p + 
          geom_hline(data = Bref  , aes(yintercept = value), lwd = 1.2, color = "forestgreen") +
          geom_label(data = Bref , label = "Reference", aes(x = min(vb$Year) + 10, y = value), size = 5, color = "forestgreen", fill = "white") 
      }
      
      if (nmod > 6) {
        p <- p +
          scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
          scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
      } else {
        p <- p +
          scale_fill_brewer(palette = "Set1") +
          scale_color_brewer(palette = "Set1")
      }
      
      # if (sum(by.Region) >= 1) {
      #   q <- ggplot(data = vb %>% filter(Year %in% years), aes(x = Year, y = value, color = Model, fill = Model)) +
      #     geom_vline(aes(xintercept = max(years) + 0.5), linetype = 2) +
      #     stat_summary(data = vb, fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
      #     #stat_summary(data = vb, fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
      #     stat_summary(data = vb %>% filter(Year %in% years), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
      #     stat_summary(data = vb %>% filter(Year %in% years), fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75) +
      #     geom_hline(data = Bref %>% filter(Region == "Total"), aes(yintercept = value), lwd = 1.2, color = "forestgreen") +
      #     geom_label(data = Bref %>% filter(Region == "Total"), label = "Reference", aes(x = min(vb$Year) + 10, y = value), size = 5, color = "forestgreen", fill = "white") +
      #     #scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
      #     xlab("Fishing year") + ylab("Adjusted vulnerable biomass (tonnes)") +
      #     scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1), expand = expansion(mult = c(0,0.01))) +
      #     scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
      #     theme_lsd(base_size = 14) +
      #     theme(axis.text.x = element_text(angle = 45,hjust = 1))# +
      #     #facet_wrap(~Region)
      #   
      #   if (nmod > 6) {
      #     q <- q +
      #       scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
      #       scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
      #   } else {
      #     q <- q +
      #       scale_fill_brewer(palette = "Set1") +
      #       scale_color_brewer(palette = "Set1")
      #   }
        
  } else {
      # Vulnerable biomass
      p <- ggplot(data = vb %>% group_by(.data$Iteration, .data$Year, .data$Model) %>% summarise(value = sum(.data$value)),
                  aes(x = .data$Year, y = .data$value, color = .data$Model, fill = .data$Model)) +
      geom_vline(Line, aes(xintercept = Year-0.5), linetype = 2) +
        stat_summary(data = vb %>% group_by(Iteration, Year, Model,) %>% summarise(value = sum(value)), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        #stat_summary(data = vb %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
        stat_summary(data = vb %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
        stat_summary(data = vb %>% group_by(Iteration, Year, Model) %>% summarise(value = sum(value)), fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
        xlab("Fishing year") + ylab("Adjusted vulnerable biomass (tonnes)") +
        scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1), expand = expansion(mult = c(0,0.01))) +
        theme_lsd(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      if (nmod > 6) {
        p <- p +
          scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
          scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
      } else {
        p <- p +
          scale_fill_brewer(palette = "Set1") +
          scale_color_brewer(palette = "Set1")
      }
      
      
      # if (sum(by.Region) >= 1) {
      #   q <- ggplot(data = vb, aes(x = .data$Year, y = .data$value, color = .data$Model, fill = .data$Model)) +
      #     geom_vline(aes(xintercept = max(years) + 0.5), linetype = 2) +
      #     stat_summary(data = vb, fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
      #     #stat_summary(data = vb, fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
      #     stat_summary(data = vb, fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
      #     stat_summary(data = vb, fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75) +
      #     scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
      #     labs(x = "Fishing year", y = "Adjusted vulnerable biomass (tonnes)") +
      #     scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1), expand = expansion(mult = c(0, 0.01))) +
      #     theme_lsd(base_size = 14) +
      #     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      #     facet_wrap(~Region)
      #   
      #   if (nmod > 6) {
      #     q <- q +
      #       scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
      #       scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
      #   } else {
      #     q <- q +
      #       scale_fill_brewer(palette = "Set1") +
      #       scale_color_brewer(palette = "Set1")
      #   }
      #   
      # }
      
  }
    return(p)
  }
  
}


#' Compare recruitment from multiple models
#'
#' @param object_list list of 'lsd.rds' files from multiple models
#' @param object_names vector of model names associated with each of the output files in object_list
#' @param figure_dir the directory to save to
#' @param save_plot to save the plot to file or not
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @importFrom RColorBrewer brewer.pal
#' @export
#' @param show_proj for rapid updates plots to show projections only for full assessment or for all model comparisons
#'
plot_compare_recruitment <- function(object_list, object_names, figure_dir = "compare_figure/", save_plot = TRUE, show_proj = TRUE, Region = NA)
{
  data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
  mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)
  ny_list <- lapply(1:length(object_list), function(x) dim(mcmc_list[[x]]$recruits_ry)[3])
  years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
  pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:(data_list[[x]]$first_yr + ny_list[[x]] - 1))
  regions_list <- lapply(1:length(object_list), function(x) 1:data_list[[x]]$n_area)

  rec_list <- lapply(1:length(object_list), function(x) {
    n_iter <- nrow(mcmc_list[[x]][[1]])
    recruits2 <- mcmc_list[[x]]$recruits_ry
    dimnames(recruits2) <- list("Iteration" = 1:n_iter, "Region" = regions_list[[x]], "Year" = pyears_list[[x]])
    recruits2 <- melt(recruits2) %>%
      group_by(Iteration, Year, Region) %>%
      summarise(value = sum(value)) %>%
      mutate(YearType = ifelse(Year %in% years_list[[x]], "Assessment", "Projection")) %>%
      mutate(YearType = ifelse(Year == max(years_list[[x]])+1, "FirstProjYear", YearType))
    recruits2$Model <- object_names[x]
    recruits2$qconstant <- as.character(ifelse(grepl("qconstant", object_names[[x]]), 1,0))
    recruits2
  })

  recruits <- data.frame(do.call(rbind, rec_list)) %>%
    group_by(.data$Iteration, .data$Year, .data$Model, .data$Region, .data$qconstant, .data$YearType) %>%
    summarise(value = sum(.data$value) / 1e6) %>%
    mutate(Model = factor(.data$Model), qconstant = factor(.data$qconstant))

  years <- unique(unlist(years_list))

  nmod <- length(unique(recruits$Model))
  mods <- unique(object_names)
  # mod_num <- sapply(1:length(mods), function(m) as.numeric(strsplit(as.character(mods[m]), "_")[[1]][1]))
  recruits$Model <- factor(recruits$Model, levels = mods)


  # plot recruitment
  p <- ggplot(data = recruits %>% filter(.data$YearType == "Assessment"), aes(x = .data$Year, y = .data$value, color = .data$Model, fill = .data$Model)) +
    stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    #stat_summary(fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
    labs(x = "Fishing year", y = "Recruitment (millions of individuals)") +
    scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1)) +
    theme_lsd(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  if (nmod > 6) {
    p <- p +
      scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
      scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
  } else{
    p <- p +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
  }

  if (length(unique(recruits$Region)) > 1) {
    p <- p + facet_wrap(~Region)
  }

  if (save_plot) {
    ggsave(paste0(figure_dir, "recruitment_compare.png"), p, width = 10)
  }
  
  Line <- recruits %>% filter(YearType == "FirstProjYear")
  
  if (show_proj == FALSE) {
    recruits1 <- recruits[recruits$Model == object_names[1],]
    Line <- recruits[recruits$Model == object_names[1],] %>% filter(YearType == "FirstProjYear")
    for (i in 2:nmod){
      tmp <- recruits[recruits$Model == object_names[i] & recruits$YearType != "Projection", ]
      recruits1 <- rbind(recruits1, tmp)
    }
    recruits <- recruits1
  }
  Region = Region
  if (!is.na(Region)) {
    recruits2 <- recruits[recruits$Region == Region,]
    recruits <- recruits2
    }
  
  p <- ggplot(data = recruits, aes(x = .data$Year, y = .data$value, color = .data$Model, fill = .data$Model)) +
      geom_vline(data = Line, aes(xintercept = Year - 0.5), linetype = 2) +
    stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "point", size = 1.5, alpha = 0.75) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
    labs(x = "Fishing year", y = "Recruitment (millions of individuals)") +
    scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1)) +
    theme_lsd(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  if (nmod > 6) {
    p <- p +
      scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
      scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
  } else {
    p <- p +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
  }

  if (length(unique(recruits$Region)) > 1) {
    p <- p + facet_wrap(~Region)
  }

  if (save_plot) {
    ggsave(paste0(figure_dir, "recruitment_compare_v2.png"), p, width = 10)
  } else {
    return(p)
  }
}


#' Compare selectivity
#'
#' @param object_list list of 'lsd.rds' files from multiple stocks
#' @param object_names vector of model names associated with each of the output files in object_list
#' @param figure_dir the directory to save to
#' @param save_plot to save the plot to file or not
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#'
plot_compare_selectivity <- function(object_list, object_names, figure_dir = "compare_figure/", save_plot = TRUE){

  data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
  mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)
  years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
  pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_proj_yr)

  sex <- c("Male","Female" , "Female")

  slist <- lapply(1:length(object_list), function(x) {
    n_iter <- nrow(mcmc_list[[x]][[1]])
    n_season <- data_list[[x]]$n_season
    regions <- 1:data_list[[x]]$n_area
    pyears <- pyears_list[[x]]

    if(any(grepl('which_sel_rsyt', names(data_list[[x]])))){
      w <- data_list[[x]]$which_sel_rsyt
      dimnames(w) <- list("Region" = paste0("Region ", regions), "Sex" = sex, "Year" = pyears, "Season" = 1:n_season)
      w <- melt(w, value.name = "Selex")
    } else {
      w <- data_list[[x]]$which_sel_rsy
      dimnames(w) <- list("Region" = paste0("Region ", regions), "Sex" = sex, "Year" = pyears)
      w <- melt(w, value.name = "Selex") %>% mutate(Season = 1)
    }


    sel2 <- mcmc_list[[x]]$selectivity_ml
    dimnames(sel2) <- list("Iteration" = 1:n_iter, "Selex" = 1:data_list[[x]]$n_sel, "Size" = data_list[[x]]$size_midpoint_l)
    sel2 <- melt(sel2, value.name = "Selectivity") %>%
      inner_join(w, by = "Selex") %>%
      filter(Year <= max(pyears_list[[x]])) %>%
      mutate(Year = factor(Year)) %>%
      # mutate(Sex = ifelse(grepl("female", Sex), "Female", "Male")) %>%
      distinct(Iteration, Sex, Size, Selectivity, Region, .keep_all = TRUE)
    sel2$Model <- object_names[[x]]

    return(sel2)
  })

  sel <- do.call(rbind, slist) %>%
    rename(Epoch = Year) %>%
    mutate(Season = factor(Season))

  nmod <- length(unique(sel$Model))
  mods <- unique(sel$Model)
  mod_num <- sapply(1:length(mods), function(m) as.numeric(strsplit(as.character(mods[m]), "_")[[1]][1]))
  sel$Model <- factor(sel$Model, levels = object_names)

  # if multiple seasons, regardless of year
  # if (length(unique(sel$Season)) > 1) {
  #   p <- ggplot(data = sel, aes(x = Size, y = Selectivity, col = Model, fill = Model, linetype = Season)) +
  #     stat_summary(data = sel, aes(x = Size, y = Selectivity, col = Model, linetype = Season), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
  #     stat_summary(data = sel, aes(x = Size, y = Selectivity, col = Model, linetype = Season), fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
  #     stat_summary(data = sel, aes(x = Size, y = Selectivity, col = Model, linetype = Season), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.8) +
  #     scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1)))
  # } else {
    p <- ggplot(data = sel, aes(x = Size, y = Selectivity, col = Model, fill = Model)) +
      stat_summary(data = sel, aes(x = Size, y = Selectivity, col = Model, linetype = Epoch), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
      stat_summary(data = sel, aes(x = Size, y = Selectivity, col = Model, linetype = Epoch), fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
      stat_summary(data = sel, aes(x = Size, y = Selectivity, col = Model, linetype = Epoch), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.8) +
      scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1)))
  # }

  if (nmod > 6) {
    p <- p +
      scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
      scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
  } else {
    p <- p +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
  }

  if (length(unique(sel$Year)) == 1 & length(unique(sel$Season)) == 1) p <- p + guides(linetype = FALSE)

  areas <- unique(sapply(1:length(data_list), function(x) data_list[[x]]$n_area))
  if (areas > 1) {
    if (length(unique(sel$Season)) > 1 & length(unique(sel$Year)) > 1) {
      p <- p + facet_grid(Region + Year ~  Sex)
    } else {
      p <- p + facet_grid(Region ~ Sex)
    }
  } else {
    if (length(unique(sel$Season)) > 1 & length(unique(sel$Year)) > 1) {
      p <- p + facet_grid(Year ~  Sex)
    } else {
      p <- p + facet_grid( ~  Sex)
    }
  }

  p <- p + #scale_x_continuous(breaks = seq(30, 90, 10)) +
    scale_y_continuous(expand = c(0,0)) +
    coord_cartesian(ylim = c(0, 1)) +
    xlab("Length bin") +
    theme_lsd(base_size = 14)

  if (save_plot) {
    ggsave(paste0(figure_dir, "selectivity_compare.png"), p, width = 10)
  } else {
    return(p)
  }
}


#' Compare CPUE
#'
#' Plot the CPUE data and fit to the data.
#'
#' @param object_list list of 'lsd.rds' files from multiple models
#' @param object_names vector of model names associated with each of the output files in object_list
#' @param scales free or fixed
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param figure_dir the directory to save to
#' @param save_plot save the plot or return it
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#'
plot_compare_cpue <- function(object_list,
                              object_names,
                              scales = "fixed",
                              xlab = "Fishing year",
                              ylab = "CPUE",
                              figure_dir = "compare_figure/",
                              save_plot = TRUE)
{
  data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
  mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)
  years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
  pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_proj_yr)
  regions_list <- lapply(1:length(object_list), function(x) 1:data_list[[x]]$n_area)
  regions_list2 <- lapply(1:length(object_list), function(x) {
    if (length(regions_list[[x]]) == 1) out <- regions_list[[x]]
    if (length(regions_list[[x]]) > 1) out <- c(regions_list[[x]], "Total")
    return(out)
  })
  seasons <- c("AW", "SS")

  nmod <- length(object_names)
  n_area <- max(sapply(1:length(object_list), function(x) data_list[[x]]$n_area))

  pcpue <- lapply(1:length(object_list), function(x) {
    n_iter <- nrow(mcmc_list[[x]][[1]])
    pcpue1 <- mcmc_list[[x]]$pred_cpue_i
    dimnames(pcpue1) <- list("Iteration" = 1:n_iter, "I" = 1:data_list[[x]]$n_cpue)
    pcpue1 <- melt(pcpue1, value.name = "CPUE") %>%
      dplyr::select(Iteration, CPUE) %>%
      mutate(Data = "Expected", Type = "CPUE", Region = rep(data_list[[x]]$data_cpue_area_i, each = n_iter), Year = rep(data_list[[x]]$data_cpue_year_i, each = n_iter), Season = seasons[rep(data_list[[x]]$data_cpue_season_i, each = n_iter)])
    if(any(names(data_list[[x]]) == "data_cpue_type_i")) {
      pcpue1 <- bind_cols(pcpue1, "CPUE_type" = rep(data_list[[x]]$data_cpue_type_i, each = n_iter))
    } else {
      pcpue1 <- bind_cols(pcpue1, "CPUE_type" = 1)
    }
    pcpue1$Model <- object_names[[x]]
    return(pcpue1)
  })
  pcpue <- do.call(rbind, pcpue)

  # CPUE
  ocpue <- lapply(1:length(object_list), function(x){
    df <-   data.frame(Iteration = NA,
                       CPUE = data_list[[x]]$data_cpue_i,
                       Data = "Observed", Type = "CPUE",
                       Region = data_list[[x]]$data_cpue_area_i,
                       Year = data_list[[x]]$data_cpue_year_i,
                       Season = seasons[data_list[[x]]$data_cpue_season_i],
                       qtype = data_list[[x]]$data_cpue_q_i,
                       SD = sqrt(data_list[[x]]$cov_cpue_sd_i^2 + data_list[[x]]$cov_cpue_process_error_i^2) * 1.0 / data_list[[x]]$cpue_like_wt[data_list[[x]]$data_cpue_q_i],
                       Model = object_names[x])
    if(any(names(data_list[[x]]) == "data_cpue_type_i")) {
      df <- bind_cols(df, "CPUE_type" = data_list[[x]]$data_cpue_type_i)
    } else {
      df <- bind_cols(df, "CPUE_type" = 1)
    }
    return(df)
  })
  ocpue <- do.call(rbind, ocpue)

  # CELR
  dfilter <- expand.grid("Year" = 1989:min(c(max(ocpue$Year),2019)), "Season" = c("AW","SS"))
  dfilter <- dfilter[-which(dfilter$Year == 1989 & dfilter$Season == "AW"),]
  if(!max(ocpue$Year) < 2019) dfilter <- dfilter[-which(dfilter$Year == 2019 & dfilter$Season == "SS"),]

  ocr_yrs <- ocpue %>% right_join(dfilter) %>% 
    filter(CPUE_type == 1) %>%
    mutate(Region = paste0("Region ", Region))
  pcr_yrs <- pcpue %>% right_join(dfilter) %>% 
    filter(CPUE_type == 1) %>%
    mutate(Region = paste0("Region ", Region))

  ocr_yrs$Model <- factor(ocr_yrs$Model, levels = object_names)
  pcr_yrs$Model <- factor(pcr_yrs$Model, levels = object_names)
  p <- ggplot(data = ocr_yrs) +
    geom_point(aes(x = Year, y = CPUE, color = Model), alpha = 0.75) +
    geom_linerange(aes(x = Year, ymin = exp(log(CPUE) - SD), ymax = exp(log(CPUE) + SD), color = Model), alpha = 0.75) +
    scale_x_continuous(breaks = pretty(c(min(ocr_yrs$Year), max(ocr_yrs$Year)))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
    xlab(xlab) + ylab(paste0(ylab, " (CELR)")) +
    theme_lsd()

  if (!is.null(pcpue)) {
    p <- p + stat_summary(data = pcr_yrs, aes(x = .data$Year, y = .data$CPUE, fill = .data$Model), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
      # stat_summary(data = pcr_yrs, aes(x = Year, y = CPUE, fill = Model), fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
      stat_summary(data = pcr_yrs, aes(x = .data$Year, y = .data$CPUE, color = .data$Model), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1)
  }

  if (nmod > 6) {
    p <- p +
      scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
      scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
  } else {
    p <- p +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
  }

  if (length(unique(ocr_yrs$Region)) > 1) {
    p <- p + facet_wrap(Region~Season, scales = "free", ncol = n_area)
    if (save_plot) ggsave(paste0(figure_dir, "cpue_CELR.png"), p, width = 9, height = 10)
  } else {
    p <- p + facet_wrap(~Season, scales = "free", ncol = n_area)
    if (save_plot) ggsave(paste0(figure_dir, "cpue_CELR.png"), p, height = 9)
  }


  ## LOGBOOK
  ocr_yrs <- ocpue %>% 
    filter(CPUE_type == 2) %>%
    mutate(Region = paste0("Region ", Region))
  pcr_yrs <- pcpue %>%
    filter(CPUE_type == 2) %>%
    mutate(Region = paste0("Region ", Region))

if(nrow(ocr_yrs) > 0){
  ocr_yrs$Model <- factor(ocr_yrs$Model, levels = unique(ocr_yrs$Model))
  pcr_yrs$Model <- factor(pcr_yrs$Model, levels = unique(pcr_yrs$Model))
  p <- ggplot(data = ocr_yrs) +
    geom_point(aes(x = Year, y = CPUE, color = Model), alpha = 0.75) +
    geom_linerange(aes(x = Year, ymin = exp(log(CPUE) - SD), ymax = exp(log(CPUE) + SD), color = Model), alpha = 0.75) +
    scale_x_continuous(breaks = pretty(c(min(ocr_yrs$Year), max(ocr_yrs$Year)))) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
    xlab(xlab) + ylab(paste0(ylab, " (Logbook)")) +
    theme_lsd()

  if (!is.null(pcpue)) {
    p <- p + stat_summary(data = pcr_yrs, aes(x = .data$Year, y = .data$CPUE, fill = .data$Model), fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
      # stat_summary(data = pcr_yrs, aes(x = Year, y = CPUE, fill = Model), fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
      stat_summary(data = pcr_yrs, aes(x = .data$Year, y = .data$CPUE, color = .data$Model), fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1)
  }

  if (nmod > 6) {
    p <- p +
      scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
      scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
  } else {
    p <- p +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
  }

  if (length(unique(ocr_yrs$Region)) > 1) {
    p <- p + facet_wrap(Region~Season, scales = "free", ncol = n_area)
    if (save_plot) ggsave(paste0(figure_dir, "cpue_Logbook.png"), p, width = 9, height = 10)
  } else {
    p <- p + facet_wrap(~Season, scales = "free", ncol = n_area)
    if (save_plot) ggsave(paste0(figure_dir, "cpue_Logbook.png"), p, height = 9)
  }
}

  if (!save_plot) return(p)
}



#' Compare catchability coefficient q from multiple models
#'
#' @param object_list list of 'lsd.rds' files from multiple models
#' @param object_names vector of model names associated with each of the output files in object_list
#' @param figure_dir the directory to save to
#' @param save_plot to save the plot to file or not
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#'
plot_compare_q <- function(object_list, object_names, figure_dir = "compare_figure/", save_plot = TRUE)
{
  data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
  mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)

  n_iter <- nrow(mcmc_list[[1]][[1]])

  years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
  pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_proj_yr)
  nq_list <- lapply(1:length(object_list), function(x) data_list[[x]]$n_q)
  maxq <- max(unlist(sapply(1:length(object_list), function(x) nq_list[[x]])))
  q_info_list <- lapply(1:length(object_list), function(x) data.frame("qtype" = data_list[[x]]$data_cpue_q_i, "Season" = data_list[[x]]$data_cpue_season_i, "Year" = data_list[[x]]$data_cpue_year_i, "Region" = data_list[[x]]$data_cpue_area_i))
  q_info_list <- lapply(1:length(object_list), function(x) {
    if (max(q_info_list[[x]]$qtype) < maxq & max(q_info_list[[x]]$Year) == max(unlist(sapply(1:length(object_list), function(x) q_info_list[[x]]$Year)))) {
      subq <- q_info_list[[x]]$qtype
      max <- max(subq)
      for (i in 1:length(subq)) {
        if (subq[i] == max) {
          subq[i] <- maxq
          next
        }
        if (subq[i] != max) subq[i] <- subq[i] + (maxq - max)
      }
      q_info_list[[x]]$qtype <- subq
    }
    q_info_list[[x]] %>%
      filter(Season == 1) %>%
      mutate(QY = paste(Year, qtype))
  })

  q_list <- lapply(1:length(object_list), function(x) {
    n_iter <- nrow(mcmc_list[[x]][[1]])
    q2 <- mcmc_list[[x]]$par_q_cpue_qy
    dimnames(q2) <- list("Iteration" = 1:n_iter, "qtype" = 1:nq_list[[x]], "Year" = pyears_list[[x]])
    q2 <- melt(q2)

    if (max(q2$qtype) < maxq) {
      subq <- q2$qtype
      max <- max(subq)
      for (i in 1:length(subq)) {
        if (subq[i] == max) {
          subq[i] <- maxq
          next
        }
        if (subq[i] != max) subq[i] <- subq[i] + (maxq - max)
      }
      q2$qtype <- subq
    }

    q2 <- q2 %>%
      # filter(Year <= max(years_list[[x]])) %>%
      filter(Year %in% unique(q_info_list[[x]]$Year)) %>%
      mutate(QY = paste(Year, qtype)) %>%
      filter(QY %in% q_info_list[[x]]$QY)

    q2$Model <- object_names[x]
    q2$qconstant <- as.character(ifelse(grepl("qdrift",object_names[[x]]),0,1))
    # if (data_list[[x]]$n_area > 1 & "Region" %in% colnames(q2) == FALSE) q2$Region <- "All regions"
    return(q2)
  })

  q <- data.frame(do.call(rbind, q_list)) %>%
    group_by(Iteration, Year, Model, qconstant, qtype, QY) %>%
    mutate(Model = factor(Model), qconstant = factor(qconstant))

  nmod <- length(unique(q$Model))
  years <- unique(unlist(years_list))
  q$Model <- factor(q$Model, levels = object_names)

  p <- ggplot(data = q %>% filter(Year %in% years), aes(x = Year, y = value, colour = Model, fill = Model)) +
    stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    # stat_summary(fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
    # scale_fill_manual(values=cols_all, labels=object_names) +
    # scale_colour_manual(values=cols_all, labels=object_names) +
    # guides(colour = guide_legend(override.aes = list(colour = cols_all, linetype = lty_all))) +
    # scale_linetype(guide=FALSE) +
    expand_limits(y = 0) +
    xlab("Fishing year") + ylab("Catchability coefficient (q)") +
    # scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
    theme_lsd() +
    facet_wrap(~qtype, scales = "free")

  if (nmod > 6) {
    p <- p +
      scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
      scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
  } else{
    p <- p +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
  }

  # if (data_list[[1]]$n_area > 1) {
  #     p <- p + facet_wrap(~Region)
  # }

  if (save_plot) {
    ggsave(paste0(figure_dir, "q_y_compare.png"), p, width = 10)
  } else {
    return(p)
  }
}

plot_compare_movement <- function(object_list , object_names , figure_dir  = "compare_figure/",
                                  save_plot = TRUE)
{

  data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
  mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)

  n_iter <- nrow(mcmc_list[[1]][[1]])

  years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
  pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_proj_yr)

  mov_list <- lapply(1:length(object_list), function(x) {
    n_iter <- nrow(mcmc_list[[x]][[1]])
    mov <- mcmc_list[[x]]$movement_iy
    dimnames(mov) <- list("Iteration" = 1:n_iter, "Model" = object_names[x], "Year" = pyears_list[[x]])
    mov2 <- melt(mov)
  } )

  MOV <- data.frame(do.call(rbind, mov_list)) %>%
    group_by(Iteration, Year, Model) %>%
    mutate(Model = factor(Model))

  mov_list_yr <- lapply(1:length(object_list), function(x) data_list[[1]]$move_yrs)

  nmod <- length(unique(MOV$Model))
  years <- unique(unlist(years_list))

  p <- ggplot(data = MOV %>% filter(Year %in% years), aes(x = Year, y = value, colour = Model, fill = Model)) +
    stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    stat_summary(fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
    stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    xlab("Fishing year") + ylab("Movement (proportion)") +
    geom_point(data = MOV %>% filter (Year %in% min(mov_list_yr[[1]]):max(mov_list_yr[[1]])),
             mapping = aes(x = Year, y = value)) +
    # scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
    theme_lsd()

  if (nmod > 6) {
    p <- p +
      scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
      scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
  } else{
    p <- p +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
  }

  if (save_plot) {
    ggsave(paste0(figure_dir, "Movement_prop.png"), p, width = 10)
  } else {
    return(p)
  }
}

#' Compare Numbers at length from multiple models
#'
#' @param object_list list of 'lsd.rds' files from multiple models
#' @param object_names vector of model names associated with each of the output files in object_list
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param figure_dir figure directory
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom grDevices colorRampPalette gray
#' @importFrom stats quantile
#' @export
#'
plot_compare_numbers <- function(object_list,
                             object_names,
                             xlab = "Size (mm)",
                             ylab = "Number of individuals (thousands)",
                             figure_dir)
{
  data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
  mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)
  n_iter_list <- lapply(1:length(object_list), function(x) nrow(mcmc_list[[x]][[1]]))
  years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
  pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_proj_yr)

    sex <- c("Male","Immature female","Mature female")
    seasons <- c("AW", "SS")
    bins <- data_list[[1]]$size_midpoint_l
    regions <- 1:data_list[[1]]$n_area
    n_rules <- 1:data_list[[1]]$n_rules

  nmod <- length(object_names)

  year_rdev <- data_list[[1]]$last_rdev_yr
  year_proj <- max(pyears_list[[1]])

num_df <- lapply(1:length(object_list), function(x) {
  lf <- mcmc_list[[x]]$proj_numbers_jytrsl
  dimnames(lf) <- list("Iteration" = 1:n_iter_list[[x]], "RuleNum" = 1:n_rules, "Year" = pyears_list[[x]], "Season" = c(seasons, "EOY"), "Region" = regions, "Sex" = sex, "Size" = bins)
  numbers_proj <- reshape2::melt(lf, value.name = "N") %>%
    filter(Year %in% c(year_rdev, year_proj)) %>%
    mutate(Region = as.character(Region)) %>%
    filter(Season == "AW") %>%
    select(-Season) %>%
    filter(Sex != "Immature female") %>%
    mutate(Model = object_names[[x]])
  return(numbers_proj)
})
num_comp <- do.call(rbind, num_df)

num_comp$Model = factor(num_comp$Model, levels = unique(num_comp$Model))

  ymax_df <- num_comp %>%
  group_by(Size) %>%
  summarise(Median = quantile(N, 0.5))
  ymax <- max(ymax_df$Median)

mls_df <- lapply(1:length(object_list), function(x){
    mls <- data_list[[x]]$cov_mls_ytrs
    dimnames(mls) <- list("Year" = data_list[[x]]$first_yr:data_list[[x]]$last_proj_yr, "Season" = seasons, "Region" = regions, "Sex" = sex)
    mls <- melt(mls, id.var = "Year", variable.name = "Sex", value.name = "MLS") %>%
    mutate(Model = object_names[[x]]) %>%
    filter(Sex != "Immature female")

    return(mls)
  })
mls <- do.call(rbind, mls_df)
mls$Model = factor(mls$Model, levels = unique(mls$Model))


## compare current year numbers
    p <- ggplot(data = num_comp %>% filter(Year == year_rdev), aes(x = Size, y = N/1000, color = Model, fill = Model)) +
        stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        stat_summary(fun = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
        scale_y_continuous(limits = c(0, ymax/1000), expand = expansion(mult = c(0, 0.1))) +
        geom_vline(data = mls %>% filter(Year == year_rdev, Season == "AW"), aes(xintercept = MLS, color = Model, linetype = Model), lwd = 1) +
        xlab(xlab) + ylab(ylab) +
        facet_wrap(~Sex) +
        theme_lsd()

  if (nmod > 6) {
    p <- p +
      scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
      scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
  } else{
    p <- p +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
  }
    ggsave(file.path(figure_dir, "Numbers_final_rdev_year.png"), p, width = 12)

    p <- ggplot(data = num_comp %>% filter(Year == year_proj), aes(x = Size, y = N/1000, color = Model, fill = Model)) +
        stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        stat_summary(fun = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
        scale_y_continuous(limits = c(0, ymax/1000), expand = expansion(mult = c(0, 0.1))) +
        geom_vline(data = mls %>% filter(Year == year_proj, Season == "AW"), aes(xintercept = MLS, color = Model, linetype = Model), lwd = 1) +
        xlab(xlab) + ylab(ylab) +
        facet_wrap(~Sex) +
        theme_lsd()

  if (nmod > 6) {
    p <- p +
      scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
      scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
  } else{
    p <- p +
      scale_fill_brewer(palette = "Set1") +
      scale_color_brewer(palette = "Set1")
  }
    ggsave(file.path(figure_dir, "Numbers_final_proj_year.png"), p, width = 12)

  return(p)
}

#' Table comparing residuals for various data types across models
#'
#' @param object_list list of 'lsd.rds' files from multiple models
#' @param object_names vector of model names associated with each of the output files in object_list
#' @param figure_dir the directory to save to
#' @import dplyr
#' @importFrom reshape2 melt
#' @importFrom tidyr pivot_longer pivot_wider
#' @export
#'
table_compare_residuals <- function(object_list, object_names, figure_dir = "compare_figure/") {

  rlist <- lapply(1:length(object_list), function(x) {
    res <- table_residuals(object = object_list[[x]], figure_dir = figure_dir, save_table = FALSE) %>%
      mutate("model" = object_names[[x]])
    return(res)
  })

  rdf <- do.call(rbind, rlist) %>%
    pivot_longer(-c(model,data), names_to = "residual_type", values_to = "value") %>%
    pivot_wider(names_from = model)

  write.csv(rdf, file = file.path(figure_dir, "Residual_summaries.csv"), row.names = FALSE)
}


#' Table comparing estimated parameters across models
#'
#' @param object_list list of 'lsd.rds' files from multiple models
#' @param object_names vector of model names associated with each of the output files in object_list
#' @param figure_dir the directory to save to
#' @param save_plot to save the plot to file or not
#' @import dplyr
#' @importFrom reshape2 melt
#' @importFrom tidyr pivot_longer pivot_wider
#' @export
#'
table_compare_parameters <- function(object_list, object_names, figure_dir = "compare_figure/", save_plot = TRUE) {
  # apply fun to each object
  plist <- lapply(1:length(object_list), function(x) {
    pars <- table_parameters(object = object_list[[x]], figure_dir = figure_dir, save_table = FALSE)
    pars <- pars %>% mutate("model" = object_names[[x]])
    return(pars)
  })
  pdf <- do.call(rbind, plist)
  pdf2 <- pdf %>%
    pivot_longer(-c(model,Parameter), names_to = "Type", values_to = "value") %>%
    mutate(Name_long = paste(model, Type, sep = "_")) %>%
    select(-c(model, Type)) %>%
    pivot_wider(names_from = "Name_long", values_from = "value")

  if (save_plot) {
    write.csv(pdf2, file = file.path(figure_dir, "parameter_summaries.csv"), row.names = FALSE)
  } else {
    return(pdf2)
  }
}


#' Table computing leave-one-out information criterion for various datasets
#'
#' @param object_list list of 'lsd.rds' files from multiple models
#' @param object_names vector of model names associated with each of the output files in object_list
#' @param figure_dir the directory to save to
#' @import loo dplyr
#' @importFrom reshape2 melt
#' @importFrom parallel detectCores
#' @export
#'
looic <- function(object_list, object_names, figure_dir = "compare_figure/") {

  options(mc.cores = detectCores())

  mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)
  n_iter <- sapply(1:length(object_list), function(x) nrow(mcmc_list[[x]][[1]]))

  if (any(n_iter < 3)) return(NULL)
  if (all(n_iter > 3)) {

    ## cpue
    cpue_list <- lapply(1:length(object_list), function(x) {
      llcpue <- mcmc_list[[x]]$lp_cpue_i
      llcpue_array <- array(llcpue, dim = c(nrow(llcpue) / 4, 4, ncol(llcpue)))
      r_eff_cpue <- loo::relative_eff(exp(llcpue_array))
      loo_cpue <- loo::loo(x = llcpue_array, r_eff = r_eff_cpue)
      return(loo_cpue)
    })
    names(cpue_list) <- object_names

    comp1 <- loo::loo_compare(cpue_list)
    m1 <- rownames(comp1)
    df1 <- data.frame("model" = m1, "dataset" = "cpue")
    df2 <- data.frame(comp1)
    cpue_df <- cbind.data.frame(df1, df2)

    ## sexr
    sexr_list <- lapply(1:length(object_list), function(x){
      llsexr <- mcmc_list[[x]]$lp_sexr_i
      llsexr_array <- array(llsexr, dim = c(nrow(llsexr) / 4, 4, ncol(llsexr)))
      r_eff_sexr <- loo::relative_eff(exp(llsexr_array))
      loo_sexr <- loo::loo(x = llsexr_array, r_eff = r_eff_sexr)
      return(loo_sexr)
    })
    names(sexr_list) <- object_names

    comp1 <- loo::loo_compare(sexr_list)
    m1 <- rownames(comp1)
    df1 <- data.frame("model" = m1, "dataset" = "sexratio")
    df2 <- data.frame(comp1)
    sexr_df <- cbind.data.frame(df1, df2)

    # # ## lfs
    # lf_list <- lapply(1:length(object_list), function(x){
    #   lllf <- mcmc_list[[x]]$lp_lf_is
    #   lllf2 <- lapply(1:dim(lllf)[3], function(x){
    #    sub <- lllf[,,x]
    #    check <- colSums(sub)
    #    if(any(check == 0)){
    #      index <- which(check==0)
    #      sub[1:nrow(sub),index] <- 1e-2
    #    }
    #    return(sub)
    #   })
    #   lllf2 <- do.call(rbind, lllf2)
    #   lllf_array <- array(lllf2, dim = c(nrow(lllf2)/4,4,ncol(lllf2)))
    #   r_eff_lf <- loo::relative_eff(exp(lllf_array))
    #   loo_lf <- loo::loo(x=lllf_array, r_eff=r_eff_lf)
    #   return(loo_lf)
    # })
    # names(lf_list) <- object_names

    # lf_info <- data.frame(loo_compare(lf_list)) %>% mutate("dataset" = "lf")

    out <- rbind.data.frame(cpue_df, sexr_df)
    write.csv(out, file.path(figure_dir, "LOOIC.csv"), row.names = FALSE)

    return(out)
  }
}


