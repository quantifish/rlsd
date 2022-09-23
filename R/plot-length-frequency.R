#' Length-frequency plots
#'
#' @param object and LSD object
#' @param n_panel The number of rows of panels to include per plot.
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param figure_dir the directory to save to
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom grDevices hcl
#' @importFrom stats quantile
#' @importFrom forcats fct_rev
#' @export
#'
plot_lfs <- function(object,
                     n_panel = 10,
                     xlab = "Midpoint of size-class (mm)",
                     ylab = "Proportion at size (mm)",
                     figure_dir = "figure/")
{
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
    mls <- reshape2::melt(mls, id.var = "Year", variable.name = "Sex", value.name = "MLS")

    lim <- data$data_lf_bin_limits_i
    colnames(lim) <- c("Min", "Max")
    lim <- data.frame(lim) %>%
        mutate(LF = 1:data$n_lf) %>%
        mutate(lower = bins[Min],
               upper = bins[Max]) %>%
        select("LF", "lower", "upper")
    #----------------------------------------------


    rawW <- data$data_lf_weight_il
    dimnames(rawW) <- list("LF" = 1:data$n_lf, "Size" = bins)
    rawW <- reshape2::melt(rawW, value.name = "rawW") %>%
      mutate(Iteration = 1)


    elf2 <- rawW %>%
        left_join(w, by = "LF") %>%
        mutate(Season = seasons[Season], Sex = sex[Sex]) %>%
        left_join(mls, by = c("Region","Year","Season","Sex")) %>%
        left_join(lim, by = c("LF")) %>%
        select(-Iteration) %>%
        group_by(Size, Year, Season, Region, Sex, MLS, lower, upper) %>%
        mutate(rawW2 = max(rawW))

    # Observed LF
    dlf <- mcmc$data_lf_obs2_il
    dimnames(dlf) <- list("Iteration" = 1:n_iter, "LF" = 1:data$n_lf, "Size" = bins)
    dlf <- reshape2::melt(dlf) %>%
        left_join(w, by = "LF") %>%
        filter(Iteration == 1, value >= 0) %>%
        select(-Iteration) %>%
        left_join(lim, by = c("LF")) %>%
        mutate(Season = seasons[Season], Sex = sex[Sex])
    # head(dlf)

    # Predicted LF
    plf <- mcmc$pred_lf_il
    dimnames(plf) <- list("Iteration" = 1:n_iter, "LF" = 1:data$n_lf,
                          "Size" = bins)
    plf <- reshape2::melt(plf) %>%
        left_join(w, by = "LF") %>%
        left_join(lim, by = c("LF")) %>%
        mutate(Season = seasons[Season], Sex = sex[Sex]) 
    # head(plf)

    # Give each Region/Year/Season its own plot number, the number of
    # rows in this object is the number of plot panels that will be
    # generated in total
    n2 <- plf %>%
        distinct(Region, Year, Season) %>%
        mutate(Plot = 1:nrow(.))
    sq <- seq(1, nrow(n2), n_panel)

    elf2 <- elf2 %>%
        left_join(n2)
    plf <- plf %>%
        left_join(n2)
    dlf <- dlf %>%
        left_join(n2)

    ggplotColours <- function(n = 6, h = c(0, 360) + 15) {
        if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
        hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
    }

    # Plot observed only - by sex and source
    p <- ggplot(data = filter(dlf, value > 0)) +
            geom_point(aes(x = Year, y = Size, size = value), alpha = 0.5) +
            guides(size = guide_legend(title = "Proportion")) +
            theme_lsd()
    if (data$n_area == 1) {
        p <- p + facet_grid(Sex ~ ., scales = "fixed")
    } else{
        p <- p + facet_grid(Sex ~ Region, scales = "fixed")
    }
    ggsave(paste0(figure_dir, "lf_observed.png"), p, width = 12)

  # observed by sex, year, source and area
    dlfw <- right_join(dlf, elf2, by = c("LF","Sex","Year","Size","Region","Season","lower","upper"))
    dlfw$Sex <- factor(dlfw$Sex, levels = sex )

    for (i in 1:data$n_area) {
        p <- ggplot(data = filter(dlfw, Region %in% i, Size >= lower & Size <= upper),
                  aes(x = Size, y = fct_rev(paste(Year, Season)), height = value, alpha = rawW2)) +
        ggridges::geom_density_ridges(stat = "identity", scale = 3.5) +
        xlab(xlab) + ylab(ylab) +
        guides(shape = "none", colour = "none") +

        # scale_colour_manual(values = rev(ggplotColours(n = length(sources)))) +
        scale_x_continuous(minor_breaks = seq(0, 1e6, 2), limits = c(30, max(dlfw$upper)),
                           expand = c(0, 0)) +
        scale_y_discrete(expand = c(0, 0)) +
        theme_lsd() +
        theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
        labs(alpha = "weight")
      p <- p + facet_grid(~ Sex, scales = "free") +
        ggtitle(paste("Region", i, sep="  ")) +
            theme(plot.title = element_text(hjust = 0.5))
      ggsave(paste0(figure_dir, "lf_obs_Region", i, ".png"), p, height = 10, width = 9)
      }

    ####
    for (i in 1:data$n_area) {
      for (j in unique(dlfw$Season)) {
        p <- ggplot(data = filter(dlfw, Region %in% i, Season == j, Size >= lower & Size <= upper),
                    aes(x = Size, y = fct_rev(paste(Year)), height = value, alpha = rawW2)) +
          ggridges::geom_density_ridges(stat = "identity", scale = 3.5) +
          xlab(xlab) + ylab(ylab) +
          guides(shape = "none", colour = "none") +

          # scale_colour_manual(values = rev(ggplotColours(n = length(sources)))) +
          scale_x_continuous(minor_breaks = seq(0, 1e6, 2), limits = c(30, max(dlfw$upper)),
                             expand = c(0, 0)) +
          scale_y_discrete(expand = c(0, 0)) +
          theme_lsd() +
          theme(axis.text.x = element_text(angle = 45,hjust = 1)) +
          labs(alpha = "weight")
        p <- p + facet_grid(~ Sex, scales = "free") +
          ggtitle(paste("Region", i, "Season", j, sep="  ")) +
          theme(plot.title = element_text(hjust = 0.5))
        ggsave(paste0(figure_dir, "lf_obs_Region", i, j, ".png"), p, height = 10, width = 9)
      }
    }

    for (i in 1:length(sq)) {
        # pq <- sq[i]:(sq[i] + n_panel - 1)
        pq <- (sq[i] - n_panel + 1):sq[i]
        p <- ggplot() +
            geom_vline(data = filter(elf2, Plot %in% pq), aes(xintercept = MLS), linetype = "dashed") +
            geom_label(data = filter(elf2, Plot %in% pq), aes(x = Inf, y = Inf, label = paste("w:", round(rawW2,2))), hjust = 1, vjust = 1) +
            stat_summary(data = filter(plf, Plot %in% pq, Size >= lower & Size <= upper), aes(x = as.numeric(as.character(Size)), y = value), fun.min = function(x) stats::quantile(x, 0.05), fun.max = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
            stat_summary(data = filter(plf, Plot %in% pq, Size >= lower & Size <= upper), aes(x = as.numeric(as.character(Size)), y = value), fun.min = function(x) stats::quantile(x, 0.25), fun.max = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
            stat_summary(data = filter(plf, Plot %in% pq, Size >= lower & Size <= upper), aes(x = as.numeric(as.character(Size)), y = value), fun = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
            geom_point(data = filter(dlf, Plot %in% pq, Size >= lower & Size <= upper), aes(x = as.numeric(as.character(Size)), y = value, colour = Season)) +
            xlab(xlab) + ylab(ylab) +
            guides(shape = "none", colour = "none") +
            scale_x_continuous(minor_breaks = seq(0, 1e6, 2), limits = c(min(elf$lower), max(elf$upper))) +
            scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
            theme_lsd()
        if (data$n_area == 1) {
            p <- p + facet_grid(Year + Season~ Sex, scales = "free_y")
        } else {
            p <- p + facet_grid(Region + Year + Season ~ Sex, scales = "free_y")
        }
        ggsave(paste0(figure_dir, "lf_", i, ".png"), p, height = 12, width = 9)
    }
}


#' Plot length frequency residuals2
#'
#' @param object and LSD object.
#' @param n_panel The number of rows of panels to include per plot.
#' @param figure_dir the directory to save to.
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @importFrom tidyr spread
#' @export
#'
plot_lfs_resid2 <- function(object, n_panel = 10, figure_dir = "figure/")
{
    data <- object@data
    mcmc <- object@mcmc

    n_iter <- nrow(mcmc[[1]])
    years <- data$first_yr:data$last_yr
    sex <- c("Male", "Immature female", "Mature female")
    seasons <- c("AW", "SS")
    bins <- data$size_midpoint_l
    regions <- 1:data$n_area

   w <- data.frame(LF = 1:data$n_lf,
                    Year = data$data_lf_year_i, Season = data$data_lf_season_i,
                    Region = data$data_lf_area_i, Sex = data$data_lf_sex_i)

        resid <- mcmc$resid_lf_il
    dimnames(resid) <- list("Iteration" = 1:n_iter, "LF" = 1:data$n_lf, "Size" = bins)
    resid <- reshape2::melt(resid) %>%
        left_join(w, by = "LF") %>%
        mutate(Season = seasons[Season], Sex = sex[Sex])

    mls <- data$cov_mls_ytrs
    dimnames(mls) <- list("Year" = data$first_yr:data$last_proj_yr, "Season" = seasons, "Region" = regions, "Sex" = sex)
    mls <- reshape2::melt(mls, id.var = "Year", variable.name = "Sex", value.name = "MLS")

    # New residual plot
    lim <- data$data_lf_bin_limits_i
    colnames(lim) <- c("Min", "Max")
    lim <- data.frame(lim) %>%
        mutate(LF = 1:data$n_lf) %>%
        mutate(lower = bins[Min],
               upper = bins[Max]) %>%
        select("LF", "lower", "upper")

    resid_lim <- left_join(resid, lim, by = c("LF")) %>%
      filter(Size > lower, Size < upper)
    n2 <- resid_lim %>%
        distinct(Region, Year, Season) %>%
        mutate(Plot = 1:nrow(.))
    sq <- seq(1, nrow(n2), n_panel)
    resid_lim <- left_join(resid_lim, n2, by = c("Year", "Season", "Region")) %>%
      left_join(mls, by = c("Sex", "Year", "Season", "Region"))

    for (i in 1:length(sq)) {
      pq <- (sq[i] - n_panel + 1):sq[i]
      df <- resid_lim %>% filter(Plot %in% pq)

      p <- ggplot(data = df, aes(x = as.numeric(as.character(Size)), y = value)) +
        geom_vline(aes(xintercept = MLS), linetype = "dashed") +
        geom_hline(yintercept = 0, linetype = "dashed") +
        stat_summary(fun.min = function(x) quantile(x, 0.05), fun.max = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(fun.min = function(x) quantile(x, 0.25), fun.max = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(fun = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
        scale_x_continuous(minor_breaks = seq(0, 1e6, 2), limits = c(min(lim$lower), max(lim$upper))) +
        xlab("Midpoint of size-class (mm)") + ylab("Standardised residuals") +
        theme_lsd()
      if (data$n_area == 1) {
        p <- p + facet_grid(Year + Season ~ Sex, scales = "free_y")
      } else {
        p <- p + facet_grid(Region + Year + Season ~ Sex, scales = "free_y")
      }
      ggsave(paste0(figure_dir, "lf_resid_", i, ".png"), p, height = 12, width = 9)
  }
}


#' Plot length frequency residuals
#'
#' @param object and LSD object
#' @param figure_dir the directory to save to
#' @param ylim the y axis limit
#' @export
#'
plot_lfs_resid <- function(object, figure_dir = "figure/", ylim = c(-5, 5))
{
    data <- object@data
    mcmc <- object@mcmc

    n_iter <- nrow(mcmc[[1]])
    years <- data$first_yr:data$last_yr
    sex <- c("Male","Immature female","Mature female")
    seasons <- c("AW","SS")
    bins <- data$size_midpoint_l
    regions <- 1:data$n_area

   w <- data.frame(LF = 1:data$n_lf,
                    Year = data$data_lf_year_i, Season = data$data_lf_season_i,
                    Region = data$data_lf_area_i, Sex = data$data_lf_sex_i)

    resid <- mcmc$resid_lf_il
    dimnames(resid) <- list("Iteration" = 1:n_iter, "LF" = 1:data$n_lf, "Size" = bins)
    resid <- reshape2::melt(resid) %>%
        left_join(w, by = "LF") %>%
        mutate(Season = seasons[Season], Sex = sex[Sex])
    head(resid)

    rawW <- data$data_lf_weight_il
    dimnames(rawW) <- list("LF" = 1:data$n_lf, "Size" = bins)
    rawW <- reshape2::melt(rawW, value.name = "rawW")

    ## add fake data
    allyrs <- min(resid$Year):max(resid$Year)
    #z <- data.frame("Iteration"=1, "LF"=1, "Sex"="Male", "Size"=31, value=0, Year=allyrs[!allyrs %in% unique(resid$Year)], Season="SS", Source="CS", Region = regions, "Weight"=0, "N.1"=0, "N.2"=0, "N.3"=0)
    #resid <- rbind(resid, z)

    if (min(resid$value) > ylim[1]) ylim <- c(min(resid$value), ylim[2])
    if (max(resid$value) < ylim[2]) ylim <- c(ylim[1], max(resid$value))

    resid <- resid %>%
        mutate(Size = as.numeric(Size)) %>%
        left_join(rawW, by = c("LF", "Size")) %>%
        group_by(Year) %>%
        mutate(WtYr = sum(rawW)) %>%
        ungroup() %>%
        group_by(Size) %>%
        mutate(WtSz = sum(rawW)) 

        resid$Region <- sapply(1:nrow(resid), function(x) paste0("Region ", resid$Region[x]))
        resid$Sex <- factor(resid$Sex, levels = sex)

        p <- ggplot(data = resid) +
            geom_violin(aes(x = factor(Year), y = value, alpha = WtYr, fill = Region, color = Region), scale="width") +
            xlab("Year") + ylab("Standardised residuals") +
            geom_hline(yintercept = 0, alpha = 0.8) +
            scale_x_discrete(breaks = rev(seq(max(years), by = -5))) +
            guides(fill = 'none', color = "none") +
            theme_lsd() +
            scale_alpha(guide = "none") +
            scale_color_brewer(palette = "Set1") +
            scale_fill_brewer(palette = "Set1") +
            coord_cartesian(ylim = ylim)
        if(length(regions)>1) p <- p + facet_grid(Region~.)

        ggsave(paste0(figure_dir, "lf_residuals_year.png"), p, width=12)

        p <- ggplot(data = resid) +
            geom_violin(aes(x = factor(Size), y = value, fill = Region, color = Region, alpha=WtSz), scale="width") +
             xlab("Size (mm)") + ylab("Standardised residuals") +
            geom_hline(yintercept = 0, alpha = 0.8) +
            theme_lsd() +
            scale_color_brewer(palette = "Set1") +
            scale_fill_brewer(palette = "Set1") +
            scale_alpha(guide = "none")  +
            guides(fill = "none", color = "none") +
            coord_cartesian(ylim = ylim)
        if(length(regions)>1) p <- p + facet_grid(Region~.)

        ggsave(paste0(figure_dir, "lf_residuals_size.png"), p, width=16)

        p <- ggplot(data = resid) +
            geom_violin(aes(x = factor(Size), y = value, fill=Season, colour=Season, alpha=WtSz), scale="width") +
            xlab("Size (mm)") + ylab("Standardised residuals") +
            geom_hline(yintercept = 0, alpha = 0.8) +
            theme_lsd() +
            scale_alpha(guide = "none") +
            coord_cartesian(ylim = ylim) +
            scale_color_brewer(palette = "Set1") +
            scale_fill_brewer(palette = "Set1") +
            guides(fill = "none", color = "none")

        if(length(regions)>1){
            p <- p + facet_grid(Region~Season)
        } else {
            p <- p + facet_grid(~Season)
        }

        ggsave(paste0(figure_dir, "lf_residuals_size_season.png"), p, width=16)

    	p <- ggplot(data = resid) +
        	geom_violin(aes(x = factor(Year), y = value, fill=Region, color = Region, alpha=WtYr), scale="width") +
        	xlab("Year") + ylab("Standardised residuals") +
            scale_x_discrete(breaks = rev(seq(max(years), by = -5))) +
            geom_hline(yintercept = 0, alpha = 0.8) +
        	theme_lsd() +
            guides(fill = "none", color = "none") +
            scale_alpha(guide = "none") +
            coord_cartesian(ylim = ylim) +
            scale_color_brewer(palette = "Set1") +
            scale_fill_brewer(palette = "Set1") 
        if(length(regions)>1){
            p <- p + facet_grid(Sex ~ Region, scales="free_y")
        } else {
            p <- p + facet_grid(Sex ~ ., scales = "free_y")
        }
        ggsave(paste0(figure_dir, "lf_residuals_sex_year.png"), p, width=12)

    	p <- ggplot(data = resid) +
        	geom_violin(aes(x = factor(Size), y = value, fill=Region, color = Region, alpha=WtSz), scale="width") +
        	xlab("Size (mm)") + ylab("Standardised residuals") +
            geom_hline(yintercept = 0, alpha = 0.8) +
        	theme_lsd() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
            scale_alpha(guide = "none") +
            guides(fill = "none", color = "none") +
            coord_cartesian(ylim = ylim) +
            scale_color_brewer(palette = "Set1") +
            scale_fill_brewer(palette = "Set1") 
        if(length(regions)>1){
            p <- p + facet_grid(Sex ~ Region, scales="free_y")
        } else {
            p <- p + facet_grid(Sex ~ ., scales = "free_y")
        }
        ggsave(paste0(figure_dir, "lf_residuals_sex_size.png"), p, width=16)

        p <- ggplot(data = resid) +
            geom_violin(aes(x = factor(Size), y = value, fill=Season, colour=Season, alpha=WtSz), scale="width") +
            xlab("Size (mm)") + ylab("Standardised residuals") +
            geom_hline(yintercept = 0, alpha = 0.8) +
            theme_lsd() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
            scale_alpha(guide = "none") +
            coord_cartesian(ylim = ylim) +
            scale_color_brewer(palette = "Set1") +
            scale_fill_brewer(palette = "Set1") 
        if(length(regions)>1){
            p <- p + facet_grid(Sex ~ Region + Season, scales="free_y")
        } else {
            p <- p + facet_grid(Sex ~ Season, scales = "free_y")
        }
        ggsave(paste0(figure_dir, "lf_residuals_sex_size_season.png"), p, width=16)

    	p <- ggplot(data = resid) +
        	geom_violin(aes(x = factor(Year), y = value, color = Region, fill = Region, alpha = WtYr), scale="width") +
            scale_x_discrete(breaks = rev(seq(max(years), by = -5))) +
        	xlab("Year") + ylab("Standardised residuals") +
            geom_hline(yintercept = 0, alpha = 0.8) +
        	theme_lsd() +
            scale_alpha(guide = "none") +
            coord_cartesian(ylim = ylim) +
            guides(fill = "none", color = "none") +
            scale_color_brewer(palette = "Set1") +
            scale_fill_brewer(palette = "Set1") 
        if(length(regions)>1){
            p <- p + facet_grid(Sex ~ Region, scales="free_y")
        } else {
            p <- p + facet_grid(Sex ~ ., scales = "free_y")
        }
        ggsave(paste0(figure_dir, "lf_residuals_sex_year.png"), p, width=10)


}
