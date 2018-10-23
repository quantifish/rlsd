#' Plot tag residuals
#' 
#' @export
#' 
plot_tag_residuals <- function(object,
                               ylab = "Standardised residual", 
                               figure_dir = "figure/",
                               ylim = c(-5, 5))
{
    data <- object@data
    map <- object@map
    mcmc <- object@mcmc

    g <- length(data$cov_grow_morph_g)
    n_iter <- nrow(mcmc[[1]])
    bins <- data$size_midpoint_l
    n_morph <- data$n_growth_subset
    pyears <- data$first_yr:data$last_proj_yr
    sex <- c("Male", "Female")
    
    #w <- data$which_growth_rsy
    #dimnames(w) <- list("Region" = object@regions, "Sex" = sex, "Year" = pyears)
    #w <- reshape2::melt(w, value.name = "Morph") %>%
    #    dplyr::distinct(Region, Sex, Morph, .keep_all = TRUE)

    morph <- data.frame("Morph" = data$cov_grow_morph_g,
                        "Size" = data$data_grow_size_capture_g,
                        "Area" = data$cov_grow_release_area_g,
                        "Sex" = sex[data$cov_grow_sex_g],
                        "Release" = data$cov_grow_release_no_g,
                        "Year" = data$cov_grow_release_yr_g) %>%
        dplyr::mutate(I = 1:g)
    
    resid <- mcmc$resid_grow_g
    dimnames(resid) <- list("Iteration" = 1:n_iter, "I" = 1:g)
    resid <- reshape2::melt(resid) %>%
        dplyr::inner_join(morph, by = "I")
        #dplyr::inner_join(w, by = "I") %>%
        #dplyr::distinct(Iteration, I, Region, .keep_all = TRUE)

    if (min(resid$value) > ylim[1]) ylim <- c(min(resid$value), ylim[2])
    if (max(resid$value) < ylim[2]) ylim <- c(ylim[1], max(resid$value))

    wtsx <- sapply(1:length(sex), function(x){
        sub <- resid %>% filter(Sex==sex[x])
        out <- nrow(sub)/nrow(resid)
        names(out) <- sex[x]
        return(out)
    })
    resid$WtSex <- sapply(1:nrow(resid), function(x) wtsx[match(resid$Sex[x], names(wtsx))])

    areas <- unique(resid$Area)
    wtar <- sapply(1:length(areas), function(x){
        sub <- resid %>% filter(Area==areas[x])
        out <- nrow(sub)/nrow(resid)
        names(out) <- areas[x]
        return(out)
    })
    resid$WtArea <- sapply(1:nrow(resid), function(x) wtar[match(resid$Area[x], names(wtar))])

    years <- unique(resid$Year)
    wtyr <- sapply(1:length(years), function(x){
        sub <- resid %>% filter(Year==years[x])
        out <- nrow(sub)/nrow(resid)
        names(out) <- years[x]
        return(out)
    })
    resid$WtYear <- sapply(1:nrow(resid), function(x) wtyr[match(resid$Year[x], names(wtyr))])

    #yarrr::pirateplot(formula = value ~ Area, data = resid, pal = "xmen")
    #yarrr::pirateplot(formula = value ~ Sex + Area, data = resid, pal = "xmen", point.o = .05, bean.o = 1, bean.lwd = 2, gl.col = "gray")

    p <- ggplot(data = resid) +
        geom_violin(aes(x = factor(Release), y = value, alpha=WtSex), colour = "forestgreen", fill = "forestgreen", scale = "width") +
        facet_grid(Sex ~ .) +
        geom_hline(yintercept = 0, alpha = 0.8) +
        labs(x = "Release", y = ylab, fill = "Morph") +
        theme_lsd() +
        guides(alpha=FALSE) +
        coord_cartesian(ylim = ylim)
    ggsave(paste0(figure_dir, "tag_residuals_release_no.png"), p)
    
    p <- ggplot(data = resid) +
        geom_violin(aes(x = factor(Area), y = value, colour = factor(Morph), fill = factor(Morph), alpha=WtArea), scale = "width") +
        facet_grid(Sex ~ .) +
        labs(x = "Statistical area", y = ylab, colour = "Morph", fill = "Morph") +
        geom_hline(yintercept = 0, alpha = 0.8) +
        theme_lsd() +
        guides(alpha=FALSE) +
        coord_cartesian(ylim = ylim)
    ggsave(paste0(figure_dir, "tag_residuals_area.png"), p)
    
    p <- ggplot(data = resid) +
        geom_violin(aes(x = factor(Year), y = value, alpha=WtYear), colour = "purple", fill = "purple", scale = "width") +
        facet_grid(Sex ~ .) +
        labs(x = "Release year", y = ylab, fill = "Morph") +
        geom_hline(yintercept = 0, alpha = 0.8) +
        theme_lsd() +
        guides(alpha=FALSE) +
        coord_cartesian(ylim = ylim)
    ggsave(paste0(figure_dir, "tag_residuals_year.png"), p, width = 12)

    resid_cut <- resid %>%
        dplyr::mutate(class = floor(Size/2)*2)

    szclasses <- unique(resid_cut$class)
    wtcls <- sapply(1:length(szclasses), function(x){
        sub <- resid_cut %>% filter(class==szclasses[x])
        out <- nrow(sub)/nrow(resid_cut)
        names(out) <- szclasses[x]
        return(out)
    })
    resid_cut$WtClass <- sapply(1:nrow(resid_cut), function(x) wtcls[match(resid_cut$class[x], names(wtcls))])
    
    p <- ggplot(data = resid_cut) +
        geom_violin(aes(x = factor(class), y = value, colour = factor(Morph), fill = factor(Morph), alpha=WtClass), scale = "width") +
        facet_grid(Area ~ Sex) +
        labs(x = "Initial size (mm)", y = ylab, colour = "Morph", fill = "Morph") +
        geom_hline(yintercept = 0, alpha = 0.8) +
        theme_lsd() +
        guides(alpha=FALSE) +
        scale_x_discrete(breaks = seq(0, 1e6, 10)) +
        coord_cartesian(ylim = ylim)
    ggsave(paste0(figure_dir, "tag_residuals_size.png"), p, height = 8)
}



#' Plot growth increment
#' 
#' @export
#' 
plot_growth_increment <- function(object, 
                                  xlab = "Size (mm)", 
                                  ylab = "Increment (mm)", 
                                  figure_dir = "figure/", empirical = FALSE)
{
    data <- object@data
    map <- object@map
    mcmc <- object@mcmc
    
    n_iter <- nrow(mcmc[[1]])
    bins <- data$size_midpoint_l
    n_morph <- data$n_growth_subset
    pyears <- data$first_yr:data$last_proj_yr
    #sex <- c("Male","Immature female","Mature female")
    sex <- c("Male", "Female", "Female")

    w <- data$which_growth_rsy
    dimnames(w) <- list("Region" = object@regions, "Sex" = sex, "Year" = pyears)
    w <- reshape2::melt(w, value.name = "Morph")

    gi <- mcmc$growth_increment_iil
    dimnames(gi) <- list("Iteration" = 1:n_iter, "Morph" = 1:n_morph, "Type" = c("Increment", "SD"), "Size" = bins)
    gi <- reshape2::melt(gi) %>%
        tidyr::spread(Type, value) %>%
        dplyr::mutate(Lo = Increment - SD, Hi = Increment + SD) %>%
        dplyr::inner_join(w, by = "Morph") %>%
        dplyr::distinct(Iteration, Increment, Region, .keep_all = TRUE)
    
    p <- ggplot(data = gi, aes(x = Size, y = Increment, color = Sex, fill = Sex)) +
        stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
        stat_summary(aes(x = Size, y = Lo, color = Sex), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(aes(x = Size, y = Lo, color = Sex), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, lty=2) +
        stat_summary(aes(x = Size, y = Hi, color = Sex), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(aes(x = Size, y = Hi, color = Sex), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, lty=2) +
        expand_limits(y = 0) +
        xlab(xlab) + ylab(ylab) +
        guides(colour = FALSE, fill = FALSE) +
        theme_lsd()

    # plot .75 quantile ribbon, default is FALSE   
    if (empirical == T) {
    p = p +   
        stat_summary(fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(aes(x = Size, y = Lo, color = Sex), fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(aes(x = Size, y = Hi, color = Sex), fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) 
     }

    #if (data$n_growth_morph > 1) {
    p <- p + facet_wrap(Sex ~ Morph)
    #} else {
    #    p <- p + facet_wrap(~Sex)
    #}

    ggsave(paste0(figure_dir, "growth_increment.png"), p)
}


#' Plot growth matrix
#'
#' This function produces two different plots of the growth matrix. The first version is on a single panel. The second version plots a panel for each size category.
#'
#' @param scales should facet_wrap scales be fixed (the default), free, or free in one dimension (free_x, free_y).
#' @export
#' 
plot_growth_matrix <- function(object, 
                               xlab = "Size after growth (mm)", 
                               ylab = "Probability", scales = "fixed", 
                               figure_dir = "figure/")
{
    data <- object@data
    mcmc <- object@mcmc
    
    bins <- data$size_midpoint_l

    tag <- data.frame(Liberty = data$cov_grow_liberty_g, Sex = data$cov_grow_morph_g, capture = data$data_grow_size_capture_g, recapture = data$data_grow_size_recapture_g) %>%
        dplyr::mutate(Size1 = bins[cut(capture, breaks = bins - 1, labels = FALSE)], Size2 = bins[cut(recapture, breaks = bins - 1, labels = FALSE)]) %>%
        dplyr::filter(Sex == 1, Liberty > 0.5, Liberty < 1.5) %>%
        dplyr::select(Size1, Size2)
    ttag <- table(tag)
    ttag <- ttag / rowSums(ttag)
    tm <- reshape2::melt(ttag)
    
    G <- mcmc$growth_itll
    dimnames(G) <- list(
        "Iteration" = 1:nrow(mcmc[[1]]), 
        "File" = 1:data$n_growth_subset, 
        "Season" = c("AW","SS"), 
        "Size2" = bins, "Size1" = bins)
    G <- reshape2::melt(G) %>%
        dplyr::mutate(Size1 = as.factor(Size1), File = as.factor(File))# %>%
        #dplyr::filter(Season == "AW", File == 1, Size1 %in% c(31,51,91))
    
    p <- ggplot(data = dplyr::filter(G, Season == "AW", value > 0), aes(x = Size2, y = value, colour = Size1, fill = Size1, group = Size1)) +
        stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.15, colour = NA) +
        stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "path", lwd = 0.5) +
        expand_limits(y = 0) +
        xlab(xlab) + ylab(ylab) +
        guides(colour = FALSE, fill = FALSE) +
        facet_wrap(File~.) +
        theme_lsd()
        #guides(colour = guide_legend("Size before growth (mm)")) + 
    ggsave(paste0(figure_dir, "growth_matrix_v1.png"), p, width=14)

    p <- ggplot(data = dplyr::filter(G, Season == "AW", value > 0), aes(x = Size2, y = value, colour = File, fill = File)) +
        geom_vline(aes(xintercept = as.numeric(as.character(Size1)))) +
        stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.15, colour = NA) +
        stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "path", lwd = 0.5) +
        expand_limits(y = 0) +
        xlab(xlab) + ylab(ylab) +
        facet_wrap(Size1~.) +
        theme_lsd()
    ggsave(paste0(figure_dir, "growth_matrix_v2.png"), p, width=14)

    ii <- sort(unique(as.numeric(as.character(G$Size1))))
    GG <- dplyr::filter(G, File == 1, Size1 %in% ii[(length(ii)-5):length(ii)], Season == "AW", value > 0)
    p <- ggplot(data = GG, aes(x = Size2, y = value, colour = File, fill = File)) +
        geom_vline(aes(xintercept = as.numeric(as.character(Size1)))) +
        stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.15, colour = NA) +
        stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "path", lwd = 0.5) +
        expand_limits(y = 0) +
        xlab(xlab) + ylab(ylab) +
        facet_wrap(Size1~.) +
        theme_lsd()
    ggsave(paste0(figure_dir, "growth_matrix_vs_empirical_males_v2.png"), p, width=14)
    
    GG <- dplyr::filter(G, File == 1, Season == "AW", value > 0)
    GG$Size1 <- factor(GG$Size1, levels = sort(as.numeric(as.character(unique(GG$Size1)))))
    p <- ggplot(data = GG, aes(x = Size2, y = value)) +
        geom_vline(aes(xintercept = as.numeric(as.character(Size1)))) +
        #stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.35, colour = NA) +
        stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "path", lwd = 1, colour = "grey") +
        geom_line(data = tm, colour = "red") +
        facet_wrap(Size1~.) +
        expand_limits(y = 0) +
        xlab(xlab) + ylab(ylab) +
        theme_lsd()
    ggsave(paste0(figure_dir, "growth_matrix_vs_empirical_males_v1.png"), p, width=14)
}
