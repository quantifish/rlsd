#' Plot Offset CPUE
#'
#' Plot the offset CPUE data and fit to the data.
#'
#' @export
#' 
plot_offset_cpue <- function(object,
                             scales = "fixed",
                             xlab = "Offset year (October-September)",
                             ylab = "Offset year CPUE (kg/potlift)",
                             figure_dir = "figure/")
{
    data <- object@data
    map <- object@map
    mcmc <- object@mcmc
    
    n_iter <- nrow(mcmc[[1]])
    n_area <- data$n_area
    seasons <- c("AW","SS")
    years <- data$first_yr:data$last_proj_yr
    
    obs_offset <- data$data_offset_cpue_ry
    dimnames(obs_offset) <- list(Region = 1:n_area, Year = data$data_offset_cpue_year_i)
    obs_offset <- reshape2::melt(obs_offset) %>%
        dplyr::mutate(Iteration = NA, Data = "Observed")
    
    if (length(map) > 0) {
        map_offset <- map$mp_offset_cpue_ry
        dimnames(map_offset) <- list(Iteration = 1, Region = 1:n_area, Year = years)
        map_offset <- reshape2::melt(map_offset) %>%
            dplyr::mutate(Data = "Expected") %>%
            dplyr::filter(Year >= min(obs_offset$Year))
    } else {
        map_offset <- NULL
    }

    if (length(mcmc) > 0) {
        mcmc_offset <- mcmc$mp_offset_cpue_ry
        dimnames(mcmc_offset) <- list(Iteration = 1:n_iter, Region = 1:n_area, Year = data$first_yr:data$last_proj_yr)
        mcmc_offset <- reshape2::melt(mcmc_offset) %>%
            dplyr::mutate(Data = "Expected") %>%
            dplyr::filter(Year >= min(obs_offset$Year))
    } else {
        mcmc_offset <- NULL
    }
    
    p <- ggplot(data = obs_offset) +
        expand_limits(y = 0) +
        xlab(xlab) + ylab(ylab) +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd()
    if (!is.null(mcmc_offset)) {
        p <- p + stat_summary(data = mcmc_offset, aes(x = Year, y = value), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
            stat_summary(data = mcmc_offset, aes(x = Year, y = value), fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
            stat_summary(data = mcmc_offset, aes(x = Year, y = value), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1)
    }
    if (!is.null(map_offset)) {
        p <- p + geom_line(data = map_offset, aes(x = Year, y = value), linetype = 2)
    }
    if (data$n_area > 1) {
        p <- p + facet_wrap(~ Region, scales = scales)
    }
    p <- p + geom_point(data = obs_offset, aes(x = Year, y = value), color = "red", alpha = 0.75)
    p
    ggsave(paste0(figure_dir, "cpue_offset.png"), p, width = 12)
}


#' Plot CPUE
#'
#' Plot the CPUE data and fit to the data.
#'
#' @export
#' 
plot_cpue <- function(object,
                      scales = "fixed",
                      xlab = "Fishing year",
                      ylab = "CPUE",
                      figure_dir = "figure/")
{
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
        pq1 <- reshape2::melt(pq1, value.name = "q") %>%
          dplyr::filter(Year <= max(years))
    
        pcpue1 <- map$pred_cpue_i
        dimnames(pcpue1) <- list("Iteration" = 1, "I" = 1:data$n_cpue)
        pcpue1 <- reshape2::melt(pcpue1, value.name = "CPUE") %>%
            dplyr::select(Iteration, CPUE) %>%
            dplyr::mutate(Data = "Expected", Type = "CPUE", Region = data$data_cpue_area_i, Year = data$data_cpue_year_i, Season = seasons[data$data_cpue_season_i])  %>%
            dplyr::full_join(pq1, by = c("Iteration", "Year")) %>%
            dplyr::select(-q)
        pcpue1$SD <- NA

        rcpue1 <- map$resid_cpue_i
        dimnames(rcpue1) <- list("Iteration" = 1, "I" = 1:data$n_cpue)
        rcpue1 <- reshape2::melt(rcpue1, value.name = "CPUE") %>%
            dplyr::select(Iteration, CPUE) %>%
            dplyr::mutate(Data = "Residual", Type = "CPUE", Region = data$data_cpue_area_i, Year = data$data_cpue_year_i, Season = seasons[data$data_cpue_season_i]) %>%
            dplyr::full_join(pq1, by = c("Iteration", "Year")) %>%
            dplyr::select(-q)
    } else {
        pcpue1 <- NULL
        rcpue1 <- NULL
    }

    if (length(mcmc) > 0) {
        pq <- mcmc$par_q_cpue_qy
        dimnames(pq) <- list("Iteration" = 1:n_iter, "qtype" = 1:n_q, "Year" = pyears)
        pq <- reshape2::melt(pq, value.name = "q") %>%
          dplyr::filter(Year <= max(years))

        pcpue <- mcmc$pred_cpue_i
        dimnames(pcpue) <- list("Iteration" = 1:n_iter, "I" = 1:data$n_cpue)
        pcpue <- reshape2::melt(pcpue, value.name = "CPUE") %>%
            dplyr::select(Iteration, CPUE) %>%
            dplyr::mutate(Data = "Expected", Type = "CPUE", Region = rep(data$data_cpue_area_i, each = n_iter), Year = rep(data$data_cpue_year_i, each = n_iter), Season = seasons[rep(data$data_cpue_season_i, each = n_iter)]) %>%
            dplyr::full_join(pq, by = c("Iteration", "Year")) %>%
            dplyr::select(-q)
        pcpue$SD <- NA

        rcpue <- mcmc$resid_cpue_i
        dimnames(rcpue) <- list("Iteration" = 1:n_iter, "I" = 1:data$n_cpue)
        rcpue <- reshape2::melt(rcpue, value.name = "CPUE") %>%
            dplyr::select(Iteration, CPUE) %>%
            dplyr::mutate(Data = "Residual", Type = "CPUE", Region = rep(data$data_cpue_area_i, each = n_iter), Year = rep(data$data_cpue_year_i, each = n_iter), Season = seasons[rep(data$data_cpue_season_i, each = n_iter)]) %>%
            dplyr::full_join(pq, by=c("Iteration", "Year")) %>%
            dplyr::select(-q)
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

    if (!is.null(pcpue)) {
        pcpue <- dplyr::mutate(pcpue, QY = paste(Year, qtype))
        ocpue <- dplyr::mutate(ocpue, QY = paste(Year, qtype))
        rcpue <- dplyr::mutate(rcpue, QY = paste(Year, qtype))
        pcpue <- dplyr::filter(pcpue, QY %in% ocpue$QY)
        rcpue <- dplyr::filter(rcpue, QY %in% ocpue$QY)
    }
    if (!is.null(pcpue1)) {
        pcpue1 <- pcpue1 %>%
            dplyr::mutate(QY = paste(Year, qtype)) %>%
            dplyr::filter(QY %in% ocpue$QY)
    }
    if (!is.null(rcpue1)) {
        rcpue1 <- rcpue1 %>%
            dplyr::mutate(QY = paste(Year, qtype)) %>%
            dplyr::filter(QY %in% ocpue$QY)
    }


    ocpue <- ocpue %>%
        dplyr::mutate("Region_q" = paste("Region", Region, "q", qtype)) %>%
        dplyr::mutate(qtype = as.character(qtype))
    ocpue$qtype <- factor(ocpue$qtype, levels = rev(unique(ocpue$qtype)))

    pcpue <- pcpue %>% dplyr::mutate("Region_q" = paste("Region", Region, "q", qtype)) %>%
        dplyr::filter(Region_q %in% ocpue$Region_q) %>%
        dplyr::mutate(qtype = as.character(qtype))
    pcpue$qtype <- factor(pcpue$qtype, levels = rev(unique(pcpue$qtype)))

    rcpue <- rcpue %>% dplyr::mutate("Region_q" = paste("Region", Region, "q", qtype)) %>%
        dplyr::filter(Region_q %in% ocpue$Region_q) %>%
        dplyr::mutate(qtype = as.character(qtype))
    rcpue$qtype <- factor(rcpue$qtype, levels = rev(unique(rcpue$qtype)))

    if(!is.null(pcpue1)){
        pcpue1 <- pcpue1 %>% dplyr::mutate("Region_q" = paste("Region", Region, "q", qtype)) %>%
            dplyr::filter(Region_q %in% ocpue$Region_q) %>%
            dplyr::mutate(qtype = as.character(qtype))
        pcpue1$qtype <- factor(pcpue1$qtype, levels = rev(unique(pcpue1$qtype)))    
    }
    if(!is.null(rcpue1)){
        rcpue1 <- rcpue1 %>% dplyr::mutate("Region_q" = paste("Region", Region, "q", qtype)) %>%
            dplyr::filter(Region_q %in% ocpue$Region_q) %>%
            dplyr::mutate(qtype = as.character(qtype))
        rcpue1$qtype <- factor(rcpue1$qtype, levels = rev(unique(rcpue1$qtype)))
    }

    # Plot CPUE
    p <- ggplot(data = ocpue) +
        geom_point(aes(x = Year, y = CPUE), color = "red", alpha = 0.75) +
        geom_linerange(aes(x = Year, ymin = exp(log(CPUE) - SD), ymax = exp(log(CPUE) + SD)), color = "red", alpha = 0.75) +
        #expand_limits(y = 0) +
        xlab(xlab) + ylab(ylab) +
        theme_lsd()
    if (!is.null(pcpue)) {
        p <- p + stat_summary(data = dplyr::filter(pcpue, pcpue$QY %in% ocpue$QY), aes(x = Year, y = CPUE), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
            stat_summary(data = dplyr::filter(pcpue, pcpue$QY %in% ocpue$QY), aes(x = Year, y = CPUE), fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
            stat_summary(data = dplyr::filter(pcpue, pcpue$QY %in% ocpue$QY), aes(x = Year, y = CPUE), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1)
    }
    if (!is.null(pcpue1)) {
        p <- p + geom_line(data = pcpue1, aes(x = Year, y = CPUE), linetype = 2)
    }
    if (data$n_area > 1) {
        p <- p + facet_wrap(Season ~ Region_q, scales = "free", ncol = data$n_area)
        ggsave(paste0(figure_dir, "cpue.png"), p, width = 9, height = 10)
    } else {
        p <- p + facet_wrap(qtype ~ Season, scales = "free", ncol = 2)
        ggsave(paste0(figure_dir, "cpue.png"), p, height = 9)
    }

    
    # CPUE residuals
    p <- ggplot(rcpue) +
        geom_hline(yintercept = 0, alpha = 0.2) +
        # expand_limits(y = 0) +
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
        p <- p + facet_wrap(Season ~ Region_q, scales = "free", ncol = data$n_area)
    } else {
        p <- p + facet_wrap(qtype ~ Season, scales = "free", ncol = 2)
    }
    ggsave(paste0(figure_dir, "cpue_resid.png"), p, height = 9, width=9)
}


#' Plot offset year CPUE linear model
#'
#' Plot the offset year CPUE vs the mean of the AW CPUE during year y and SS CPUE during year y-1.
#'
#' @export
#' 
plot_offset_cpue_lm <- function(object, figure_dir = "figure/") {
    data <- object@data
    mcmc <- object@mcmc

    xx1 <- mcmc$mp_pred_offset_cpue_ry[1,1,2:dim(mcmc$mp_pred_offset_cpue_ry)[3]] # y is n_offset_cpue
    yy1 <- data$data_offset_cpue_ry[1,]
    yr <- data$data_offset_cpue_year_i
    dd1 <- data.frame(yr = yr, x = xx1, y = yy1)

    xx2 <- log(seq(0, max(xx1) * 1.1, 0.0001))
    params <- mcmc$mp_offset_cpue_pars_ri[1,1,]
    dd2 <- data.frame(x = exp(xx2),
                      y = exp(params[1] + xx2 * params[2]),
                      yerr = exp(params[1] + xx2 * params[2] + rnorm(length(xx2), 0, params[3])))
    
    p <- ggplot(dd1, aes(x, y)) +
        geom_point(data = dd2, aes(x, yerr), colour = "grey", alpha = 0.25) +
        geom_smooth(method = "lm", se = FALSE, colour = "green", size = 0.5) +
        geom_line(data = dd2, aes(x, y), colour = "red") +
        geom_text(aes(label = yr, colour = yr)) +
        expand_limits(x = 0, y = 0) +
        coord_fixed() +
        theme_lsd() +
        theme(legend.position = "none") +
        ggtitle(bquote(R^2 == .(round(params[5], 2)))) +
        labs(x = "Mean(AW CPUE[y], SS CPUE[y-1]) (kg/potlift)", y = "Offset year CPUE (kg/potlift)")
    
    ggsave(paste0(figure_dir, "lm_offset_cpue.png"), p)
}


#' Plot offset year CPUE linear model
#'
#' Plot the offset year CPUE vs the mean of the AW CPUE during year y and SS CPUE during year y-1.
#'
#' @export
#' 
plot_aw_cpue_lm <- function(object, figure_dir = "figure/") {
    logit <- function() { log(x / (1 - x)) }
    inv_logit <- function(x) { 1 / (1 + exp(-x)) }
    
    data <- object@data
    mcmc <- object@mcmc
    
    yr <- data$data_aw_cpue_year_i
    yy1 <- data.frame(yr = yr, py = mcmc$mp_proportion_catch_aw_ry[1,1,])
    xx1 <- data.frame(yr = yr, px = mcmc$mp_pred_aw_cpue_ry[1,1,])
    dd1 <- dplyr::inner_join(yy1, xx1)

    xx2 <- seq(0, max(xx1$px) * 1.1, 0.0001)
    params <- mcmc$mp_split_catch_pars_ri[1,1,]
    dd2 <- data.frame(x = xx2,
                      y = inv_logit(params[1] + xx2 * params[2]),
                      yerr = inv_logit(params[1] + xx2 * params[2] + rnorm(length(xx2), 0, params[3])))
    
    p <- ggplot(data = dd1, aes(x = px, y = py)) +
        geom_point(data = dd2, aes(x, yerr), colour = "grey", alpha = 0.25) +
        geom_smooth(method = "lm", se = FALSE, colour = "green", size = 0.5) +
        geom_line(data = dd2, aes(x, y), colour = "red") +
        geom_text(aes(label = yr, colour = yr)) +
        expand_limits(x = 0, y = c(0,1)) +
        coord_fixed() +
        theme_lsd() +
        labs(x = "Standardised AW CPUE (kg/potlift)", y = "Proportion AW") +
        ggtitle(bquote(R^2 == .(round(params[5], 2)))) +
        theme(legend.position = "none")
    
    ggsave(paste0(figure_dir, "lm_aw_cpue.png"), p)
}