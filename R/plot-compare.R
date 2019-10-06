#' Compare vulnerable biomass from multiple models
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
#' 
plot_compare_ssb <- function(object_list, object_names, figure_dir = "compare_figure/", save_plot = TRUE)
{
    data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
    mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)
    data <- data_list[[1]]

    years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
    pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_proj_yr)
    regions_list <- lapply(1:length(object_list), function(x) 1:data_list[[x]]$n_area)

    sb_list <- lapply(1:length(object_list), function(x) {
        n_iter <- nrow(mcmc_list[[x]][[1]])
        bio <- mcmc_list[[x]]$biomass_ssb_jyr
        dimnames(bio) <- list("Iteration" = 1:n_iter, "Rule" = 1, "Year" = pyears_list[[x]], "Region" = regions_list[[x]])
        bio2 <- reshape2::melt(bio) #%>% dplyr::filter(Year <= max(years_list[[x]]))
        bio2$Model <- object_names[x]
        return(bio2)
    })
    ssb <- data.frame(do.call(rbind, sb_list))
    ssb$Model <- factor(ssb$Model)
    ssb <- ssb %>% mutate(type="SSB")
    
    ssb0_list <- lapply(1:length(object_list), function(x) {
        n_iter <- nrow(mcmc_list[[x]][[1]])
        bio <- mcmc_list[[x]]$SSB0_r
        dimnames(bio) <- list("Iteration" = 1:n_iter, "Region" = regions_list[[x]])
        hl <- reshape2::melt(bio) %>%
            dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
            dplyr::group_by(Iteration, Region, value, Year) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Rule = 1, type = "Hard limit", value = value * 0.1)
        sl <- reshape2::melt(bio) %>%
            dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
            dplyr::group_by(Iteration, Region, value, Year) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Rule = 1, type = "Soft limit", value = value * 0.2)
        ssb0 <- reshape2::melt(bio) %>%
            dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
            dplyr::group_by(Iteration, Region, value, Year) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Rule = 1, type = "SSB0")
        # bio <- mcmc_list[[x]]$SSBref_jr
        # dimnames(bio) <- list("Iteration" = 1:n_iter, "Rule" = 1, "Region" = regions_list[[x]])
        # ref <- reshape2::melt(bio) %>%
        #     dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
        #     dplyr::group_by(Iteration, Region, Rule, value, Year) %>%
        #     dplyr::ungroup() #%>%
            # dplyr::mutate(type = "Target")
        bio2 <- rbind(ssb0, sl, hl) #%>% ##, ref)
           # dplyr::filter(Year <= max(years_list[[x]]))
        bio2$Model <- object_names[x]
        return(bio2)
    })
    ssb0 <- data.frame(do.call(rbind, ssb0_list))

    labs <- dplyr::filter(ssb0, Year == max(Year)) %>%
        dplyr::group_by(Year, type) %>%
        dplyr::summarise(value = mean(value))

    nmod <- length(unique(ssb$Model))
    years <- unique(unlist(years_list))
    pyears <- unique(unlist(pyears_list))

    p1 <- ggplot(ssb %>% dplyr::filter(Year %in% years), aes(x = Year, y = value)) +
        stat_summary(data = ssb0 %>% dplyr::filter(Year %in% years) %>% dplyr::filter(type == "Soft limit"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        stat_summary(data = ssb0 %>% dplyr::filter(Year %in% years) %>% dplyr::filter(type == "Soft limit"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
        stat_summary(data = ssb0 %>% dplyr::filter(Year %in% years) %>% dplyr::filter(type == "Hard limit"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        stat_summary(data = ssb0 %>% dplyr::filter(Year %in% years) %>% dplyr::filter(type == "Hard limit"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
        stat_summary(data = ssb0 %>% dplyr::filter(Year %in% years) %>% dplyr::filter(type == "SSB0"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        stat_summary(data = ssb0 %>% dplyr::filter(Year %in% years) %>% dplyr::filter(type == "SSB0"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
        # stat_summary(data = dplyr::filter(ssb0, type == "Target"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        # stat_summary(data = dplyr::filter(ssb0, type == "Target"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
        stat_summary(data = ssb %>% dplyr::filter(Year %in% years), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        stat_summary(data = ssb %>% dplyr::filter(Year %in% years), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
        geom_text(data = labs, aes(x = Year, y = value, label = type), nudge_x = -5) +
        expand_limits(y = 0) +
        labs(x = "Fishing year", y = "Spawning stock biomass (tonnes)") +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        scale_y_continuous(expand = c(0,0), limits = c(0, max(ssb0$value)*1.05)) +
        theme_lsd(base_size = 14)

    if(nmod > 5){
        p1 <- p1 +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else{
        p1 <- p1 + 
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }
    if (save_plot) {
      ggsave(paste0(figure_dir, "biomass_ssb_compare.png"), p1, width = 10)
    }

    p1 <- ggplot(ssb, aes(x = Year, y = value)) +
        geom_vline(aes(xintercept = max(years)+0.5), linetype=2) +
        stat_summary(data = ssb0 %>% dplyr::filter(type == "Soft limit"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        stat_summary(data = ssb0 %>% dplyr::filter(type == "Soft limit"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
        stat_summary(data = ssb0 %>% dplyr::filter(type == "Hard limit"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        stat_summary(data = ssb0 %>% dplyr::filter(type == "Hard limit"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
        stat_summary(data = ssb0 %>% dplyr::filter(type == "SSB0"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        stat_summary(data = ssb0 %>% dplyr::filter(type == "SSB0"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
        # stat_summary(data = dplyr::filter(ssb0, type == "Target"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        # stat_summary(data = dplyr::filter(ssb0, type == "Target"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
        stat_summary(data = ssb, fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        stat_summary(data = ssb, fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
        geom_text(data = labs, aes(x = Year, y = value, label = type), nudge_x = -5) +
        expand_limits(y = 0) +
        labs(x = "Fishing year", y = "Spawning stock biomass (tonnes)") +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        scale_y_continuous(expand = c(0,0), limits = c(0, max(ssb0$value)*1.05)) +
        theme_lsd(base_size = 14)

    if(nmod > 5){
        p1 <- p1 +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else{
        p1 <- p1 + 
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }
    if (save_plot) {
      ggsave(paste0(figure_dir, "biomass_ssb_compare_v2.png"), p1, width = 10)
    }

    relssb <- rbind(ssb, ssb0 %>% dplyr::filter(type=="SSB0")) %>%
        tidyr::spread(type, value) %>%
        dplyr::mutate(RelSSB = SSB/SSB0)

    labs_rel <- labs %>%
        dplyr::mutate(value = ifelse(type == "Hard limit", 0.1, ifelse(type == "Soft limit", 0.2, ifelse(type == "SSB0", 1, NA)))) %>%
        dplyr::filter(type!="SSB0")

    nmod <- length(unique(relssb$Model))

    relssb_next <- relssb %>% dplyr::filter(Year == max(years)+1)

    p <- ggplot(relssb_next) +
         theme_lsd(base_size=14) +
         theme(axis.text.x=element_blank()) +
         expand_limits(y = 0) +
         geom_hline(aes(yintercept = 0.2), col="gray") +
         geom_hline(aes(yintercept = 0.1), col="gray") +
         geom_text(data = labs_rel, aes(x = "base", y = value, label = type), nudge_x = nmod-1) +
         ylab("Terminal year relative spawning biomass") +
         xlab("Model") +
         scale_y_continuous(expand = c(0,0), limits = c(0, 1))
    if(nmod > 5){
        p <- p +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else{
        p <- p + 
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }
    if(max(relssb_next$Iteration)==1){
        p <- p + geom_point(aes(x = Model, y=RelSSB, fill = Model), cex=4, pch=21) +
                 geom_hline(data = relssb_next %>% filter(Model == "base"), aes(yintercept = unique(RelSSB)), linetype=2) 

    } else {
        p <- p + geom_violin(aes(x = Model, y=RelSSB, fill = Model)) +
                 geom_hline(data = relssb_next %>% filter(Model == "base"), aes(yintercept = median(RelSSB)), linetype=2)

    }
    if(save_plot) {
        ggsave(paste0(figure_dir, "relssb_nextyear_compare.png"), p, width=10)
    }

    relssb_next_proj <- relssb %>% dplyr::filter(Year %in% c(max(years)+1, max(pyears)))
    relssb_next_proj$Year <- factor(relssb_next_proj$Year)

    p <- ggplot(relssb_next_proj) +
         theme_lsd(base_size=14) +
         theme(axis.text.x=element_blank()) +
         expand_limits(y = 0) +
         geom_hline(aes(yintercept = 0.2), col="gray") +
         geom_hline(aes(yintercept = 0.1), col="gray") +
         geom_text(data = labs_rel, aes(x = "base", y = value, label = type), nudge_x = nmod-1) +
         ylab("Terminal year relative spawning biomass") +
         xlab("Model") +
         scale_y_continuous(expand = c(0,0), limits = c(0, 1)) +
         scale_alpha_manual(values = c(1, 0.5), guide=F)
    if(nmod > 5){
        p <- p +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else{
        p <- p + 
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }
    if(max(relssb_next_proj$Iteration)==1){
        p <- p + geom_point(aes(x = Model, y=RelSSB, fill = Model, alpha=Year), cex=4, pch=21) +
                 geom_hline(data = relssb_next_proj %>% filter(Model == "base") %>% filter(Year==max(years)+1), aes(yintercept = unique(RelSSB)), linetype=2) 

    } else {
        p <- p + geom_violin(aes(x = Model, y=RelSSB, fill = Model, alpha=Year)) +
                 geom_hline(data = relssb_next_proj %>% filter(Model == "base") %>% filter(Year==max(years)+1), aes(yintercept = median(RelSSB)), linetype=2)

    }
    if(save_plot) {
        ggsave(paste0(figure_dir, "relssb_nextyear_projyear_compare.png"), p, width=10)
    }

    p <- ggplot(relssb %>% dplyr::filter(Year %in% years)) +
         theme_lsd(base_size=14) +
         expand_limits(y = 0) +
         geom_hline(aes(yintercept = 0.2), col="gray") +
         geom_hline(aes(yintercept = 0.1), col="gray") +
         geom_text(data = labs_rel, aes(x = min(Year), y = value, label = type), nudge_x=-5) +
         stat_summary(fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(x = Year, y = RelSSB, fill = Model)) +
         stat_summary(fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(x = Year, y = RelSSB, color = Model)) +
         xlab("Year") + 
         ylab("Relative spawning biomass") +
         scale_y_continuous(expand = c(0,0), limits = c(0,1.05))
    if(nmod > 5){
        p <- p +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else{
        p <- p + 
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }
    if(save_plot){
        ggsave(paste0(figure_dir, "relssb_compare.png"), p, width=10)
    }

    p <- ggplot(relssb) +
         theme_lsd(base_size=14) +
         geom_vline(aes(xintercept = max(years)+0.5), linetype=2) +
         expand_limits(y = 0) +
         geom_hline(aes(yintercept = 0.2), col="gray") +
         geom_hline(aes(yintercept = 0.1), col="gray") +
         geom_text(data = labs_rel, aes(x = min(Year), y = value, label = type), nudge_x=-5) +
         stat_summary(fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(x = Year, y = RelSSB, fill = Model)) +
         stat_summary(fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(x = Year, y = RelSSB, color = Model)) +
         xlab("Year") + 
         ylab("Relative spawning biomass") +
         scale_y_continuous(expand = c(0,0), limits = c(0,1.05))
    if(nmod > 5){
        p <- p +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else{
        p <- p + 
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }
    if(save_plot){
        ggsave(paste0(figure_dir, "relssb_compare_v2.png"), p, width=10)
    }

    if (save_plot == FALSE){
        return(p1)
    }
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
#' 
plot_compare_vb <- function(object_list, object_names, figure_dir = "compare_figure/", save_plot = TRUE)
{
    data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
    mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)

    years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
    pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_proj_yr)
    regions_list <- lapply(1:length(object_list), function(x) 1:data_list[[x]]$n_area)
    sex <- c("Male","Immature female","Mature female")
    seasons <- c("AW","SS")
    YR <- "YR" # label for the season before the season change year

    vb_list <- lapply(1:length(object_list), function(x) {
        n_iter <- nrow(mcmc_list[[x]][[1]])

        bvuln_ytr <- mcmc_list[[x]]$biomass_vulnref_ytr
        dimnames(bvuln_ytr) <- list("Iteration" = 1:n_iter, "Year" = pyears_list[[x]], "Season" = seasons, "Region" = regions_list[[x]])
        bvuln_ytr2 <- reshape2::melt(bvuln_ytr) %>%
            dplyr::filter(value > 0) %>%
            dplyr::mutate(Season = as.character(Season), Season = ifelse(Year >= data_list[[x]]$season_change_yr, Season, YR)) %>%
            dplyr::filter(Season %in% c("YR","AW")) %>%
            #dplyr::filter(Year <= max(years_list[[x]])) %>%
            dplyr::group_by(Iteration, Year, Season) %>% 
            dplyr::summarise(value = sum(value))
        bvuln_ytr2$Model <- object_names[x]
        bvuln_ytr2$qconstant <- as.character(ifelse(grepl("qconstant",object_names[[x]]),1,0))
        return(bvuln_ytr2)
    })
    vb <- data.frame(do.call(rbind, vb_list))
    vb$Model <- factor(vb$Model)
    vb$qconstant <- factor(vb$qconstant)

    # n1 <- sapply(1:length(object_names), function(x) strsplit(object_names[x], "_")[[1]][1])
    # # n2 <- sapply(1:length(object_names), function(x) strsplit(object_names[x], paste0(n1[x],"_"))[[1]][2])
    # # n2[which(is.na(n2))] <- "base"

    # n2u <- unique(n1)
    # nm <- length(n2u)
    # if (nm > 2) cols <- brewer.pal(nm, "Set1")
    # if (nm == 2) cols <- c("tomato", "steelblue")
    # if (nm == 1) cols <- "tomato"

    # cols_all <- unlist(as.vector(sapply(1:nm, function(x) rep(cols[x],length(which(n1==n2u[x]))))))
    # names(cols_all) <- object_names
    # lty_all <- rep(1, length(object_names))
    # if (length(unique(n1)) > 1) lty_all[which(n1=="qconstant")] <- 3

    nmod <- length(unique(vb$Model))
    years <- unique(unlist(years_list))
    pyears <- unique(unlist(pyears_list))

    # Vulnerable biomass
    p <- ggplot(data = vb %>% dplyr::filter(Year %in% years), aes(x = Year, y = value, color = Model, fill = Model)) +
        stat_summary(data=vb %>% dplyr::filter(Year %in% years), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        #stat_summary(data=vb, fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
        stat_summary(data=vb %>% dplyr::filter(Year %in% years), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha=0.75) +
        # scale_fill_manual(values = cols_all, labels = object_names) +
        # scale_colour_manual(values = cols_all, labels = object_names) +
        # guides(colour = guide_legend(override.aes = list(colour = cols_all, linetype = lty_all))) + 
        # scale_linetype(guide=FALSE) +
        expand_limits(y = 0) +
        xlab("Fishing year") + ylab("Vulnerable reference biomass (tonnes)") +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd(base_size=14) +
        scale_y_continuous(expand = c(0,0), limits = c(0, max(vb$value)*1.05))
    if(nmod > 5){
        p <- p +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else{
        p <- p + 
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }
    if (save_plot) {
      ggsave(paste0(figure_dir, "biomass_vulnref_compare.png"), p, width = 10)
    }

    # Vulnerable biomass
    p <- ggplot(data = vb, aes(x = Year, y = value, color = Model, fill = Model)) +
        geom_vline(aes(xintercept = max(years) + 0.5), linetype=2) +
        stat_summary(data=vb, fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        #stat_summary(data=vb, fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
        stat_summary(data=vb, fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha=0.75) +
        # scale_fill_manual(values = cols_all, labels = object_names) +
        # scale_colour_manual(values = cols_all, labels = object_names) +
        # guides(colour = guide_legend(override.aes = list(colour = cols_all, linetype = lty_all))) + 
        # scale_linetype(guide=FALSE) +
        expand_limits(y = 0) +
        xlab("Fishing year") + ylab("Vulnerable reference biomass (tonnes)") +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd(base_size=14) +
        scale_y_continuous(expand = c(0,0), limits = c(0, max(vb$value)*1.05))
    if(nmod > 5){
        p <- p +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else{
        p <- p + 
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }
    if (save_plot) {
      ggsave(paste0(figure_dir, "biomass_vulnref_compare_v2.png"), p, width = 10)
    } else {
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
#' @export
#' 
plot_compare_recruitment <- function(object_list, object_names, figure_dir = "compare_figure/", save_plot = TRUE)
{
    data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
    mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)
    
    ny_list <- lapply(1:length(object_list), function(x) dim(mcmc_list[[x]]$recruits_ry)[3])
    years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
    pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:(data_list[[x]]$first_yr + ny_list[[x]] - 1))
    regions_list <- lapply(1:length(object_list), function(x) 1:data_list[[x]]$n_area)

    rec_list <- lapply(1:length(object_list), function(x){
        n_iter <- nrow(mcmc_list[[x]][[1]])
        recruits2 <- mcmc_list[[x]]$recruits_ry
        dimnames(recruits2) <- list("Iteration" = 1:n_iter, "Region" = regions_list[[x]], "Year" = pyears_list[[x]])
        recruits2 <- reshape2::melt(recruits2) %>%
           # dplyr::filter(Year <= max(years_list[[x]])) %>%
            dplyr::group_by(Iteration, Year) %>%
            dplyr::summarise(value = sum(value))

        recruits2$Model <- object_names[x]
        recruits2$qconstant <- as.character(ifelse(grepl("qconstant",object_names[[x]]),1,0))
        recruits2
    })
    recruits <- data.frame(do.call(rbind, rec_list)) %>%
        group_by(Iteration, Year, Model, qconstant) %>%
        summarise(value = sum(value))
    recruits$Model <- factor(recruits$Model)
    recruits$qconstant <- factor(recruits$qconstant)
    recruits$value <- recruits$value/1e6

    # plot recruitment
    xmin <- min(recruits$Year)
    xmax <- max(recruits$Year)	
    
    # n1 <- sapply(1:length(object_names), function(x) strsplit(object_names[x], "_")[[1]][1])
    # # n2 <- sapply(1:length(object_names), function(x) strsplit(object_names[x], paste0(n1[x],"_"))[[1]][2])
    # # n2[which(is.na(n2))] <- "base"
    
    # n2u <- unique(n1)
    # nm <- length(n2u)
    # if(nm > 2) cols <- brewer.pal(nm, "Set1")
    # if(nm == 2) cols <- c("tomato", "steelblue")
    # if(nm == 1) cols <- "tomato"
    
    # cols_all <- unlist(as.vector(sapply(1:nm, function(x) rep(cols[x],length(which(n1==n2u[x]))))))
    # names(cols_all) <- object_names
    # lty_all <- rep(1, length(object_names))
    # if (length(unique(n1)) > 1) lty_all[which(n1=="qconstant")] <- 3

    nmod <- length(unique(recruits$Model))
    years <- unique(unlist(years_list))
    pyears <- unique(unlist(pyears_list))

   p <- ggplot(data = recruits %>% dplyr::filter(Year %in% years), aes(x = Year, y = value, color = Model, fill = Model)) +
        stat_summary(fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        #stat_summary(fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
        stat_summary(fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha=0.75) +
        # scale_fill_manual(values = cols_all, labels = object_names) +
        # scale_colour_manual(values = cols_all, labels = object_names) +
        # guides(colour = guide_legend(override.aes = list(colour = cols_all, linetype = lty_all))) + 
        # scale_linetype(guide=FALSE) +
        expand_limits(y = 0) +
        xlab("Fishing year") + ylab("Recruitment (millions of individuals)") +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd(base_size=14) +
         scale_y_continuous(expand = c(0,0), limits = c(0, max(recruits$value)*1.05))
    if(nmod > 5){
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
      ggsave(paste0(figure_dir, "recruitment_compare.png"), p, width = 10)
   }

   p <- ggplot(data = recruits, aes(x = Year, y = value, color = Model, fill = Model)) +
        geom_vline(aes(xintercept = max(years) + 0.5), linetype=2) +
        stat_summary(fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        #stat_summary(fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
        stat_summary(fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha=0.75) +
        # scale_fill_manual(values = cols_all, labels = object_names) +
        # scale_colour_manual(values = cols_all, labels = object_names) +
        # guides(colour = guide_legend(override.aes = list(colour = cols_all, linetype = lty_all))) + 
        # scale_linetype(guide=FALSE) +
        expand_limits(y = 0) +
        xlab("Fishing year") + ylab("Recruitment (millions of individuals)") +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd(base_size=14) +
         scale_y_continuous(expand = c(0,0), limits = c(0, max(recruits$value)*1.05))
    if(nmod > 5){
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
plot_compare_selectivity <- function(object_list, object_names, figure_dir = "compare_figure/", save_plot=TRUE){
    data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
    mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)

    years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
    pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_proj_yr)
    sex <- c("Male","Immature female" , "Mature female")

    slist <- lapply(1:length(object_list), function(x){
        n_iter <- nrow(mcmc_list[[x]][[1]])
        n_season <- data_list[[x]]$n_season
        
        w <- data_list[[x]]$which_sel_rsyt
        dimnames(w) <- list("Region" = object_list[[x]]@regions, "Sex" = sex, "Year" = pyears_list[[x]], "Season" = 1:n_season)
        w <- reshape2::melt(w, value.name = "Selex")


        sel2 <- mcmc_list[[x]]$selectivity_ml
        dimnames(sel2) <- list("Iteration" = 1:n_iter, "Selex" = 1:data_list[[x]]$n_sel, "Size" = data_list[[x]]$size_midpoint_l)
        sel2 <- reshape2::melt(sel2, value.name = "Selectivity") %>%
            dplyr::inner_join(w, by = "Selex") %>%
            dplyr::filter(Year <= max(years_list[[x]])) %>%
            dplyr::mutate(Year = factor(Year)) %>%
            dplyr::mutate(Sex = ifelse(grepl("female", Sex), "Female", "Male")) %>%
            dplyr::distinct(Iteration, Sex, Size, Selectivity, Region, .keep_all = TRUE)
        sel2$Model <- object_names[[x]]

        return(sel2)
    })
    sel <- do.call(rbind, slist)

    nmod <- length(unique(sel$Model))

    ## if multiple seasons, regardless of year
    if(length(unique(sel$Season)) > 1){
      p <- ggplot(data = sel, aes(x = Size, y = Selectivity, col = Model, fill = Model, linetype = Season)) +
        stat_summary(data = sel, aes(x = Size, y = Selectivity, col = Model, linetype = Season), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(data = sel, aes(x = Size, y = Selectivity, col = Model, linetype = Season), fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(data = sel, aes(x = Size, y = Selectivity, col = Model, linetype = Season), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha=0.8) +
        scale_y_continuous(expand = c(0,0), limits = c(0, 1.05))
    }
    ## if multiple years and only one season
    if(length(unique(sel$Year)) > 1 & length(unique(sel$Season == 1))){
      p <- ggplot(data = sel, aes(x = Size, y = Selectivity, col = Model, fill = Model, linetype = Year)) +
        stat_summary(data = sel, aes(x = Size, y = Selectivity, col = Model, linetype = Year), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(data = sel, aes(x = Size, y = Selectivity, col = Model, linetype = Year), fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(data = sel, aes(x = Size, y = Selectivity, col = Model, linetype = Year), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha=0.8) +
        scale_y_continuous(expand = c(0,0), limits = c(0, 1.05))
    }
    if(nmod > 5){
        p <- p +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else{
        p <- p + 
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }

        
        if(length(unique(sel$Year))==1 | length(unique(sel$Season))==1) p <- p + guides(linetype = FALSE)

        areas <- unique(sapply(1:length(data_list), function(x) data_list[[x]]$n_area))
        if (length(areas) > 1) {
          if(length(unique(sel$Season))>1 & length(unique(sel$Year))>1){
            p <- p + facet_grid(Region + Year ~ Sex)
          } else {
            p <- p + facet_grid(Region ~ Sex)
          }
        } else {
          if(length(unique(sel$Season))>1 & length(unique(sel$Year))>1){
            p <- p + facet_grid(Year ~ Sex)
          } else {
            p <- p + facet_grid( ~ Sex)
          }
        }
        
        p <- p + #scale_x_continuous(breaks = seq(30, 90, 10)) +
            expand_limits(y = c(0, 1)) +
            xlab("Length bin") +
            theme_lsd(base_size=14)

   if (save_plot) {
      ggsave(paste0(figure_dir, "selectivity_compare.png"), p, width = 10)
   } else {
     return(p)
   }
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
    q_info_list <- lapply(1:length(object_list), function(x) data.frame("qtype"=data_list[[x]]$data_cpue_q_i, "Season" = data_list[[x]]$data_cpue_season_i, "Year" = data_list[[x]]$data_cpue_year_i, "Region" = data_list[[x]]$data_cpue_area_i))
    q_info_list <- lapply(1:length(object_list), function(x){
        if (max(q_info_list[[x]]$qtype) < maxq & max(q_info_list[[x]]$Year) == max(unlist(sapply(1:length(object_list), function(x) q_info_list[[x]]$Year)))){
            subq <- q_info_list[[x]]$qtype
            max <- max(subq)
            for (i in 1:length(subq)) {
                if (subq[i]==max) {
                    subq[i] <- maxq
                    next
                }
                if (subq[i]!=max) subq[i] <- subq[i] + (maxq - max) 
            }
            q_info_list[[x]]$qtype <- subq
        }
        q_info_list[[x]] %>%
            dplyr::filter(Season == 1) %>%
            dplyr::mutate(QY = paste(Year, qtype))
    })

    q_list <- lapply(1:length(object_list), function(x) {
        n_iter <- nrow(mcmc_list[[x]][[1]])
        q2 <- mcmc_list[[x]]$par_q_cpue_qy
        dimnames(q2) <- list("Iteration" = 1:n_iter, "qtype"=1:nq_list[[x]], "Year" = pyears_list[[x]])
        q2 <- reshape2::melt(q2)

        if(max(q2$qtype) < maxq) {
            subq <- q2$qtype
            max <- max(subq)
            for (i in 1:length(subq)) {
                if (subq[i]==max){
                    subq[i] <- maxq
                    next
                }
                if(subq[i]!=max) subq[i] <- subq[i] + (maxq - max)             }
            q2$qtype <- subq
        }

        q2 <- q2 %>%
           # dplyr::filter(Year <= max(years_list[[x]])) %>%
            dplyr::filter(Year %in% unique(q_info_list[[x]]$Year)) %>%
            dplyr::mutate(QY = paste(Year, qtype)) %>%
            dplyr::filter(QY %in% q_info_list[[x]]$QY)
        
        q2$Model <- object_names[x]
        q2$qconstant <- as.character(ifelse(grepl("qdrift",object_names[[x]]),0,1))
        # if (data_list[[x]]$n_area > 1 & "Region" %in% colnames(q2) == FALSE) q2$Region <- "All regions"
        return(q2)
    })
    
    q <- data.frame(do.call(rbind, q_list)) %>%
        group_by(Iteration, Year, Model, qconstant, qtype, QY)
    q$Model <- factor(q$Model)
    q$qconstant <- factor(q$qconstant)


    # n1 <- sapply(1:length(object_names), function(x) strsplit(object_names[x], "_")[[1]][1])
    # # n2 <- sapply(1:length(object_names), function(x) strsplit(object_names[x], paste0(n1[x],"_"))[[1]][2])
    # # n2[which(is.na(n2))] <- "base"

    # n2u <- unique(n1)
    # nm <- length(n2u)
    # if(nm > 2) cols <- brewer.pal(nm, "Set1")
    # if(nm == 2) cols <- c("tomato", "steelblue")
    # if(nm == 1) cols <- "tomato"

    # cols_all <- unlist(as.vector(sapply(1:nm, function(x) rep(cols[x],length(which(n1==n2u[x]))))))    
    # names(cols_all) <- object_names
    # lty_all <- rep(1, length(object_names))
    # if (length(unique(n1)) > 1) lty_all[which(n1 == "qconstant")] <- 3

    nmod <- length(unique(q$Model))
    years <- unique(unlist(years_list))
    pyears <- unique(unlist(pyears_list))

    p <- ggplot(data = q %>% dplyr::filter(Year %in% years), aes(x = Year, y = value, colour=Model, fill=Model)) +
        stat_summary(fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        # stat_summary(fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
        stat_summary(fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha=0.75) +
        # scale_fill_manual(values=cols_all, labels=object_names) +
        # scale_colour_manual(values=cols_all, labels=object_names) +
        # guides(colour = guide_legend(override.aes = list(colour = cols_all, linetype = lty_all))) + 
        # scale_linetype(guide=FALSE) +
        expand_limits(y = 0) +
        xlab("Fishing year") + ylab("Catchability coefficient (q)") +
        # scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd() +
        facet_wrap(~qtype, scales = "free")
    if(nmod > 5){
        p <- p +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else{
        p <- p + 
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }    
    if (data_list[[1]]$n_area > 1) {
        p <- p + facet_wrap(~Region)
    }
    if (save_plot) {
      ggsave(paste0(figure_dir, "q_y_compare.png"), p, width = 10)
    } else {
      return(p)
    }
}

#' Table comparing residuals for various data types across models
#' 
#' @param object_list list of 'lsd.rds' files from multiple models
#' @param object_names vector of model names associated with each of the output files in object_list
#' @param figure_dir the directory to save to
#' @import dplyr
#' @importFrom reshape2 melt
#' @export
#' 
table_compare_residuals <- function(object_list, object_names, figure_dir = "compare_figure/")
{


  rlist <- lapply(1:length(object_list), function(x){
    res <- table_residuals(object = object_list[[x]], figure_dir = figure_dir, save_table = FALSE)
    res <- res %>% mutate("model"=object_names[[x]])
    return(res)
  })
  rdf <- do.call(rbind, rlist)
  rdf2 <- rdf %>% 
        tidyr::pivot_longer(-c(model,data), names_to = "residual_type", values_to="value") %>%
        tidyr::pivot_wider(names_from = model)

  write.csv(rdf2, file = file.path(figure_dir, "Residual_summaries.csv"), row.names=FALSE)

}