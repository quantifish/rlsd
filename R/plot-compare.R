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
plot_compare_ssb <- function(object_list, object_names, figure_dir = "compare_figure/", save_plot = TRUE) {
    data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
    mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)
    data <- data_list[[1]]

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
        bio2 <- reshape2::melt(bio) %>%
            group_by(Iteration, Year, Rule) %>%
            summarise(value = sum(value))
        bio2$Model <- object_names[x]
        return(bio2)
    })
    ssb <- data.frame(do.call(rbind, sb_list))
    ssb <- ssb %>% 
        mutate(Model = factor(Model), type = "SSB")

    ssb0_list <- lapply(1:length(object_list), function(x) {
        n_iter <- nrow(mcmc_list[[x]][[1]])
        bio <- mcmc_list[[x]]$SSB0_r
        dimnames(bio) <- list("Iteration" = 1:n_iter, "Region" = regions_list2[[x]])
        hl <- reshape2::melt(bio) %>%
            left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
            filter(Region != "Total") %>%
            group_by(Iteration, Year) %>%
            summarise(value = sum(value)) %>%
            ungroup() %>%
            mutate(Rule = 1, type = "Hard limit", value = value * 0.1)
        sl <- reshape2::melt(bio) %>%
            left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
            filter(Region != "Total") %>%
            group_by(Iteration, Year) %>%
            summarise(value = sum(value)) %>%
            ungroup() %>%
            mutate(Rule = 1, type = "Soft limit", value = value * 0.2)
        ssb0 <- reshape2::melt(bio) %>%
            left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
            filter(Region != "Total") %>%
            group_by(Iteration, Year) %>%
            summarise(value = sum(value)) %>%
            ungroup() %>%
            mutate(Rule = 1, type = "SSB0")
        # bio <- mcmc_list[[x]]$SSBref_jr
        # dimnames(bio) <- list("Iteration" = 1:n_iter, "Rule" = 1, "Region" = regions_list[[x]])
        # ref <- reshape2::melt(bio) %>%
        #     left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
        #     group_by(Iteration, Region, Rule, value, Year) %>%
        #     ungroup() #%>%
            # mutate(type = "Target")
        bio2 <- rbind(ssb0, sl, hl) #%>% ##, ref)
           # filter(Year <= max(years_list[[x]]))
        bio2$Model <- object_names[x]
        return(bio2)
    })
    ssb0 <- data.frame(do.call(rbind, ssb0_list))

    labs <- ssb0 %>%
        filter(Year == max(Year)) %>%
        group_by(Year, type) %>%
        summarise(value = mean(value))

    nmod <- length(unique(ssb$Model))
    years <- unique(unlist(years_list))
    pyears <- unique(unlist(pyears_list))

    mods <- unique(ssb$Model)
    mod_num <- sapply(1:length(mods), function(m) as.numeric(strsplit(as.character(mods[m]),"_")[[1]][1]))
    ssb$Model <- factor(ssb$Model, levels = unique(mods)[order(mod_num)])
    ssb0$Model <- factor(ssb0$Model, levels = unique(mods)[order(mod_num)])
    p1 <- ggplot(ssb %>% filter(Year %in% years), aes(x = Year, y = value)) +
        stat_summary(data = ssb0 %>% filter(Year %in% years) %>% filter(type == "Soft limit"), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        stat_summary(data = ssb0 %>% filter(Year %in% years) %>% filter(type == "Soft limit"), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
        stat_summary(data = ssb0 %>% filter(Year %in% years) %>% filter(type == "Hard limit"), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        stat_summary(data = ssb0 %>% filter(Year %in% years) %>% filter(type == "Hard limit"), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
        stat_summary(data = ssb0 %>% filter(Year %in% years) %>% filter(type == "SSB0"), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        stat_summary(data = ssb0 %>% filter(Year %in% years) %>% filter(type == "SSB0"), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
        # stat_summary(data = filter(ssb0, type == "Target"), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        # stat_summary(data = filter(ssb0, type == "Target"), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
        stat_summary(data = ssb %>% filter(Year %in% years), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        stat_summary(data = ssb %>% filter(Year %in% years), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
        geom_text(data = labs, aes(x = Year, y = value, label = type), nudge_x = -5) +
        expand_limits(y = 0) +
        labs(x = "Fishing year", y = "Spawning stock biomass (tonnes)") +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        # scale_y_continuous(expand = c(0,0), limits = c(0, max(ssb0$value)*1.05)) +
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
        stat_summary(data = ssb0 %>% filter(type == "Soft limit"), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        stat_summary(data = ssb0 %>% filter(type == "Soft limit"), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
        stat_summary(data = ssb0 %>% filter(type == "Hard limit"), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        stat_summary(data = ssb0 %>% filter(type == "Hard limit"), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
        stat_summary(data = ssb0 %>% filter(type == "SSB0"), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        stat_summary(data = ssb0 %>% filter(type == "SSB0"), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
        # stat_summary(data = filter(ssb0, type == "Target"), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        # stat_summary(data = filter(ssb0, type == "Target"), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
        stat_summary(data = ssb, fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Model)) +
        stat_summary(data = ssb, fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Model)) +
        geom_text(data = labs, aes(x = Year, y = value, label = type), nudge_x = -5) +
        expand_limits(y = 0) +
        labs(x = "Fishing year", y = "Spawning stock biomass (tonnes)") +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        # scale_y_continuous(expand = c(0,0), limits = c(0, max(ssb0$value)*1.05)) +
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

    relssb <- rbind(ssb, ssb0 %>% filter(type=="SSB0")) %>%
        tidyr::spread(type, value) %>%
        mutate(RelSSB = SSB/SSB0)

    labs_rel <- labs %>%
        mutate(value = ifelse(type == "Hard limit", 0.1, ifelse(type == "Soft limit", 0.2, ifelse(type == "SSB0", 1, NA)))) %>%
        filter(type!="SSB0")

    nmod <- length(unique(relssb$Model))

    relssb_next <- relssb %>% filter(Year == max(years)+1)

    p <- ggplot(relssb_next) +
         theme_lsd(base_size=14) +
         theme(axis.text.x=element_blank()) +
         expand_limits(y = 0) +
         geom_hline(aes(yintercept = 0.2), col="gray") +
         geom_hline(aes(yintercept = 0.1), col="gray") +
         geom_text(data = labs_rel, aes(x = "base", y = value, label = type)) +
         ylab("Terminal year relative spawning biomass") +
         xlab("Model") #+
         # scale_y_continuous(expand = c(0,0), limits = c(0, 1))
    if (nmod > 5) {
        p <- p +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else {
        p <- p +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }
    if (max(relssb_next$Iteration) == 1) {
        p <- p + geom_point(aes(x = Model, y=RelSSB, fill = Model), cex=4, pch=21) +
                 geom_hline(data = relssb_next %>% filter(Model == "base"), aes(yintercept = unique(RelSSB)), linetype=2)

    } else {
        p <- p + geom_violin(aes(x = Model, y=RelSSB, fill = Model)) +
                 geom_hline(data = relssb_next %>% filter(Model == "base"), aes(yintercept = median(RelSSB)), linetype=2)

    }
    if(save_plot) {
        ggsave(paste0(figure_dir, "relssb_nextyear_compare.png"), p, width=10)
    }

    relssb_next_proj <- relssb %>% filter(Year %in% c(max(years) + 1, max(pyears)))
    relssb_next_proj$Year <- factor(relssb_next_proj$Year)

    p <- ggplot(relssb_next_proj) +
         theme_lsd(base_size=14) +
         theme(axis.text.x=element_blank()) +
         expand_limits(y = 0) +
         geom_hline(aes(yintercept = 0.2), col="gray") +
         geom_hline(aes(yintercept = 0.1), col="gray") +
         geom_text(data = labs_rel, aes(x = "base", y = value, label = type)) +
         ylab("Terminal year relative spawning biomass") +
         xlab("Model") +
         # scale_y_continuous(expand = c(0,0), limits = c(0, 1)) +
         scale_alpha_manual(values = c(1, 0.5), guide=F)
    if (nmod > 5) {
        p <- p +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else {
        p <- p +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }
    if (max(relssb_next_proj$Iteration) == 1) {
        p <- p + geom_point(aes(x = Model, y=RelSSB, fill = Model, alpha=Year), cex=4, pch=21) +
                 geom_hline(data = relssb_next_proj %>% filter(Model == "base") %>% filter(Year==max(years)+1), aes(yintercept = unique(RelSSB)), linetype=2)

    } else {
        p <- p + geom_violin(aes(x = Model, y=RelSSB, fill = Model, alpha=Year)) +
                 geom_hline(data = relssb_next_proj %>% filter(Model == "base") %>% filter(Year==max(years)+1), aes(yintercept = median(RelSSB)), linetype=2)

    }
    if (save_plot) {
        ggsave(paste0(figure_dir, "relssb_nextyear_projyear_compare.png"), p, width=10)
    }

    p <- ggplot(relssb %>% filter(Year %in% years)) +
         theme_lsd(base_size=14) + 
         expand_limits(y = 0) +
         geom_hline(aes(yintercept = 0.2), col="gray") +
         geom_hline(aes(yintercept = 0.1), col="gray") +
         geom_text(data = labs_rel, aes(x = (min(Year) - 10), y = value, label = type)) +
         stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(x = Year, y = RelSSB, fill = Model)) +
         stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(x = Year, y = RelSSB, color = Model)) +
         xlab("Year") +
         ylab("Relative spawning biomass") #+
         # scale_y_continuous(expand = c(0,0), limits = c(0,1.05))
    if (nmod > 5) {
        p <- p +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else {
        p <- p +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }
    if (save_plot){
        ggsave(paste0(figure_dir, "relssb_compare.png"), p, width = 10)
    }

    p <- ggplot(relssb) +
         theme_lsd(base_size=14) +
         geom_vline(aes(xintercept = max(years)+0.5), linetype = 2) +
         expand_limits(y = 0) +
         geom_hline(aes(yintercept = 0.2), col = "gray") +
         geom_hline(aes(yintercept = 0.1), col = "gray") +
         geom_text(data = labs_rel, aes(x = min(Year)-10, y = value, label = type)) +
         stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(x = Year, y = RelSSB, fill = Model)) +
         stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(x = Year, y = RelSSB, color = Model)) +
         xlab("Year") +
         ylab("Relative spawning biomass") #+
         # scale_y_continuous(expand = c(0,0), limits = c(0,1.05))
    if (nmod > 5) {
        p <- p +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else {
        p <- p +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }
    if (save_plot) {
        ggsave(paste0(figure_dir, "relssb_compare_v2.png"), p, width=10)
    }

    if (save_plot == FALSE) {
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
    regions_list2 <- lapply(1:length(object_list), function(x) {
      if (length(regions_list[[x]]) == 1) out <- regions_list[[x]]
      if (length(regions_list[[x]]) > 1) out <- c(regions_list[[x]], "Total")
      return(out)
    })
    rules_list <- lapply(1:length(object_list), function(x) 1:data_list[[x]]$n_rules)
    sex <- c("Male", "Immature female", "Mature female")
    seasons <- c("AW", "SS")
    YR <- "YR" # label for the season before the season change year

    vb_list <- lapply(1:length(object_list), function(x) {
        n_iter <- nrow(mcmc_list[[x]][[1]])

        if("biomass_vulnref_jytr" %in% names(mcmc_list[[x]])){
            bvuln_ytr <- mcmc_list[[x]]$biomass_vulnref_jytr
            dimnames(bvuln_ytr) <- list("Iteration" = 1:n_iter, "Rules" = 1:rules_list[[x]], "Year" = pyears_list[[x]], "Season" = seasons, "Region" = regions_list[[x]])
            bvuln_ytr2 <- reshape2::melt(bvuln_ytr) %>%
                filter(value > 0) %>%
                mutate(Season = as.character(Season), Season = ifelse(Year >= data_list[[x]]$season_change_yr, Season, YR)) %>%
                filter(Season %in% c("YR","AW")) %>%
                #filter(Year <= max(years_list[[x]])) %>%
                group_by(Iteration, Year, Season) %>%
                summarise(value = sum(value))
        } else {
            bvuln_ytr <- mcmc_list[[x]]$biomass_vulnref_ytr
            dimnames(bvuln_ytr) <- list("Iteration" = 1:n_iter, "Year" = pyears_list[[x]], "Season" = seasons, "Region" = regions_list[[x]])
            bvuln_ytr2 <- reshape2::melt(bvuln_ytr) %>%
                filter(value > 0) %>%
                mutate(Season = as.character(Season), Season = ifelse(Year >= data_list[[x]]$season_change_yr, Season, YR)) %>%
                filter(Season %in% c("YR","AW")) %>%
                #filter(Year <= max(years_list[[x]])) %>%
                group_by(Iteration, Year, Season) %>%
                summarise(value = sum(value))
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
      vb0 <- reshape2::melt(bio) %>%
        left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
        filter(Region != "Total") %>%
        group_by(Iteration, Year) %>%
        summarise(value = sum(value)) %>%
        ungroup() %>%
        mutate(Rule = 1, type = "VB0")
      # bio <- mcmc_list[[x]]$SSBref_jr
      # dimnames(bio) <- list("Iteration" = 1:n_iter, "Rule" = 1, "Region" = regions_list[[x]])
      # ref <- reshape2::melt(bio) %>%
      #     left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
      #     group_by(Iteration, Region, Rule, value, Year) %>%
      #     ungroup() #%>%
      # mutate(type = "Target")
      bio2 <- vb0
      # filter(Year <= max(years_list[[x]]))
      bio2$Model <- object_names[x]
      return(bio2)
    })
    vb0 <- data.frame(do.call(rbind, vb0_list))
    vb0$Model <- factor(vb0$Model)

    mods <- unique(vb$Model)
    mod_num <- sapply(1:length(mods), function(m) as.numeric(strsplit(as.character(mods[m]),"_")[[1]][1]))
    vb$Model <- factor(vb$Model, levels = unique(mods)[order(mod_num)])

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
    p <- ggplot(data = vb %>% filter(Year %in% years), aes(x = Year, y = value, color = Model, fill = Model)) +
        stat_summary(data = vb %>% filter(Year %in% years), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        #stat_summary(data=vb, fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
        stat_summary(data = vb %>% filter(Year %in% years), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha=0.75) +
        # scale_fill_manual(values = cols_all, labels = object_names) +
        # scale_colour_manual(values = cols_all, labels = object_names) +
        # guides(colour = guide_legend(override.aes = list(colour = cols_all, linetype = lty_all))) +
        # scale_linetype(guide=FALSE) +
        expand_limits(y = 0) +
        xlab("Fishing year") + ylab("Vulnerable reference biomass (tonnes)") +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd(base_size=14) #+
        # scale_y_continuous(expand = c(0,0), limits = c(0, max(vb$value)*1.05))
    if (nmod > 5) {
        p <- p +
          scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
          scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else {
        p <- p +
          scale_fill_brewer(palette = "Set1") +
          scale_color_brewer(palette = "Set1")
    }
    if (save_plot) {
      ggsave(paste0(figure_dir, "biomass_vulnref_compare.png"), p, width = 10)
    }

    relvb <- full_join(vb %>% rename(VB = value), vb0 %>% rename(VB0 = value)) %>%
      mutate(RelVB = VB/VB0)
    
    # Relative Vulnerable biomass
    p <- ggplot(data = relvb %>% filter(Year %in% years), aes(x = Year, y = RelVB, color = Model, fill = Model)) +
      stat_summary(data = relvb %>% filter(Year %in% years), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
      #stat_summary(data=vb, fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
      stat_summary(data = relvb %>% filter(Year %in% years), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha=0.75) +
      # scale_fill_manual(values = cols_all, labels = object_names) +
      # scale_colour_manual(values = cols_all, labels = object_names) +
      # guides(colour = guide_legend(override.aes = list(colour = cols_all, linetype = lty_all))) +
      # scale_linetype(guide=FALSE) +
      expand_limits(y = 0) +
      xlab("Fishing year") + ylab("Relative vulnerable reference biomass (tonnes)") +
      scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
      theme_lsd(base_size=14) #+
    # scale_y_continuous(expand = c(0,0), limits = c(0, max(vb$value)*1.05))
    if (nmod > 5) {
      p <- p +
        scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
        scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else {
      p <- p +
        scale_fill_brewer(palette = "Set1") +
        scale_color_brewer(palette = "Set1")
    }
    if (save_plot) {
      ggsave(paste0(figure_dir, "biomass_relvulnref_compare.png"), p, width = 10)
    }

    # Vulnerable biomass
    p <- ggplot(data = vb, aes(x = Year, y = value, color = Model, fill = Model)) +
        geom_vline(aes(xintercept = max(years) + 0.5), linetype = 2) +
        stat_summary(data = vb, fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        #stat_summary(data = vb, fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
        stat_summary(data = vb, fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
        expand_limits(y = 0) +
        xlab("Fishing year") + ylab("Vulnerable reference biomass (tonnes)") +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd(base_size = 14)# +

    if (nmod > 5) {
        p <- p +
          scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
          scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else {
        p <- p +
          scale_fill_brewer(palette = "Set1") +
          scale_color_brewer(palette = "Set1")
    }

    if (save_plot) {
      ggsave(paste0(figure_dir, "biomass_vulnref_compare_v2.png"), p, width = 10)
      vb_summary <- vb %>%
        group_by(Year, Season, Model) %>%
        summarise(p05 = quantile(value, probs = 0.05), p50 = median(value), p95 = quantile(value, probs = 0.95))
      write.csv(vb_summary, file = paste0(figure_dir, "biomass_vulnref_compare_v2.csv"))      
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
#' @importFrom RColorBrewer brewer.pal
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
           # filter(Year <= max(years_list[[x]])) %>%
            group_by(Iteration, Year) %>%
            summarise(value = sum(value))

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

    mods <- unique(recruits$Model)
    mod_num <- sapply(1:length(mods), function(m) as.numeric(strsplit(as.character(mods[m]),"_")[[1]][1]))
    recruits$Model <- factor(recruits$Model, levels = unique(mods)[order(mod_num)])

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

   p <- ggplot(data = recruits %>% filter(Year %in% years), aes(x = Year, y = value, color = Model, fill = Model)) +
        stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        #stat_summary(fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
        stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha=0.75) +
        # scale_fill_manual(values = cols_all, labels = object_names) +
        # scale_colour_manual(values = cols_all, labels = object_names) +
        # guides(colour = guide_legend(override.aes = list(colour = cols_all, linetype = lty_all))) +
        # scale_linetype(guide=FALSE) +
        expand_limits(y = 0) +
        xlab("Fishing year") + ylab("Recruitment (millions of individuals)") +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd(base_size=14) #+
         # scale_y_continuous(expand = c(0,0), limits = c(0, max(recruits$value)*1.05))
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
        stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        #stat_summary(fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
        stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha=0.75) +
        # scale_fill_manual(values = cols_all, labels = object_names) +
        # scale_colour_manual(values = cols_all, labels = object_names) +
        # guides(colour = guide_legend(override.aes = list(colour = cols_all, linetype = lty_all))) +
        # scale_linetype(guide=FALSE) +
        expand_limits(y = 0) +
        xlab("Fishing year") + ylab("Recruitment (millions of individuals)") +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd(base_size=14) #+
         # scale_y_continuous(expand = c(0,0), limits = c(0, max(recruits$value)*1.05))
    if (nmod > 5) {
        p <- p +
          scale_fill_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod))) +
          scale_color_manual(values = c(colorRampPalette(brewer.pal(9, "Spectral"))(nmod)))
    } else {
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
        regions <- 1:data_list[[x]]$n_area
        pyears <- pyears_list[[x]]

        w <- data_list[[x]]$which_sel_rsyt
        dimnames(w) <- list("Region" = paste0("Region ", regions), "Sex" = sex, "Year" = pyears, "Season" = 1:n_season)
        w <- reshape2::melt(w, value.name = "Selex")


        sel2 <- mcmc_list[[x]]$selectivity_ml
        dimnames(sel2) <- list("Iteration" = 1:n_iter, "Selex" = 1:data_list[[x]]$n_sel, "Size" = data_list[[x]]$size_midpoint_l)
        sel2 <- reshape2::melt(sel2, value.name = "Selectivity") %>%
            inner_join(w, by = "Selex") %>%
            filter(Year <= max(years_list[[x]])) %>%
            mutate(Year = factor(Year)) %>%
            mutate(Sex = ifelse(grepl("female", Sex), "Female", "Male")) %>%
            distinct(Iteration, Sex, Size, Selectivity, Region, .keep_all = TRUE)
        sel2$Model <- object_names[[x]]

        return(sel2)
    })
    sel <- do.call(rbind, slist) %>%
      dplyr::rename(Epoch = Year)

    nmod <- length(unique(sel$Model))
    sel$Season <- factor(sel$Season)

    mods <- unique(sel$Model)
    mod_num <- sapply(1:length(mods), function(m) as.numeric(strsplit(as.character(mods[m]),"_")[[1]][1]))
    sel$Model <- factor(sel$Model, levels = unique(mods)[order(mod_num)])
    
    ## if multiple seasons, regardless of year
    if(length(unique(sel$Season)) > 1){
      p <- ggplot(data = sel, aes(x = Size, y = Selectivity, col = Model, fill = Model, linetype = Season)) +
        stat_summary(data = sel, aes(x = Size, y = Selectivity, col = Model, linetype = Season), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(data = sel, aes(x = Size, y = Selectivity, col = Model, linetype = Season), fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(data = sel, aes(x = Size, y = Selectivity, col = Model, linetype = Season), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha=0.8) #+
        # scale_y_continuous(expand = c(0,0), limits = c(0, 1.05))
    } else {
      p <- ggplot(data = sel, aes(x = Size, y = Selectivity, col = Model, fill = Model)) +
        stat_summary(data = sel, aes(x = Size, y = Selectivity, col = Model), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(data = sel, aes(x = Size, y = Selectivity, col = Model), fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(data = sel, aes(x = Size, y = Selectivity, col = Model), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha=0.8) #+
        # scale_y_continuous(expand = c(0,0), limits = c(0, 1.05))
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


        if(length(unique(sel$Year))==1 & length(unique(sel$Season))==1) p <- p + guides(linetype = FALSE)

        areas <- unique(sapply(1:length(data_list), function(x) data_list[[x]]$n_area))
        if (areas > 1) {
          if(length(unique(sel$Season))>1 & length(unique(sel$Year))>1){
            p <- p + facet_grid(Region + Year ~ Epoch + Sex)
          } else {
            p <- p + facet_grid(Region ~ Epoch + Sex)
          }
        } else {
          if(length(unique(sel$Season))>1 & length(unique(sel$Year))>1){
            p <- p + facet_grid(Year ~ Epoch + Sex)
          } else {
            p <- p + facet_grid( ~ Epoch + Sex)
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
            filter(Season == 1) %>%
            mutate(QY = paste(Year, qtype))
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

    p <- ggplot(data = q %>% filter(Year %in% years), aes(x = Year, y = value, colour=Model, fill=Model)) +
        stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        # stat_summary(fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
        stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha=0.75) +
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
    # if (data_list[[1]]$n_area > 1) {
    #     p <- p + facet_wrap(~Region)
    # }
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
#' @importFrom tidyr pivot_longer pivot_wider
#' @export
#'
table_compare_residuals <- function(object_list, object_names, figure_dir = "compare_figure/") {
  rlist <- lapply(1:length(object_list), function(x){
    res <- table_residuals(object = object_list[[x]], figure_dir = figure_dir, save_table = FALSE)
    res <- res %>% mutate("model"=object_names[[x]])
    return(res)
  })
  rdf <- do.call(rbind, rlist)
  rdf2 <- rdf %>%
        tidyr::pivot_longer(-c(model,data), names_to = "residual_type", values_to = "value") %>%
        tidyr::pivot_wider(names_from = model)

  write.csv(rdf2, file = file.path(figure_dir, "Residual_summaries.csv"), row.names = FALSE)
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

    if(any(n_iter<3)) return(NULL)
    if(all(n_iter>3)){

       ## cpue
       cpue_list <- lapply(1:length(object_list), function(x){
         llcpue <- mcmc_list[[x]]$lp_cpue_i
         llcpue_array <- array(llcpue, dim = c(nrow(llcpue)/4,4,ncol(llcpue)))
         r_eff_cpue <- loo::relative_eff(exp(llcpue_array))
         loo_cpue <- loo::loo(x=llcpue_array, r_eff=r_eff_cpue)
         return(loo_cpue)
       })
       names(cpue_list) <- object_names

       comp1 <- loo::loo_compare(cpue_list)
       m1 <- rownames(comp1)
       df1 <- data.frame("model"=m1, "dataset"="cpue")
       df2 <- data.frame(comp1)
       cpue_df <- cbind.data.frame(df1, df2)


       ## sexr
       sexr_list <- lapply(1:length(object_list), function(x){
         llsexr <- mcmc_list[[x]]$lp_sexr_i
         llsexr_array <- array(llsexr, dim = c(nrow(llsexr)/4,4,ncol(llsexr)))
         r_eff_sexr <- loo::relative_eff(exp(llsexr_array))
         loo_sexr <- loo::loo(x=llsexr_array, r_eff=r_eff_sexr)
         return(loo_sexr)
       })
       names(sexr_list) <- object_names

       comp1 <- loo::loo_compare(sexr_list)
       m1 <- rownames(comp1)
       df1 <- data.frame("model"=m1, "dataset"="sexratio")
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
       write.csv(out, file.path(figure_dir, "LOOIC.csv"), row.names=FALSE)

       return(out)
    }

}
