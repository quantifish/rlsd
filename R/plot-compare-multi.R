#' Compare probability of being under risk constraints
#' 
#' @param object_list list of mcmc results
#' @param object_names list of model names
#' @param figure_dir the directory to save the figure to
#' @import dplyr
#' @importFrom reshape2 melt
#' @importFrom grDevices colorRampPalette gray
#' @importFrom stats runif quantile
#' @export
#' 
compare_multi_risk <- function(object_list, object_names, figure_dir = "compare_figure/") 
{

    data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
    mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)
    data <- data_list[[1]]

    years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
    pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_proj_yr)
    regions_list <- lapply(1:length(object_list), function(x) 1:data_list[[x]]$n_area)
    cutyears_list <- lapply(1:length(object_list), function(x) (max(pyears_list[[x]])-99):max(pyears_list[[x]]))
    cutyears <- unique(unlist(cutyears_list))
    n_iter <- nrow(mcmc_list[[1]][[1]])

    ssb_list <- lapply(1:length(object_list), function(x) {
        n_iter <- nrow(mcmc_list[[x]][[1]])
        ssb <- mcmc_list[[x]]$biomass_ssb_jyr
        dimnames(ssb) <- list("Iteration" = 1:n_iter, "Rule" = 1, "Year" = pyears_list[[x]], "Region" = regions_list[[x]])
        ssb2 <- reshape2::melt(ssb) %>% dplyr::rename("SSB"=value) %>% select(-Rule)
        stock <- strsplit(object_names[x],"_")[[1]][1]
        strat <- strsplit(object_names[x],"_")[[1]][2]
        ssb2$Stock <- stock
        ssb2$Strategy <- strat
        ssb0 <- mcmc_list[[x]]$SSB0_r
        dimnames(ssb0) <- list("Iteration" = 1:n_iter, "Region" = regions_list[[x]])

        ssb0 <- reshape2::melt(ssb0) %>%
            dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
            dplyr::group_by(Iteration, Region, value, Year) %>%
            dplyr::ungroup() %>%
            dplyr::rename("SSB0" = value) %>%
            dplyr::mutate(Stock = stock) %>% 
            dplyr::mutate(Strategy = strat)

        out <- full_join(ssb0, ssb2)

        return(out)
    })
    ssb_df <- do.call(rbind, ssb_list)

    ssb_proj <- ssb_df %>% dplyr::filter(Year %in% cutyears) %>% select(-c(Region))

    ssb_calc <- ssb_proj %>%
            dplyr::group_by(Stock, Strategy, "SSB", "SSB0") %>%
            dplyr::summarise(Pext = length(which(SSB <= 0.01*SSB0)==TRUE)/length(SSB),
                            P10 = length(which(SSB <= 0.1*SSB0)==TRUE)/length(SSB),
                            P20 = length(which(SSB <= 0.2*SSB0)==TRUE)/length(SSB))

    ssb_out <- data.frame(ssb_calc) %>% select(Stock, Strategy, Pext, P10, P20)
    write.table(ssb_out, file=paste0(figure_dir, "Prisk_table.txt"), sep="\t", row.names=FALSE, col.names=TRUE)
    return(ssb_out)
}


#' Compare catch vs. biomass
#' 
#' @param object_list list of 'lsd.rds' files from multiple models
#' @param object_names vector of model names associated with each of the output files in object_list
#' @param figure_dir the directory to save the figure to
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#' 
plot_compare_catch_bio <- function(object_list, object_names, figure_dir = "compare_figure/")
{
    data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
    mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)
    data <- data_list[[1]]

    years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
    pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_proj_yr)
    regions_list <- lapply(1:length(object_list), function(x) 1:data_list[[x]]$n_area)
    cutyears_list <- lapply(1:length(object_list), function(x) (max(pyears_list[[x]])-99):max(pyears_list[[x]]))
    cutyears <- unique(unlist(cutyears_list))
    seasons <- c("AW","SS")
    regions <- 1:data$n_area
    n_iter <- nrow(mcmc_list[[1]][[1]])

    ssb_list <- lapply(1:length(object_list), function(x){
        n_iter <- nrow(mcmc_list[[x]][[1]])
        ssb <- mcmc_list[[x]]$biomass_ssb_jyr
        dimnames(ssb) <- list("Iteration" = 1:n_iter, "Rule" = 1, "Year" = pyears_list[[x]], "Region" = regions_list[[x]])
        ssb2 <- reshape2::melt(ssb) %>% dplyr::rename("SSB"=value) %>% 
            dplyr::filter(Year %in% cutyears)
            # dplyr::filter(Year > years_list[[x]][length(years_list[[x]])])

        stock <- strsplit(object_names[x],"_")[[1]][1]
        strat <- strsplit(object_names[x],"_")[[1]][2]
        ssb2$Stock <- stock
        ssb2$Strategy <- strat

        ssb0 <- mcmc_list[[x]]$SSB0_r
        dimnames(ssb0) <- list("Iteration" = 1:n_iter, "Region" = regions_list[[x]])
        ssb0 <- reshape2::melt(ssb0) %>%
            dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
            dplyr::rename(SSB0=value) %>%
            dplyr::filter(Year %in% cutyears) %>%
            # dplyr::filter(Year > years_list[[x]][length(years_list[[x]])]) %>%
            dplyr::select(-Region)
        stock <- strsplit(object_names[x],"_")[[1]][1]
        strat <- strsplit(object_names[x],"_")[[1]][2]
        ssb0$Stock <- stock
        ssb0$Strategy <- strat

        ssb_out <- ssb2 %>% select(Iteration, Year, SSB, Stock, Strategy)
        relssb <- full_join(ssb_out, ssb0) %>%
                dplyr::mutate(RelSSB = SSB/SSB0) #%>%
                # dplyr::select(Iteration, Year, Stock, Strategy, RelSSB)

        return(relssb)
    })
    relssb <- do.call(rbind, ssb_list)

    catch_list <- lapply(1:length(object_list), function(x){
        n_iter <- nrow(mcmc_list[[x]][[1]])

        catch <- data_list[[x]]$proj_catch_commercial_r

        psl <- mcmc_list[[x]]$pred_catch_sl_ryt
        dimnames(psl) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = pyears_list[[x]], "Season" = seasons)
        psl2 <- reshape2::melt(psl, value.name = "Catch") %>% 
            dplyr::filter(Year %in% cutyears) %>%
            # dplyr::filter(Year > years_list[[x]][length(years_list[[x]])]) %>%
            dplyr::group_by(Iteration, Year) %>%
            dplyr::summarise(sum(Catch)) %>%
            dplyr::rename(Catch="sum(Catch)")
        stock <- strsplit(object_names[x],"_")[[1]][1]
        strat <- strsplit(object_names[x],"_")[[1]][2]
        psl2$Stock <- stock
        psl2$Strategy <- strat
        psl2$InputCatch <- catch

        return(psl2)
    })
    catch <- do.call(rbind, catch_list)

    fcatch <- catch %>% filter(grepl("F=", Strategy))
    ccatch <- catch %>% filter(grepl("C=", Strategy))

    p <- ggplot(fcatch) +
        stat_summary(aes(x=Year, y=Catch, fill=Strategy), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(aes(x=Year, y=Catch, color=Strategy), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
        facet_grid(Stock~Strategy) +
        guides(color=FALSE, fill=FALSE) +
        theme_lsd(base_size = 14)
    ggsave(file.path(figure_dir, "Catch_FixedF.png"), p, height=10, width=15)  

    p <- ggplot(ccatch) +
        stat_summary(aes(x=Year, y=Catch, fill=Strategy), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(aes(x=Year, y=Catch, color=Strategy), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
        facet_grid(Stock~Strategy) +
        guides(color=FALSE, fill=FALSE) +
        theme_lsd(base_size = 14)
    ggsave(file.path(figure_dir, "Catch_FixedC.png"), p, height=10, width=15)    

    cb <- full_join(catch, relssb) %>%
            dplyr::mutate(RuleType = ifelse(grepl("F=", Strategy), "FixedF", ifelse(grepl("C=", Strategy), "FixedCatch", "X")))

    summary_byIter <- cb %>%
        dplyr::group_by(Iteration, Stock, Strategy, RuleType) %>%
        dplyr::summarise(AvgCatch = mean(Catch), AvgRelSSB = mean(RelSSB)) 

    summary <- summary_byIter %>%
        dplyr::group_by(Stock, Strategy, RuleType) %>%
        dplyr::summarise(C5 = stats::quantile(AvgCatch, prob=0.05), C50 = stats::quantile(AvgCatch, prob=0.5), C95 = stats::quantile(AvgCatch, prob=0.95), B5 = stats::quantile(AvgRelSSB, prob=0.05), B50 = stats::quantile(AvgRelSSB, prob=0.5), B95 = stats::quantile(AvgRelSSB, prob=0.95))


    fc <- summary %>% dplyr::filter(RuleType == "FixedCatch")
    p <- ggplot(fc) +
        geom_vline(xintercept = 0.1, color=gray(0.3), alpha=0.75) +
        geom_vline(xintercept = 0.2, color=gray(0.5), alpha=0.75) +
        # geom_segment(data = summary %>% dplyr::filter(RuleType=="FixedF"), aes(x=B5, xend=B95, y=C50, yend=C50), alpha=0.25, lwd=1.3) +
        # geom_segment(data = summary %>% dplyr::filter(RuleType=="FixedF"), aes(x=B50, xend=B50, y=C5, yend=C95), alpha=0.25, lwd=1.3) +
        geom_segment(aes(x=B5, xend=B95, y=C50, yend=C50, color = Strategy), alpha=0.75, lwd=1.3) +
        geom_segment(aes(x=B50, xend=B50, y=C5, yend=C95, color = Strategy), alpha=0.75, lwd=1.3) +
        # geom_point(data = summary %>% dplyr::filter(RuleType=="FixedF"), aes(x=B50, y=C50, color=Strategy), pch=19, alpha=0.75, cex=4) +
        geom_point(aes(x=B50, y=C50, fill=Strategy), pch=21, alpha=0.75, cex=4) +
        expand_limits(x = 0) +
        scale_x_continuous(limits = c(0, 0.7)) +
        facet_grid(Stock~., scales="free_y", shrink=FALSE) +
        xlab("Relative spawning stock biomass") +
        ylab("Catch") +
        scale_colour_viridis_d() +
        scale_fill_viridis_d() +
        theme_lsd(base_size=14)
    ggsave(file.path(figure_dir, "Catch_versus_RelSSB_FixedCatch.png"), p, width=10)

    fc <- summary %>% dplyr::filter(RuleType == "FixedF")
    p <- ggplot(fc) +
        geom_vline(xintercept = 0.1, color=gray(0.3), alpha=0.75) +
        geom_vline(xintercept = 0.2, color=gray(0.5), alpha=0.75) +
        # geom_segment(data = summary %>% dplyr::filter(RuleType=="FixedF"), aes(x=B5, xend=B95, y=C50, yend=C50), alpha=0.25, lwd=1.3) +
        # geom_segment(data = summary %>% dplyr::filter(RuleType=="FixedF"), aes(x=B50, xend=B50, y=C5, yend=C95), alpha=0.25, lwd=1.3) +
        geom_segment(aes(x=B5, xend=B95, y=C50, yend=C50, color = Strategy), alpha=0.75, lwd=1.3) +
        geom_segment(aes(x=B50, xend=B50, y=C5, yend=C95, color = Strategy), alpha=0.75, lwd=1.3) +
        # geom_point(data = summary %>% dplyr::filter(RuleType=="FixedF"), aes(x=B50, y=C50, color=Strategy), pch=19, alpha=0.75, cex=4) +
        geom_point(aes(x=B50, y=C50, fill=Strategy), pch=21, alpha=0.75, cex=4) +
        expand_limits(x = 0) +
        scale_x_continuous(limits = c(0, 0.7)) +
        facet_grid(Stock~., scales="free_y", shrink=FALSE) +
        xlab("Relative spawning stock biomass") +
        ylab("Catch") +
        scale_colour_viridis_d() +
        scale_fill_viridis_d() +
        theme_lsd(base_size=14)
    ggsave(file.path(figure_dir, "Catch_versus_RelSSB_FixedF.png"), p, width=10)

    ## include risk
    prisk <- compare_multi_risk(object_list = object_list, object_names = object_names, figure_dir = figure_dir)
    summary_wRisk <- full_join(summary, prisk)
    write.table(summary_wRisk, file=paste0(figure_dir, "Catch_RelSSB_Risk.txt"), sep="\t", row.names=FALSE, col.names=TRUE)

    p <- ggplot(summary_wRisk) +
        # geom_vline(xintercept = 0.1, color=gray(0.3), alpha=0.75) +
        geom_vline(xintercept = 0.2, color=gray(0.5), alpha=0.75) +
        # geom_segment(data = summary %>% dplyr::filter(RuleType=="FixedF"), aes(x=B5, xend=B95, y=C50, yend=C50), alpha=0.25, lwd=1.3) +
        # geom_segment(data = summary %>% dplyr::filter(RuleType=="FixedF"), aes(x=B50, xend=B50, y=C5, yend=C95), alpha=0.25, lwd=1.3) +
        geom_segment(aes(x=B5, xend=B95, y=C50, yend=C50, color = P20), alpha=0.75, lwd=1.3) +
        geom_segment(aes(x=B50, xend=B50, y=C5, yend=C95, color = P20), alpha=0.75, lwd=1.3) +
        # geom_point(data = summary %>% dplyr::filter(RuleType=="FixedF"), aes(x=B50, y=C50, color=Strategy), pch=19, alpha=0.75, cex=4) +
        geom_point(aes(x=B50, y=C50, fill=P20), pch=21, alpha=0.75, cex=4) +
        expand_limits(x = 0) +
        scale_x_continuous(limits = c(0, 0.7)) +
        facet_grid(Stock~RuleType, scales="free_y", shrink=FALSE) +
        xlab("Relative spawning stock biomass") +
        ylab("Catch") +
        scale_colour_viridis_c() +
        scale_fill_viridis_c() +
        theme_lsd(base_size=14)
    ggsave(file.path(figure_dir, "Catch_versus_RelSSB_byProb.png"), p, width=12, height=10)      

    find_msy <- summary_wRisk %>%   
                dplyr::group_by(Stock, RuleType) %>%
                dplyr::summarise(MSY = max(C50[which(P20 < 0.10)]))

    msy_info <- inner_join(summary_wRisk, find_msy) %>%
                dplyr::filter(C50 == MSY)
    write.table(msy_info, file=paste0(figure_dir, "MSY.txt"), sep="\t", row.names=FALSE, col.names=TRUE)


    msy_list <- lapply(1:length(object_list), function(x){
        n_iter <- nrow(mcmc_list[[x]][[1]])
        msy <- mcmc_list[[x]]$MSY_r
        dimnames(msy) <- list("Iteration" = 1:n_iter, "Region" = regions_list[[x]])
        msy2 <- reshape2::melt(msy) %>% dplyr::rename("dMSY"=value)

        bmsy <- mcmc_list[[x]]$SSBmsy_r
        dimnames(bmsy) <- list("Iteration" = 1:n_iter, "Region" = regions_list[[x]])
        bmsy2 <- reshape2::melt(bmsy) %>% dplyr::rename("dBmsy"=value)

        ssb0 <- mcmc_list[[x]]$SSB0_r
        dimnames(ssb0) <- list("Iteration" = 1:n_iter, "Region" = regions_list[[x]])
        ssb0 <- reshape2::melt(ssb0) %>% dplyr::rename("SSB0" = value)  

        det <- data.frame("dMSY"=unique(msy2$dMSY), "dBmsy"=unique(bmsy2$dBmsy), "SSB0" = unique(ssb0$SSB0))

        stock <- strsplit(object_names[x],"_")[[1]][1]
        strat <- strsplit(object_names[x],"_")[[1]][2]
        det$Stock <- stock
        det$Strategy <- strat

        return(det)
    })
    dmsy_info <- do.call(rbind, msy_list)

    compare_msy <- inner_join(dmsy_info, msy_info)

    p <- ggplot(compare_msy) + 
        geom_vline(aes(xintercept = 1)) + 
        geom_hline(aes(yintercept = 1)) +
        geom_point(aes(x = B50/(dBmsy/SSB0), y = MSY/dMSY, color = RuleType, shape = Stock), cex=4) +
        xlab("Bmsy/dBmsy") +
        ylab("MSY/dMSY") +
        scale_color_brewer(palette = "Set1") +
        expand_limits(x = 0, y = 0) +
        scale_x_continuous(expand = c(0,0), limits = c(0, max(c(1.05, compare_msy$B50/(compare_msy$dBmsy/compare_msy$SSB0))*1.05))) +
        scale_y_continuous(expand = c(0,0), limits = c(0, max(c(1.05, compare_msy$MSY/compare_msy$dMSY)*1.05))) + 
        theme_lsd(base_size = 14) 
    ggsave(file.path(figure_dir, "MSY_dMSY.png"), p, width=10)

    # ## assessment error adjustment - bias
    # slope <- runif(n_iter, 0, 0.5)
    # mult <- lapply(1:n_iter, function(x){
    #     seq1 <- seq(from=0,to=slope[x],length.out=length(cutyears))
    #     seq2 <- seq1 - mean(seq1) +1
    #     df <- data.frame("Iteration" = x, "Year" = cutyears, "Multiplier" = seq2)
    #     return(df)
    # })
    # mult_df <- do.call(rbind, mult)

    # p <- ggplot(mult_df) + 
    #     geom_line(aes(x = Year, y = Multiplier, color = factor(Iteration))) + 
    #     guides(color = FALSE) + 
    #     scale_color_manual(values = colorRampPalette(c("gray", "black"))(n_iter)) +
    #     theme_lsd(base_size = 14)
    # ggsave(file.path(figure_dir, "Multipliers.png"), p)

    # ssb_bias <- full_join(relssb, mult_df) %>%
    #         dplyr::mutate(SSBadj = SSB * Multiplier) %>%
    #         dplyr::mutate(RelSSB = SSB / SSB0) %>%
    #         dplyr::mutate(RelSSBadj = SSBadj / SSB0)

    # ssb_calc_adj <- ssb_bias %>%
    #         dplyr::group_by(Stock, Strategy, "SSBadj", "SSB0") %>%
    #         dplyr::summarise(Pext_bias = length(which(SSBadj <= 0.01*SSB0)==TRUE)/length(SSBadj),
    #                         P10_bias = length(which(SSBadj <= 0.1*SSB0)==TRUE)/length(SSBadj),
    #                         P20_bias = length(which(SSBadj <= 0.2*SSB0)==TRUE)/length(SSBadj))

    # ssb_comp <- ssb_bias %>%
    #         dplyr::select(Iteration, Year, Stock, Strategy, RelSSB, RelSSBadj) %>%
    #         tidyr::gather(SSBtype, value, -c(Iteration, Year, Stock, Strategy) )

    # ssb_comp_msy <- semi_join(ssb_comp, msy_info) %>%
    #         dplyr::mutate(RuleType= ifelse(grepl("F=",Strategy), "FixedF", ifelse(grepl("C=",Strategy), "FixedCatch", NA)))

    # p <- ggplot(data = ssb_comp_msy, aes(x = Year, y = value)) +
    #     stat_summary(data = ssb_comp_msy, fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = SSBtype)) +
    #     stat_summary(data = ssb_comp_msy, fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = SSBtype)) +
    #     expand_limits(y = 0) +
    #     facet_grid(Stock~RuleType) + 
    #     labs(x = "Projected fishing year", y = "Spawning stock biomass (tonnes)") +
    #     # scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1)) +
    #     scale_y_continuous(limits = c(0, max(ssb_comp_msy$value)*1.02)) +
    #     scale_color_brewer(palette="Set1") +
    #     scale_fill_brewer(palette="Set1") +
    #     theme_lsd(base_size = 14)
}


#' Compare vulnerable biomass from multiple models
#' 
#' @param object_list list of 'lsd.rds' files from multiple models
#' @param object_names vector of model names associated with each of the output files in object_list
#' @param figure_dir the directory to save the figure to
#' @param save_plot to save the plot to file or not
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#' 
plot_compare_multi_ssb <- function(object_list, object_names, figure_dir = "compare_figure/", save_plot = TRUE)
{
    data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
    mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)
    data <- data_list[[1]]

    years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
    pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_proj_yr)
    regions_list <- lapply(1:length(object_list), function(x) 1:data_list[[x]]$n_area)
    cutyears_list <- lapply(1:length(object_list), function(x) (max(pyears_list[[x]])-99):max(pyears_list[[x]]))
    cutyears <- unique(unlist(cutyears_list))

    ssb_list <- lapply(1:length(object_list), function(x) {
        n_iter <- nrow(mcmc_list[[x]][[1]])
        bio <- mcmc_list[[x]]$biomass_ssb_jyr
        dimnames(bio) <- list("Iteration" = 1:n_iter, "Rule" = 1, "Year" = pyears_list[[x]], "Region" = regions_list[[x]])
        bio2 <- reshape2::melt(bio) #%>% dplyr::filter(Year <= max(years_list[[x]]))
        stock <- strsplit(object_names[x],"_")[[1]][1]
        strat <- strsplit(object_names[x],"_")[[1]][2]
        bio2$Stock <- stock
        bio2$Strategy <- strat
        return(bio2)
    })
    ssb <- data.frame(do.call(rbind, ssb_list))
    ssb$Stock <- factor(ssb$Stock)
    ssb$Strategy <- factor(ssb$Strategy)
    
    ssb0_list <- lapply(1:length(object_list), function(x) {
        n_iter <- nrow(mcmc_list[[x]][[1]])
        bio <- mcmc_list[[x]]$SSB0_r
        dimnames(bio) <- list("Iteration" = 1:n_iter, "Region" = regions_list[[x]])
        stock <- strsplit(object_names[x],"_")[[1]][1]
        strat <- strsplit(object_names[x],"_")[[1]][2]

        hl <- reshape2::melt(bio) %>%
            dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
            dplyr::group_by(Iteration, Region, value, Year) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Rule = 1, type = "Hard limit", value = value * 0.1) %>%
            dplyr::mutate(Stock = stock) %>% 
            dplyr::mutate(Strategy = strat)
        sl <- reshape2::melt(bio) %>%
            dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
            dplyr::group_by(Iteration, Region, value, Year) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Rule = 1, type = "Soft limit", value = value * 0.2) %>%
            dplyr::mutate(Stock = stock) %>% 
            dplyr::mutate(Strategy = strat)
        ssb0 <- reshape2::melt(bio) %>%
            dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
            dplyr::group_by(Iteration, Region, value, Year) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Rule = 1, type = "SSB0") %>%
            dplyr::mutate(Stock = stock) %>% 
            dplyr::mutate(Strategy = strat)

        # bio <- mcmc_list[[x]]$SSBref_jr
        # dimnames(bio) <- list("Iteration" = 1:n_iter, "Rule" = 1, "Region" = regions_list[[x]])
        # ref <- reshape2::melt(bio) %>%
        #     dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
        #     dplyr::group_by(Iteration, Region, Rule, value, Year) %>%
        #     dplyr::ungroup() #%>%
            # dplyr::mutate(type = "Target")
        bio2 <- rbind(ssb0, sl, hl) #%>% ##, ref)
           # dplyr::filter(Year <= max(years_list[[x]]))
        bio2$Stock <- stock
        bio2$Strategy <- strat
        return(bio2)
    })
    ssb0 <- data.frame(do.call(rbind, ssb0_list))

    labs <- dplyr::filter(ssb0, Year == max(Year)) %>%
        dplyr::group_by(Year, type, Stock, Strategy) %>%
        dplyr::summarise(value = mean(value))

    ssb_cut <- ssb %>% dplyr::filter(Year %in% cutyears)
    ssb0_cut <- ssb0 %>% dplyr::filter(Year %in% cutyears)

    p <- ggplot(data = ssb_cut, aes(x = Year, y = value)) +
        stat_summary(data = dplyr::filter(ssb0_cut, type == "Soft limit"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, fill="gray") +
        stat_summary(data = dplyr::filter(ssb0_cut, type == "Soft limit"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, colour="gray") +
        stat_summary(data = dplyr::filter(ssb0_cut, type == "Hard limit"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, fill="gray") +
        stat_summary(data = dplyr::filter(ssb0_cut, type == "Hard limit"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, colour="gray") +
        stat_summary(data = dplyr::filter(ssb0_cut, type == "SSB0"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, fill="gray") +
        stat_summary(data = dplyr::filter(ssb0_cut, type == "SSB0"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, colour="gray") +
        # stat_summary(data = dplyr::filter(ssb0_cut, type == "Target"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Strategy)) +
        # stat_summary(data = dplyr::filter(ssb0_cut, type == "Target"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Strategy)) +
        stat_summary(data = ssb_cut, fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Strategy)) +
        stat_summary(data = ssb_cut, fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Strategy)) +
        geom_text(data = labs, aes(x = Year, y = value, label = type), nudge_x=-10) +
        expand_limits(y = 0) +
        facet_wrap(.~Stock) + 
        labs(x = "Projected fishing year", y = "Spawning stock biomass (tonnes)") +
        # scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1)) +
        scale_y_continuous(limits = c(0, max(ssb0_cut$value)*1.02)) +
        theme_lsd(base_size = 14)
    if (save_plot) {
      ggsave(paste0(figure_dir, "biomass_ssb_compare.png"), p, width = 15)
    } else {
      return(p)
    }
}


#' Compare vulnerable biomass from multiple models
#' 
#' @param object_list list of 'lsd.rds' files from multiple models
#' @param object_names vector of model names associated with each of the output files in object_list
#' @param figure_dir the directory to save the figure to
#' @param save_plot to save the plot to file or not
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#' 
plot_compare_multi_relssb <- function(object_list, object_names, figure_dir = "compare_figure/", save_plot = TRUE)
{
    data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
    mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)
    data <- data_list[[1]]

    years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
    pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_proj_yr)
    regions_list <- lapply(1:length(object_list), function(x) 1:data_list[[x]]$n_area)
    cutyears_list <- lapply(1:length(object_list), function(x) (max(pyears_list[[x]])-99):max(pyears_list[[x]]))
    cutyears <- unique(unlist(cutyears_list))

    ssb_list <- lapply(1:length(object_list), function(x) {
        n_iter <- nrow(mcmc_list[[x]][[1]])
        bio <- mcmc_list[[x]]$biomass_ssb_jyr
        dimnames(bio) <- list("Iteration" = 1:n_iter, "Rule" = 1, "Year" = pyears_list[[x]], "Region" = regions_list[[x]])
        bio2 <- reshape2::melt(bio) #%>% dplyr::filter(Year <= max(years_list[[x]]))
        stock <- strsplit(object_names[x],"_")[[1]][1]
        strat <- strsplit(object_names[x],"_")[[1]][2]
        bio2$Stock <- stock
        bio2$Strategy <- strat
        return(bio2)
    })
    ssb <- data.frame(do.call(rbind, ssb_list))
    ssb$Stock <- factor(ssb$Stock)
    ssb$Strategy <- factor(ssb$Strategy)
    
    ssb0_list <- lapply(1:length(object_list), function(x) {
        n_iter <- nrow(mcmc_list[[x]][[1]])
        bio <- mcmc_list[[x]]$SSB0_r
        dimnames(bio) <- list("Iteration" = 1:n_iter, "Region" = regions_list[[x]])
        stock <- strsplit(object_names[x],"_")[[1]][1]
        strat <- strsplit(object_names[x],"_")[[1]][2]

        hl <- reshape2::melt(bio) %>%
            dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
            dplyr::group_by(Iteration, Region, value, Year) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Rule = 1, type = "Hard limit", value = value * 0.1) %>%
            dplyr::mutate(Stock = stock) %>% 
            dplyr::mutate(Strategy = strat)
        sl <- reshape2::melt(bio) %>%
            dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
            dplyr::group_by(Iteration, Region, value, Year) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Rule = 1, type = "Soft limit", value = value * 0.2) %>%
            dplyr::mutate(Stock = stock) %>% 
            dplyr::mutate(Strategy = strat)
        ssb0 <- reshape2::melt(bio) %>%
            dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
            dplyr::group_by(Iteration, Region, value, Year) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(Rule = 1, type = "SSB0") %>%
            dplyr::mutate(Stock = stock) %>% 
            dplyr::mutate(Strategy = strat)

        # bio <- mcmc_list[[x]]$SSBref_jr
        # dimnames(bio) <- list("Iteration" = 1:n_iter, "Rule" = 1, "Region" = regions_list[[x]])
        # ref <- reshape2::melt(bio) %>%
        #     dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
        #     dplyr::group_by(Iteration, Region, Rule, value, Year) %>%
        #     dplyr::ungroup() #%>%
            # dplyr::mutate(type = "Target")
        bio2 <- rbind(ssb0, sl, hl) #%>% ##, ref)
           # dplyr::filter(Year <= max(years_list[[x]]))
        bio2$Stock <- stock
        bio2$Strategy <- strat
        return(bio2)
    })
    ssb0 <- data.frame(do.call(rbind, ssb0_list))

    ssb_cut <- ssb %>% dplyr::filter(Year %in% cutyears) %>% dplyr::mutate(type="SSB")
    ssb0_cut <- ssb0 %>% dplyr::filter(Year %in% cutyears)

    df <- rbind.data.frame(ssb_cut, ssb0_cut)
    df2 <- df %>% spread(type, value)
    colnames(df2)[which(colnames(df2)=="Hard limit")] <- "Hard_limit"
    colnames(df2)[which(colnames(df2)=="Soft limit")] <- "Soft_limit"

    df2 <- df2 %>% 
            mutate(Hard_limit_rel = Hard_limit/SSB0) %>%
            mutate(Soft_limit_rel = Soft_limit/SSB0) %>%
            mutate(SSB0_rel = SSB0/SSB0) %>%
            mutate(SSB_rel = SSB/SSB0) %>%
            mutate(Rule_type = ifelse(grepl("FixedCatch", as.character(Strategy)), "FixedCatch", 
                                ifelse(grepl("FixedF", as.character(Strategy)), "FixedF", NA))) %>%
            mutate(Catch_prop = ifelse(grepl("Catch", as.character(Strategy)), as.numeric(strsplit(as.character(Strategy),"Catch")[[1]][2])/SSB0, NA))

    df3 <- df2 %>% gather(type, value, Hard_limit:SSB_rel) %>%
                    filter(grepl("rel", type))

    labs <- df3 %>% filter(type %in% c("Hard_limit_rel","Soft_limit_rel","SSB0_rel")) %>% 
        dplyr::group_by(type) %>%
        dplyr::summarise(value = mean(value)) 
    labs[which(labs$type == "Hard_limit_rel"),'type'] <- "Hard limit"
    labs[which(labs$type == "Soft_limit_rel"),'type'] <- "Soft limit"
    labs[which(labs$type == "SSB0_rel"),'type'] <- "SSB0"



    p <- ggplot(data = df3, aes(x = Year, y=value)) +
        stat_summary(data = df3 %>% filter(type=="Soft_limit_rel"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, fill="gray") +
        stat_summary(data = df3 %>% filter(type=="Soft_limit_rel"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, colour="gray") +
        stat_summary(data = df3 %>% filter(type=="Hard_limit_rel"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, fill="gray") +
        stat_summary(data = df3 %>% filter(type=="Hard_limit_rel"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, colour="gray") +
        stat_summary(data = df3 %>% filter(type=="SSB0_rel"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, fill="gray") +
        stat_summary(data = df3 %>% filter(type=="SSB0_rel"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, colour="gray") +
        # stat_summary(data = dplyr::filter(ssb0_cut, type == "Target"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Strategy)) +
        # stat_summary(data = dplyr::filter(ssb0_cut, type == "Target"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Strategy)) +
        stat_summary(data = df3 %>% filter(type=="SSB_rel"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Stock)) +
        stat_summary(data = df3 %>% filter(type=="SSB_rel"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Stock)) +
        geom_text(data = labs, aes(x = mean(df3$Year), y = value, label = type), nudge_x=-3) +
        expand_limits(y = 0) +
        facet_wrap(Strategy~.) + 
        labs(x = "Projected fishing year", y = "Relative spawning stock biomass (tonnes)") +
        # scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd(base_size = 14)
    if (save_plot) {
      ggsave(paste0(figure_dir, "biomass_relssb_compare.png"), p, width = 15)
    } else {
      return(p)
    }
}


#' Add bias to recalculate risk probabilities
#' 
#' @param object_list list of 'lsd.rds' files from multiple models
#' @param object_names vector of model names associated with each of the output files in object_list
#' @param figure_dir the directory to save the figure to
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#' 
compare_multi_risk_wBias <- function(object_list, object_names, 
                                     figure_dir = "compare_figure/")
{
    data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
    mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)
    data <- data_list[[1]]

    years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
    pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_proj_yr)
    regions_list <- lapply(1:length(object_list), function(x) 1:data_list[[x]]$n_area)
    cutyears_list <- lapply(1:length(object_list), function(x) (max(pyears_list[[x]])-99):max(pyears_list[[x]]))
    cutyears <- unique(unlist(cutyears_list))
    n_iter <- nrow(mcmc_list[[1]][[1]])

    ssb_list <- lapply(1:length(object_list), function(x) {
        n_iter <- nrow(mcmc_list[[x]][[1]])
        ssb <- mcmc_list[[x]]$biomass_ssb_jyr
        dimnames(ssb) <- list("Iteration" = 1:n_iter, "Rule" = 1, "Year" = pyears_list[[x]], "Region" = regions_list[[x]])
        ssb2 <- reshape2::melt(ssb) %>% dplyr::rename("SSB"=value) %>% select(-Rule)
        stock <- strsplit(object_names[x],"_")[[1]][1]
        strat <- strsplit(object_names[x],"_")[[1]][2]
        ssb2$Stock <- stock
        ssb2$Strategy <- strat

        ssb0 <- mcmc_list[[x]]$SSB0_r
        dimnames(ssb0) <- list("Iteration" = 1:n_iter, "Region" = regions_list[[x]])

        ssb0 <- reshape2::melt(ssb0) %>%
            dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
            dplyr::group_by(Iteration, Region, value, Year) %>%
            dplyr::ungroup() %>%
            dplyr::rename("SSB0" = value) %>%
            dplyr::mutate(Stock = stock) %>% 
            dplyr::mutate(Strategy = strat)

        out <- full_join(ssb0, ssb2)

        return(out)
    })
    ssb_df <- do.call(rbind, ssb_list)

    ssb_proj <- ssb_df %>% dplyr::filter(Year %in% cutyears) %>% select(-c(Region))

    slope <- runif(n_iter, 0, 0.2)
    mult <- lapply(1:n_iter, function(x){
        seq1 <- seq(from=0,to=slope[x],length.out=length(cutyears))
        seq2 <- seq1 - mean(seq1) +1
        df <- data.frame("Iteration" = x, "Year" = cutyears, "Multiplier" = seq2)
        return(df)
    })
    mult_df <- do.call(rbind, mult)

    p <- ggplot(mult_df) + 
        geom_line(aes(x = Year, y = Multiplier, color = factor(Iteration))) + 
        guides(color = FALSE) + 
        scale_color_manual(values = colorRampPalette(c("gray", "black"))(n_iter))

    ssb_bias <- full_join(ssb_proj, mult_df) %>%
            dplyr::mutate(SSBadj = SSB * Multiplier)


    ssb_calc <- ssb_bias %>%
            dplyr::group_by(Stock, Strategy, "SSB", "SSB0") %>%
            dplyr::summarise(Pext = length(which(SSBadj <= 0.01*SSB0)==TRUE)/length(SSBadj),
                            P10 = length(which(SSBadj <= 0.1*SSB0)==TRUE)/length(SSBadj),
                            P20 = length(which(SSBadj <= 0.2*SSB0)==TRUE)/length(SSBadj))

    ssb_out <- data.frame(ssb_calc) %>% select(Stock, Strategy, Pext, P10, P20)
    write.table(ssb_out, file=paste0(figure_dir, "Prisk_table.txt"), sep="\t", row.names=FALSE, col.names=TRUE)
    return(ssb_out)
}


#' Compare vulnerable biomass from multiple models
#' 
#' @param object_list list of 'lsd.rds' files from multiple models
#' @param object_names vector of model names associated with each of the output files in object_list
#' @param figure_dir the directory to save the figure to
#' @param save_plot to save the plot to file or not
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#' 
plot_compare_multi_vb <- function(object_list, object_names, 
                                  figure_dir = "compare_figure/", 
                                  save_plot = TRUE)
{
    data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
    mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)

    years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
    pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_proj_yr)
    regions_list <- lapply(1:length(object_list), function(x) 1:data_list[[x]]$n_area)
    sex <- c("Male","Immature female","Mature female")
    seasons <- c("AW", "SS")
    YR <- "YR" # label for the season before the season change year
    cutyears_list <- lapply(1:length(object_list), function(x) (max(pyears_list[[x]])-99):max(pyears_list[[x]]))
    cutyears <- unique(unlist(cutyears_list))


    vb_list <- lapply(1:length(object_list), function(x) {
        n_iter <- nrow(mcmc_list[[x]][[1]])
        biomass_vuln_ytrs2 <- mcmc_list[[x]]$biomass_vuln_ytrs
        dimnames(biomass_vuln_ytrs2) <- list("Iteration" = 1:n_iter, "Year" = pyears_list[[x]], "Season" = seasons, "Region" = regions_list[[x]], Sex = sex)
        biomass_vuln_ytr2 <- reshape2::melt(biomass_vuln_ytrs2) %>%
            dplyr::filter(value>0) %>%
            dplyr::mutate(Season = as.character(Season), Season = ifelse(Year > 1978, Season, YR)) %>%
            dplyr::filter(Season %in% c("YR", "AW")) %>%
            dplyr::group_by(Iteration, Year, Season) %>%
            dplyr::summarise(value = sum(value))
        stock <- strsplit(object_names[x],"_")[[1]][1]
        strat <- strsplit(object_names[x],"_")[[1]][2]
        biomass_vuln_ytr2$Stock <- stock
        biomass_vuln_ytr2$Strategy <- strat
        return(biomass_vuln_ytr2)
    })
    vb <- data.frame(do.call(rbind, vb_list))
    vb$Stock <- factor(vb$Stock)
    vb$Strategy <- factor(vb$Strategy)

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

    vb_cut <- vb %>% dplyr::filter(Year %in% cutyears)

    # Vulnerable biomass
    p <- ggplot(data = vb_cut, aes(x = Year, y = value, color = Strategy, fill = Strategy)) +
        stat_summary(data=vb_cut, fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        #stat_summary(data=vb_cut, fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
        stat_summary(data=vb_cut, fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha=0.75) +
        # scale_fill_manual(values = cols_all, labels = object_names) +
        # scale_colour_manual(values = cols_all, labels = object_names) +
        # guides(colour = guide_legend(override.aes = list(colour = cols_all, linetype = lty_all))) + 
        facet_grid(.~Stock) +
        # scale_linetype(guide=FALSE) +
        expand_limits(y = 0) +
        xlab("Projected fishing year") + ylab("Vulnerable biomass (tonnes)") +
        # scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd()
    
    ## remove    
    # if (data_list[[1]]$n_area > 1) {
    #     p <- p + facet_wrap(~Region)
    # }
    if (save_plot) {
      ggsave(paste0(figure_dir, "biomass_vuln_compare.png"), p, width = 15)
    } else {
      return(p)
    }
}


#' Compare recruitment from multiple models
#' 
#' @param object_list list of 'lsd.rds' files from multiple models
#' @param object_names vector of model names associated with each of the output files in object_list
#' @param figure_dir the directory to save the figure to
#' @param save_plot to save the plot to file or not
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#' 
plot_compare_multi_recruitment <- function(object_list, object_names, 
                                           figure_dir = "compare_figure/", save_plot = TRUE)
{
    data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
    mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)
    
    ny_list <- lapply(1:length(object_list), function(x) dim(mcmc_list[[x]]$recruits_ry)[3])
    years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
    pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:(data_list[[x]]$first_yr + ny_list[[x]] - 1))
    regions_list <- lapply(1:length(object_list), function(x) 1:data_list[[x]]$n_area)
    cutyears_list <- lapply(1:length(object_list), function(x) (max(pyears_list[[x]])-99):max(pyears_list[[x]]))
    cutyears <- unique(unlist(cutyears_list))

    rec_list <- lapply(1:length(object_list), function(x){
        n_iter <- nrow(mcmc_list[[x]][[1]])
        recruits2 <- mcmc_list[[x]]$recruits_ry
        dimnames(recruits2) <- list("Iteration" = 1:n_iter, "Region" = regions_list[[x]], "Year" = pyears_list[[x]])
        recruits2 <- reshape2::melt(recruits2) %>%
           # dplyr::filter(Year <= max(years_list[[x]])) %>%
            dplyr::group_by(Iteration, Year) %>%
            dplyr::summarise(value = sum(value))

        stock <- strsplit(object_names[x],"_")[[1]][1]
        strat <- strsplit(object_names[x],"_")[[1]][2]
        recruits2$Stock <- stock
        recruits2$Strategy <- strat
        recruits2$qconstant <- as.character(ifelse(grepl("qconstant",object_names[[x]]),1,0))
        recruits2
    })
    recruits <- data.frame(do.call(rbind, rec_list)) %>%
        group_by(Iteration, Year, qconstant, Stock, Strategy) %>%
        summarise(value = sum(value))
    recruits$Stock <- factor(recruits$Stock)
    recruits$Strategy <- factor(recruits$Strategy)
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

    rec_cut <- recruits %>% dplyr::filter(Year %in% cutyears)

   p <- ggplot(data = rec_cut, aes(x = Year, y = value, color = Strategy, fill = Strategy)) +
        stat_summary(data=rec_cut, fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        #stat_summary(data=rec_cut, fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
        stat_summary(data=rec_cut, fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha=0.75) +
        # scale_fill_manual(values = cols_all, labels = object_names) +
        # scale_colour_manual(values = cols_all, labels = object_names) +
        # guides(colour = guide_legend(override.aes = list(colour = cols_all, linetype = lty_all))) + 
        # scale_linetype(guide=FALSE) +
        facet_grid(.~Stock) +
        expand_limits(y = 0) +
        xlab("Projected fishing year") + ylab("Recruitment (millions of individuals)") +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd()
   
    # if (data_list[[1]]$n_area > 1) {
    #     p <- p + facet_wrap(~Region)
    # }
    
   if (save_plot) {
      ggsave(paste0(figure_dir, "recruitment_compare.png"), p, width = 10)
   } else {
     return(p)
   }
}


##' Compare catchability coefficient q from multiple models
# #' 
# #' @param object_list list of 'lsd.rds' files from multiple models
# #' @param object_names vector of model names associated with each of the output files in object_list
# #' @export
# #' 
# plot_compare_multi_q <- function(object_list, object_names, figure_dir = "compare_figure/", save_plot = TRUE)
# {
#     data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
#     mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)

#     n_iter <- nrow(mcmc_list[[1]][[1]])

#     years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
#     pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_proj_yr)
#     cutyears_list <- lapply(1:length(object_list), function(x) (max(pyears_list[[x]])-20):max(pyears_list[[x]]))
#     cutyears <- unique(unlist(cutyears_list))

#     nq_list <- lapply(1:length(object_list), function(x) data_list[[x]]$n_q)
#     maxq <- max(unlist(sapply(1:length(object_list), function(x) nq_list[[x]])))
#     q_info_list <- lapply(1:length(object_list), function(x) data.frame("qtype"=data_list[[x]]$data_cpue_q_i, "Season" = data_list[[x]]$data_cpue_season_i, "Year" = data_list[[x]]$data_cpue_year_i, "Region" = data_list[[x]]$data_cpue_area_i))
#     q_info_list <- lapply(1:length(object_list), function(x){
#         if (max(q_info_list[[x]]$qtype) < maxq & max(q_info_list[[x]]$Year) == max(unlist(sapply(1:length(object_list), function(x) q_info_list[[x]]$Year)))){
#             subq <- q_info_list[[x]]$qtype
#             max <- max(subq)
#             for (i in 1:length(subq)) {
#                 if (subq[i]==max) {
#                     subq[i] <- maxq
#                     next
#                 }
#                 if (subq[i]!=max) subq[i] <- subq[i] + (maxq - max) 
#             }
#             q_info_list[[x]]$qtype <- subq
#         }
#         stock <- strsplit(object_names[x],"_")[[1]][1]
#         strat <- strsplit(object_names[x],"_")[[1]][2]

#         q_info_list[[x]] %>%
#             dplyr::filter(Season == 1) %>%
#             dplyr::mutate(QY = paste(Year, qtype))
#     })

#     q_list <- lapply(1:length(object_list), function(x) {
#         n_iter <- nrow(mcmc_list[[x]][[1]])
#         q2 <- mcmc_list[[x]]$par_q_cpue_qy
#         dimnames(q2) <- list("Iteration" = 1:n_iter, "qtype"=1:nq_list[[x]], "Year" = pyears_list[[x]])
#         q2 <- reshape2::melt(q2)

#         if(max(q2$qtype) < maxq) {
#             subq <- q2$qtype
#             max <- max(subq)
#             for (i in 1:length(subq)) {
#                 if (subq[i]==max){
#                     subq[i] <- maxq
#                     next
#                 }
#                 if(subq[i]!=max) subq[i] <- subq[i] + (maxq - max)             }
#             q2$qtype <- subq
#         }

#         q2 <- q2 %>%
#            # dplyr::filter(Year <= max(years_list[[x]])) %>%
#             dplyr::filter(Year %in% unique(q_info_list[[x]]$Year)) %>%
#             dplyr::mutate(QY = paste(Year, qtype)) %>%
#             dplyr::filter(QY %in% q_info_list[[x]]$QY)
        
#         q2$Model <- object_names[x]
#         q2$qconstant <- as.character(ifelse(grepl("qconstant",object_names[[x]]),1,0))
#         # if (data_list[[x]]$n_area > 1 & "Region" %in% colnames(q2) == FALSE) q2$Region <- "All regions"
#         return(q2)
#     })
    
#     q <- data.frame(do.call(rbind, q_list)) %>%
#         group_by(Iteration, Year, Model, qconstant, qtype, QY)
#     q$Model <- factor(q$Model)
#     q$qconstant <- factor(q$qconstant)


#     # n1 <- sapply(1:length(object_names), function(x) strsplit(object_names[x], "_")[[1]][1])
#     # # n2 <- sapply(1:length(object_names), function(x) strsplit(object_names[x], paste0(n1[x],"_"))[[1]][2])
#     # # n2[which(is.na(n2))] <- "base"

#     # n2u <- unique(n1)
#     # nm <- length(n2u)
#     # if(nm > 2) cols <- brewer.pal(nm, "Set1")
#     # if(nm == 2) cols <- c("tomato", "steelblue")
#     # if(nm == 1) cols <- "tomato"

#     # cols_all <- unlist(as.vector(sapply(1:nm, function(x) rep(cols[x],length(which(n1==n2u[x]))))))    
#     # names(cols_all) <- object_names
#     # lty_all <- rep(1, length(object_names))
#     # if (length(unique(n1)) > 1) lty_all[which(n1 == "qconstant")] <- 3


#     p <- ggplot(data = q, aes(x = Year, y = value, colour=Model, fill=Model)) +
#         stat_summary(fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
#         # stat_summary(fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
#         stat_summary(fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha=0.75) +
#         # scale_fill_manual(values=cols_all, labels=object_names) +
#         # scale_colour_manual(values=cols_all, labels=object_names) +
#         # guides(colour = guide_legend(override.aes = list(colour = cols_all, linetype = lty_all))) + 
#         # scale_linetype(guide=FALSE) +
#         expand_limits(y = 0) +
#         xlab("Fishing year") + ylab("Catchability coefficient (q)") +
#         scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
#         theme_lsd() +
#         facet_wrap(~qtype, scales = "free")
    
#     if (data_list[[1]]$n_area > 1) {
#         p <- p + facet_wrap(~Region)
#     }
    
#     if (save_plot) {
#       ggsave(paste0(figure_dir, "q_y_compare.png"), p, width = 10)
#     } else {
#       return(p)
#     }

# }
