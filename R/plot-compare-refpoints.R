#' Plot rules
#' 
#' @param rules data frame with rule numbers and parameter values
#' @param rule_labels data frame with labels for rules - must be the same length as number of rows in rules
#' @param fig_name name for figure if rule labels are included
#' @param figure_dir the directory to save the figure to
#' @param width option to add width of figure
#' @param height option to add height of figure
#' @import dplyr
#' @importFrom reshape2 melt
#' @importFrom grDevices colorRampPalette gray
#' @importFrom stats runif quantile
#' @export
#' 
plot_rules <- function(rules, rule_labels = NULL, fig_name = NULL, figure_dir = "compare_figure/", width = 8, height=6){

    if(all(is.null(rule_labels))){
        rp <- ggplot(rules) +
        geom_segment(aes(x=0,y=0,xend=par2,yend=0,color=factor(par2))) +
        geom_segment(aes(x=par2,y=0,xend=par3,yend=par5,color=factor(par2))) +
        geom_segment(aes(x=par3,y=par5,xend=par4,yend=par5,color=factor(par2))) +
        geom_segment(aes(x=par4,y=par5,xend=par4,yend=par5*(1+par7),color=factor(par2))) +
        geom_segment(aes(x=par4,y=par5*(1+par7),xend=par4+par6,yend=par5*(1+par7),color=factor(par2)))+
        geom_segment(aes(x=par4+par6,y=par5*(1+par7),xend=par4+par6,yend=(par5*(1+par7))*(1+par7),color=factor(par2))) +
        geom_segment(aes(x=par4+par6,y=(par5*(1+par7))*(1+par7),xend=par4+par6*2,yend=(par5*(1+par7))*(1+par7),color=factor(par2)))+
        geom_segment(aes(x=par4+par6*2,y=(par5*(1+par7))*(1+par7),xend=par4+par6*2,yend=((par5*(1+par7))*(1+par7))*(1+par7),color=factor(par2))) +
        geom_segment(aes(x=par4+par6*2,y=((par5*(1+par7))*(1+par7))*(1+par7),xend=par4+par6*3,yend=((par5*(1+par7))*(1+par7))*(1+par7),color=factor(par2)))+
        guides(color=guide_legend(title="CPUE at TACC=0")) +
        xlab("Offset year CPUE") + ylab("TACC") +
        xlim(c(0,3)) +
        facet_grid(par3~par4) +
        theme_lsd(base_size=14)
        ggsave(file.path(figure_dir, paste0("Rules_CPUE at TACC=0_", fig_name, ".png")), rp)

        rp <- ggplot(rules) +
        geom_segment(aes(x=0,y=0,xend=par2,yend=0,color=factor(par5),linetype=factor(par2))) +
        geom_segment(aes(x=par2,y=0,xend=par3,yend=par5,color=factor(par5),linetype=factor(par2))) +
        geom_segment(aes(x=par3,y=par5,xend=par4,yend=par5,color=factor(par5),linetype=factor(par2))) +
        geom_segment(aes(x=par4,y=par5,xend=par4,yend=par5*(1+par7),color=factor(par5),linetype=factor(par2))) +
        geom_segment(aes(x=par4,y=par5*(1+par7),xend=par4+par6,yend=par5*(1+par7),color=factor(par5),linetype=factor(par2)))+
        geom_segment(aes(x=par4+par6,y=par5*(1+par7),xend=par4+par6,yend=(par5*(1+par7))*(1+par7),color=factor(par5),linetype=factor(par2))) +
        geom_segment(aes(x=par4+par6,y=(par5*(1+par7))*(1+par7),xend=par4+par6*2,yend=(par5*(1+par7))*(1+par7),color=factor(par5),linetype=factor(par2)))+
        geom_segment(aes(x=par4+par6*2,y=(par5*(1+par7))*(1+par7),xend=par4+par6*2,yend=((par5*(1+par7))*(1+par7))*(1+par7),color=factor(par5),linetype=factor(par2))) +
        geom_segment(aes(x=par4+par6*2,y=((par5*(1+par7))*(1+par7))*(1+par7),xend=par4+par6*3,yend=((par5*(1+par7))*(1+par7))*(1+par7),color=factor(par5),linetype=factor(par2)))+
        guides(color=guide_legend(title="TACC Plateau"), linetype=guide_legend(title="CPUE at TACC=0")) +
        xlab("Offset year CPUE") + ylab("TACC") +
        facet_grid(par3~par4) +
        xlim(c(0,3)) +
        theme_lsd(base_size=14)
        ggsave(file.path(figure_dir, paste0("Rules_TACC Plateau_CPUE at TACC=0_", fig_name, ".png")), rp)
    }

    if(all(is.null(rule_labels)==FALSE)){
        rule_df <- rules %>% mutate(Label=rule_labels)
            rp <- ggplot(rule_df) +
            geom_segment(aes(x=0,y=0,xend=par2,yend=0,color=factor(Label))) +
            geom_segment(aes(x=par2,y=0,xend=par3,yend=par5,color=factor(Label))) +
            geom_segment(aes(x=par3,y=par5,xend=par4,yend=par5,color=factor(Label))) +
            geom_segment(aes(x=par4,y=par5,xend=par4,yend=par5*(1+par7),color=factor(Label))) +
            geom_segment(aes(x=par4,y=par5*(1+par7),xend=par4+par6,yend=par5*(1+par7),color=factor(Label)))+
            geom_segment(aes(x=par4+par6,y=par5*(1+par7),xend=par4+par6,yend=(par5*(1+par7))*(1+par7),color=factor(Label))) +
            geom_segment(aes(x=par4+par6,y=(par5*(1+par7))*(1+par7),xend=par4+par6*2,yend=(par5*(1+par7))*(1+par7),color=factor(Label)))+
            geom_segment(aes(x=par4+par6*2,y=(par5*(1+par7))*(1+par7),xend=par4+par6*2,yend=((par5*(1+par7))*(1+par7))*(1+par7),color=factor(Label))) +
            geom_segment(aes(x=par4+par6*2,y=((par5*(1+par7))*(1+par7))*(1+par7),xend=par4+par6*3,yend=((par5*(1+par7))*(1+par7))*(1+par7),color=factor(Label)))+
            guides(color=FALSE) +
            xlab("Offset year CPUE") + ylab("TACC") +
            xlim(c(0,3)) +
            scale_color_viridis_d() +
            facet_wrap(.~factor(Label)) +
            theme_lsd(base_size=14)
            ggsave(file.path(figure_dir, paste0(fig_name, ".png")), rp, width=width, height=height)
    }

}

#' Read SSB info
#' 
#' @param object_list list of mcmc results
#' @param object_names list of model names
#' @import dplyr
#' @importFrom reshape2 melt
#' @importFrom grDevices colorRampPalette gray
#' @importFrom stats runif quantile
#' @export
#' 
read_SSB <- function(object_list, object_names){

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
    sex <- c("Male","Immature female","Mature female")
    n_iter <- nrow(mcmc_list[[1]][[1]])

        rules <- data_list[[grep("rules",object_names)[1]]]$mp_rule_parameters
        if(all(is.null(rules))==FALSE){
            colnames(rules) <- paste0("par",1:ncol(rules))
            rules <- data.frame(rules) %>% mutate(RuleNum = 1:nrow(rules))
        }

    ssb_list <- lapply(1:length(object_list), function(x){
        n_iter <- nrow(mcmc_list[[x]][[1]])
        ssb <- mcmc_list[[x]]$biomass_ssb_jyr
        dimnames(ssb) <- list("Iteration" = 1:n_iter, "RuleNum" = 1:dim(ssb)[2], "Year" = pyears_list[[x]], "Region" = regions_list[[x]])
        ssb2 <- reshape2::melt(ssb) %>% dplyr::rename("SSB"=value) %>% 
            dplyr::filter(Year %in% cutyears)
            # dplyr::filter(Year > years_list[[x]][length(years_list[[x]])])

        scenario <- strsplit(object_names[x],"_")[[1]][1]
        rule <- strsplit(object_names[x],"_")[[1]][2]
        ssb2$Scenario <- scenario
        ssb2$RuleName <- rule

        ssb0 <- mcmc_list[[x]]$SSB0_r
        dimnames(ssb0) <- list("Iteration" = 1:n_iter, "Region" = regions_list[[x]])
        ssb0 <- reshape2::melt(ssb0) %>%
            dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
            dplyr::rename(SSB0=value) %>%
            dplyr::filter(Year %in% cutyears) %>%
            # dplyr::filter(Year > years_list[[x]][length(years_list[[x]])]) %>%
            dplyr::select(-Region)
        ssb0$Scenario <- scenario
        ssb0$RuleName <- rule

        ssb_out <- ssb2 %>% select(Iteration, Year, SSB, Scenario, RuleName, RuleNum)
        relssb <- full_join(ssb_out, ssb0) %>%
                dplyr::mutate(RelSSB = SSB/SSB0) #%>%
                # dplyr::select(Iteration, Year, Scenario, RuleType, RelSSB)

        return(relssb)
    })
    relssb <- do.call(rbind, ssb_list)

    if(all(is.null(rules))==FALSE){
        relssb_full <- full_join(relssb, rules)     
        relssb1 <- relssb_full %>% filter(RuleName!="rules")
        relssb1 <- mutate(relssb1, 'par1'=NA, 'par2'=NA, 'par3'=NA, 'par4'=NA, 'par5'=NA, 'par6'=NA, 'par7'=NA, 'par8'=NA,'par9'=NA, 'par10'=NA)
        relssb2 <- relssb_full %>% filter(RuleName=="rules") 
        relssb <- rbind.data.frame(relssb1, relssb2)
    }
    return(relssb)
}

#' Read Catch and CPUE info
#' 
#' @param object_list list of mcmc results
#' @param object_names list of model names
#' @import dplyr
#' @importFrom reshape2 melt
#' @importFrom grDevices colorRampPalette gray
#' @importFrom stats runif quantile
#' @export
#' 
read_catch <- function(object_list, object_names){

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
    sex <- c("Male","Immature female","Mature female")
    n_iter <- nrow(mcmc_list[[1]][[1]])

        rules <- data_list[[grep("rules",object_names)[1]]]$mp_rule_parameters
        if(all(is.null(rules))==FALSE){
            colnames(rules) <- paste0("par",1:ncol(rules))
            rules <- data.frame(rules) %>% mutate(RuleNum = 1:nrow(rules))
        }


    catch_list <- lapply(1:length(object_list), function(x){
        n_iter <- nrow(mcmc_list[[x]][[1]])

        # catch <- data_list[[x]]$proj_catch_commercial_r
        dcatch <- mcmc_list[[x]]$proj_catch_commercial_jryt
        dimnames(dcatch) <- list("Iteration"=1:n_iter, "RuleNum"=1:dim(dcatch)[2], "Region"=regions, "Year"=pyears_list[[x]], "Season"=seasons)
        dcatch2 <- reshape2::melt(dcatch, value.name = "Input_catch") %>% 
            dplyr::group_by(Iteration, Year, RuleNum) %>%
            dplyr::summarise(sum(Input_catch)) %>%
            dplyr::rename("Input_catch"="sum(Input_catch)")

            pcatch <- mcmc_list[[x]]$pred_catch_sl_jryt
            dimnames(pcatch) <- list("Iteration"=1:n_iter, "RuleNum"=1:dim(pcatch)[2], "Region"=regions, "Year"=pyears_list[[x]], "Season"=seasons)
            pcatch2 <- reshape2::melt(pcatch, value.name = "Catch")
            pcatch2 <- reshape2::melt(pcatch, value.name = "Catch") %>% 
                dplyr::group_by(Iteration, Year, RuleNum) %>%
                dplyr::summarise(sum(Catch)) %>%
                dplyr::rename("Catch"="sum(Catch)")


        catch <- full_join(dcatch2, pcatch2)

        cpue <- mcmc_list[[x]]$mp_offset_cpue_jry
        dimnames(cpue) <- list("Iteration"=1:n_iter, "RuleNum"=1:dim(cpue)[2], "Region"=regions, "Year"=pyears_list[[x]])
        cpue2 <- reshape2::melt(cpue, value.name="CPUE") 

        catch_cpue <- full_join(catch, cpue2)

        # cresid <- mcmc_list[[x]]$resid_catch_sl_jryt
        # dimnames(cresid) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = pyears_list[[x]], "Season" = seasons)
        # cresid2 <- reshape2::melt(cresid, value.name="Catch_residual") %>%
        #     dplyr::group_by(Iteration, Year) %>%
        #     dplyr::summarise(sum(Catch_residual)) %>%
        #     dplyr::rename("Catch_residual"="sum(Catch_residual)")

        cinfo <- catch_cpue %>% 
             filter(Year %in% cutyears) %>%
             mutate("Catch_residual" = Input_catch - Catch)
        if(grepl("F=", object_names[x])) cinfo$Catch_residual = 0


        ## compare with vulnerable bio
            vuln <- mcmc_list[[x]]$biomass_vuln_jytrs
            dimnames(vuln) <- list("Iteration" = 1:n_iter, "RuleNum"=1:dim(vuln)[2], "Year" = pyears_list[[x]], "Season" = seasons, "Region" = regions, "Sex"=sex)
            vuln2 <- reshape2::melt(vuln) %>% dplyr::rename("VB"=value) %>% 
                dplyr::group_by(Iteration, Year, RuleNum) %>%
                dplyr::summarise(sum(VB)) %>%
                dplyr::rename("VB" = "sum(VB)") %>%
                dplyr::filter(Year %in% cutyears)

        cinfo_out <- full_join(cinfo, vuln2)

        scenario <- strsplit(object_names[x],"_")[[1]][1]
        rule <- strsplit(object_names[x],"_")[[1]][2]
        cinfo_out$Scenario <- scenario
        cinfo_out$RuleName <- rule

        return(cinfo_out)
    })
    catch <- do.call(rbind, catch_list)
    # catch$RuleType <- factor(catch$RuleType)

    if(all(is.null(rules))==FALSE){
        catch_full <- full_join(catch, rules)     
        catch1 <- catch_full %>% filter(RuleName!="rules")
        catch1 <- mutate(catch1, 'par1'=NA, 'par2'=NA, 'par3'=NA, 'par4'=NA, 'par5'=NA, 'par6'=NA, 'par7'=NA, 'par8'=NA,'par9'=NA, 'par10'=NA)
        catch2 <- catch_full %>% filter(RuleName=="rules") #%>% filter(Rule %in% rules_use$Rule)
        catch <- rbind.data.frame(catch1, catch2)
    }

    return(catch)
}

find_refs <- function(object_list, object_names, figure_dir = "compare_figure/"){

    ## spawning stock biomass over time and simulation replicate
    ssb_year <- read_SSB(object_list = object_list, object_names = object_names)

    ## ssb constrained by soft limit
    ssb_summary_risk <- ssb_year %>%
            dplyr::group_by(Scenario, RuleName, RuleNum) %>%
            dplyr::summarise(Extinction = length(which(RelSSB <= 0.01)==TRUE)/length(RelSSB),
                            HardLimit = length(which(RelSSB <= 0.1)==TRUE)/length(RelSSB),
                            SoftLimit = length(which(RelSSB <= 0.2)==TRUE)/length(RelSSB))

    ## catch and cpue over time and simulation replicate
    catch_year <- read_catch(object_list = object_list, object_names = object_names) %>% ungroup()

    ## catch constraint
    catch_year <- catch_year %>% 
            mutate(Catch_residual = round(Catch_residual, 0)) %>% 
            mutate(RuleType = ifelse(grepl("F=", RuleName), "FixedF", ifelse(grepl("C=", RuleName), "FixedCatch", ifelse(grepl("rules", RuleName), "CPUErule", "X")))) %>%
            mutate(CheckCatch = ifelse(Catch_residual > 0, 1, 0))

    ## ssb information with catch constraint
    info_year <- full_join(ssb_year, catch_year)

    ## summarise quantiles, CV
    summary1 <- info_year %>%
        dplyr::group_by(Scenario, RuleType, RuleName, RuleNum) %>%
        dplyr::summarise(C5 = quantile(Catch, prob=0.05),
                         C50 = quantile(Catch, prob=0.5),
                         C95 = quantile(Catch, prob=0.95),
                         B5 = quantile(RelSSB, prob=0.05),
                         B50 = quantile(RelSSB, prob=0.5),
                         B95 = quantile(RelSSB, prob=0.95),
                         CatchConstraint = max(CheckCatch),
                         CV = sd(Catch)/mean(Catch))

    ## summarise total catch
    summary2 <- info_year %>%
        dplyr::group_by(Scenario, RuleType, RuleName, RuleNum, Iteration) %>%
        dplyr::summarise(TotalCatch = sum(Catch)) %>%
        dplyr::group_by(Scenario, RuleType, RuleName, RuleNum) %>%
        dplyr::summarise(TC5 = quantile(TotalCatch, prob=0.05), TC50 = quantile(TotalCatch, prob=0.5), TC95 = quantile(TotalCatch, prob=0.95))   

    ## summary with quantiles, CV, and total catch
    summary <- full_join(summary1, summary2) 

    ## unique rules
    if(any(grepl("rules",object_names))){
        rules <- unique(info_year %>% select(RuleType, RuleName, RuleNum, par1, par2, par3, par4, par5, par6, par7, par8, par9, par10))
        CPUE_rules <- rules %>% filter(RuleType=="CPUErule")
        plot_rules(rules=CPUE_rules, fig_name = "all_rules", figure_dir = figure_dir)
    }
   
    ## summary of quantiles, CV, and total catch labeled with rule descriptions
    if(any(grepl("rules", object_names))) summary <- full_join(x=summary, y=rules)

    ## summary with ssb-related risk constraints, and save
    summary_risk <- full_join(summary, ssb_summary_risk)

    ## identify runs affected by risk constraint
    summary_risk$RiskConstraint <- sapply(1:nrow(summary_risk), function(x) ifelse(summary_risk$SoftLimit[x] > 0.1, 1, 0))
    write.csv(summary_risk, file=paste0(figure_dir, "Summary_Table.csv"), row.names=FALSE)

    summary_risk$Constraint <- sapply(1:nrow(summary_risk), function(x){
        ifelse(summary_risk$RiskConstraint[x] == 1 & summary_risk$CatchConstraint[x] == 1, "Risk&Catch",
            ifelse(summary_risk$RiskConstraint[x] == 1 & summary_risk$CatchConstraint[x] == 0, "Risk",
                ifelse(summary_risk$RiskConstraint[x] == 0 & summary_risk$CatchConstraint[x] == 1, "Catch",
                    "Pass")))
    })
    summary_risk$Constraint <- factor(summary_risk$Constraint, levels = c("Pass","Catch","Risk","Risk&Catch"))

    ## curves -- relative SSB by annual catch
    p <- ggplot(summary_risk) +
        geom_segment(aes(x=B5, xend=B95, y=C50, yend=C50, color = factor(RiskConstraint)), alpha=0.75, lwd=1.3) +
        geom_segment(aes(x=B50, xend=B50, y=C5, yend=C95, color = factor(RiskConstraint)), alpha=0.75, lwd=1.3) +
        geom_point(aes(x=B50, y=C50, fill=factor(RiskConstraint)), pch=21, alpha=0.75, cex=4) +
        expand_limits(x = 0) +
        scale_x_continuous(limits = c(0, 0.7)) +
        facet_grid(Scenario~RuleType, scales="free_y", shrink=FALSE) +
        xlab("Relative spawning stock biomass") +
        ylab("Catch") +
        scale_colour_viridis_d() +
        scale_fill_viridis_d() +
        guides(fill = guide_legend(title = ">10% below soft limit"), color = guide_legend(title = ">10% below soft limit")) +
        theme_lsd(base_size=14)
    ggsave(file.path(figure_dir, "Catch_versus_RelSSB_RiskConstraint.png"), p, width=15, height=6) 

    ## curves -- relative SSB by annual catch
    p <- ggplot(summary_risk) +
        geom_segment(aes(x=B5, xend=B95, y=C50, yend=C50, color = factor(CatchConstraint)), alpha=0.75, lwd=1.3) +
        geom_segment(aes(x=B50, xend=B50, y=C5, yend=C95, color = factor(CatchConstraint)), alpha=0.75, lwd=1.3) +
        geom_point(aes(x=B50, y=C50, fill=factor(CatchConstraint)), pch=21, alpha=0.75, cex=4) +
        expand_limits(x = 0) +
        scale_x_continuous(limits = c(0, 0.7)) +
        facet_grid(Scenario~RuleType, scales="free_y", shrink=FALSE) +
        xlab("Relative spawning stock biomass") +
        ylab("Catch") +
        scale_colour_viridis_d() +
        scale_fill_viridis_d() +
        guides(fill = guide_legend(title = "Fails catch constraint"), color = guide_legend(title = "Fails catch constraint")) +
        theme_lsd(base_size=14)
    ggsave(file.path(figure_dir, "Catch_versus_RelSSB_CatchConstraint.png"), p, width=15, height=6) 

    p <- ggplot(summary_risk) +
        geom_segment(aes(x=B5, xend=B95, y=C50, yend=C50, color = factor(Constraint)), alpha=0.75, lwd=1.3) +
        geom_segment(aes(x=B50, xend=B50, y=C5, yend=C95, color = factor(Constraint)), alpha=0.75, lwd=1.3) +
        geom_point(aes(x=B50, y=C50, fill=factor(Constraint)), pch=21, alpha=0.75, cex=4) +
        expand_limits(x = 0) +
        scale_x_continuous(limits = c(0, 0.7)) +
        facet_grid(Scenario~RuleType, scales="free_y", shrink=FALSE) +
        xlab("Relative spawning stock biomass") +
        ylab("Catch") +
        scale_colour_viridis_d() +
        scale_fill_viridis_d() +
        guides(fill = guide_legend(title = "Constraints"), color = guide_legend(title = "Constraints")) +
        theme_lsd(base_size=14)
    ggsave(file.path(figure_dir, "Catch_versus_RelSSB_Constraints.png"), p, width=15, height=6) 


    ## find MSY for fixed catch and fishing mortality rate
    summary_fixed <- summary_risk %>% filter(RuleType != "CPUErule")

    ## msy unconstrained
    msy1 <- summary_fixed %>% #filter(RuleType!="CPUErule") %>%
        dplyr::group_by(Scenario, RuleType) %>%
        dplyr::summarise(MSY = max(C50[which(CatchConstraint == 0)]))
    msy1$Bmsy <- sapply(1:nrow(msy1), function(x){
        sub <- summary_fixed %>% filter(RuleType==msy1$RuleType[x])
        out <- sub$B50[which(sub$C50==msy1$MSY[x] & sub$CatchConstraint == 0)]
        return(out)
    })
    msy1 <- msy1 %>%  mutate("MSY_type" = "Unconstrained")

    ## maximum catch subject to constraints
    msy2 <- summary_fixed %>%
        dplyr::group_by(Scenario, RuleType) %>%
        dplyr::summarise(MSY = max(C50[which(CatchConstraint == 0 & RiskConstraint == 0)]))
    msy2$Bmsy <- sapply(1:nrow(msy2), function(x){
        sub <- summary_fixed %>% filter(RuleType==msy2$RuleType[x])
        out <- sub$B50[which(sub$C50==msy2$MSY[x] & sub$CatchConstraint == 0 & sub$RiskConstraint == 0)]
        return(out)
    })
    msy2 <- msy2 %>%  mutate("MSY_type" = "Constrained")

    ## theoretical and empirical MSY
    msy <- rbind.data.frame(msy1, msy2)

    ## attach other info
    msy_info_raw <- lapply(1:nrow(msy), function(x){
        sub1 <- msy[x,]
        sub2 <- summary_risk %>% 
            filter(Scenario==sub1$Scenario) %>%
            filter(RuleType==sub1$RuleType) %>%
            filter(C50==sub1$MSY)
        sub2$MSY_type  <- sub1$MSY_type
        return(sub2)
    })
    msy_info_raw <- do.call(rbind, msy_info_raw) %>% ungroup()

    if(any(grepl("rules",object_names))){
        ## filter rules
        summary_rules <- summary_risk %>% filter(RuleType=="CPUErule")

        msy3 <- summary_rules %>%
            dplyr::group_by(Scenario, RuleType) %>%
            dplyr::summarise( MSY = max(C50[which(CatchConstraint == 0)]))

        msy_info_raw_rules <- summary_rules %>% filter(C50 == msy3$MSY) %>% filter(CatchConstraint==0) %>% mutate(MSY_type = "Unconstrained")

        summary_rules$Constraint <- sapply(1:nrow(summary_rules), function(x){
            ifelse(summary_rules$Constraint[x]!="Pass", as.character(summary_rules$Constraint[x]),
                ifelse(summary_rules$C50[x] < as.numeric(msy2[which(msy2$RuleType == "FixedCatch"),"MSY"]), "LowerThanFixedCatch",
                    ifelse(summary_rules$CV[x] > as.numeric(msy_info_raw[which(msy_info_raw$RuleType=="FixedF" & msy_info_raw$MSY_type=="Constrained"),"CV"]), "HigherCV", "Pass")))
        })
        summary_rules$Constraint <- factor(summary_rules$Constraint, levels = c("Pass", "LowerThanFixedCatch","HigherCV","Catch","Risk","Risk&Catch"))  

        filter_rules <- summary_rules %>% filter(Constraint == "Pass") %>% mutate("MSY_type"="Constrained") 

        max <- as.numeric(filter_rules[which(filter_rules$C50==max(filter_rules$C50)), "RuleNum"])
        min <- as.numeric(filter_rules[which(filter_rules$CV==min(filter_rules$CV)), "RuleNum"])

        msy_info_raw_rules_out <- rbind.data.frame(filter_rules, msy_info_raw_rules)
        msy_info_raw_rules_out$MSY_examples <- NA
        msy_info_raw_rules_out$MSY_examples[which(msy_info_raw_rules_out$RuleNum == max)] <- "MaxCatch"
        msy_info_raw_rules_out$MSY_examples[which(msy_info_raw_rules_out$RuleNum == min)] <- "MinCV"
        msy_info_raw$MSY_examples <- NA

            rp <- ggplot(summary_rules) +
            geom_segment(aes(x=0,y=0,xend=par2,yend=0,color=Constraint)) +
            geom_segment(aes(x=par2,y=0,xend=par3,yend=par5,color=Constraint)) +
            geom_segment(aes(x=par3,y=par5,xend=par4,yend=par5,color=Constraint)) +
            geom_segment(aes(x=par4,y=par5,xend=par4,yend=par5*(1+par7),color=Constraint)) +
            geom_segment(aes(x=par4,y=par5*(1+par7),xend=par4+par6,yend=par5*(1+par7),color=Constraint))+
            geom_segment(aes(x=par4+par6,y=par5*(1+par7),xend=par4+par6,yend=(par5*(1+par7))*(1+par7),color=Constraint)) +
            geom_segment(aes(x=par4+par6,y=(par5*(1+par7))*(1+par7),xend=par4+par6*2,yend=(par5*(1+par7))*(1+par7),color=Constraint))+
            geom_segment(aes(x=par4+par6*2,y=(par5*(1+par7))*(1+par7),xend=par4+par6*2,yend=((par5*(1+par7))*(1+par7))*(1+par7),color=Constraint)) +
            geom_segment(aes(x=par4+par6*2,y=((par5*(1+par7))*(1+par7))*(1+par7),xend=par4+par6*3,yend=((par5*(1+par7))*(1+par7))*(1+par7),color=Constraint))+
            # guides(color=guide_legend(title="CPUE at TACC=0")) +
            scale_color_brewer(palette = "Set1") +
            geom_segment(data=filter_rules, aes(x=0,y=0,xend=par2,yend=0,color=Constraint), lwd=2) +
            geom_segment(data=filter_rules, aes(x=par2,y=0,xend=par3,yend=par5,color=Constraint), lwd=2) +
            geom_segment(data=filter_rules, aes(x=par3,y=par5,xend=par4,yend=par5,color=Constraint), lwd=2) +
            geom_segment(data=filter_rules, aes(x=par4,y=par5,xend=par4,yend=par5*(1+par7),color=Constraint), lwd=2) +
            geom_segment(data=filter_rules, aes(x=par4,y=par5*(1+par7),xend=par4+par6,yend=par5*(1+par7),color=Constraint), lwd=2)+
            geom_segment(data=filter_rules, aes(x=par4+par6,y=par5*(1+par7),xend=par4+par6,yend=(par5*(1+par7))*(1+par7),color=Constraint), lwd=2) +
            geom_segment(data=filter_rules, aes(x=par4+par6,y=(par5*(1+par7))*(1+par7),xend=par4+par6*2,yend=(par5*(1+par7))*(1+par7),color=Constraint), lwd=2)+
            geom_segment(data=filter_rules, aes(x=par4+par6*2,y=(par5*(1+par7))*(1+par7),xend=par4+par6*2,yend=((par5*(1+par7))*(1+par7))*(1+par7),color=Constraint), lwd=2) +
            geom_segment(data=filter_rules, aes(x=par4+par6*2,y=((par5*(1+par7))*(1+par7))*(1+par7),xend=par4+par6*3,yend=((par5*(1+par7))*(1+par7))*(1+par7),color=Constraint), lwd=2)+
            # scale_color_viridis_d() +
            xlab("Offset year CPUE") + ylab("TACC") +
            xlim(c(0,3)) +
            facet_grid(par3~par4) +
            theme_lsd(base_size=14)
            ggsave(file.path(figure_dir, "Filter_rules.png"), rp, width = 15, height = 10)  

        summary_msy <- rbind.data.frame(msy_info_raw, msy_info_raw_rules_out)
        summary_all <- rbind.data.frame(summary_fixed, summary_rules)
    } else {
        summary_msy <- msy_info_raw
        summary_msy$MSY_examples <- NA
        summary_all <- summary_fixed
    }

  ## curves with MSY
    p <- ggplot(summary_risk) +
        geom_segment(aes(x=B5, xend=B95, y=C50, yend=C50, color = factor(Constraint)), alpha=0.75, lwd=1.3) +
        geom_segment(aes(x=B50, xend=B50, y=C5, yend=C95, color = factor(Constraint)), alpha=0.75, lwd=1.3) +
        geom_point(aes(x=B50, y=C50, fill=factor(Constraint)), pch=21, alpha=0.75, cex=4) +
        geom_hline(data=summary_msy %>% filter(MSY_type=="Constrained"), aes(yintercept = C50, linetype = "Constrained")) +
        geom_hline(data=summary_msy %>% filter(MSY_type=="Constrained"), aes(yintercept = C50, lty="Constrained")) +
        geom_hline(data=summary_msy %>% filter(MSY_type=="Unconstrained"), aes(yintercept = C50, linetype = "Unconstrained")) + 
        expand_limits(x = 0) +
        scale_x_continuous(limits = c(0, 0.7)) +
        facet_grid(Scenario~RuleType, scales="free_y", shrink=FALSE) +
        xlab("Relative spawning stock biomass") +
        ylab("Catch") +
        scale_colour_viridis_d() +
        scale_fill_viridis_d() +
        guides(fill = guide_legend(title = "Constraints"), color = guide_legend(title = "Constraints")) +
        theme_lsd(base_size=14)
    ggsave(file.path(figure_dir, "Catch_versus_RelSSB_Constraints_MSYlines.png"), p, width=15, height=6) 

    ## total yield vs cv by rule type
    p <- ggplot(summary_risk) +
        geom_segment(aes(x = TC5, xend = TC95, y = CV, yend = CV)) +
        geom_point(aes(x = TC50, y = CV), cex=3) +
        xlab("Total yield") +
        ylab("CV") +
        facet_grid(Scenario~RuleType) +
        theme_lsd(base_size = 14)
    ggsave(file.path(figure_dir, "TotalYield_vs_CV_RuleType.png"), p)

    ## total yield vs cv by constraint and rule type
    p <- ggplot(summary_risk) +
        geom_segment(aes(x = TC5, xend = TC95, y = CV, yend = CV, color = Constraint)) +
        geom_point(aes(x = TC50, y = CV, fill = Constraint), pch=21, cex=3) +
        xlab("Total yield") +
        ylab("CV") +
        scale_color_viridis_d(option="C") +
        scale_fill_viridis_d(option="C") +
        facet_grid(Scenario~RuleType) +
        guides( color = FALSE) +
        theme_lsd(base_size = 14)
    ggsave(file.path(figure_dir, "TotalYield_vs_CV_Constraint_RuleType.png"), p)

    ## total yield vs cv for constrained MSY (fixed) and filtered rules
    p <- ggplot(summary_msy) +
        geom_segment(aes(x = TC5, xend = TC95, y = CV, yend = CV, color = RuleType, alpha = MSY_type)) +
        geom_point(aes(x = TC50, y = CV, fill = RuleType, alpha = MSY_type), pch=21, cex=3) +
        scale_alpha_manual(values = c(1, 0.3)) +
        xlab("Total yield") +
        ylab("CV") +
        scale_color_viridis_d(option="C") +
        scale_fill_viridis_d(option="C") +
        facet_grid(Scenario~.) +
        guides( color = FALSE) +
        theme_lsd(base_size = 14)
    ggsave(file.path(figure_dir, "TotalYield_vs_CV_Filtered.png"), p)

    ## total yield vs cv for constrained MSY
    p <- ggplot(summary_msy %>% filter(MSY_type == "Constrained")) +
        geom_segment(aes(x = TC5, xend = TC95, y = CV, yend = CV, color = RuleType)) +
        geom_point(aes(x = TC50, y = CV, fill = RuleType), pch=21, cex=3) +
        xlab("Total yield") +
        ylab("CV") +
        scale_color_viridis_d(option="C") +
        scale_fill_viridis_d(option="C") +
        facet_grid(Scenario~.) +
        guides( color = FALSE) +
        theme_lsd(base_size = 14)
    ggsave(file.path(figure_dir, "TotalYield_vs_CV_Constrained.png"), p)

    ## total yield vs cv by rule type
    p <- ggplot(summary_risk) +
        geom_segment(aes(x = C5, xend = C95, y = CV, yend = CV)) +
        geom_point(aes(x = C50, y = CV), cex=3) +
        xlab("Total yield") +
        ylab("CV") +
        facet_grid(Scenario~RuleType) +
        theme_lsd(base_size = 14)
    ggsave(file.path(figure_dir, "AnnualYield_vs_CV_RuleType.png"), p)

    ## total yield vs cv by constraint and rule type
    p <- ggplot(summary_risk) +
        geom_segment(aes(x = C5, xend = C95, y = CV, yend = CV, color = Constraint)) +
        geom_point(aes(x = C50, y = CV, fill = Constraint), pch=21, cex=3) +
        xlab("Total yield") +
        ylab("CV") +
        scale_color_viridis_d(option="C") +
        scale_fill_viridis_d(option="C") +
        facet_grid(Scenario~RuleType) +
        guides( color = FALSE) +
        theme_lsd(base_size = 14)
    ggsave(file.path(figure_dir, "AnnualYield_vs_CV_Constraint_RuleType.png"), p)

    ## total yield vs cv for constrained MSY (fixed) and filtered rules
    p <- ggplot(summary_msy) +
        geom_segment(aes(x = C5, xend = C95, y = CV, yend = CV, color = RuleType, alpha = MSY_type)) +
        geom_point(aes(x = C50, y = CV, fill = RuleType, alpha = MSY_type), pch=21, cex=3) +
        scale_alpha_manual(values = c(1, 0.3)) +
        xlab("Total yield") +
        ylab("CV") +
        scale_color_viridis_d(option="C") +
        scale_fill_viridis_d(option="C") +
        facet_grid(Scenario~.) +
        guides( color = FALSE) +
        theme_lsd(base_size = 14)
    ggsave(file.path(figure_dir, "AnnualYield_vs_CV_Filtered.png"), p)

    ## total yield vs cv for constrained MSY
    p <- ggplot(summary_msy %>% filter(MSY_type == "Constrained")) +
        geom_segment(aes(x = C5, xend = C95, y = CV, yend = CV, color = RuleType)) +
        geom_point(aes(x = C50, y = CV, fill = RuleType), pch=21, cex=3) +
        xlab("Total yield") +
        ylab("CV") +
        scale_color_viridis_d(option="C") +
        scale_fill_viridis_d(option="C") +
        facet_grid(Scenario~.) +
        guides( color = FALSE) +
        theme_lsd(base_size = 14)
    ggsave(file.path(figure_dir, "AnnualYield_vs_CV_Constrained.png"), p)


    catch_year2 <- inner_join(catch_year, summary_all)
    if(any(grepl("rules",object_names))){
        catch_year2$RuleType = factor(catch_year2$RuleType, levels = c("CPUErule","FixedF","FixedCatch"))
        catch_year2$Constraint <- factor(catch_year2$Constraint, levels = c("Risk&Catch","Risk","Catch","HigherCV","LowerThanFixedCatch","Pass"))
    } else { 
        catch_year2$RuleType = factor(catch_year2$RuleType, levels = c("FixedF","FixedCatch")) 
        catch_year2$Constraint <- factor(catch_year2$Constraint, levels = c("Risk&Catch","Risk","Catch","Pass"))
    }

    catch_year_msy <- inner_join(catch_year2, summary_msy)
    catch_year_msy$MSY_type <- factor(catch_year_msy$MSY_type, levels = c(NA, "Unconstrained", "Constrained"))
    catch_year_msy$MSY_examples <- factor(catch_year_msy$MSY_examples, levels = c(NA, "MaxCatch", "MinCV"))

    ## CPUE vs TACC by rule type
    p <- ggplot(catch_year2 %>% filter(Iteration == 1:12)) +
        geom_line(aes(x = CPUE, y = Catch, color = RuleType), alpha=0.7) +
        scale_color_viridis_d() +
        facet_wrap(Iteration~.) +
        theme_lsd(base_size = 14)
    ggsave(file.path(figure_dir, "CPUE_vs_TACC_RuleType_multiiter.png"), p)

    p <- ggplot(catch_year2 %>% filter(Iteration == 1)) +
        geom_line(aes(x = CPUE, y = Catch, color = RuleType), alpha=0.7) +
        scale_color_viridis_d() +
        theme_lsd(base_size = 14)
    ggsave(file.path(figure_dir, "CPUE_vs_TACC_RuleType_iterexample.png"), p)

    ## CPUE vs TACC by constraint and rule type
    p <- ggplot(catch_year2 %>% filter(Iteration == 1)) +
        geom_line(aes(x = CPUE, y = Catch, color = Constraint), alpha=0.7, lwd=1.2) +
        scale_color_viridis_d() +
        facet_wrap(RuleType~.) +
        theme_lsd(base_size = 14)
    ggsave(file.path(figure_dir, "CPUE_vs_TACC_Constraint_RuleType_iterexample.png"), p)

    ## CPUE vs TACC by constraint and rule type with constrained MSY
    p <- ggplot(catch_year2 %>% filter(Iteration == 1)) +
        geom_line(aes(x = CPUE, y = Catch, color = Constraint), alpha=0.7, lwd=1.2) +
        geom_line(data = catch_year_msy %>% filter(Iteration == 1) %>% filter(MSY_type == "Constrained"), aes(x = CPUE, y = Catch), alpha = 0.7, lwd = 2) +
        scale_color_viridis_d() +
        facet_wrap(RuleType~.) +
        theme_lsd(base_size = 14)
    ggsave(file.path(figure_dir, "CPUE_vs_TACC_Constraint_RuleType_MSY_iterexample.png"), p)

    p <- ggplot(catch_year_msy %>% filter(is.na(MSY_type)==FALSE)) +
        geom_line(aes(x = CPUE, y = Catch, color = RuleType), alpha=0.7, lwd=1.2) +
        scale_color_viridis_d() +
        facet_grid(.~MSY_type) +
        theme_lsd(base_size = 14)
    ggsave(file.path(figure_dir, "CPUE_vs_TACC_Constraint_RuleType_MSY.png"), p)

    p <- ggplot(catch_year_msy %>% filter(is.na(MSY_type)==FALSE)) +
        geom_line(aes(x = CPUE, y = Catch, color = RuleType), alpha=0.7, lwd=1.2) +
        geom_line(data = catch_year_msy %>% filter(MSY_examples == "MaxCatch"), aes(x = CPUE, y = Catch), alpha = 0.7, lwd=2) +
        geom_line(data = catch_year_msy %>% filter(MSY_examples == "MinCV"), aes(x = CPUE, y = Catch), alpha = 0.7, lwd=2, lty=2, color = "gray") +        
        scale_color_viridis_d() +
        facet_grid(.~MSY_type) +
        theme_lsd(base_size = 14)
    ggsave(file.path(figure_dir, "CPUE_vs_TACC_RuleType_MSY_MSYexample.png"), p)



    # ## catch over time
    # # catch_year_msy <- catch_year %>% filter(RuleNum %in% msy_info$RuleNum) %>% filter(RuleName %in% msy_info$RuleName) 

    # attach_info <- msy_info %>% select(Scenario, RuleType, RuleName, RuleNum, MSY_type, MSY_desc, MSY_desc2)

    # catch_year_msy_info <- inner_join(catch_year_msy, attach_info)
    # group <- catch_year_msy_info$MSY_desc2
    # catch_year_msy_info$MSY_desc2 <- factor(group, levels = c("CPUErule, Option", "CPUErule, MaxCatch", "CPUErule, MinCV", "FixedCatch", "FixedF"))

    # ## attach description - maximising catch or minimising cv
    # msy_info_raw$MSY_desc <- sapply(1:nrow(msy_info_raw), function(x) ifelse(msy_info_raw$RuleType[x]=="FixedCatch", "MinCV", ifelse(msy_info_raw$RuleType[x] %in% c("FixedF","CPUErule"), "MaxCatch", "X")))

    # ## constrain rules between fixed catch and fixed F MSY
    # ## MSY for fixed catch
    # msy_catch <- msy_info_raw %>% filter(RuleType=="FixedCatch") %>% filter(MSY_type=="Empirical")
    # msy_F <- msy_info_raw %>% filter(RuleType=="FixedF") %>% filter(MSY_type=="Empirical")

    # ## find rules that have higher average yield than fixed catch
    # summary_rules <- summary_risk %>% filter(RuleType=="CPUErule")
    # choose_rules <- summary_rules %>% 
    #                 filter(SoftLimit < 0.1) %>% 
    #                 filter(CatchConstraint <= 0) %>%
    #                 filter(CV < msy_F$CV) %>% 
    #                 filter(C50 > msy_catch$C50)

    # plot_rules(rules=filter_rules, fig_name="constraints_met", figure_dir = figure_dir)
    # plot_rules(rules=filter_rules, rule_labels = filter_rules$RuleNum, fig_name="Rules_constraints_met", figure_dir = figure_dir)

    # ## find rule that has higher average yield than fixed catch with the lowest CV, and include other options
    # rule_maxcatch <- choose_rules %>% filter(C50==max(C50)) %>% mutate(MSY_type = "Empirical") %>% mutate(MSY_desc = "MaxCatch")
    # rule_mincv <- choose_rules %>% filter(CV==min(CV)) %>% mutate(MSY_type = "Empirical") %>% mutate(MSY_desc = "MinCV")
    # rules_other <- choose_rules %>% filter(C50!=max(C50)) %>% filter(CV!=min(CV)) %>% mutate(MSY_type= "Empirical") %>% mutate(MSY_desc="Option")

    # ## remove previous empirical MSY for rules that was not subject to CPUE-rule constraints
    # msy_info_rm <- msy_info_raw[-c(which(msy_info_raw$RuleType=="CPUErule"&msy_info_raw$MSY_type=="Empirical")),]

    # ## include maxcatch and mincv rule
    # msy_info_minmax <- rbind.data.frame(msy_info_rm, rbind.data.frame(rule_maxcatch, rule_mincv))

    # ## include other options
    # msy_info <- rbind.data.frame(msy_info_minmax, rules_other)

    # ## save msy_info
    # write.csv(msy_info, file=paste0(figure_dir, "MSY.csv"),row.names=FALSE)

    # msy_info_rules <- msy_info %>% ungroup() %>% filter(RuleName == "rules") %>% filter(MSY_desc != "Option")
    # plot_rules(rules = msy_info_rules, rule_labels = c("Theoretical MSY", "Empirical MSY, MaxCatch", "Empirical MSY, MinCV"), fig_name = "Rules_MSY", figure_dir = figure_dir)

    # msy_info$MSY_desc2 <- sapply(1:nrow(msy_info), function(x) ifelse(msy_info$RuleType[x]=="CPUErule", paste0("CPUErule, ", msy_info$MSY_desc[x]), as.character(msy_info$RuleType[x])))
    # group <- msy_info$MSY_desc2
    # msy_info$MSY_desc2 <- factor(group, levels = c("CPUErule, Option", "CPUErule, MaxCatch", "CPUErule, MinCV", "FixedCatch", "FixedF"))

    # ## total yield vs cv
    # p <- ggplot(msy_info %>% filter(MSY_type == "Empirical")) +
    #     geom_point(data = msy_info %>% filter(MSY_type=="Empirical") %>% filter(MSY_desc2 == "CPUErule, Option"), aes(x = MedTotalCatch, y = CV, fill = MSY_desc2), pch=21, cex=4) +    
    #     geom_point(data = msy_info %>% filter(MSY_type=="Empirical") %>% filter(MSY_desc2 != "CPUErule, Option"), aes(x = MedTotalCatch, y = CV, fill = MSY_desc2), pch=21, cex=4) +
    #     xlab("Median total yield") +
    #     scale_fill_viridis_d() +
    #     guides(fill = guide_legend(title = "Rule type")) +
    #     theme_lsd(base_size = 14)
    # ggsave(file.path(figure_dir, "TotalYield_vs_CV_all.png"), p, width=9)  

    # p <- ggplot(msy_info %>% filter(MSY_type == "Empirical")) +
    #     geom_point(data = msy_info %>% filter(MSY_type=="Empirical") %>% filter(MSY_desc2 != "CPUErule, Option"), aes(x = MedTotalCatch, y = CV, fill = MSY_desc2), pch=21, cex=4) +
    #     xlab("Median total yield") +
    #     scale_fill_viridis_d() +
    #     guides(fill = guide_legend(title = "Rule type")) +
    #     theme_lsd(base_size = 14)
    # ggsave(file.path(figure_dir, "TotalYield_vs_CV_msy.png"), p, width=9)  

    # ## average yield vs cv
    # p <- ggplot(msy_info %>% filter(MSY_type == "Empirical")) +
    #     geom_point(data = msy_info %>% filter(MSY_type=="Empirical") %>% filter(MSY_desc2 == "CPUErule, Option"), aes(x = C50, y = CV, fill = MSY_desc2), pch=21, cex=4) +    
    #     geom_point(data = msy_info %>% filter(MSY_type=="Empirical") %>% filter(MSY_desc2 != "CPUErule, Option"), aes(x = C50, y = CV, fill = MSY_desc2), pch=21, cex=4) +
    #     xlab("Median annual yield") +
    #     scale_fill_viridis_d() +
    #     guides(fill = guide_legend(title = "Rule type")) +
    #     theme_lsd(base_size = 14)
    # ggsave(file.path(figure_dir, "AnnualYield_vs_CV_all.png"), p, width=9)  

    # p <- ggplot(msy_info %>% filter(MSY_type == "Empirical")) +
    #     geom_point(data = msy_info %>% filter(MSY_type=="Empirical") %>% filter(MSY_desc2 != "CPUErule, Option"), aes(x = C50, y = CV, fill = MSY_desc2), pch=21, cex=4) +
    #     xlab("Median annual yield") +
    #     scale_fill_viridis_d() +
    #     guides(fill = guide_legend(title = "Rule type")) +
    #     theme_lsd(base_size = 14)
    # ggsave(file.path(figure_dir, "AnnualYield_vs_CV_msy.png"), p, width=9)  

    # ## comparing empirical and theoretical MSY
    # ## theoretical
    # msy_det <- msy_info %>%# ungroup() %>%
    # select(Scenario, RuleType, MSY_type, MSY_desc, C50, B50) %>% 
    # filter(MSY_type=="Theoretical") %>%
    # rename(MSY = C50) %>%
    # rename(Bmsy = B50) #%>%
    # # select(-c(MSY_type))

    # ## empirical
    # msy_emp <- msy_info %>%# ungroup() %>%
    # select(Scenario, RuleType, MSY_type, MSY_desc, C50, B50) %>%
    # filter(grepl("Empirical",MSY_type)) %>%
    # rename(eMSY = C50) %>%
    # rename(eBmsy = B50) #%>%
    # # select(-c(MSY_type))

    # ## putting info in the same data frame
    # msy_ratios <- msy_emp 
    # msy_ratios$MSY <- sapply(1:nrow(msy_ratios), function(x) msy_det$MSY[match(msy_ratios$RuleType[x], msy_det$RuleType)])
    # msy_ratios$Bmsy <- sapply(1:nrow(msy_ratios), function(x) msy_det$Bmsy[match(msy_ratios$RuleType[x], msy_det$RuleType)])
        
    # msy_ratios$MSY_desc <- sapply(1:nrow(msy_ratios), function(x) ifelse(msy_ratios$RuleType[x]=="CPUErule", paste0("CPUErule, ", msy_ratios$MSY_desc[x]), as.character(msy_ratios$RuleType[x])))
    # group <- msy_ratios$MSY_desc
    # msy_ratios$MSY_desc <- factor(group, levels = c("CPUErule, Option", "CPUErule, MaxCatch", "CPUErule, MinCV", "FixedCatch", "FixedF"))

    # ## ratios plot eMSY/MSY
    # p <- ggplot(msy_ratios) +
    #     geom_vline(aes(xintercept = 1)) + 
    #     geom_hline(aes(yintercept = 1)) +
    #     geom_point(aes(x = (eBmsy/Bmsy), y = (eMSY / MSY), fill=MSY_desc), cex=4, pch=21, alpha=0.7) +
    #     xlab("Empirical / theoretical Bmsy") + ylab("Empirical / theoretical MSY") + 
    #     scale_fill_viridis_d() +
    #     # scale_fill_brewer(palette = "Paired") +
    #     # scale_shape_manual(values = seq(21,by=1,length.out=length(unique(msy_ratios$Scenario)))) +
    #     expand_limits(x = 0, y = 0) + 
    #     guides(fill = guide_legend(title = "Rule type")) +
    #     scale_x_continuous(expand = c(0,0), limits = c(0, max(c(1.05, msy_ratios$eBmsy/msy_ratios$Bmsy)*1.05))) +
    #     scale_y_continuous(expand = c(0,0), limits = c(0, max(c(1.05, msy_ratios$eMSY/msy_ratios$MSY)*1.05))) + 
    #     theme_lsd(base_size = 14) 
    # ggsave(file.path(figure_dir, "eMSY_MSY_all.png"), p, width=9)  

    # p <- ggplot(msy_ratios %>% filter(grepl("Option", MSY_desc)==FALSE)) +
    #     geom_vline(aes(xintercept = 1)) + 
    #     geom_hline(aes(yintercept = 1)) +
    #     geom_point(aes(x = (eBmsy/Bmsy), y = (eMSY / MSY), fill=MSY_desc), cex=4, pch=21, alpha=0.7) +
    #     xlab("Empirical / theoretical Bmsy") + ylab("Empirical / theoretical MSY") + 
    #     scale_fill_viridis_d() +
    #     # scale_fill_brewer(palette = "Paired") +
    #     # scale_shape_manual(values = seq(21,by=1,length.out=length(unique(msy_ratios$Scenario)))) +
    #     expand_limits(x = 0, y = 0) + 
    #     guides(fill = guide_legend(title = "Rule type")) +
    #     scale_x_continuous(expand = c(0,0), limits = c(0, max(c(1.05, msy_ratios$eBmsy/msy_ratios$Bmsy)*1.05))) +
    #     scale_y_continuous(expand = c(0,0), limits = c(0, max(c(1.05, msy_ratios$eMSY/msy_ratios$MSY)*1.05))) + 
    #     theme_lsd(base_size = 14) 
    # ggsave(file.path(figure_dir, "eMSY_MSY.png"), p, width=9)  



 
    # p1 <- ggplot(catch_year_msy_info %>% filter(MSY_desc != "Option")) +
    #     stat_summary(aes(x=Year, y=Catch, color = MSY_desc2, fill = MSY_desc2), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    #     stat_summary(aes(x=Year, y=Catch, color = MSY_desc2), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
    #     geom_line(data = catch_year_msy_info %>% filter(MSY_desc != "Option")%>% filter(Iteration==1), aes(x=Year, y=Catch)) +
    #     ylab("TACC") +
    #     guides(color = FALSE, fill = FALSE) +
    #     scale_color_viridis_d() +
    #     scale_fill_viridis_d() +
    #     # scale_color_brewer(palette = "RdYlBu") +
    #     # scale_fill_brewer(palette = "RdYlBu") +
    #     expand_limits(y = 0) +
    #     theme_lsd(base_size = 14) +
    #     facet_grid(MSY_type~MSY_desc2)
    # ggsave(file.path(figure_dir, "TACC_over_time_MSYcompare.png"), p1, width=15, height=6)

    # p2 <- ggplot(catch_year_msy_info %>% filter(MSY_desc != "Option")) +
    #     stat_summary(aes(x=Year, y=VB, color = MSY_desc2, fill = MSY_desc2), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    #     stat_summary(aes(x=Year, y=VB, color = MSY_desc2), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
    #     geom_line(data = catch_year_msy_info %>% filter(MSY_desc != "Option")%>% filter(Iteration==1), aes(x=Year, y=VB)) +
    #     ylab("Vulnerable biomass") +
    #     guides(color = FALSE, fill = FALSE) +
    #     scale_color_viridis_d() +
    #     scale_fill_viridis_d() +
    #     # scale_color_brewer(palette = "RdYlBu") +
    #     # scale_fill_brewer(palette = "RdYlBu") +
    #     expand_limits(y = 0) +
    #     theme_lsd(base_size = 14) +
    #     facet_grid(MSY_type~MSY_desc2)
    # ggsave(file.path(figure_dir, "VB_over_time_MSYcompare.png"), p1, width=15, height=6)

    # p4 <- ggplot(catch_year_msy_info) +
    #     geom_line(aes(x = CPUE, y = Catch, color = MSY_desc2), lwd=1.5, alpha=0.8) +
    #     facet_grid(MSY_type~.) +
    #     ylab("TACC") + xlab("Offset-year CPUE") +
    #     scale_color_viridis_d() +
    #     guides(color = guide_legend(title = "Rule type")) +
    #     scale_x_continuous(limits = c(0, max(catch_year_msy_info$CPUE)*1.01), expand=c(0,0)) +
    #     scale_y_continuous(limits = c(0, max(catch_year_msy_info$Catch)*1.05), expand=c(0,0)) +
    #     theme_lsd(base_size=14)
    # ggsave(file.path(figure_dir, "CPUE_vs_TACC_options.png"), p4, width=8, height=6)

    # p4 <- ggplot(catch_year_msy_info  %>% filter(grepl("Option", MSY_desc2)==FALSE)) +
    #     geom_line(aes(x = CPUE, y = Catch, color = MSY_desc2), lwd=1.5, alpha=0.8) +
    #     facet_grid(MSY_type~.) +
    #     ylab("TACC") + xlab("Offset-year CPUE") +
    #     scale_color_viridis_d() +
    #     guides(color = guide_legend(title = "Rule type")) +
    #     scale_x_continuous(limits = c(0, max(catch_year_msy_info$CPUE)*1.01), expand=c(0,0)) +
    #     scale_y_continuous(limits = c(0, max(catch_year_msy_info$Catch)*1.05), expand=c(0,0)) +
    #     theme_lsd(base_size=14)
    # ggsave(file.path(figure_dir, "CPUE_vs_TACC_MSY.png"), p4, width=8, height=6)

    # catch_year_rules <- catch_year %>% filter(RuleName == "rules")
    # maxnum <- msy_info$RuleNum[which(msy_info$RuleType=="CPUErule" & msy_info$MSY_type=="Theoretical" & msy_info$MSY_desc == "MaxCatch")]
    # emp_maxc <- msy_info$RuleNum[which(msy_info$RuleType=="CPUErule" & msy_info$MSY_type=="Empirical" & msy_info$MSY_desc == "MaxCatch")]
    # emp_mincv <- msy_info$RuleNum[which(msy_info$RuleType=="CPUErule" & msy_info$MSY_type=="Empirical" & msy_info$MSY_desc == "MinCV")]
    # emp_opt <- msy_info$RuleNum[which(msy_info$RuleType=="CPUErule" & msy_info$MSY_type=="Empirical" & msy_info$MSY_desc == "Option")]

    # Group <- sapply(1:nrow(catch_year_rules), function(x){
    #         out <- "All rules"
    #         if(catch_year_rules$RuleNum[x] == maxnum) out <- "Theoretical MSY"
    #         if(catch_year_rules$RuleNum[x] %in% emp_opt) out <- "Empirical MSY, Option"
    #         if(catch_year_rules$RuleNum[x] == emp_mincv) out <- "Empirical MSY, MinCV"
    #         if(catch_year_rules$RuleNum[x] == emp_maxc) out <- "Empirical MSY, MaxCatch"
    #         return(out)
    # })
    # catch_year_rules$Group <- factor(Group, levels=c("All rules", "Theoretical MSY", "Empirical MSY, Option", "Empirical MSY, MaxCatch", "Empirical MSY, MinCV"))

    # p5 <- ggplot(catch_year_rules %>% filter(Iteration == 1)) +
    #     geom_line(aes(x = CPUE, y = Catch, color = Group), lwd=1.5, alpha=0.8) + 
    #     ylab("TACC") + xlab("Offset-year CPUE") +
    #     scale_color_viridis_d() +
    #     guides(color = guide_legend(title = "Rule type")) +
    #     expand_limits(y = 0) +
    #     theme_lsd(base_size=14) +
    #     scale_x_continuous(limits = c(0, max(catch_year_rules$CPUE)*1.01), expand=c(0,0)) +
    #     scale_y_continuous(limits = c(0, max(catch_year_rules$Catch)*1.05), expand=c(0,0)) 
    # ggsave(file.path(figure_dir, "CPUE_vs_TAC_allrules.png"), p5, width=8, height=6)
}
