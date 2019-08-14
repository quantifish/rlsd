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


#' Compare probability of being under risk constraints
#' 
#' @param ssb data frame of ssb output by read_SSB
#' @param object_list list of mcmc results
#' @param object_names list of model names
#' @import dplyr
#' @importFrom reshape2 melt
#' @importFrom grDevices colorRampPalette gray
#' @importFrom stats runif quantile
#' @export
#' 
ssb_risk_constraints <- function(ssb, object_list, object_names){
 
    # ssb_df <- read_SSB(object_list = object_list, object_names = object_names)

    ## calculate SSB-based risk constraints
    ssb_calc <- ssb %>%
            dplyr::group_by(Scenario, RuleName, RuleNum) %>%
            dplyr::summarise(Extinction = length(which(RelSSB <= 0.01)==TRUE)/length(RelSSB),
                            HardLimit = length(which(RelSSB <= 0.1)==TRUE)/length(RelSSB),
                            SoftLimit = length(which(RelSSB <= 0.2)==TRUE)/length(RelSSB))

    ssb_out <- data.frame(ssb_calc) %>% select(Scenario, RuleName, RuleNum, Extinction, HardLimit, SoftLimit)
    # write.table(ssb_out, file=paste0(figure_dir, "SSB_Risk_Table.txt"), sep="\t", row.names=FALSE, col.names=TRUE)
    return(ssb_out)
}


#' Catch constraints
#' 
#' @param catch data frame of catch output by read_catch
#' @param object_list list of 'lsd.rds' files from multiple models
#' @param object_names vector of model names associated with each of the output files in object_list
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#' 
catch_constraint <- function(catch, object_list, object_names)
{
    ## obtain catch data frame
    # catch_df <- read_catch(object_list = object_list, object_names = object_names)

    catch <- catch %>% ungroup() %>%
        dplyr::mutate(RuleType = ifelse(grepl("F=", RuleName), "FixedF", ifelse(grepl("C=", RuleName), "FixedCatch", ifelse(grepl('rules', RuleName), "CPUErule", "X"))))

    catch$CheckCatch <- 0
    catch[which(catch$Catch_residual > 0),"CheckCatch"] <- 1
    catch[which(catch$RuleType!="FixedCatch"),"CheckCatch"] <- 0
    
    return(catch)
}


#' All constraints
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
summary_fun <- function(object_list, object_names, figure_dir = "compare_figure/")
{

    ssb <- read_SSB(object_list = object_list, object_names = object_names)
    catch <- read_catch(object_list = object_list, object_names = object_names)

    ssb_risk <- ssb_risk_constraints(ssb =ssb, object_list=object_list, object_names=object_names)
    catch_risk <- catch_constraint(catch = catch, object_list=object_list, object_names=object_names)

    vals_all <- full_join(ssb, catch_risk)

    summary1 <- vals_all %>%
        dplyr::group_by(Scenario, RuleType, RuleName, RuleNum) %>%
        dplyr::summarise(C5 = quantile(Catch, prob=0.05),
                         C50 = quantile(Catch, prob=0.5),
                         C95 = quantile(Catch, prob=0.95),
                         B5 = quantile(RelSSB, prob=0.05),
                         B50 = quantile(RelSSB, prob=0.5),
                         B95 = quantile(RelSSB, prob=0.95),
                         CatchConstraint = max(CheckCatch),
                         CV = sd(Catch)/mean(Catch),
                         SD = sd(Catch))

    summary2 <- vals_all %>%
        dplyr::group_by(Scenario, RuleType, RuleName, RuleNum, Iteration) %>%
        dplyr::summarise(TotalCatch = sum(Catch)) %>%
        dplyr::group_by(Scenario, RuleType, RuleName, RuleNum) %>%
        dplyr::summarise(MedTotalCatch = median(TotalCatch))

    rules <- unique(vals_all %>% select(RuleType, RuleName, RuleNum, par1, par2, par3, par4, par5, par6, par7, par8, par9, par10))

    summary <- full_join(summary1, summary2)
    summary_wRules <- full_join(x=summary, y=rules)

    ## include risk
    summary_wRisk <- full_join(summary_wRules, ssb_risk)
    summary_wRisk$RuleName <- factor(summary_wRisk$RuleName)
    summary_wRisk$RuleType <- factor(summary_wRisk$RuleType)

    rres <- summary_wRisk %>% filter(RuleType=="CPUErule")
    fres <- summary_wRisk %>% filter(RuleType=="FixedF")
    cres <- summary_wRisk %>% filter(RuleType=="FixedCatch")

    write.csv(summary_wRisk, file=paste0(figure_dir, "Summary_Table_wConstraints.csv"), row.names=FALSE)

    return(summary_wRisk)
}

#' Find MSY
#' 
#' @param risk_summary data frame output by all_risk_constraints
#' @param figure_dir the directory to save the figure to
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#' 
find_msy <- function(risk_summary, soft_limit_req=0.1, catch_resid_req=0, figure_dir = "compare_figure/")
{

    ## maximum catch
    msy1 <- risk_summary %>% #filter(RuleType!="CPUErule") %>%
        dplyr::group_by(Scenario, RuleType) %>%
        dplyr::summarise(MSY = max(C50),
                        Bmsy = B50[which(C50==MSY)]) %>%
        mutate("MSY_type" = "Theoretical")

    ## maximum catch subject to constraints
    msy2 <- risk_summary %>% #filter(RuleType!="CPUErule") %>%
        dplyr::group_by(Scenario, RuleType) %>%
        dplyr::summarise(MSY = max(C50[which(SoftLimit < soft_limit_req & CatchConstraint <= catch_resid_req)]),
                        Bmsy = B50[which(C50==MSY)]) %>%
        mutate("MSY_type" = "Empirical")

    msy <- rbind.data.frame(msy1, msy2)

    msy_info <- lapply(1:nrow(msy), function(x){
        sub1 <- msy[x,]
        sub2 <- risk_summary %>% 
            filter(Scenario==sub1$Scenario) %>%
            filter(RuleType==sub1$RuleType) %>%
            filter(C50==sub1$MSY)
        sub2$MSY_type  <- sub1$MSY_type
        return(sub2)
    })
    msy_info <- do.call(rbind, msy_info) %>% ungroup()

    msy_info$MSY_desc <- sapply(1:nrow(msy_info), function(x) ifelse(msy_info$RuleType[x]=="FixedCatch", "MinCV", ifelse(msy_info$RuleType[x] %in% c("FixedF","CPUErule"), "MaxCatch", "X")))

    ## filter rules with info
    rule_summary <- risk_summary %>% filter(RuleType=="CPUErule") #%>% filter(SoftLimit < soft_limit_req) %>% filter(CatchConstraint <= catch_resid_req)
    plot_rules(rules=rule_summary, fig_name = "all_rules", figure_dir = figure_dir)

    # rule_msydet_maxc <- rule_summary %>% filter(C50 == max(C50)) %>% mutate(MSY_type = "Theoretical") %>% mutate(MSY_desc = "MaxCatch")
    # rule_msydet_mincv <- rule_summary %>% filter(CV==min(CV)) %>% mutate(MSY_type = "Theoretical") %>% mutate(MSY_desc = "MinCV")

    ## MSY for fixed catch
    msy_catch <- msy_info %>% filter(RuleType=="FixedCatch") %>% filter(MSY_type=="Empirical")
    msy_F <- msy_info %>% filter(RuleType=="FixedF") %>% filter(MSY_type=="Empirical")

    ## find rules that have higher average yield than fixed catch
    choose_rules <- rule_summary %>% 
                    filter(SoftLimit < soft_limit_req) %>% 
                    filter(CatchConstraint <= catch_resid_req) %>%
                    filter(CV < msy_F$CV) %>% 
                    filter(C50 > msy_catch$C50)

    plot_rules(rules=choose_rules, fig_name="constraints_met", figure_dir = figure_dir)
    plot_rules(rules=choose_rules, rule_labels = choose_rules$RuleNum, fig_name="Rules_constraints_met", figure_dir = figure_dir)

    ## find rule that has higher average yield than fixed catch with the lowest CV
    rule_maxcatch <- choose_rules %>% filter(C50==max(C50)) %>% mutate(MSY_type = "Empirical") %>% mutate(MSY_desc = "MaxCatch")
    rule_mincv <- choose_rules %>% filter(CV==min(CV)) %>% mutate(MSY_type = "Empirical") %>% mutate(MSY_desc = "MinCV")

    msy_info <- msy_info[-c(which(msy_info$RuleType=="CPUErule"&msy_info$MSY_type=="Empirical")),]
    msy_info <- rbind.data.frame(msy_info, rbind.data.frame(rule_maxcatch, rule_mincv))

    write.csv(msy_info, file=paste0(figure_dir, "MSY.csv"),row.names=FALSE)
    write.csv(choose_rules, file=paste0(figure_dir, "Intermediate_CPUE_rules.csv"), row.names=FALSE)

    msy_info_rules <- msy_info %>% ungroup() %>% filter(RuleName == "rules")
    plot_rules(rules = msy_info_rules, rule_labels = c("Theoretical MSY", "Empirical MSY, MaxCatch", "Empirical MSY, MinCV"), fig_name = "Rules_MSY", figure_dir = figure_dir)

    msy_det <- msy_info %>%# ungroup() %>%
    select(Scenario, RuleType, MSY_type, MSY_desc, C50, B50) %>% 
    filter(MSY_type=="Theoretical") %>%
    rename(MSY = C50) %>%
    rename(Bmsy = B50) #%>%
    # select(-c(MSY_type))

    msy_emp <- msy_info %>%# ungroup() %>%
    select(Scenario, RuleType, MSY_type, MSY_desc, C50, B50) %>%
    filter(grepl("Empirical",MSY_type)) %>%
    rename(eMSY = C50) %>%
    rename(eBmsy = B50) #%>%
    # select(-c(MSY_type))

    msy_ratios <- msy_emp
    msy_ratios$MSY <- sapply(1:nrow(msy_ratios), function(x) msy_det$MSY[match(msy_ratios$RuleType[x], msy_det$RuleType)])
    msy_ratios$Bmsy <- sapply(1:nrow(msy_ratios), function(x) msy_det$Bmsy[match(msy_ratios$RuleType[x], msy_det$RuleType)])
        
    msy_ratios$MSY_desc <- sapply(1:nrow(msy_ratios), function(x) ifelse(msy_ratios$RuleType[x]=="CPUErule", paste0("CPUErule, ", msy_ratios$MSY_desc[x]), as.character(msy_ratios$RuleType[x])))

    p <- ggplot(msy_ratios) +
        geom_vline(aes(xintercept = 1)) + 
        geom_hline(aes(yintercept = 1)) +
        geom_point(aes(x = (eBmsy/Bmsy), y = (eMSY / MSY), fill=MSY_desc), cex=4, pch=21) +
        xlab("Empirical / theoretical Bmsy") + ylab("Empirical / theoretical MSY") + 
        scale_fill_viridis_d() +
        # scale_fill_brewer(palette = "Paired") +
        # scale_shape_manual(values = seq(21,by=1,length.out=length(unique(msy_ratios$Scenario)))) +
        expand_limits(x = 0, y = 0) + 
        guides(fill = guide_legend(title = "Rule type")) +
        scale_x_continuous(expand = c(0,0), limits = c(0, max(c(1.05, msy_ratios$eBmsy/msy_ratios$Bmsy)*1.05))) +
        scale_y_continuous(expand = c(0,0), limits = c(0, max(c(1.05, msy_ratios$eMSY/msy_ratios$MSY)*1.05))) + 
        theme_lsd(base_size = 14) 
    ggsave(file.path(figure_dir, "eMSY_MSY.png"), p, width=9)  
    out <- list()
    out$msy <- msy_info
    out$intermediate_rules <- choose_rules

    return(out) 
}

#' Plot SSB vs Catch
#' 
#' @param risk_summary data frame output by all_risk_constraints
#' @param msy_info msy information output by find_msy
#' @param figure_dir the directory to save the figure to
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#'
plot_curves <- function(risk_summary, msy_info, figure_dir = "compare_figure/"){

    p <- ggplot(risk_summary) +
        # geom_vline(xintercept = 0.1, color=gray(0.3), alpha=0.75) +
        # geom_vline(xintercept = 0.2, color=gray(0.5), alpha=0.75) +
        # geom_segment(data = summary %>% dplyr::filter(RuleType=="FixedF"), aes(x=B5, xend=B95, y=C50, yend=C50), alpha=0.25, lwd=1.3) +
        # geom_segment(data = summary %>% dplyr::filter(RuleType=="FixedF"), aes(x=B50, xend=B50, y=C5, yend=C95), alpha=0.25, lwd=1.3) +
        geom_segment(aes(x=B5, xend=B95, y=C50, yend=C50, color = SoftLimit), alpha=0.75, lwd=1.3) +
        geom_segment(aes(x=B50, xend=B50, y=C5, yend=C95, color = SoftLimit), alpha=0.75, lwd=1.3) +
        # geom_point(data = summary %>% dplyr::filter(RuleType=="FixedF"), aes(x=B50, y=C50, color=RuleType), pch=19, alpha=0.75, cex=4) +
        geom_point(aes(x=B50, y=C50, fill=SoftLimit), pch=21, alpha=0.75, cex=4) +
        expand_limits(x = 0) +
        scale_x_continuous(limits = c(0, 0.7)) +
        facet_grid(Scenario~RuleType, scales="free_y", shrink=FALSE) +
        xlab("Relative spawning stock biomass") +
        ylab("Catch") +
        scale_colour_viridis_c() +
        scale_fill_viridis_c() +
        guides(fill = guide_legend(title = "Probability below\nsoft limit"), color = guide_legend(title = "Probability below\nsoft limit")) +
        theme_lsd(base_size=14)
    ggsave(file.path(figure_dir, "Catch_versus_RelSSB_byProb.png"), p, width=15, height=6) 

    index <- which(msy_info$RuleType=="CPUErule" & msy_info$MSY_type=="Theoretical" & msy_info$MSY_desc=="MinCV")
    if(length(index)>0){
        msy <- msy_info[-index,] %>% ungroup()
    } else { msy <- msy_info %>% ungroup()}
    p <- ggplot(risk_summary) +
        # geom_vline(xintercept = 0.1, color=gray(0.3), alpha=0.75) +
        # geom_vline(xintercept = 0.2, color=gray(0.5), alpha=0.75) +
        # geom_segment(data = summary %>% dplyr::filter(RuleType=="FixedF"), aes(x=B5, xend=B95, y=C50, yend=C50), alpha=0.25, lwd=1.3) +
        # geom_segment(data = summary %>% dplyr::filter(RuleType=="FixedF"), aes(x=B50, xend=B50, y=C5, yend=C95), alpha=0.25, lwd=1.3) +
        geom_segment(aes(x=B5, xend=B95, y=C50, yend=C50, color = SoftLimit), alpha=0.75, lwd=1.3) +
        geom_segment(aes(x=B50, xend=B50, y=C5, yend=C95, color = SoftLimit), alpha=0.75, lwd=1.3) +
        # geom_point(data = summary %>% dplyr::filter(RuleType=="FixedF"), aes(x=B50, y=C50, color=RuleType), pch=19, alpha=0.75, cex=4) +
        geom_point(aes(x=B50, y=C50, fill=SoftLimit), pch=21, alpha=0.75, cex=4) +
        geom_hline(data=msy %>% filter(MSY_type=="Empirical") %>% filter(MSY_desc == "MaxCatch"), aes(yintercept = C50, linetype = "Empirical MSY, MaxCatch")) +
        geom_hline(data=msy %>% filter(MSY_type=="Empirical") %>% filter(MSY_desc == "MinCV"), aes(yintercept = C50, lty="Empirical MSY, MinCV")) +
        geom_hline(data=msy %>% filter(MSY_type=="Theoretical"), aes(yintercept = C50, linetype = "Theoretical MSY")) + 
        expand_limits(x = 0) +
        scale_x_continuous(limits = c(0, 0.7)) +
        facet_grid(Scenario~RuleType, scales="free_y", shrink=FALSE) +
        xlab("Relative spawning stock biomass") +
        ylab("Catch") +
        scale_colour_viridis_c() +
        scale_fill_viridis_c() +
        guides(fill = guide_legend(title = "Probability below\nsoft limit"), color = guide_legend(title = "Probability below\nsoft limit")) +
        theme_lsd(base_size=14) +
        scale_linetype_manual(name = "MSY type", values = c(1,2,3))
    ggsave(file.path(figure_dir, "Catch_versus_RelSSB_byProb_MSYlines.png"), p, width=15, height=6) 
}

#' Various plots to explore rule options
#' 
#' @param msy_list msy information output by find_msy
#' @param object_list
#' @param object_names
#' @param figure_dir the directory to save the figure to
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#'
explore_rules <- function(msy_list, object_list, object_names, figure_dir = "compare_figure/"){

    msy_info <- msy_list$msy %>% ungroup()
    model_names <- unique(sapply(1:nrow(msy_info), function(x) paste0(msy_info$Scenario[x], "_", msy_info$RuleName[x])))
    sub_list <- lapply(1:length(model_names), function(x){
        index <- which(object_names == model_names[x])
        return(object_list[[index]])
    })

    catch_list <- lapply(1:nrow(msy_info), function(x){
        name <- paste0(msy_info$Scenario[x], "_", msy_info$RuleName[x])
        object_list_new <- list()
        object_list_new[[1]] <- sub_list[[which(model_names==name)]]
        catch <- read_catch(object_list = object_list_new, object_names = name)
        if(grepl("rule",name)==FALSE){
            catch_out <- catch
        }
        if(grepl("rule",name)){
            catch_out <- catch %>% filter(RuleNum == msy_info$RuleNum[x])
        }
        catch_out$MSY_type = msy_info$MSY_type[x]
        catch_out$MSY_desc = msy_info$MSY_desc[x]
        catch_out$CV = msy_info$CV[x]
        catch_out$MedTotalCatch = msy_info$MedTotalCatch[x]
        return(catch_out)
    })
    catch_df <- do.call(rbind, catch_list)
    catch_df$RuleType = sapply(1:nrow(catch_df), function(x) ifelse(grepl("rules", catch_df$RuleName[x]), "CPUErule", "Fixed"))
    catch_df$RuleType2 = sapply(1:nrow(catch_df), function(x) ifelse(grepl("rules", catch_df$RuleName[x]), "CPUErule", ifelse(grepl("F=",catch_df$RuleName[x]), "FixedF", "FixedCatch")))
    catch_df$RuleName2 = sapply(1:nrow(catch_df), function(x) paste0(catch_df$RuleType2[x], ", ", catch_df$MSY_desc[x]))
    catch_df$MSY_desc2 = sapply(1:nrow(catch_df), function(x) ifelse(grepl("rules", catch_df$RuleName[x]), paste0("CPUErule, ", catch_df$MSY_desc[x]), catch_df$RuleType2[x]))

    # catch_df$RuleType = sapply(1:nrow(catch_df), function(x) strsplit(catch_df$MSY_label[x],"_")[[1]][1])
    # catch_df$MSYType = sapply(1:nrow(catch_df), function(x) strsplit(catch_df$MSY_label[x],"_")[[1]][2])
    catch_df$MSY_desc[which(catch_df$MSY_desc=="CPUErule")] <- "MaxCatch"

        name <- "base_rules"
        object_list_new <- list()
        object_list_new[[1]] <- sub_list[[which(model_names==name)]]
        catch_rules <- read_catch(object_list = object_list_new, object_names = name)
        maxnum <- data.frame(msy_info %>% filter(RuleType=="CPUErule") %>% filter(MSY_type=="Theoretical"))$RuleNum
        empnum_mincv <- data.frame(msy_info %>% filter(RuleType=="CPUErule") %>% filter(MSY_type=="Empirical") %>% filter(MSY_desc=="MinCV"))$RuleNum
        empnum_maxc <- data.frame(msy_info %>% filter(RuleType=="CPUErule") %>% filter(MSY_type=="Empirical") %>% filter(MSY_desc=="MaxCatch"))$RuleNum

        Group <- sapply(1:nrow(catch_rules), function(x){
            out <- "All rules"
            if(catch_rules$RuleNum[x] == maxnum) out <- "Theoretical MSY"
            if(catch_rules$RuleNum[x] %in% msy_list$intermediate_rules$RuleNum) out <- "Empirical MSY alternatives"
            if(catch_rules$RuleNum[x] == empnum_mincv) out <- "Empirical MSY, MinCV"
            if(catch_rules$RuleNum[x] == empnum_maxc) out <- "Empirical MSY, MaxCatch"
            return(out)
        })
        catch_rules$Group <- Group


    p1 <- ggplot(catch_df %>% filter(MSY_type=="Empirical")) +
        stat_summary(aes(x=Year, y=Catch, color = RuleName2, fill = RuleName2), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(aes(x=Year, y=Catch, color = RuleName2), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
        geom_line(data = catch_df %>% filter(MSY_type=="Empirical") %>% filter(Iteration==1), aes(x=Year, y=Catch)) +
        ylab("TACC") +
        guides(color = FALSE, fill = FALSE) +
        scale_color_viridis_d() +
        scale_fill_viridis_d() +
        # scale_color_brewer(palette = "RdYlBu") +
        # scale_fill_brewer(palette = "RdYlBu") +
        expand_limits(y = 0) +
        theme_lsd(base_size = 14) +
        facet_grid(RuleType~MSY_desc)
    # if(length(unique(catch_df$Scenario))==1) p1 <- p1 + facet_grid(MSYType~RuleType)
    # if(length(unique(catch_df$Scenario))>1) p1 <- p1 + facet_wrap(MSYType~Scenario+RuleType)
    ggsave(file.path(figure_dir, "TACC_over_time_MSYcompare.png"), p1, width = 8, height=6)

    p1_v2 <- ggplot(catch_df) +
        stat_summary(aes(x=Year, y=Catch, color = RuleName2, fill = RuleName2), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(aes(x=Year, y=Catch, color = RuleName2), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
        geom_line(data = catch_df %>% filter(Iteration==1), aes(x=Year, y=Catch)) +
        ylab("TACC") +
        guides(color = FALSE, fill = FALSE) +
        scale_color_viridis_d() +
        scale_fill_viridis_d() +
        # scale_color_brewer(palette = "RdYlBu") +
        # scale_fill_brewer(palette = "RdYlBu") +
        expand_limits(y = 0) +
        theme_lsd(base_size = 14) +
        facet_grid(MSY_type~MSY_desc2)
    ggsave(file.path(figure_dir, "TACC_over_time_MSYcompare_v2.png"), p1_v2, width=15, height=6)

    p2 <- ggplot(catch_df %>% filter(MSY_type=="Empirical")) +
        stat_summary(aes(x=Year, y=VB, color = RuleType, fill = RuleType), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(aes(x=Year, y=VB, color = RuleType), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
        geom_line(data = catch_df %>% filter(MSY_type=="Empirical") %>% filter(Iteration==1), aes(x=Year, y=VB)) +
        ylab("Vulnerable biomass") +
        guides(color = FALSE, fill = FALSE) +
        scale_color_viridis_d() +
        scale_fill_viridis_d() +
        # scale_color_brewer(palette = "RdYlBu") +
        # scale_fill_brewer(palette = "RdYlBu") +
        expand_limits(y = 0) +
        theme_lsd(base_size = 14) +
        facet_grid(MSY_desc ~ RuleType)
    # if(length(unique(catch_df$Scenario))==1) p2 <- p2 + facet_grid(MSYType~RuleType)
    # if(length(unique(catch_df$Scenario))>1) p2 <- p2 + facet_wrap(MSYType~Scenario+RuleType)
    ggsave(file.path(figure_dir, "VB_over_time_MSYcompare.png"), p2, width = 8, height=6)


    p2_v2 <- ggplot(catch_df) +
        stat_summary(aes(x=Year, y=VB, color = RuleName2, fill = RuleName2), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(aes(x=Year, y=VB, color = RuleName2), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75) +
        geom_line(data = catch_df %>% filter(Iteration==1), aes(x=Year, y=VB)) +
        ylab("Vulnerable biomass") +
        guides(color = FALSE, fill = FALSE) +
        scale_color_viridis_d() +
        scale_fill_viridis_d() +
        # scale_color_brewer(palette = "RdYlBu") +
        # scale_fill_brewer(palette = "RdYlBu") +
        expand_limits(y = 0) +
        theme_lsd(base_size = 14) +
        facet_grid(MSY_type~MSY_desc2)
    # if(length(unique(catch_df$Scenario))==1) p2 <- p2 + facet_grid(MSYType~RuleType)
    # if(length(unique(catch_df$Scenario))>1) p2 <- p2 + facet_wrap(MSYType~Scenario+RuleType)
    ggsave(file.path(figure_dir, "VB_over_time_MSYcompare_v2.png"), p2_v2, width = 15, height=6)


    p3 <- ggplot(catch_df %>% filter(MSY_type=="Empirical")) +
        geom_point(aes(x = MedTotalCatch/max(MedTotalCatch), y = CV, fill = MSY_desc2), pch=21, cex=4) +
        scale_fill_viridis_d() +
        guides(fill = guide_legend(title="Rule type")) +
        xlab("Proportion of maximum total yield") +
        # expand_limits(x=0) +
        theme_lsd(base_size = 14)
    ggsave(file.path(figure_dir, "Total_yield_vs_CV.png"), p3, width=8, height=6)

    p4 <- ggplot(catch_df) +
        geom_line(aes(x = CPUE, y = Catch, color = RuleName2), lwd=1.5, alpha=0.8) +
        facet_grid(MSY_type~.) +
        ylab("TACC") + xlab("Offset-year CPUE") +
        scale_color_viridis_d() +
        guides(color = guide_legend(title = "Rule type")) +
        scale_x_continuous(limits = c(0, max(catch_df$CPUE)*1.01), expand=c(0,0)) +
        scale_y_continuous(limits = c(0, max(catch_df$Catch)*1.05), expand=c(0,0)) +
        theme_lsd(base_size=14)
    ggsave(file.path(figure_dir, "CPUE_vs_TACC_MSYcompare.png"), p4, width=8, height=6)


    p5 <- ggplot(catch_rules %>% filter(Iteration == 1)) +
        geom_line(aes(x = CPUE, y = Catch, color = Group), lwd=1.5, alpha=0.8) + 
        ylab("TACC") + xlab("Offset-year CPUE") +
        scale_color_viridis_d() +
        guides(color = guide_legend(title = "Rule type")) +
        expand_limits(y = 0) +
        theme_lsd(base_size=14) +
        scale_x_continuous(limits = c(0, max(catch_rules$CPUE)*1.01), expand=c(0,0)) +
        scale_y_continuous(limits = c(0, max(catch_rules$Catch)*1.05), expand=c(0,0)) 

        # facet_wrap(.~Group)
    ggsave(file.path(figure_dir, "CPUE_vs_TAC_allrules.png"), p5, width=8, height=6)
}

# #' Explore CPUE-based rules
# #' 
# #' @param risk_summary data frame output by all_risk_constraints
# #' @param msy
# #' @param figure_dir the directory to save the figure to
# #' @import dplyr
# #' @import ggplot2
# #' @importFrom reshape2 melt
# #' @importFrom stats quantile
# #' @export
# #'
# explore_rules <- function(risk_summary, msy, soft_limit_req=0.1, catch_resid_req=0, figure_dir){
#     ## rules affected by same constraints as fixed catch and F rules, plus a subjective constraints on variability
#     ## plot maximum average catch compared with CV of catch over time
#     ## potential options

#     fixedF_cv <- data.frame(msy %>% filter(MSY_type=="Empirical") %>% filter(RuleType=="FixedF"))[,"CV"]
#     fixedF_sd <- data.frame(msy %>% filter(MSY_type=="Empirical") %>% filter(RuleType=="FixedF"))[,"SD"]

#     p <- ggplot(rule_summary) +
#         geom_point(aes(x = C50, y = CV, color=factor(par5))) +
#         facet_grid(par2~par3) +
#         guides(color = guide_legend(title = "Plateau height")) +
#         theme_lsd(base_size = 14) +
#         xlab("Median catch") + ylab("CV of catch over time")
#     ggsave(file.path(figure_dir, "Catch_vs_CV.png"), p)


#     plot_timeseries(msy_info=msy, object_list=object_list, object_names=object_names)

#     return(msy)
# }





    # p <- ggplot(compare_msy) + 
    #     geom_vline(aes(xintercept = 1)) + 
    #     geom_hline(aes(yintercept = 1)) +
    #     geom_point(aes(x = B50/(dBmsy/SSB0), y = MSY/dMSY, color = RuleType, shape = Scenario), cex=4) +
    #     xlab("Bmsy/dBmsy") +
    #     ylab("MSY/dMSY") +
    #     scale_color_brewer(palette = "Dark2") +
    #     expand_limits(x = 0, y = 0) +
    #     scale_x_continuous(expand = c(0,0), limits = c(0, max(c(1.05, compare_msy$B50/(compare_msy$dBmsy/compare_msy$SSB0))*1.05))) +
    #     scale_y_continuous(expand = c(0,0), limits = c(0, max(c(1.05, compare_msy$MSY/compare_msy$dMSY)*1.05))) + 
    #     theme_lsd(base_size = 14) 
    # ggsave(file.path(figure_dir, "MSY_dMSY.png"), p, width=10)   

    # find_msy <- summary_wRisk %>%   
    #             dplyr::group_by(Scenario, RuleType) %>%
    #             dplyr::summarise(MSY = max(C50[which(P20 < 0.10)]))

    # msy_info <- inner_join(summary_wRisk, find_msy) %>%
    #             dplyr::filter(C50 == MSY)
    # write.table(msy_info, file=paste0(figure_dir, "MSY.txt"), sep="\t", row.names=FALSE, col.names=TRUE)


    # msy_list <- lapply(1:length(object_list), function(x){
    #     n_iter <- nrow(mcmc_list[[x]][[1]])
    #     msy <- mcmc_list[[x]]$MSY_r
    #     dimnames(msy) <- list("Iteration" = 1:n_iter, "Region" = regions_list[[x]])
    #     msy2 <- reshape2::melt(msy) %>% dplyr::rename("dMSY"=value)

    #     bmsy <- mcmc_list[[x]]$SSBmsy_r
    #     dimnames(bmsy) <- list("Iteration" = 1:n_iter, "Region" = regions_list[[x]])
    #     bmsy2 <- reshape2::melt(bmsy) %>% dplyr::rename("dBmsy"=value)

    #     ssb0 <- mcmc_list[[x]]$SSB0_r
    #     dimnames(ssb0) <- list("Iteration" = 1:n_iter, "Region" = regions_list[[x]])
    #     ssb0 <- reshape2::melt(ssb0) %>% dplyr::rename("SSB0" = value)  

    #     det <- data.frame("dMSY"=unique(msy2$dMSY), "dBmsy"=unique(bmsy2$dBmsy), "SSB0" = unique(ssb0$SSB0))

    #     stock <- strsplit(object_names[x],"_")[[1]][1]
    #     strat <- strsplit(object_names[x],"_")[[1]][2]
    #     det$Scenario <- stock
    #     det$RuleType <- strat

    #     return(det)
    # })
    # dmsy_info <- do.call(rbind, msy_list)

    # compare_msy <- inner_join(dmsy_info, msy_info)

    # p <- ggplot(compare_msy) + 
    #     geom_vline(aes(xintercept = 1)) + 
    #     geom_hline(aes(yintercept = 1)) +
    #     geom_point(aes(x = B50/(dBmsy/SSB0), y = MSY/dMSY, color = RuleType, shape = Scenario), cex=4) +
    #     xlab("Bmsy/dBmsy") +
    #     ylab("MSY/dMSY") +
    #     scale_color_brewer(palette = "Dark2") +
    #     expand_limits(x = 0, y = 0) +
    #     scale_x_continuous(expand = c(0,0), limits = c(0, max(c(1.05, compare_msy$B50/(compare_msy$dBmsy/compare_msy$SSB0))*1.05))) +
    #     scale_y_continuous(expand = c(0,0), limits = c(0, max(c(1.05, compare_msy$MSY/compare_msy$dMSY)*1.05))) + 
    #     theme_lsd(base_size = 14) 
    # ggsave(file.path(figure_dir, "MSY_dMSY.png"), p, width=10)

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
    #         dplyr::group_by(Scenario, RuleType, "SSBadj", "SSB0") %>%
    #         dplyr::summarise(Pext_bias = length(which(SSBadj <= 0.01*SSB0)==TRUE)/length(SSBadj),
    #                         P10_bias = length(which(SSBadj <= 0.1*SSB0)==TRUE)/length(SSBadj),
    #                         P20_bias = length(which(SSBadj <= 0.2*SSB0)==TRUE)/length(SSBadj))

    # ssb_comp <- ssb_bias %>%
    #         dplyr::select(Iteration, Year, Scenario, RuleType, RelSSB, RelSSBadj) %>%
    #         tidyr::gather(SSBtype, value, -c(Iteration, Year, Scenario, RuleType) )

    # ssb_comp_msy <- semi_join(ssb_comp, msy_info) %>%
    #         dplyr::mutate(RuleType= ifelse(grepl("F=",RuleType), "FixedF", ifelse(grepl("C=",RuleType), "FixedCatch", NA)))

    # p <- ggplot(data = ssb_comp_msy, aes(x = Year, y = value)) +
    #     stat_summary(data = ssb_comp_msy, fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = SSBtype)) +
    #     stat_summary(data = ssb_comp_msy, fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = SSBtype)) +
    #     expand_limits(y = 0) +
    #     facet_grid(Scenario~RuleType) + 
    #     labs(x = "Projected fishing year", y = "Spawning stock biomass (tonnes)") +
    #     # scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1)) +
    #     scale_y_continuous(limits = c(0, max(ssb_comp_msy$value)*1.02)) +
    #     scale_color_brewer(palette="Dark2") +
    #     scale_fill_brewer(palette="Dark2") +
    #     theme_lsd(base_size = 14)
# }


# #' Compare vulnerable biomass from multiple models
# #' 
# #' @param object_list list of 'lsd.rds' files from multiple models
# #' @param object_names vector of model names associated with each of the output files in object_list
# #' @param figure_dir the directory to save the figure to
# #' @param save_plot to save the plot to file or not
# #' @import dplyr
# #' @import ggplot2
# #' @importFrom reshape2 melt
# #' @importFrom stats quantile
# #' @export
# #' 
# plot_compare_multi_ssb <- function(object_list, object_names, figure_dir = "compare_figure/", save_plot = TRUE)
# {
#     data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
#     mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)
#     data <- data_list[[1]]

#     years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
#     pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_proj_yr)
#     regions_list <- lapply(1:length(object_list), function(x) 1:data_list[[x]]$n_area)
#     cutyears_list <- lapply(1:length(object_list), function(x) (max(pyears_list[[x]])-99):max(pyears_list[[x]]))
#     cutyears <- unique(unlist(cutyears_list))

#     ssb_list <- lapply(1:length(object_list), function(x) {
#         n_iter <- nrow(mcmc_list[[x]][[1]])
#         bio <- mcmc_list[[x]]$biomass_ssb_jyr
#         dimnames(bio) <- list("Iteration" = 1:n_iter, "Rule" = 1, "Year" = pyears_list[[x]], "Region" = regions_list[[x]])
#         bio2 <- reshape2::melt(bio) #%>% dplyr::filter(Year <= max(years_list[[x]]))
#         stock <- strsplit(object_names[x],"_")[[1]][1]
#         strat <- strsplit(object_names[x],"_")[[1]][2]
#         bio2$Scenario <- stock
#         bio2$RuleType <- strat
#         return(bio2)
#     })
#     ssb <- data.frame(do.call(rbind, ssb_list))
#     ssb$Scenario <- factor(ssb$Scenario)
#     ssb$RuleType <- factor(ssb$RuleType)
    
#     ssb0_list <- lapply(1:length(object_list), function(x) {
#         n_iter <- nrow(mcmc_list[[x]][[1]])
#         bio <- mcmc_list[[x]]$SSB0_r
#         dimnames(bio) <- list("Iteration" = 1:n_iter, "Region" = regions_list[[x]])
#         stock <- strsplit(object_names[x],"_")[[1]][1]
#         strat <- strsplit(object_names[x],"_")[[1]][2]

#         hl <- reshape2::melt(bio) %>%
#             dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
#             dplyr::group_by(Iteration, Region, value, Year) %>%
#             dplyr::ungroup() %>%
#             dplyr::mutate(Rule = 1, type = "Hard limit", value = value * 0.1) %>%
#             dplyr::mutate(Scenario = stock) %>% 
#             dplyr::mutate(RuleType = strat)
#         sl <- reshape2::melt(bio) %>%
#             dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
#             dplyr::group_by(Iteration, Region, value, Year) %>%
#             dplyr::ungroup() %>%
#             dplyr::mutate(Rule = 1, type = "Soft limit", value = value * 0.2) %>%
#             dplyr::mutate(Scenario = stock) %>% 
#             dplyr::mutate(RuleType = strat)
#         ssb0 <- reshape2::melt(bio) %>%
#             dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
#             dplyr::group_by(Iteration, Region, value, Year) %>%
#             dplyr::ungroup() %>%
#             dplyr::mutate(Rule = 1, type = "SSB0") %>%
#             dplyr::mutate(Scenario = stock) %>% 
#             dplyr::mutate(RuleType = strat)

#         # bio <- mcmc_list[[x]]$SSBref_jr
#         # dimnames(bio) <- list("Iteration" = 1:n_iter, "Rule" = 1, "Region" = regions_list[[x]])
#         # ref <- reshape2::melt(bio) %>%
#         #     dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
#         #     dplyr::group_by(Iteration, Region, Rule, value, Year) %>%
#         #     dplyr::ungroup() #%>%
#             # dplyr::mutate(type = "Target")
#         bio2 <- rbind(ssb0, sl, hl) #%>% ##, ref)
#            # dplyr::filter(Year <= max(years_list[[x]]))
#         bio2$Scenario <- stock
#         bio2$RuleType <- strat
#         return(bio2)
#     })
#     ssb0 <- data.frame(do.call(rbind, ssb0_list))

#     labs <- dplyr::filter(ssb0, Year == max(Year)) %>%
#         dplyr::group_by(Year, type, Scenario, RuleType) %>%
#         dplyr::summarise(value = mean(value))

#     ssb_cut <- ssb %>% dplyr::filter(Year %in% cutyears)
#     ssb0_cut <- ssb0 %>% dplyr::filter(Year %in% cutyears)

#     p <- ggplot(data = ssb_cut, aes(x = Year, y = value)) +
#         stat_summary(data = dplyr::filter(ssb0_cut, type == "Soft limit"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, fill="gray") +
#         stat_summary(data = dplyr::filter(ssb0_cut, type == "Soft limit"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, colour="gray") +
#         stat_summary(data = dplyr::filter(ssb0_cut, type == "Hard limit"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, fill="gray") +
#         stat_summary(data = dplyr::filter(ssb0_cut, type == "Hard limit"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, colour="gray") +
#         stat_summary(data = dplyr::filter(ssb0_cut, type == "SSB0"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, fill="gray") +
#         stat_summary(data = dplyr::filter(ssb0_cut, type == "SSB0"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, colour="gray") +
#         # stat_summary(data = dplyr::filter(ssb0_cut, type == "Target"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = RuleType)) +
#         # stat_summary(data = dplyr::filter(ssb0_cut, type == "Target"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = RuleType)) +
#         stat_summary(data = ssb_cut, fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = RuleType)) +
#         stat_summary(data = ssb_cut, fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = RuleType)) +
#         geom_text(data = labs, aes(x = Year, y = value, label = type), nudge_x=-10) +
#         expand_limits(y = 0) +
#         facet_wrap(.~Scenario) + 
#         labs(x = "Projected fishing year", y = "Spawning stock biomass (tonnes)") +
#         # scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1)) +
#         scale_y_continuous(limits = c(0, max(ssb0_cut$value)*1.02)) +
#         theme_lsd(base_size = 14)
#     if (save_plot) {
#       ggsave(paste0(figure_dir, "biomass_ssb_compare.png"), p, width = 15)
#     } else {
#       return(p)
#     }
# }


# #' Compare vulnerable biomass from multiple models
# #' 
# #' @param object_list list of 'lsd.rds' files from multiple models
# #' @param object_names vector of model names associated with each of the output files in object_list
# #' @param figure_dir the directory to save the figure to
# #' @param save_plot to save the plot to file or not
# #' @import dplyr
# #' @import ggplot2
# #' @importFrom reshape2 melt
# #' @importFrom stats quantile
# #' @export
# #' 
# plot_compare_multi_relssb <- function(object_list, object_names, figure_dir = "compare_figure/", save_plot = TRUE)
# {
#     data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
#     mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)
#     data <- data_list[[1]]

#     years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
#     pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_proj_yr)
#     regions_list <- lapply(1:length(object_list), function(x) 1:data_list[[x]]$n_area)
#     cutyears_list <- lapply(1:length(object_list), function(x) (max(pyears_list[[x]])-99):max(pyears_list[[x]]))
#     cutyears <- unique(unlist(cutyears_list))

#     ssb_list <- lapply(1:length(object_list), function(x) {
#         n_iter <- nrow(mcmc_list[[x]][[1]])
#         bio <- mcmc_list[[x]]$biomass_ssb_jyr
#         dimnames(bio) <- list("Iteration" = 1:n_iter, "Rule" = 1, "Year" = pyears_list[[x]], "Region" = regions_list[[x]])
#         bio2 <- reshape2::melt(bio) #%>% dplyr::filter(Year <= max(years_list[[x]]))
#         stock <- strsplit(object_names[x],"_")[[1]][1]
#         strat <- strsplit(object_names[x],"_")[[1]][2]
#         bio2$Scenario <- stock
#         bio2$RuleType <- strat
#         return(bio2)
#     })
#     ssb <- data.frame(do.call(rbind, ssb_list))
#     ssb$Scenario <- factor(ssb$Scenario)
#     ssb$RuleType <- factor(ssb$RuleType)
    
#     ssb0_list <- lapply(1:length(object_list), function(x) {
#         n_iter <- nrow(mcmc_list[[x]][[1]])
#         bio <- mcmc_list[[x]]$SSB0_r
#         dimnames(bio) <- list("Iteration" = 1:n_iter, "Region" = regions_list[[x]])
#         stock <- strsplit(object_names[x],"_")[[1]][1]
#         strat <- strsplit(object_names[x],"_")[[1]][2]

#         hl <- reshape2::melt(bio) %>%
#             dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
#             dplyr::group_by(Iteration, Region, value, Year) %>%
#             dplyr::ungroup() %>%
#             dplyr::mutate(Rule = 1, type = "Hard limit", value = value * 0.1) %>%
#             dplyr::mutate(Scenario = stock) %>% 
#             dplyr::mutate(RuleType = strat)
#         sl <- reshape2::melt(bio) %>%
#             dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
#             dplyr::group_by(Iteration, Region, value, Year) %>%
#             dplyr::ungroup() %>%
#             dplyr::mutate(Rule = 1, type = "Soft limit", value = value * 0.2) %>%
#             dplyr::mutate(Scenario = stock) %>% 
#             dplyr::mutate(RuleType = strat)
#         ssb0 <- reshape2::melt(bio) %>%
#             dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
#             dplyr::group_by(Iteration, Region, value, Year) %>%
#             dplyr::ungroup() %>%
#             dplyr::mutate(Rule = 1, type = "SSB0") %>%
#             dplyr::mutate(Scenario = stock) %>% 
#             dplyr::mutate(RuleType = strat)

#         # bio <- mcmc_list[[x]]$SSBref_jr
#         # dimnames(bio) <- list("Iteration" = 1:n_iter, "Rule" = 1, "Region" = regions_list[[x]])
#         # ref <- reshape2::melt(bio) %>%
#         #     dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
#         #     dplyr::group_by(Iteration, Region, Rule, value, Year) %>%
#         #     dplyr::ungroup() #%>%
#             # dplyr::mutate(type = "Target")
#         bio2 <- rbind(ssb0, sl, hl) #%>% ##, ref)
#            # dplyr::filter(Year <= max(years_list[[x]]))
#         bio2$Scenario <- stock
#         bio2$RuleType <- strat
#         return(bio2)
#     })
#     ssb0 <- data.frame(do.call(rbind, ssb0_list))

#     ssb_cut <- ssb %>% dplyr::filter(Year %in% cutyears) %>% dplyr::mutate(type="SSB")
#     ssb0_cut <- ssb0 %>% dplyr::filter(Year %in% cutyears)

#     df <- rbind.data.frame(ssb_cut, ssb0_cut)
#     df2 <- df %>% spread(type, value)
#     colnames(df2)[which(colnames(df2)=="Hard limit")] <- "Hard_limit"
#     colnames(df2)[which(colnames(df2)=="Soft limit")] <- "Soft_limit"

#     df2 <- df2 %>% 
#             mutate(Hard_limit_rel = Hard_limit/SSB0) %>%
#             mutate(Soft_limit_rel = Soft_limit/SSB0) %>%
#             mutate(SSB0_rel = SSB0/SSB0) %>%
#             mutate(SSB_rel = SSB/SSB0) %>%
#             mutate(Rule_type = ifelse(grepl("FixedCatch", as.character(RuleType)), "FixedCatch", 
#                                 ifelse(grepl("FixedF", as.character(RuleType)), "FixedF", NA))) %>%
#             mutate(Catch_prop = ifelse(grepl("Catch", as.character(RuleType)), as.numeric(strsplit(as.character(RuleType),"Catch")[[1]][2])/SSB0, NA))

#     df3 <- df2 %>% gather(type, value, Hard_limit:SSB_rel) %>%
#                     filter(grepl("rel", type))

#     labs <- df3 %>% filter(type %in% c("Hard_limit_rel","Soft_limit_rel","SSB0_rel")) %>% 
#         dplyr::group_by(type) %>%
#         dplyr::summarise(value = mean(value)) 
#     labs[which(labs$type == "Hard_limit_rel"),'type'] <- "Hard limit"
#     labs[which(labs$type == "Soft_limit_rel"),'type'] <- "Soft limit"
#     labs[which(labs$type == "SSB0_rel"),'type'] <- "SSB0"



#     p <- ggplot(data = df3, aes(x = Year, y=value)) +
#         stat_summary(data = df3 %>% filter(type=="Soft_limit_rel"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, fill="gray") +
#         stat_summary(data = df3 %>% filter(type=="Soft_limit_rel"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, colour="gray") +
#         stat_summary(data = df3 %>% filter(type=="Hard_limit_rel"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, fill="gray") +
#         stat_summary(data = df3 %>% filter(type=="Hard_limit_rel"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, colour="gray") +
#         stat_summary(data = df3 %>% filter(type=="SSB0_rel"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, fill="gray") +
#         stat_summary(data = df3 %>% filter(type=="SSB0_rel"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, colour="gray") +
#         # stat_summary(data = dplyr::filter(ssb0_cut, type == "Target"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = RuleType)) +
#         # stat_summary(data = dplyr::filter(ssb0_cut, type == "Target"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = RuleType)) +
#         stat_summary(data = df3 %>% filter(type=="SSB_rel"), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, aes(fill = Scenario)) +
#         stat_summary(data = df3 %>% filter(type=="SSB_rel"), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha = 0.75, aes(color = Scenario)) +
#         geom_text(data = labs, aes(x = mean(df3$Year), y = value, label = type), nudge_x=-3) +
#         expand_limits(y = 0) +
#         facet_wrap(RuleType~.) + 
#         labs(x = "Projected fishing year", y = "Relative spawning stock biomass (tonnes)") +
#         # scale_x_continuous(breaks = seq(0, 1e6, 5), minor_breaks = seq(0, 1e6, 1)) +
#         theme_lsd(base_size = 14)
#     if (save_plot) {
#       ggsave(paste0(figure_dir, "biomass_relssb_compare.png"), p, width = 15)
#     } else {
#       return(p)
#     }
# }


# #' Add bias to recalculate risk probabilities
# #' 
# #' @param object_list list of 'lsd.rds' files from multiple models
# #' @param object_names vector of model names associated with each of the output files in object_list
# #' @param figure_dir the directory to save the figure to
# #' @import dplyr
# #' @import ggplot2
# #' @importFrom reshape2 melt
# #' @importFrom stats quantile
# #' @export
# #' 
# compare_multi_risk_wBias <- function(object_list, object_names, 
#                                      figure_dir = "compare_figure/")
# {
#     data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
#     mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)
#     data <- data_list[[1]]

#     years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
#     pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_proj_yr)
#     regions_list <- lapply(1:length(object_list), function(x) 1:data_list[[x]]$n_area)
#     cutyears_list <- lapply(1:length(object_list), function(x) (max(pyears_list[[x]])-99):max(pyears_list[[x]]))
#     cutyears <- unique(unlist(cutyears_list))
#     n_iter <- nrow(mcmc_list[[1]][[1]])

#     ssb_list <- lapply(1:length(object_list), function(x) {
#         n_iter <- nrow(mcmc_list[[x]][[1]])
#         ssb <- mcmc_list[[x]]$biomass_ssb_jyr
#         dimnames(ssb) <- list("Iteration" = 1:n_iter, "Rule" = 1, "Year" = pyears_list[[x]], "Region" = regions_list[[x]])
#         ssb2 <- reshape2::melt(ssb) %>% dplyr::rename("SSB"=value) %>% select(-Rule)
#         stock <- strsplit(object_names[x],"_")[[1]][1]
#         strat <- strsplit(object_names[x],"_")[[1]][2]
#         ssb2$Scenario <- stock
#         ssb2$RuleType <- strat

#         ssb0 <- mcmc_list[[x]]$SSB0_r
#         dimnames(ssb0) <- list("Iteration" = 1:n_iter, "Region" = regions_list[[x]])

#         ssb0 <- reshape2::melt(ssb0) %>%
#             dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears_list[[x]]), by = "Iteration") %>%
#             dplyr::group_by(Iteration, Region, value, Year) %>%
#             dplyr::ungroup() %>%
#             dplyr::rename("SSB0" = value) %>%
#             dplyr::mutate(Scenario = stock) %>% 
#             dplyr::mutate(RuleType = strat)

#         out <- full_join(ssb0, ssb2)

#         return(out)
#     })
#     ssb_df <- do.call(rbind, ssb_list)

#     ssb_proj <- ssb_df %>% dplyr::filter(Year %in% cutyears) %>% select(-c(Region))

#     slope <- runif(n_iter, 0, 0.2)
#     mult <- lapply(1:n_iter, function(x){
#         seq1 <- seq(from=0,to=slope[x],length.out=length(cutyears))
#         seq2 <- seq1 - mean(seq1) +1
#         df <- data.frame("Iteration" = x, "Year" = cutyears, "Multiplier" = seq2)
#         return(df)
#     })
#     mult_df <- do.call(rbind, mult)

#     p <- ggplot(mult_df) + 
#         geom_line(aes(x = Year, y = Multiplier, color = factor(Iteration))) + 
#         guides(color = FALSE) + 
#         scale_color_manual(values = colorRampPalette(c("gray", "black"))(n_iter))

#     ssb_bias <- full_join(ssb_proj, mult_df) %>%
#             dplyr::mutate(SSBadj = SSB * Multiplier)


#     ssb_calc <- ssb_bias %>%
#             dplyr::group_by(Scenario, RuleType, "SSB", "SSB0") %>%
#             dplyr::summarise(Pext = length(which(SSBadj <= 0.01*SSB0)==TRUE)/length(SSBadj),
#                             P10 = length(which(SSBadj <= 0.1*SSB0)==TRUE)/length(SSBadj),
#                             P20 = length(which(SSBadj <= 0.2*SSB0)==TRUE)/length(SSBadj))

#     ssb_out <- data.frame(ssb_calc) %>% select(Scenario, RuleType, Pext, P10, P20)
#     write.table(ssb_out, file=paste0(figure_dir, "Prisk_table.txt"), sep="\t", row.names=FALSE, col.names=TRUE)
#     return(ssb_out)
# }


# #' Compare vulnerable biomass from multiple models
# #' 
# #' @param object_list list of 'lsd.rds' files from multiple models
# #' @param object_names vector of model names associated with each of the output files in object_list
# #' @param figure_dir the directory to save the figure to
# #' @param save_plot to save the plot to file or not
# #' @import dplyr
# #' @import ggplot2
# #' @importFrom reshape2 melt
# #' @importFrom stats quantile
# #' @export
# #' 
# plot_compare_multi_vb <- function(object_list, object_names, 
#                                   figure_dir = "compare_figure/", 
#                                   save_plot = TRUE)
# {
#     data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
#     mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)

#     years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
#     pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_proj_yr)
#     regions_list <- lapply(1:length(object_list), function(x) 1:data_list[[x]]$n_area)
#     sex <- c("Male","Immature female","Mature female")
#     seasons <- c("AW", "SS")
#     YR <- "YR" # label for the season before the season change year
#     cutyears_list <- lapply(1:length(object_list), function(x) (max(pyears_list[[x]])-99):max(pyears_list[[x]]))
#     cutyears <- unique(unlist(cutyears_list))


#     vb_list <- lapply(1:length(object_list), function(x) {
#         n_iter <- nrow(mcmc_list[[x]][[1]])
#         biomass_vuln_ytrs2 <- mcmc_list[[x]]$biomass_vuln_ytrs
#         dimnames(biomass_vuln_ytrs2) <- list("Iteration" = 1:n_iter, "Year" = pyears_list[[x]], "Season" = seasons, "Region" = regions_list[[x]], Sex = sex)
#         biomass_vuln_ytr2 <- reshape2::melt(biomass_vuln_ytrs2) %>%
#             dplyr::filter(value>0) %>%
#             dplyr::mutate(Season = as.character(Season), Season = ifelse(Year > 1978, Season, YR)) %>%
#             dplyr::filter(Season %in% c("YR", "AW")) %>%
#             dplyr::group_by(Iteration, Year, Season) %>%
#             dplyr::summarise(value = sum(value))
#         stock <- strsplit(object_names[x],"_")[[1]][1]
#         strat <- strsplit(object_names[x],"_")[[1]][2]
#         biomass_vuln_ytr2$Scenario <- stock
#         biomass_vuln_ytr2$RuleType <- strat
#         return(biomass_vuln_ytr2)
#     })
#     vb <- data.frame(do.call(rbind, vb_list))
#     vb$Scenario <- factor(vb$Scenario)
#     vb$RuleType <- factor(vb$RuleType)

#     # n1 <- sapply(1:length(object_names), function(x) strsplit(object_names[x], "_")[[1]][1])
#     # # n2 <- sapply(1:length(object_names), function(x) strsplit(object_names[x], paste0(n1[x],"_"))[[1]][2])
#     # # n2[which(is.na(n2))] <- "base"

#     # n2u <- unique(n1)
#     # nm <- length(n2u)
#     # if (nm > 2) cols <- brewer.pal(nm, "Dark2")
#     # if (nm == 2) cols <- c("tomato", "steelblue")
#     # if (nm == 1) cols <- "tomato"

#     # cols_all <- unlist(as.vector(sapply(1:nm, function(x) rep(cols[x],length(which(n1==n2u[x]))))))
#     # names(cols_all) <- object_names
#     # lty_all <- rep(1, length(object_names))
#     # if (length(unique(n1)) > 1) lty_all[which(n1=="qconstant")] <- 3

#     vb_cut <- vb %>% dplyr::filter(Year %in% cutyears)

#     # Vulnerable biomass
#     p <- ggplot(data = vb_cut, aes(x = Year, y = value, color = RuleType, fill = RuleType)) +
#         stat_summary(data=vb_cut, fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
#         #stat_summary(data=vb_cut, fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
#         stat_summary(data=vb_cut, fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha=0.75) +
#         # scale_fill_manual(values = cols_all, labels = object_names) +
#         # scale_colour_manual(values = cols_all, labels = object_names) +
#         # guides(colour = guide_legend(override.aes = list(colour = cols_all, linetype = lty_all))) + 
#         facet_grid(.~Scenario) +
#         # scale_linetype(guide=FALSE) +
#         expand_limits(y = 0) +
#         xlab("Projected fishing year") + ylab("Vulnerable biomass (tonnes)") +
#         # scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
#         theme_lsd()
    
#     ## remove    
#     # if (data_list[[1]]$n_area > 1) {
#     #     p <- p + facet_wrap(~Region)
#     # }
#     if (save_plot) {
#       ggsave(paste0(figure_dir, "biomass_vuln_compare.png"), p, width = 15)
#     } else {
#       return(p)
#     }
# }


# #' Compare recruitment from multiple models
# #' 
# #' @param object_list list of 'lsd.rds' files from multiple models
# #' @param object_names vector of model names associated with each of the output files in object_list
# #' @param figure_dir the directory to save the figure to
# #' @param save_plot to save the plot to file or not
# #' @import dplyr
# #' @import ggplot2
# #' @importFrom reshape2 melt
# #' @importFrom stats quantile
# #' @export
# #' 
# plot_compare_multi_recruitment <- function(object_list, object_names, 
#                                            figure_dir = "compare_figure/", save_plot = TRUE)
# {
#     data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
#     mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)
    
#     ny_list <- lapply(1:length(object_list), function(x) dim(mcmc_list[[x]]$recruits_ry)[3])
#     years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
#     pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:(data_list[[x]]$first_yr + ny_list[[x]] - 1))
#     regions_list <- lapply(1:length(object_list), function(x) 1:data_list[[x]]$n_area)
#     cutyears_list <- lapply(1:length(object_list), function(x) (max(pyears_list[[x]])-99):max(pyears_list[[x]]))
#     cutyears <- unique(unlist(cutyears_list))

#     rec_list <- lapply(1:length(object_list), function(x){
#         n_iter <- nrow(mcmc_list[[x]][[1]])
#         recruits2 <- mcmc_list[[x]]$recruits_ry
#         dimnames(recruits2) <- list("Iteration" = 1:n_iter, "Region" = regions_list[[x]], "Year" = pyears_list[[x]])
#         recruits2 <- reshape2::melt(recruits2) %>%
#            # dplyr::filter(Year <= max(years_list[[x]])) %>%
#             dplyr::group_by(Iteration, Year) %>%
#             dplyr::summarise(value = sum(value))

#         stock <- strsplit(object_names[x],"_")[[1]][1]
#         strat <- strsplit(object_names[x],"_")[[1]][2]
#         recruits2$Scenario <- stock
#         recruits2$RuleType <- strat
#         recruits2$qconstant <- as.character(ifelse(grepl("qconstant",object_names[[x]]),1,0))
#         recruits2
#     })
#     recruits <- data.frame(do.call(rbind, rec_list)) %>%
#         group_by(Iteration, Year, qconstant, Scenario, RuleType) %>%
#         summarise(value = sum(value))
#     recruits$Scenario <- factor(recruits$Scenario)
#     recruits$RuleType <- factor(recruits$RuleType)
#     recruits$qconstant <- factor(recruits$qconstant)
#     recruits$value <- recruits$value/1e6

#     # plot recruitment
#     xmin <- min(recruits$Year)
#     xmax <- max(recruits$Year)	
    
#     # n1 <- sapply(1:length(object_names), function(x) strsplit(object_names[x], "_")[[1]][1])
#     # # n2 <- sapply(1:length(object_names), function(x) strsplit(object_names[x], paste0(n1[x],"_"))[[1]][2])
#     # # n2[which(is.na(n2))] <- "base"
    
#     # n2u <- unique(n1)
#     # nm <- length(n2u)
#     # if(nm > 2) cols <- brewer.pal(nm, "Dark2")
#     # if(nm == 2) cols <- c("tomato", "steelblue")
#     # if(nm == 1) cols <- "tomato"
    
#     # cols_all <- unlist(as.vector(sapply(1:nm, function(x) rep(cols[x],length(which(n1==n2u[x]))))))
#     # names(cols_all) <- object_names
#     # lty_all <- rep(1, length(object_names))
#     # if (length(unique(n1)) > 1) lty_all[which(n1=="qconstant")] <- 3

#     rec_cut <- recruits %>% dplyr::filter(Year %in% cutyears)

#    p <- ggplot(data = rec_cut, aes(x = Year, y = value, color = RuleType, fill = RuleType)) +
#         stat_summary(data=rec_cut, fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
#         #stat_summary(data=rec_cut, fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
#         stat_summary(data=rec_cut, fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha=0.75) +
#         # scale_fill_manual(values = cols_all, labels = object_names) +
#         # scale_colour_manual(values = cols_all, labels = object_names) +
#         # guides(colour = guide_legend(override.aes = list(colour = cols_all, linetype = lty_all))) + 
#         # scale_linetype(guide=FALSE) +
#         facet_grid(.~Scenario) +
#         expand_limits(y = 0) +
#         xlab("Projected fishing year") + ylab("Recruitment (millions of individuals)") +
#         scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
#         theme_lsd()
   
#     # if (data_list[[1]]$n_area > 1) {
#     #     p <- p + facet_wrap(~Region)
#     # }
    
#    if (save_plot) {
#       ggsave(paste0(figure_dir, "recruitment_compare.png"), p, width = 10)
#    } else {
#      return(p)
#    }
# }


# ##' Compare catchability coefficient q from multiple models
# # #' 
# # #' @param object_list list of 'lsd.rds' files from multiple models
# # #' @param object_names vector of model names associated with each of the output files in object_list
# # #' @export
# # #' 
# # plot_compare_multi_q <- function(object_list, object_names, figure_dir = "compare_figure/", save_plot = TRUE)
# # {
# #     data_list <- lapply(1:length(object_list), function(x) object_list[[x]]@data)
# #     mcmc_list <- lapply(1:length(object_list), function(x) object_list[[x]]@mcmc)

# #     n_iter <- nrow(mcmc_list[[1]][[1]])

# #     years_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_yr)
# #     pyears_list <- lapply(1:length(object_list), function(x) data_list[[x]]$first_yr:data_list[[x]]$last_proj_yr)
# #     cutyears_list <- lapply(1:length(object_list), function(x) (max(pyears_list[[x]])-20):max(pyears_list[[x]]))
# #     cutyears <- unique(unlist(cutyears_list))

# #     nq_list <- lapply(1:length(object_list), function(x) data_list[[x]]$n_q)
# #     maxq <- max(unlist(sapply(1:length(object_list), function(x) nq_list[[x]])))
# #     q_info_list <- lapply(1:length(object_list), function(x) data.frame("qtype"=data_list[[x]]$data_cpue_q_i, "Season" = data_list[[x]]$data_cpue_season_i, "Year" = data_list[[x]]$data_cpue_year_i, "Region" = data_list[[x]]$data_cpue_area_i))
# #     q_info_list <- lapply(1:length(object_list), function(x){
# #         if (max(q_info_list[[x]]$qtype) < maxq & max(q_info_list[[x]]$Year) == max(unlist(sapply(1:length(object_list), function(x) q_info_list[[x]]$Year)))){
# #             subq <- q_info_list[[x]]$qtype
# #             max <- max(subq)
# #             for (i in 1:length(subq)) {
# #                 if (subq[i]==max) {
# #                     subq[i] <- maxq
# #                     next
# #                 }
# #                 if (subq[i]!=max) subq[i] <- subq[i] + (maxq - max) 
# #             }
# #             q_info_list[[x]]$qtype <- subq
# #         }
# #         stock <- strsplit(object_names[x],"_")[[1]][1]
# #         strat <- strsplit(object_names[x],"_")[[1]][2]

# #         q_info_list[[x]] %>%
# #             dplyr::filter(Season == 1) %>%
# #             dplyr::mutate(QY = paste(Year, qtype))
# #     })

# #     q_list <- lapply(1:length(object_list), function(x) {
# #         n_iter <- nrow(mcmc_list[[x]][[1]])
# #         q2 <- mcmc_list[[x]]$par_q_cpue_qy
# #         dimnames(q2) <- list("Iteration" = 1:n_iter, "qtype"=1:nq_list[[x]], "Year" = pyears_list[[x]])
# #         q2 <- reshape2::melt(q2)

# #         if(max(q2$qtype) < maxq) {
# #             subq <- q2$qtype
# #             max <- max(subq)
# #             for (i in 1:length(subq)) {
# #                 if (subq[i]==max){
# #                     subq[i] <- maxq
# #                     next
# #                 }
# #                 if(subq[i]!=max) subq[i] <- subq[i] + (maxq - max)             }
# #             q2$qtype <- subq
# #         }

# #         q2 <- q2 %>%
# #            # dplyr::filter(Year <= max(years_list[[x]])) %>%
# #             dplyr::filter(Year %in% unique(q_info_list[[x]]$Year)) %>%
# #             dplyr::mutate(QY = paste(Year, qtype)) %>%
# #             dplyr::filter(QY %in% q_info_list[[x]]$QY)
        
# #         q2$Model <- object_names[x]
# #         q2$qconstant <- as.character(ifelse(grepl("qconstant",object_names[[x]]),1,0))
# #         # if (data_list[[x]]$n_area > 1 & "Region" %in% colnames(q2) == FALSE) q2$Region <- "All regions"
# #         return(q2)
# #     })
    
# #     q <- data.frame(do.call(rbind, q_list)) %>%
# #         group_by(Iteration, Year, Model, qconstant, qtype, QY)
# #     q$Model <- factor(q$Model)
# #     q$qconstant <- factor(q$qconstant)


# #     # n1 <- sapply(1:length(object_names), function(x) strsplit(object_names[x], "_")[[1]][1])
# #     # # n2 <- sapply(1:length(object_names), function(x) strsplit(object_names[x], paste0(n1[x],"_"))[[1]][2])
# #     # # n2[which(is.na(n2))] <- "base"

# #     # n2u <- unique(n1)
# #     # nm <- length(n2u)
# #     # if(nm > 2) cols <- brewer.pal(nm, "Dark2")
# #     # if(nm == 2) cols <- c("tomato", "steelblue")
# #     # if(nm == 1) cols <- "tomato"

# #     # cols_all <- unlist(as.vector(sapply(1:nm, function(x) rep(cols[x],length(which(n1==n2u[x]))))))    
# #     # names(cols_all) <- object_names
# #     # lty_all <- rep(1, length(object_names))
# #     # if (length(unique(n1)) > 1) lty_all[which(n1 == "qconstant")] <- 3


# #     p <- ggplot(data = q, aes(x = Year, y = value, colour=Model, fill=Model)) +
# #         stat_summary(fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
# #         # stat_summary(fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha=0.45, colour = NA) +
# #         stat_summary(fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1, alpha=0.75) +
# #         # scale_fill_manual(values=cols_all, labels=object_names) +
# #         # scale_colour_manual(values=cols_all, labels=object_names) +
# #         # guides(colour = guide_legend(override.aes = list(colour = cols_all, linetype = lty_all))) + 
# #         # scale_linetype(guide=FALSE) +
# #         expand_limits(y = 0) +
# #         xlab("Fishing year") + ylab("Catchability coefficient (q)") +
# #         scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
# #         theme_lsd() +
# #         facet_wrap(~qtype, scales = "free")
    
# #     if (data_list[[1]]$n_area > 1) {
# #         p <- p + facet_wrap(~Region)
# #     }
    
# #     if (save_plot) {
# #       ggsave(paste0(figure_dir, "q_y_compare.png"), p, width = 10)
# #     } else {
# #       return(p)
# #     }

# # }
