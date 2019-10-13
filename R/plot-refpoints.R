plot_refpoints <- function(object, figure_dir){
    
    mcmc <- object@mcmc
    data <- object@data
    years <- data$first_yr:data$last_yr
    pyears <- data$first_yr:data$last_proj_yr
    cutyears <- max(pyears-99):max(pyears)
    seasons <- c("AW","SS")
    regions <- 1:data$n_area
    sex <- c("Male","Immature female","Mature female")
    n_iter <- nrow(mcmc[[1]])
    rules <- data$mp_rule_parameters
    n_rules <- nrow(rules)

    dcatch <- mcmc$proj_catch_commercial_jryt
        dimnames(dcatch) <- list("Iteration"=1:n_iter, "RuleNum"=1:dim(dcatch)[2], "Region"=regions, "Year"=pyears, "Season"=seasons)
        dcatch2 <- reshape2::melt(dcatch, value.name = "Input_catch") %>% 
            dplyr::group_by(Iteration, Year, RuleNum) %>%
            dplyr::summarise(sum(Input_catch)) %>%
            dplyr::rename("Input_catch"="sum(Input_catch)")

    pcatch <- mcmc$pred_catch_sl_jryt
        dimnames(pcatch) <- list("Iteration"=1:n_iter, "RuleNum"=1:dim(pcatch)[2], "Region"=regions, "Year"=pyears, "Season"=seasons)
        pcatch2 <- reshape2::melt(pcatch, value.name = "Catch") %>% 
           group_by(Iteration, Year, RuleNum) %>%
           summarise(Catch = sum(Catch))

    rcatch <- mcmc$resid_catch_sl_jryt
        dimnames(rcatch) <- list("Iteration"=1:n_iter, "RuleNum"=1:dim(rcatch)[2], "Region"=regions, "Year"=pyears, "Season"=seasons)
        rcatch2 <- reshape2::melt(rcatch, value.name = "CatchResidual") %>% 
           group_by(Iteration, Year, RuleNum) %>%
           summarise(CatchResidual = sum(CatchResidual)) 
        rcatch2$CatchResidual[which(abs(rcatch2$CatchResidual)<1)] <- 0

    cpue <- mcmc$mp_offset_cpue_jry
        dimnames(cpue) <- list("Iteration"=1:n_iter, "RuleNum"=1:dim(cpue)[2], "Region"=regions, "Year"=pyears)
        cpue2 <- reshape2::melt(cpue, value.name="CPUE") %>%
        	group_by(Iteration, Year, RuleNum) %>%
        	summarise(CPUE = sum(CPUE)) 

    catch <- full_join(dcatch2, pcatch2)
    catch <- full_join(catch, rcatch2)
    catch_cpue <- full_join(catch, cpue2)

    catch_cpue$RuleType <- sapply(1:nrow(rules), function(x) ifelse(rules[x,1]==1, "FixedCatch", "CPUErule"))
    catch_cpue$Rule <- sapply(1:nrow(rules), function(x) ifelse(catch_cpue$RuleType[x]=="FixedCatch", rules[x,2], NA))
    catch_cpue$Rule[which(catch_cpue$RuleType=="CPUErule")] <- paste0("Rule", 1:length(which(catch_cpue$RuleType=="CPUErule")))
    catch_cpue <- catch_cpue %>% select(-RuleNum)


    ssb <- mcmc$biomass_ssb_jyr
    dimnames(ssb) <- list("Iteration" = 1:n_iter, "RuleNum" = 1:dim(ssb)[2], "Year" = pyears, "Region" = regions)
    ssb2 <- reshape2::melt(ssb) %>% dplyr::rename("SSB"=value) 

    ssb0 <- mcmc$SSB0_r
    dimnames(ssb0) <- list("Iteration" = 1:n_iter, "Region" = regions)
    ssb0 <- reshape2::melt(ssb0) %>%
        dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = pyears), by = "Iteration") %>%
        dplyr::rename(SSB0=value) 

    ssb_out <- ssb2 %>% select(Iteration, Year, SSB, RuleNum)
    relssb <- full_join(ssb_out, ssb0) %>%
        dplyr::mutate(RelSSB = SSB/SSB0)

    relssb$RuleType <- sapply(1:nrow(rules), function(x) ifelse(rules[x,1]==1, "FixedCatch", "CPUErule"))
    relssb$Rule <- sapply(1:nrow(rules), function(x) ifelse(relssb$RuleType[x]=="FixedCatch", rules[x,2], NA))
    relssb$Rule[which(relssb$RuleType=="CPUErule")] <- paste0("Rule", 1:length(which(relssb$RuleType=="CPUErule")))
    relssb <- relssb %>% select(-RuleNum)

	info <- full_join(catch_cpue, relssb)

	p <- ggplot(info, aes(x = Year, y = Catch, color = Rule, fill = Rule)) +
    	stat_summary(fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25) +
        stat_summary(fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5) +
        stat_summary(fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
        theme_lsd()

	summary <- info %>%
		dplyr::group_by(RuleNum) %>%
		dplyr::summarise(C5 = quantile(Catch, prob = 0.05), 
						C50 = quantile(Catch, prob = 0.5),
						C95 = quantile(Catch, prob = 0.95),
						B5 = quantile(RelSSB, prob = 0.05),
						B50 = quantile(RelSSB, prob = 0.5),
						B95 = quantile(RelSSB, prob = 0.99),
						AvgTotalCatch = sum(Catch)/max(Iteration),
						CV = sd(Catch)/mean(Catch),
						CatchConstraint = ifelse(quantile(Catch, prob=0.95)-quantile(Catch,prob=0.05) > 2, 1, 0), 
						RiskConstraint = length(which(RelSSB <= 0.2))/length(RelSSB))
	if(all(rules[,1]==1)) summary$FixedCatch <- rules[,2]

	findu <- summary %>% filter(CatchConstraint==0) %>% filter(C50 == max(C50))
	findc <- summary %>% filter(CatchConstraint==0) %>% filter(RiskConstraint < 0.1) %>% filter(C50 == max(C50))

	p <- ggplot(summary_risk) +
        geom_segment(aes(x=B5, xend=B95, y=C50, yend=C50, color = factor(RiskConstraint)), alpha=0.75, lwd=1.3) +
        geom_segment(aes(x=B50, xend=B50, y=C5, yend=C95, color = factor(RiskConstraint)), alpha=0.75, lwd=1.3) +
        geom_point(aes(x=B50, y=C50, fill=factor(RiskConstraint)), pch=21, alpha=0.75, cex=4) +
        expand_limits(x = 0) +
        scale_x_continuous(limits = c(0, 1)) +
        facet_grid(Scenario~RuleType, scales="free_y", shrink=FALSE) +
        xlab("Relative spawning stock biomass") +
        ylab("Catch") +
        scale_colour_viridis_d() +
        scale_fill_viridis_d() +
        guides(fill = guide_legend(title = ">10% below soft limit"), color = guide_legend(title = ">10% below soft limit")) +
        theme_lsd(base_size=14)
    ggsave(file.path(figure_dir, "Catch_versus_RelSSB_RiskConstraint.png"), p, width=15, height=6) 


	
}