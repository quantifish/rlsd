#' Calculate reference points and create relevant figures
#'
#' @param object the ref-lsd object
#' @param figure_dir the directory to save figures to
#' @import dplyr
#' @import ggplot2
#' @export
#'
plot_refpoints <- function(object, figure_dir){
  
  mcmc <- object@mcmc
  data <- object@data
  years <- data$first_yr:data$last_yr
  pyears <- data$first_yr:data$last_proj_yr
  cutyears <- max(pyears-99):max(pyears)
  seasons <- c("AW","SS")
  regions <- 1:data$n_area
  if(length(regions) > 1) regions2 <- c(regions, "Total")
  if(length(regions) == 1) regions2 <- regions
  sex <- c("Male","Immature female","Mature female")
  n_iter <- nrow(mcmc[[1]])
  rules <- data$mp_rule_parameters
  n_rules <- nrow(rules)
  
  # commcatch <- mcmc$proj_catch_commercial_jryt
  #     dimnames(commcatch) <- list("Iteration"=1:n_iter, "RuleNum"=1:dim(commcatch)[2], "Region"=regions, "Year"=pyears, "Season"=seasons)
  #     commcatch2 <- reshape2::melt(commcatch, value.name = "Input_catch") %>% 
  #         dplyr::group_by(Iteration, Year, Region, RuleNum) %>%
  #         dplyr::summarise(sum(Input_catch)) %>%
  #         dplyr::rename("Input_catch"="sum(Input_catch)") %>%
  #         dplyr::mutate("CatchType" = "Commercial")
  
  # reccatch <- mcmc$proj_catch_recreational_jryt
  #     dimnames(reccatch) <- list("Iteration"=1:n_iter, "RuleNum"=1:dim(reccatch)[2], "Region"=regions, "Year"=pyears, "Season"=seasons)
  #     reccatch2 <- reshape2::melt(reccatch, value.name = "Input_catch") %>% 
  #         dplyr::group_by(Iteration, Year, Region, RuleNum) %>%
  #         dplyr::summarise(sum(Input_catch)) %>%
  #         dplyr::rename("Input_catch"="sum(Input_catch)") %>%
  #         dplyr::mutate("CatchType" = "Recreational")
  
  # dcatch <- rbind.data.frame(commcatch2, reccatch2)
  # dcatch2 <- dcatch %>% 
  #     dplyr::group_by(Iteration, Year, Region, RuleNum) %>%
  #     dplyr::summarise("Input_catch" = sum(Input_catch))
  
  slcatch <- mcmc$pred_catch_sl_jryt
  dimnames(slcatch) <- list("Iteration"=1:n_iter, "RuleNum"=1:dim(slcatch)[2], "Region"=regions, "Year"=pyears, "Season"=seasons)
  slcatch2 <- reshape2::melt(slcatch, value.name = "Catch") %>% 
    group_by(Iteration, Year, Region, RuleNum) %>%
    summarise(Catch = sum(Catch)) %>%
    dplyr::mutate("CatchType" = "SL")
  # slcatch$RuleType <- sapply(1:nrow(slcatch2), function(x) ifelse(rules[slcatch2$RuleNum[x],1]==1, "FixedCatch", "CPUErule"))
  # slcatch$Rule <- sapply(1:nrow(slcatch2), function(x) ifelse(slcatch2$RuleType[x]=="FixedCatch", rules[slcatch2$RuleNum[x],2], slcatch2$RuleNum[x]))
  
  nslcatch <- mcmc$pred_catch_nsl_jryt
  dimnames(nslcatch) <- list("Iteration"=1:n_iter, "RuleNum"=1:dim(nslcatch)[2], "Region"=regions, "Year"=pyears, "Season"=seasons)
  nslcatch2 <- reshape2::melt(nslcatch, value.name = "Catch") %>% 
    group_by(Iteration, Year, Region, RuleNum) %>%
    summarise(Catch = sum(Catch)) %>%
    dplyr::mutate("CatchType" = "NSL")    
  
  pcatch <- rbind.data.frame(slcatch2, nslcatch2)
  pcatch2 <- pcatch %>% 
    dplyr::group_by(Iteration, Year, Region, RuleNum) %>%
    dplyr::summarise("Catch" = sum(Catch))
  
  rm(slcatch)
  rm(nslcatch)
  gc()
  
  slrcatch <- mcmc$resid_catch_sl_jryt
  dimnames(slrcatch) <- list("Iteration"=1:n_iter, "RuleNum"=1:dim(slrcatch)[2], "Region"=regions, "Year"=pyears, "Season"=seasons)
  slrcatch2 <- reshape2::melt(slrcatch, value.name = "CatchResidual") %>% 
    group_by(Iteration, Year, Region, RuleNum) %>%
    summarise(CatchResidual = sum(CatchResidual)) 
  slrcatch2$CatchResidual[which(abs(slrcatch2$CatchResidual)<1)] <- 0
  
  nslrcatch <- mcmc$resid_catch_nsl_jryt
  dimnames(nslrcatch) <- list("Iteration"=1:n_iter, "RuleNum"=1:dim(nslrcatch)[2], "Region"=regions, "Year"=pyears, "Season"=seasons)
  nslrcatch2 <- reshape2::melt(nslrcatch, value.name = "CatchResidual") %>% 
    group_by(Iteration, Year, Region, RuleNum) %>%
    summarise(CatchResidual = sum(CatchResidual)) 
  nslrcatch2$CatchResidual[which(abs(nslrcatch2$CatchResidual)<1)] <- 0
  
  rcatch <- rbind.data.frame(slrcatch2, nslrcatch2)
  rcatch2 <- rcatch %>% 
    dplyr::group_by(Iteration, Year, Region, RuleNum) %>%
    dplyr::summarise("CatchResidual" = sum(CatchResidual))
  
  rm(slrcatch)
  rm(nslrcatch)
  gc()
  
  # cpue <- mcmc$mp_offset_cpue_jry
  #     dimnames(cpue) <- list("Iteration"=1:n_iter, "RuleNum"=1:dim(cpue)[2], "Region"=regions, "Year"=pyears)
  #     cpue2 <- reshape2::melt(cpue, value.name="CPUE") %>%
  #     	group_by(Iteration, Year, RuleNum) %>%
  #     	summarise(CPUE = sum(CPUE)) 
  
  catch <- full_join(pcatch2, rcatch2)
  # catch_cpue <- full_join(catch, cpue2)
  
  rm(pcatch2)
  rm(rcatch2)
  gc()
  
  # catch_cpue$RuleType <- sapply(1:nrow(rules), function(x) ifelse(rules[x,1]==1, "FixedCatch", "CPUErule"))
  # catch_cpue$Rule <- sapply(1:nrow(rules), function(x) ifelse(catch_cpue$RuleType[x]=="FixedCatch", rules[x,2], NA))
  # catch_cpue$Rule[which(catch_cpue$RuleType=="CPUErule")] <- paste0("Rule", 1:length(which(catch_cpue$RuleType=="CPUErule")))
  # catch_cpue <- catch_cpue %>% select(-RuleNum)
  
  vb <- mcmc$biomass_vulnref_AW_jyr
  dimnames(vb) <- list("Iteration" = 1:n_iter, "RuleNum" = 1:dim(vb)[2], "Year" = pyears, "Region" = regions)
  vb2 <- reshape2::melt(vb) %>% 
    dplyr::rename(VB = value)
  
  vb0 <- mcmc$B0_r
  dimnames(vb0) <- list("Iteration" = 1:n_iter, "Region" = regions2)
  vb0 <- reshape2::melt(vb0) %>%
    dplyr::rename(VB0 = value)
  
  tb <- mcmc$biomass_total_jytrs
  dimnames(tb) <- list("Iteration" = 1:n_iter, "RuleNum" = 1:dim(vb)[2], "Year" = pyears, "Season" = seasons, "Region" = regions, "Sex" = c(sex, "Total"))
  tb2 <- reshape2::melt(tb) %>% 
    dplyr::filter(Sex == "Total") %>%
    dplyr::group_by(Iteration, RuleNum, Year, Region) %>%
    dplyr::summarise(TB = sum(value))
  
  tb0 <- mcmc$Btot0_r
  dimnames(tb0) <- list("Iteration" = 1:n_iter, "Region" = regions2)
  tb0 <- reshape2::melt(tb0) %>%
    dplyr::rename(TB0 = value)    
  
  ssb <- mcmc$biomass_ssb_jyr
  dimnames(ssb) <- list("Iteration" = 1:n_iter, "RuleNum" = 1:dim(ssb)[2], "Year" = pyears, "Region" = regions)
  ssb2 <- reshape2::melt(ssb) %>% dplyr::rename("SSB"=value) 
  
  ssb0 <- mcmc$SSB0_r
  dimnames(ssb0) <- list("Iteration" = 1:n_iter, "Region" = regions2)
  ssb0 <- reshape2::melt(ssb0) %>%
    dplyr::rename(SSB0=value) 
  
  rec <- mcmc$recruits_ry
  dimnames(rec) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = pyears)
  rec2 <- reshape2::melt(rec) %>%
    dplyr::rename(Recruitment = value)
  
  relssb <- inner_join(ssb2, ssb0) %>%
    dplyr::mutate(RelSSB = SSB/SSB0)
  
  relvb <- inner_join(vb2, vb0) %>%
    dplyr::mutate(RelVB = VB/VB0)
  
  reltb <- inner_join(tb2, tb0) %>%
    dplyr::mutate(RelTB = TB/TB0)
  
  relb <- full_join(relssb, relvb)
  relb2 <- full_join(relb, reltb)
  relb3 <- full_join(relb2, rec2)
  
  # relssb$RuleType <- sapply(1:nrow(rules), function(x) ifelse(rules[x,1]==1, "FixedCatch", "CPUErule"))
  # relssb$Rule <- sapply(1:nrow(rules), function(x) ifelse(relssb$RuleType[x]=="FixedCatch", rules[x,2], NA))
  # relssb$Rule[which(relssb$RuleType=="CPUErule")] <- paste0("Rule", 1:length(which(relssb$RuleType=="CPUErule")))
  # relssb <- relssb %>% select(-RuleNum)
  
  info <- full_join(catch, relb3) 
  info$Region <- paste0("Region ", info$Region)
  pinfo <- info %>% filter(Year %in% cutyears)
  dinfo <- info %>% ungroup() %>% filter(Year %in% years) %>% filter(RuleNum == 1) %>% select(-c(RuleNum))
  
  summary <- pinfo %>%
    dplyr::group_by(Region, RuleNum) %>%
    dplyr::summarise(C5 = quantile(Catch, prob = 0.05), 
                     C50 = quantile(Catch, prob = 0.5),
                     C95 = quantile(Catch, prob = 0.95),
                     SB5 = quantile(RelSSB, prob = 0.05),
                     SB50 = quantile(RelSSB, prob = 0.5),
                     SB95 = quantile(RelSSB, prob = 0.95),
                     VB5 = quantile(RelVB, prob = 0.05),
                     VB50 = quantile(RelVB, prob = 0.5),
                     VB95 = quantile(RelVB, prob = 0.95),
                     TB5 = quantile(RelTB, prob = 0.05),
                     TB50 = quantile(RelTB, prob = 0.5),
                     TB95 = quantile(RelTB, prob = 0.95),		
                     R5 = quantile(Recruitment, prob = 0.05),
                     R50 = quantile(Recruitment, prob = 0.5),
                     R95 = quantile(Recruitment, prob = 0.95),	
                     AvgTotalCatch = sum(Catch)/max(Iteration),
                     CV = sd(Catch)/mean(Catch),
                     CatchConstraint = ifelse(quantile(Catch, prob=0.95)-quantile(Catch,prob=0.05) > 2, 1, 0), 
                     Prisk = length(which(RelSSB <= 0.2))/length(RelSSB),
                     RiskConstraint = ifelse(Prisk >= 0.1, 1, 0))
  if(all(rules[,1]==1)) summary$FixedCatch <- rules[,2]
  summary$Constraint <- sapply(1:nrow(summary), function(x){
    if(summary$CatchConstraint[x] == 1 & summary$RiskConstraint[x] == 1) out <- "Risk&Catch"
    if(summary$CatchConstraint[x] == 1 & summary$RiskConstraint[x] == 0) out <- "Catch"
    if(summary$CatchConstraint[x] == 0 & summary$RiskConstraint[x] == 1) out <- "Risk"
    if(summary$CatchConstraint[x] == 0 & summary$RiskConstraint[x] == 0) out <- "Pass"
    return(out)
  })
  summary$Constraint <- factor(summary$Constraint, levels = c("Pass", "Catch", "Risk", "Risk&Catch"))
  
  findu1 <- summary %>% 
    dplyr::group_by(Region) %>%
    filter(C50 == max(C50)) %>% 
    mutate(Type = "Unconstrained")
  findu2 <- summary %>% 
    dplyr::group_by(Region) %>%
    filter(RiskConstraint < 0.1) %>% 
    filter(C50 == max(C50)) %>% 
    mutate(Type = "RiskConstrained")
  findu <- summary %>% 
    dplyr::group_by(Region) %>%
    filter(CatchConstraint==0) %>% 
    filter(C50 == max(C50)) %>% 
    mutate(Type = "CatchConstrained")
  findc <- summary %>% 
    dplyr::group_by(Region) %>%
    filter(CatchConstraint==0) %>% 
    filter(RiskConstraint < 0.1) %>% 
    filter(C50 == max(C50)) %>% 
    mutate(Type = "Constrained")
  
  find <- t(rbind.data.frame(findu1, findu2, findu, findc))
  colnames(find) <- find["Type",]
  write.csv(find, file.path(figure_dir, "MSY_info.csv"))
  
  status <- dinfo %>% filter(Year == (max(Year))) %>%
    dplyr::group_by(Region) %>%
    dplyr::summarise(C5 = quantile(Catch, prob = 0.05), 
                     C50 = quantile(Catch, prob = 0.5),
                     C95 = quantile(Catch, prob = 0.95),
                     SB5 = quantile(RelSSB, prob = 0.05),
                     SB50 = quantile(RelSSB, prob = 0.5),
                     SB95 = quantile(RelSSB, prob = 0.95),
                     VB5 = quantile(RelVB, prob = 0.05),
                     VB50 = quantile(RelVB, prob = 0.5),
                     VB95 = quantile(RelVB, prob = 0.95),
                     TB5 = quantile(RelTB, prob = 0.05),
                     TB50 = quantile(RelTB, prob = 0.5),
                     TB95 = quantile(RelTB, prob = 0.95))
  stat2 <- t(status)
  colnames(stat2) <- max(dinfo$Year)
  write.csv(stat2, file.path(figure_dir, "Current_status.csv"))
  
  outputs <- c("Catch", "RelSSB", "RelVB", "RelTB", "Recruitment")
  findc2 <- lapply(1:length(outputs), function(x){
    if(outputs[x] == "Catch"){
      sub <- findc %>% select(Region, RuleNum, C5, C50, C95, AvgTotalCatch, CV, CatchConstraint, Prisk, RiskConstraint, FixedCatch, Constraint, Type)
      sub2 <- sub %>% tidyr::pivot_longer(cols = c(C5, C50, C95), names_to = "Variable", values_to = "Target")
      sub2$Percentile <- c(5,50,95)
      sub2$Variable <- outputs[x]
    }
    if(outputs[x] == "RelSSB"){
      sub <- findc %>% select(Region, RuleNum, SB5, SB50, SB95, AvgTotalCatch, CV, CatchConstraint, Prisk, RiskConstraint, FixedCatch, Constraint, Type)
      sub2 <- sub %>% tidyr::pivot_longer(cols = c(SB5, SB50, SB95), names_to = "Variable", values_to = "Target")
      sub2$Percentile <- c(5,50,95)
      sub2$Variable <- outputs[x]
    }
    if(outputs[x] == "RelVB"){
      sub <- findc %>% select(Region, RuleNum, VB5, VB50, VB95, AvgTotalCatch, CV, CatchConstraint, Prisk, RiskConstraint, FixedCatch, Constraint, Type)
      sub2 <- sub %>% tidyr::pivot_longer(cols = c(VB5, VB50, VB95), names_to = "Variable", values_to = "Target")
      sub2$Percentile <- c(5,50,95)
      sub2$Variable <- outputs[x]
    }
    if(outputs[x] == "RelTB"){
      sub <- findc %>% select(Region, RuleNum, TB5, TB50, TB95, AvgTotalCatch, CV, CatchConstraint, Prisk, RiskConstraint, FixedCatch, Constraint, Type)
      sub2 <- sub %>% tidyr::pivot_longer(cols = c(TB5, TB50, TB95), names_to = "Variable", values_to = "Target")
      sub2$Percentile <- c(5,50,95)
      sub2$Variable <- outputs[x]
    }
    if(outputs[x] == "Recruitment"){
      sub <- findc %>% select(Region, RuleNum, R5, R50, R95, AvgTotalCatch, CV, CatchConstraint, Prisk, RiskConstraint, FixedCatch, Constraint, Type)
      sub2 <- sub %>% tidyr::pivot_longer(cols = c(R5, R50, R95), names_to = "Variable", values_to = "Target")
      sub2$Percentile <- c(5,50,95)
      sub2$Variable <- outputs[x]
      sub2$Target <- NA
    }
    return(sub2)
  }) 
  findc2 <- do.call(rbind, findc2)
  findc3 <- findc2 %>% tidyr::pivot_wider(names_from = Percentile, values_from = Target, names_prefix = "Percentile")
  
  dinfo2 <- dinfo %>% 
    select(Year, Region, Catch, RelSSB, RelVB, RelTB, Recruitment) %>%
    tidyr::pivot_longer(cols = c(Catch, RelSSB, RelVB, RelTB, Recruitment), names_to = "Variable")
  dinfo3 <- inner_join(dinfo2, findc3) 
  
  pinfo2 <- pinfo %>%
    select(Iteration, Year, Region, RuleNum, Catch, RelSSB, RelVB, RelTB, Recruitment) %>%
    tidyr::pivot_longer(cols = c(Catch, RelSSB, RelVB, RelTB, Recruitment), names_to = "Variable")
  pinfo3 <- inner_join(pinfo2, findc3)
  
  info2 <- info %>%
    select(Iteration, Year, Region, RuleNum, Catch, RelSSB, RelVB, RelTB, Recruitment) %>%
    tidyr::pivot_longer(cols = c(Catch, RelSSB, RelVB, RelTB, Recruitment), names_to = "Variable")
  info3 <- inner_join(info2, findc3)
  
  p <- ggplot(dinfo3) +
    geom_ribbon(aes(x = Year, ymin = Percentile5, ymax = Percentile95), alpha = 0.2, color = NA, fill = "forestgreen") +
    geom_line(aes(x = Year, y = Percentile50), color = "forestgreen", lwd = 1.5) +
    geom_line(aes(x = Year, y = value), lwd = 2) +
    facet_wrap(Variable~Region, scales = "free_y") +
    expand_limits(y=0) +
    xlab("Year") + 
    ylab("Value") + 
    theme_lsd()
  ggsave(file.path(figure_dir, "Target_Current.png"), p, width=10, height=6)
  
  p2 <- ggplot(info3) +
    geom_ribbon(aes(x = Year, ymin = Percentile5, ymax = Percentile95), alpha = 0.3, color = NA, fill = "forestgreen") +
    geom_line(aes(x = Year, y = Percentile50), color = "forestgreen", lwd = 1.5) +
    stat_summary(aes(x = Year, y = value), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.2) +
    stat_summary(aes(x = Year, y = value), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
    geom_line(data = info2 %>% filter(Iteration == 1) %>% filter(RuleNum == unique(info3$RuleNum)), aes(x = Year, y = value), lty=2) +
    geom_line(data = info2 %>% filter(Iteration == 2) %>% filter(RuleNum == unique(info3$RuleNum)), aes(x = Year, y = value), lty=2) +
    geom_line(data = info2 %>% filter(Iteration == 3) %>% filter(RuleNum == unique(info3$RuleNum)), aes(x = Year, y = value), lty=2) +
      facet_wrap(Variable~Region, scales = "free_y") +
    xlab("Year") +
    ylab("Value") + 
    expand_limits(y=0) +
    theme_lsd()
  ggsave(file.path(figure_dir, "Target_Projections.png"), p2, width=12, height=6)
  
  p3 <- ggplot(summary) +
    geom_segment(aes(x=SB5, xend=SB95, y=C50, yend=C50, color = factor(RiskConstraint)), alpha=0.75, lwd=1.3) +
    geom_segment(aes(x=SB50, xend=SB50, y=C5, yend=C95, color = factor(RiskConstraint)), alpha=0.75, lwd=1.3) +
    geom_point(aes(x=SB50, y=C50, fill=factor(RiskConstraint)), pch=21, alpha=0.75, cex=4) +
    expand_limits(x = 0, y = 0) +
    # scale_x_continuous(limits = c(0, 1)) +
    # facet_grid(.~RuleType, scales="free_y", shrink=FALSE) +
    xlab("Relative spawning stock biomass") +
    ylab("Catch") +
    scale_colour_viridis_d() +
    scale_fill_viridis_d() +
    guides(fill = guide_legend(title = ">10% below soft limit"), color = guide_legend(title = ">10% below soft limit")) +
    theme_lsd(base_size=14)
  ggsave(file.path(figure_dir, "Catch_versus_RelSSB_RiskConstraint.png"), p3, width=12, height=6) 
  
  p4 <- ggplot(summary) +
    geom_segment(aes(x=SB5, xend=SB95, y=C50, yend=C50, color = factor(CatchConstraint)), alpha=0.75, lwd=1.3) +
    geom_segment(aes(x=SB50, xend=SB50, y=C5, yend=C95, color = factor(CatchConstraint)), alpha=0.75, lwd=1.3) +
    geom_point(aes(x=SB50, y=C50, fill=factor(CatchConstraint)), pch=21, alpha=0.75, cex=4) +
    expand_limits(x = 0, y = 0) +
    # scale_x_continuous(limits = c(0, 1)) +
    # facet_grid(.~RuleType, scales="free_y", shrink=FALSE) +
    xlab("Relative spawning stock biomass") +
    ylab("Catch") +
    scale_colour_viridis_d() +
    scale_fill_viridis_d() +
    guides(fill = guide_legend(title = "Catch constraint"), color = guide_legend(title = "Catch constraint")) +
    theme_lsd(base_size=14)
  ggsave(file.path(figure_dir, "Catch_versus_RelSSB_CatchConstraint.png"), p4, width=12, height=6) 
  
  
  p5 <- ggplot(summary) +
    geom_segment(aes(x=SB5, xend=SB95, y=C50, yend=C50, color = factor(Constraint)), alpha=0.75, lwd=1.3) +
    geom_segment(aes(x=SB50, xend=SB50, y=C5, yend=C95, color = factor(Constraint)), alpha=0.75, lwd=1.3) +
    geom_point(aes(x=SB50, y=C50, fill=factor(Constraint)), pch=21, alpha=0.75, cex=4) +
    expand_limits(x = 0, y = 0) +
    # scale_x_continuous(limits = c(0, 1)) +
    # facet_grid(.~RuleType, scales="free_y", shrink=FALSE) +
    xlab("Relative spawning stock biomass") +
    ylab("Catch") +
    scale_colour_viridis_d() +
    scale_fill_viridis_d() +
    guides(fill = guide_legend(title = "Constraint"), color = guide_legend(title = "Constraint")) +
    theme_lsd(base_size=14)
  ggsave(file.path(figure_dir, "Catch_versus_RelSSB_AllConstraints.png"), p5, width=12, height=6) 
  
  
  p6 <- ggplot(summary) +
    geom_segment(aes(x=VB5, xend=VB95, y=C50, yend=C50, color = factor(Constraint)), alpha=0.75, lwd=1.3) +
    geom_segment(aes(x=VB50, xend=VB50, y=C5, yend=C95, color = factor(Constraint)), alpha=0.75, lwd=1.3) +
    geom_point(aes(x=VB50, y=C50, fill=factor(Constraint)), pch=21, alpha=0.75, cex=4) +
    expand_limits(x = 0, y = 0) +
    # geom_point(data=findc, aes(x = VB50, y = C50), pch=21, cex=4) +
    geom_vline(data = findc, aes(xintercept = VB50), lty=2) +
    geom_hline(data = findc, aes(yintercept = C50), lty=2) +
    # scale_x_continuous(limits = c(0, 1)) +
    # facet_grid(.~RuleType, scales="free_y", shrink=FALSE) +
    xlab("Relative vulnerable reference biomass") +
    ylab("Catch") +
    scale_colour_viridis_d() +
    scale_fill_viridis_d() +
    guides(fill = guide_legend(title = "Constraint"), color = guide_legend(title = "Constraint")) +
    theme_lsd(base_size=14)
  ggsave(file.path(figure_dir, "Catch_versus_RelVB_AllConstraint.png"), p6, width=12, height=6) 
  
}