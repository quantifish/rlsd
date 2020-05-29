#' Calculate reference points and create relevant figures
#'
#' @param object the ref-lsd object
#' @param figure_dir the directory to save figures to
#' @import dplyr
#' @import ggplot2
#' @import ggthemes
#' @export
#'
plot_refpoints <- function(object, figure_dir){
  
  mcmc <- object@mcmc
  data <- object@data
  years <- data$first_yr:data$last_yr
  pyears <- data$first_yr:data$last_proj_yr
  cutyears <- (data$first_proj_yr+1):data$last_proj_yr
  seasons <- c("AW","SS")
  regions <- 1:data$n_area
  if(length(regions) > 1) regions2 <- c(regions, "Total")
  if(length(regions) == 1) regions2 <- regions
  sex <- c("Male","Immature female","Mature female")
  n_iter <- nrow(mcmc[[1]])
  rules <- data$mp_rule_parameters
  n_rules <- nrow(rules)
  
  slcatch <- mcmc$pred_catch_sl_jryt
  dimnames(slcatch) <- list("Iteration"=1:n_iter, "RuleNum"=1:dim(slcatch)[2], "Region"=regions, "Year"=pyears, "Season"=seasons)
  slcatch2 <- reshape2::melt(slcatch, value.name = "Catch") %>% 
    group_by(Iteration, Year, Region, RuleNum) %>%
    summarise(Catch = sum(Catch)) %>%
    dplyr::mutate("CatchType" = "SL")

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
  

  vb <- mcmc$biomass_vulnref_AW_jyr
  dimnames(vb) <- list("Iteration" = 1:n_iter, "RuleNum" = 1:dim(vb)[2], "Year" = pyears, "Region" = regions)
  vb2 <- reshape2::melt(vb) %>% 
    dplyr::rename(VB = value)
  vb2$Region <- factor(vb2$Region)
  
  vb0 <- mcmc$B0now_r
  dimnames(vb0) <- list("Iteration" = 1:n_iter, "Region" = regions2)
  vb0 <- reshape2::melt(vb0) %>%
    dplyr::rename(VB0now = value)
  vb0$Region <- factor(vb0$Region)
  
  ssb <- mcmc$biomass_ssb_jyr
  dimnames(ssb) <- list("Iteration" = 1:n_iter, "RuleNum" = 1:dim(ssb)[2], "Year" = pyears, "Region" = regions)
  ssb2 <- reshape2::melt(ssb) %>% dplyr::rename("SSB"=value) 
  ssb2$Region <- factor(ssb2$Region)
  
  ssb0 <- mcmc$SSB0_r
  dimnames(ssb0) <- list("Iteration" = 1:n_iter, "Region" = regions2)
  ssb0 <- reshape2::melt(ssb0) %>%
    dplyr::rename(SSB0=value) 
  ssb0$Region <- factor(ssb0$Region)
  
  rec <- mcmc$recruits_ry
  dimnames(rec) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = pyears)
  rec2 <- reshape2::melt(rec) %>%
    dplyr::rename(Recruitment = value)
  rec2$Region <- factor(rec2$Region)
  
  relssb <- inner_join(ssb2, ssb0) %>%
    dplyr::mutate(RelSSB = SSB/SSB0)
  
  relvb <- inner_join(vb2, vb0) %>%
    dplyr::mutate(RelVB = VB/VB0now)

  
  relb <- full_join(relssb, relvb)
  relb2 <- full_join(relb, rec2)

  catch$Region <- factor(catch$Region)
  info <- full_join(catch, relb2) 
  
  rm(relb)
  rm(relb2)
  rm(catch)
  gc()
  
  pinfo <- info %>% filter(Year %in% cutyears)
  dinfo <- info %>% ungroup() %>% filter(Year %in% years) %>% filter(RuleNum == 1) %>% select(-c(RuleNum))
  
  constraints <- pinfo %>% 
    group_by(Region, RuleNum) %>%
    summarise(AvgTotalCatch = sum(Catch)/max(Iteration),
              CV = sd(Catch)/mean(Catch),
              CatchConstraint = ifelse(quantile(Catch, prob = 0.95) - quantile(Catch, prob = 0.05) > 2, 1, 0),
              Prisk = length(which(RelSSB <= 0.2))/length(RelSSB),
              RiskConstraint_5 = ifelse(Prisk >= 0.05, 1, 0),
              RiskConstraint_10 = ifelse(Prisk >= 0.1, 1, 0)) 
  
  summary <- pinfo %>%
    tidyr::pivot_longer(cols=c(Catch,SSB,SSB0,RelSSB,VB,VB0now,RelVB,Recruitment), names_to = "Variable", values_to = "Value") %>%
    dplyr::group_by(Region, RuleNum, Variable) %>%
    dplyr::summarise(P5 = quantile(Value, 0.05),
                     P50 = quantile(Value, 0.5),
                     P95 = quantile(Value, 0.95))
  
  output <- full_join(constraints, summary)
  
  
  rulepar <- paste0("par",1:10)
  colnames(rules) <- rulepar
  ruledf <- data.frame(RuleNum = 1:nrow(rules), rules)
  
  ## identify and label fixed catch rules
  if(all(rules[,1]==1)){
    output2 <- output %>% left_join(ruledf %>% dplyr::select(RuleNum, par2)) %>% rename(FixedCatch = par2)
  }
  output2$Constraint <- sapply(1:nrow(output2), function(x){
    if(output2$CatchConstraint[x] == 1 & output2$RiskConstraint_10[x] == 1) out <- ">10% below soft limit\n+ catch"
    if(output2$CatchConstraint[x] == 1 & output2$RiskConstraint_5[x] == 1 & output2$RiskConstraint_10[x] == 0) out <- ">5% below soft limit\n+ catch"
    if(output2$CatchConstraint[x] == 1 & output2$RiskConstraint_5[x] == 0 & output2$RiskConstraint_10[x] == 0) out <- "Catch"
    if(output2$CatchConstraint[x] == 0 & output2$RiskConstraint_10[x] == 1) out <- ">10% below soft limit"
    if(output2$CatchConstraint[x] == 0 & output2$RiskConstraint_5[x] == 1 & output2$RiskConstraint_10[x] == 0) out <- ">5% below soft limit"
    if(output2$CatchConstraint[x] == 0 & output2$RiskConstraint_5[x] == 0 & output2$RiskConstraint_10[x] == 0) out <- "Pass"
    return(out)
  })
  output2$Constraint <- factor(output2$Constraint, levels = c("Pass", "Catch", ">5% below soft limit",">10% below soft limit", ">5% below soft limit\n+ catch",">10% below soft limit\n+ catch"))
  
  write.csv(output2, file.path(figure_dir, "Summary_byVariable.csv"))
  
  output3 <- output2 %>% 
    tidyr::pivot_longer(cols = P5:P95, names_to = "Percentile", values_to = "Value") %>%
    tidyr::pivot_wider(names_from = Variable, values_from = Value)
  
  write.csv(output3, file.path(figure_dir, "Summary_byPercentile.csv"))
  
  output4 <- output3 %>%
    tidyr::pivot_wider(names_from = Percentile, values_from = Catch:VB0now)
  
  pcat <- unique(output2$Constraint)
  find_max <- lapply(1:length(pcat), function(x){
    sub <- output2 %>% 
      dplyr::filter(Constraint == pcat[x]) %>%
      dplyr::filter(Variable == "Catch") %>%
      dplyr::group_by(Region) %>%
      dplyr::filter(P50 == max(P50))
    return(sub)
  })
  find_max <- do.call(rbind, find_max)
  
  write.csv(find_max, file.path(figure_dir, "MSY_info.csv"))
  
  status <- dinfo %>%
    dplyr::filter(Year == max(Year)) %>%
    tidyr::pivot_longer(cols=c(Catch,SSB,SSB0,RelSSB,VB,VB0now,RelVB,Recruitment), names_to = "Variable", values_to = "Value") %>%
    dplyr::group_by(Region, Variable) %>%
    dplyr::summarise(P5 = quantile(Value, 0.05),
                     P50 = quantile(Value, 0.5),
                     P95 = quantile(Value, 0.95))

  write.csv(status, file.path(figure_dir, "Current_status.csv"))
  
  find_msy <- find_max %>% filter(Constraint == "Pass")
  
  msy_info <- output2 %>% filter(RuleNum == find_msy$RuleNum)
  
  p_relssb <- ggplot(output4) +
    geom_segment(aes(x = RelSSB_P5, xend = RelSSB_P95, y = Catch_P50, yend = Catch_P50, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_segment(aes(x = RelSSB_P50, xend = RelSSB_P50, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_point(aes(x = RelSSB_P50, y = Catch_P50, fill = Constraint), pch = 21, cex = 4) +
    expand_limits(y = 0, x = 0) +
    xlab("Relative spawning biomass (SSB/SSB0)") + ylab("Average annual catch") +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    theme_bw(base_size = 20)
  ggsave(file.path(figure_dir, "RelSSB_vs_Catch_byConstraint.png"), p_relssb, height = 10, width = 12)
  
  p_relvb <- ggplot(output4) +
    geom_segment(aes(x = RelVB_P5, xend = RelVB_P95, y = Catch_P50, yend = Catch_P50, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_segment(aes(x = RelVB_P50, xend = RelVB_P50, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_point(aes(x = RelVB_P50, y = Catch_P50, fill = Constraint), pch = 21, cex = 3) +
    expand_limits(y = 0, x = 0) +
    xlab("Relatve vulnerable biomass (VB/VB0now)") + ylab("Average annual catch") +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    theme_bw(base_size = 20)
  ggsave(file.path(figure_dir, "RelVB_vs_Catch_byConstraint.png"), p_relvb, height = 10, width = 12)
  
  p_relvb_v2 <- ggplot(output4) +
    geom_segment(aes(x = RelVB_P5, xend = RelVB_P95, y = Catch_P50, yend = Catch_P50, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_segment(aes(x = RelVB_P50, xend = RelVB_P50, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_point(aes(x = RelVB_P50, y = Catch_P50, fill = Constraint), pch = 21, cex = 3) +
    expand_limits(y = 0, x = 0) +
    geom_vline(data = output4 %>% filter(RuleNum %in% find_msy$RuleNum), aes(xintercept = RelVB_P50), linetype = 2) +
    geom_hline(data = output4 %>% filter(RuleNum %in% find_msy$RuleNum), aes(yintercept = Catch_P50), linetype = 2) +
    xlab("Relatve vulnerable biomass (VB/VB0now)") + ylab("Average annual catch") +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    theme_bw(base_size = 20)
  ggsave(file.path(figure_dir, "RelVB_vs_Catch_byConstraint_wTarget.png"), p_relvb_v2, height = 10, width = 12)
  
  p_vb <- ggplot(output4) +
    geom_segment(aes(x = VB_P5, xend = VB_P95, y = Catch_P50, yend = Catch_P50, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_segment(aes(x = VB_P50, xend = VB_P50, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_point(aes(x = VB_P50, y = Catch_P50, fill = Constraint), pch = 21, cex = 3) +
    expand_limits(y = 0, x = 0) +
    xlab("Vulnerable biomass") + ylab("Average annual catch") +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    theme_bw(base_size = 20)  
  ggsave(file.path(figure_dir, "VB_vs_Catch_byConstraint.png"), p_vb, height = 10, width = 12)
  
  p_vb_v2 <- ggplot(output4) +
    geom_segment(aes(x = VB_P5, xend = VB_P95, y = Catch_P50, yend = Catch_P50, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_segment(aes(x = VB_P50, xend = VB_P50, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_point(aes(x = VB_P50, y = Catch_P50, fill = Constraint), pch = 21, cex = 3) +
    expand_limits(y = 0, x = 0) +
    geom_vline(data = output4 %>% filter(RuleNum %in% find_msy$RuleNum), aes(xintercept = VB_P50), linetype = 2) +
    geom_hline(data = output4 %>% filter(RuleNum %in% find_msy$RuleNum), aes(yintercept = Catch_P50), linetype = 2) +
    xlab("Vulnerable biomass") + ylab("Average annual catch") +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    theme_bw(base_size = 20)  
  ggsave(file.path(figure_dir, "VB_vs_Catch_byConstraint_wTarget.png"), p_vb_v2, height = 10, width = 12)
  
  p_vbcatch <- ggplot(output4) +
    geom_segment(aes(x = VB_P5, xend = VB_P95, y = Catch_P50, yend = Catch_P50, color = factor(CatchConstraint)), lwd = 1.2, alpha = 0.8) +
    geom_segment(aes(x = VB_P50, xend = VB_P50, y = Catch_P5, yend = Catch_P95, color = factor(CatchConstraint)), lwd = 1.2, alpha = 0.8) +
    geom_point(aes(x = VB_P50, y = Catch_P50, fill = factor(CatchConstraint)), pch = 21, cex = 3) +
    expand_limits(y = 0, x = 0) +
    xlab("Vulnerable biomass") + ylab("Average annual catch") +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    guides(fill=guide_legend(title="Catch constraint"), color = FALSE) +
    theme_bw(base_size = 20) 
  ggsave(file.path(figure_dir, "VB_vs_Catch_CatchConstraint.png"), p_vbcatch, height = 10, width = 12)
  
  output4$RiskConstraint <- sapply(1:nrow(output4), function(x) ifelse(output4$RiskConstraint_10[x] == 1, ">10% below soft limit", 
                                                                      ifelse(output4$RiskConstraint_5[x] == 1, ">5% below soft limit", "Pass")))
  output4$RiskConstraint <- factor(output4$RiskConstraint, levels = c("Pass", ">5% below soft limit", ">10% below soft limit"))
  p_vbrisk <- ggplot(output4) +
    geom_segment(aes(x = VB_P5, xend = VB_P95, y = Catch_P50, yend = Catch_P50, color = RiskConstraint), lwd = 1.2, alpha = 0.8) +
    geom_segment(aes(x = VB_P50, xend = VB_P50, y = Catch_P5, yend = Catch_P95, color = RiskConstraint), lwd = 1.2, alpha = 0.8) +
    geom_point(aes(x = VB_P50, y = Catch_P50, fill = RiskConstraint), pch = 21, cex = 3) +
    expand_limits(y = 0, x = 0) +
    xlab("Vulnerable biomass") + ylab("Average annual catch") +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    guides(fill=guide_legend(title="Risk constraint"), color = FALSE) +
    theme_bw(base_size = 20) 
  ggsave(file.path(figure_dir, "VB_vs_Catch_RiskConstraint.png"), p_vbrisk, height = 10, width = 12)
  
  check <- dinfo %>% left_join(msy_info %>% filter(Variable == "VB"))
  p_vbcurr <- ggplot(check) +
    geom_ribbon(aes(x = Year, ymin = P5, ymax = P95), fill = "forestgreen", alpha = 0.5) +
    geom_hline(aes(yintercept = P50), color = "forestgreen") +
    geom_line(aes(x = Year, y = VB), lwd = 1.2) +
    expand_limits(y = 0) +
    ylab("Vulnerable biomass (VB)") +
    coord_cartesian(xlim = c(min(check$Year),max(check$Year)), expand = FALSE) +
    theme_bw(base_size = 20)
  ggsave(file.path(figure_dir, "VBcurrent.png"), p_vbcurr, height = 10, width = 15)
  
  check <- dinfo %>% left_join(msy_info %>% filter(Variable == "RelVB")) 
  p_relvbcurr <- ggplot(check) +
    geom_ribbon(aes(x = Year, ymin = P5, ymax = P95), fill = "forestgreen", alpha = 0.5) +
    geom_hline(aes(yintercept = P50), color = "forestgreen") +
    geom_line(aes(x = Year, y = RelVB), lwd = 1.2) +
    expand_limits(y = 0) +
    ylab("Relative vulnerable biomass (VB/VB0now)") +
    coord_cartesian(xlim = c(min(check$Year),max(check$Year)), expand = FALSE) +
    theme_bw(base_size = 20)
  ggsave(file.path(figure_dir, "RelVBcurrent.png"), p_relvbcurr, height = 10, width = 15)
  
  info <- full_join(dinfo, pinfo %>% filter(RuleNum %in% msy_info$RuleNum)) %>%
    tidyr::pivot_longer(cols = -c(Iteration, Year, Region, CatchResidual, RuleNum), names_to = "Variable", values_to = "Value") %>%
    dplyr::filter(Variable == "VB") %>%
    dplyr::group_by(Year, Region, RuleNum, Variable) %>%
    dplyr::summarise(P5 = quantile(Value, 0.05),
                     P50 = quantile(Value, 0.5),
                     P95 = quantile(Value, 0.95))
  check <- info %>% left_join(by = "Region", msy_info %>% filter(Variable == "VB") %>% rename(RefVB5 = P5, RefVB50 = P50, RefVB95 = P95) %>% dplyr::select(-Variable)) %>% filter(Variable == "VB")
  p_vbproj <- ggplot(check) +
    geom_ribbon(aes(x = Year, ymin = RefVB5, ymax = RefVB95), fill = "forestgreen", alpha = 0.5) +
    geom_hline(aes(yintercept = RefVB50), color = "forestgreen") +
    geom_ribbon(aes(x = Year, ymin = P5, ymax = P95), alpha = 0.5) +
    geom_line(aes(x = Year, y = P50), lwd = 1.2) +
    coord_cartesian(xlim = c(min(check$Year), max(check$Year)), expand = FALSE) +
    ylab("Vulnerable biomass (VB)") +
    expand_limits(y = 0) +
    theme_bw(base_size = 20)
  ggsave(file.path(figure_dir, "VBproj.png"), p_vbproj, height = 10, width = 15)
  
}
