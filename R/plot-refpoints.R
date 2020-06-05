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
  projyears <- (data$first_proj_yr):data$last_proj_yr
  seasons <- c("AW","SS")
  regions <- 1:data$n_area
  if(length(regions) > 1) regions2 <- c(regions, "Total")
  if(length(regions) == 1) regions2 <- regions
  sex <- c("Male","Immature female","Mature female")
  n_iter <- nrow(mcmc[[1]])
  rules <- data$mp_rule_parameters
  n_rules <- nrow(rules)
  fleets <- c("SL","NSL")
  
  projF <- mcmc$proj_F_jytrf
  dimnames(projF) <- list("Iteration"=1:n_iter, "RuleNum"=1:n_rules, "Year"=pyears, "Season"=seasons, "Region"=regions, "Fleet" = fleets)
  projF2 <- reshape2::melt(projF, value.name = "F")

    # sub <- projF2 %>% filter(Year == 2020) %>% filter(Iteration == 1) %>% filter(RuleNum == 41)
  
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
  
  ssb0 <- mcmc$SSB0now_r
  dimnames(ssb0) <- list("Iteration" = 1:n_iter, "Region" = regions2)
  ssb0 <- reshape2::melt(ssb0) %>%
    dplyr::rename(SSB0now=value) 
  ssb0$Region <- factor(ssb0$Region)
  
  rec <- mcmc$recruits_ry
  dimnames(rec) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = pyears)
  rec2 <- reshape2::melt(rec) %>%
    dplyr::rename(Recruitment = value)
  rec2$Region <- factor(rec2$Region)
  
  relssb <- inner_join(ssb2, ssb0) %>%
    dplyr::mutate(RelSSB = SSB/SSB0now)
  
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
  
  rulepar <- paste0("par",1:10)
  colnames(rules) <- rulepar
  ruledf <- data.frame(RuleNum = 1:nrow(rules), rules)
  

    cutyears <- (projyears[1]+10):(data$last_proj_yr)
    pinfo <- info %>% filter(Year %in% projyears)
    cinfo <- pinfo %>% filter(Year %in% cutyears)
    dinfo <- info %>% ungroup() %>% filter(Year %in% years) %>% filter(RuleNum == 1) %>% select(-c(RuleNum))  

    constraints <- cinfo %>% 
      group_by(Region, RuleNum) %>%
      summarise(AvgTotalCatch = sum(Catch)/max(Iteration),
                CV = sd(Catch)/mean(Catch),
                CatchConstraint = ifelse(quantile(Catch, prob = 0.95) - quantile(Catch, prob = 0.05) > 2, 1, 0),
                Prisk = length(which(RelSSB <= 0.2))/length(RelSSB),
                RiskConstraint_5 = ifelse(Prisk >= 0.05, 1, 0),
                RiskConstraint_10 = ifelse(Prisk >= 0.1, 1, 0)) 
    
    summary <- cinfo %>%
      tidyr::pivot_longer(cols=c(Catch,SSB,SSB0now,RelSSB,VB,VB0now,RelVB,Recruitment), names_to = "Variable", values_to = "Value") %>%
      dplyr::group_by(Region, RuleNum, Variable) %>%
      dplyr::summarise(P5 = quantile(Value, 0.05),
                       P50 = quantile(Value, 0.5),
                       P95 = quantile(Value, 0.95))
    
    output <- full_join(constraints, summary)
    
    ## identify and label fixed catch rules
    output2 <- output %>% 
      left_join(ruledf) %>%  
      mutate(RuleType = ifelse(par1 == 0, "FixedF", ifelse(par1 == 1, "FixedCatch", "CPUE-based"))) %>%
      mutate(CatchConstraint = replace(CatchConstraint, which(RuleType != "FixedCatch"), 0))  

    output2$Constraint <- sapply(1:nrow(output2), function(x){
      # if(output2$CatchConstraint[x] == 1 & output2$RiskConstraint_10[x] == 1) out <- ">10% below soft limit\n+ catch"
      # if(output2$CatchConstraint[x] == 1 & output2$RiskConstraint_5[x] == 1 & output2$RiskConstraint_10[x] == 0) out <- ">5% below soft limit\n+ catch"
      if(output2$CatchConstraint[x] == 1 & output2$RiskConstraint_5[x] == 0 & output2$RiskConstraint_10[x] == 0) out <- "Catch"
      if(output2$RiskConstraint_10[x] == 1) out <- ">10% below soft limit" #output2$CatchConstraint[x] == 0 & 
      if(output2$RiskConstraint_5[x] == 1 & output2$RiskConstraint_10[x] == 0) out <- ">5% below soft limit"
      if(output2$RiskConstraint_5[x] == 0 & output2$RiskConstraint_10[x] == 0) out <- "Pass"
      return(out)
    })
    output2$Constraint <- factor(output2$Constraint, levels = c("Pass", "Catch", ">5% below soft limit",">10% below soft limit", ">5% below soft limit\n+ catch",">10% below soft limit\n+ catch"))
      
    find_max1 <- output2 %>%
      dplyr::filter(Variable == "Catch") %>%
      dplyr::group_by(Region, RuleType, Constraint) %>%
      dplyr::filter(P50 == max(P50))  

    
    find_msy1 <- find_max1 %>% filter(Constraint == "Pass")
    msy_info1 <- output2 %>% filter(RuleNum %in% find_msy1$RuleNum) %>% filter(Variable == "RelVB")

  ## filter MP rules where CV < CV(MSY Fixed F)
  CVmax <- as.numeric(find_msy1[which(find_msy1$RuleType == "FixedF"),"CV"])
  output2$CVConstraint = 0
  output2 <- output2 %>% 
    mutate(CVConstraint = replace(CVConstraint, which(CV > CVmax), 1)) %>%
    mutate(CVConstraint = replace(CVConstraint, which(RuleType != "CPUE-based"), 0))

  output2$Constraint <- sapply(1:nrow(output2), function(x){
    if(output2$CatchConstraint[x] == 1 & output2$RiskConstraint_5[x] == 0 & output2$RiskConstraint_10[x] == 0 & output2$CVConstraint[x] == 0) out <- "Catch"
    if(output2$RiskConstraint_10[x] == 1 & output2$CVConstraint[x] == 0) out <- ">10% below soft limit" #output2$CatchConstraint[x] == 0 & 
    if(output2$RiskConstraint_5[x] == 1 & output2$RiskConstraint_10[x] == 0 & output2$CVConstraint[x] == 0) out <- ">5% below soft limit"
    if(output2$RiskConstraint_5[x] == 0 & output2$RiskConstraint_10[x] == 0 & output2$CVConstraint[x] == 0) out <- "Pass"
    
    if(output2$CatchConstraint[x] == 1 & output2$RiskConstraint_5[x] == 0 & output2$RiskConstraint_10[x] == 0 & output2$CVConstraint[x] == 1) out <- "Catch & CV"
    if(output2$RiskConstraint_10[x] == 1 & output2$CVConstraint[x] == 1) out <- ">10% below soft limit & CV" #output2$CatchConstraint[x] == 0 & 
    if(output2$RiskConstraint_5[x] == 1 & output2$RiskConstraint_10[x] == 0 & output2$CVConstraint[x] == 1) out <- ">5% below soft limit & CV"
    if(output2$RiskConstraint_5[x] == 0 & output2$RiskConstraint_10[x] == 0 & output2$CVConstraint[x] == 1) out <- "CV"

    return(out)
  })
  write.csv(output2, file.path(figure_dir, "Summary_byVariable.csv"))


  find_max <- output2 %>%
    dplyr::filter(Variable == "Catch") %>%
    dplyr::group_by(Region, RuleType, Constraint) %>%
    dplyr::filter(P50 == max(P50))

  write.csv(find_max, file.path(figure_dir, "MSY_info.csv"))

  find_msy <- find_max %>% filter(Constraint == "Pass")

  msy_info <- output2 %>% filter(RuleNum %in% find_msy$RuleNum)
  max_info <- output2 %>% filter(RuleNum %in% find_max$RuleNum)
  

  output3 <- output2 %>% 
    tidyr::pivot_longer(cols = P5:P95, names_to = "Percentile", values_to = "Value") %>%
    tidyr::pivot_wider(names_from = Variable, values_from = Value)
  
  write.csv(output3, file.path(figure_dir, "Summary_byPercentile.csv"))
  
  output4 <- output3 %>%
    tidyr::pivot_wider(names_from = Percentile, values_from = Catch:VB0now)
  if(length(unique(output4$RuleType))==3) output4$RuleType <- factor(output4$RuleType, levels = c("FixedCatch","CPUE-based","FixedF"))
  const <- unique(output4$Constraint)
  const1 <- const[grepl("CV",const)==FALSE]
  const1 <- c(const1[which(const1=="Pass")], const1[grep("5%", const1)], const1[grep("10%",const1)])
  const2 <- rev(const[grepl("CV",const)])
  const <- c(const1,const2)
  output4$Constraint <- factor(output4$Constraint, levels = const)
  

  status <- dinfo %>%
    dplyr::filter(Year == max(Year)) %>%
    tidyr::pivot_longer(cols=c(Catch,SSB,SSB0now,RelSSB,VB,VB0now,RelVB,Recruitment), names_to = "Variable", values_to = "Value") %>%
    dplyr::group_by(Region, Variable) %>%
    dplyr::summarise(P5 = quantile(Value, 0.05),
                     P50 = quantile(Value, 0.5),
                     P95 = quantile(Value, 0.95))

  write.csv(status, file.path(figure_dir, "Current_status.csv"))
  
  ###############################
  ## all rules tested, compared
  ###############################
  require(ggthemes)
  p_cv <- ggplot(output4) +
    geom_segment(aes(x = CV, xend = CV, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_point(aes(x = CV, y = Catch_P50, fill = Constraint), pch = 21, cex = 4) +
    expand_limits(y = 0, x = 0) +
    xlab("CV of catch over time and iteration") + ylab("Average annual catch") +
    scale_fill_tableau() +
    scale_color_tableau() +
    facet_grid(~RuleType, scales = "free_x") +
    theme_bw(base_size = 20)

  p_relssb <- ggplot(output4) +
    geom_segment(aes(x = RelSSB_P5, xend = RelSSB_P95, y = Catch_P50, yend = Catch_P50, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_segment(aes(x = RelSSB_P50, xend = RelSSB_P50, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_point(aes(x = RelSSB_P50, y = Catch_P50, fill = Constraint), pch = 21, cex = 4) +
    expand_limits(y = 0, x = 0) +
    xlab("Relative spawning biomass (SSB/SSB0now)") + ylab("Average annual catch") +
    scale_fill_tableau() +
    scale_color_tableau() +
    facet_grid(~RuleType) +
    theme_bw(base_size = 20)
  ggsave(file.path(figure_dir, "RelSSB_vs_Catch_byConstraint.png"), p_relssb, height = 10, width = 12)
  
  p_relvb <- ggplot(output4) +
    geom_segment(aes(x = RelVB_P5, xend = RelVB_P95, y = Catch_P50, yend = Catch_P50, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_segment(aes(x = RelVB_P50, xend = RelVB_P50, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_point(aes(x = RelVB_P50, y = Catch_P50, fill = Constraint), pch = 21, cex = 3) +
    expand_limits(y = 0, x = 0) +
    xlab("Relatve vulnerable biomass (VB/VB0now)") + ylab("Average annual catch") +
    scale_fill_tableau() +
    scale_color_tableau() +
    facet_grid(~RuleType) +
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
    scale_fill_tableau() +
    scale_color_tableau() +
    facet_grid(~RuleType) +
    theme_bw(base_size = 20)
  ggsave(file.path(figure_dir, "RelVB_vs_Catch_byConstraint_wTarget.png"), p_relvb_v2, height = 10, width = 12)
  
  p_vb <- ggplot(output4) +
    geom_segment(aes(x = VB_P5, xend = VB_P95, y = Catch_P50, yend = Catch_P50, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_segment(aes(x = VB_P50, xend = VB_P50, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_point(aes(x = VB_P50, y = Catch_P50, fill = Constraint), pch = 21, cex = 3) +
    expand_limits(y = 0, x = 0) +
    xlab("Vulnerable biomass") + ylab("Average annual catch") +
    scale_fill_tableau() +
    scale_color_tableau() +
    facet_grid(~RuleType, scales = 'free_x') +
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
    scale_fill_tableau() +
    scale_color_tableau() +
    facet_grid(~RuleType, scales = "free_x") +
    theme_bw(base_size = 20)  
  ggsave(file.path(figure_dir, "VB_vs_Catch_byConstraint_wTarget.png"), p_vb_v2, height = 10, width = 12)
  
  p_vbcatch <- ggplot(output4) +
    geom_segment(aes(x = VB_P5, xend = VB_P95, y = Catch_P50, yend = Catch_P50, color = factor(CatchConstraint)), lwd = 1.2, alpha = 0.8) +
    geom_segment(aes(x = VB_P50, xend = VB_P50, y = Catch_P5, yend = Catch_P95, color = factor(CatchConstraint)), lwd = 1.2, alpha = 0.8) +
    geom_point(aes(x = VB_P50, y = Catch_P50, fill = factor(CatchConstraint)), pch = 21, cex = 3) +
    expand_limits(y = 0, x = 0) +
    xlab("Vulnerable biomass") + ylab("Average annual catch") +
    scale_fill_tableau() +
    scale_color_tableau() +
    facet_grid(~RuleType, scales = "free_x") +
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
    scale_fill_tableau() +
    scale_color_tableau() +
    facet_grid(~RuleType, scales = "free_x") +
    guides(fill=guide_legend(title="Risk constraint"), color = FALSE) +
    theme_bw(base_size = 20) 
  ggsave(file.path(figure_dir, "VB_vs_Catch_RiskConstraint.png"), p_vbrisk, height = 10, width = 12)
  
  p_vbcv <- ggplot(output4) +
    geom_segment(aes(x = VB_P5, xend = VB_P95, y = Catch_P50, yend = Catch_P50, color = factor(CVConstraint)), lwd = 1.2, alpha = 0.8) +
    geom_segment(aes(x = VB_P50, xend = VB_P50, y = Catch_P5, yend = Catch_P95, color = factor(CVConstraint)), lwd = 1.2, alpha = 0.8) +
    geom_point(aes(x = VB_P50, y = Catch_P50, fill = factor(CVConstraint)), pch = 21, cex = 3) +
    expand_limits(y = 0, x = 0) +
    xlab("Vulnerable biomass") + ylab("Average annual catch") +
    scale_fill_tableau() +
    scale_color_tableau() +
    facet_grid(~RuleType, scales = "free_x") +
    guides(fill=guide_legend(title="CV constraint"), color = FALSE) +
    theme_bw(base_size = 20) 
  ggsave(file.path(figure_dir, "VB_vs_Catch_CVConstraint.png"), p_vbcatch, height = 10, width = 12)
  

  ###################################################
  ## Status relative to reference level by rule type
  ###################################################
  check <- dinfo %>% left_join(msy_info %>% filter(Variable == "VB"))
  if(length(unique(check$RuleType))==3) check$RuleType <- factor(check$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
  p_vbcurr <- ggplot(check) +
    geom_ribbon(aes(x = Year, ymin = P5, ymax = P95, fill = RuleType), alpha = 0.5) +
    geom_hline(aes(yintercept = P50, color =RuleType)) +
    geom_line(aes(x = Year, y = VB), lwd = 1.2) +
    expand_limits(y = 0) +
    scale_fill_tableau() +
    scale_color_tableau() +
    ylab("Vulnerable biomass (VB)") +
    facet_grid(~RuleType) +
    guides(color = FALSE, fill = FALSE) +
    coord_cartesian(xlim = c(min(check$Year),max(check$Year)), expand = FALSE) +
    theme_bw(base_size = 20)
  ggsave(file.path(figure_dir, "VBcurrent.png"), p_vbcurr, height = 10, width = 15)
  
  check <- dinfo %>% left_join(msy_info %>% filter(Variable == "RelVB")) 
  if(length(unique(check$RuleType))==3) check$RuleType <- factor(check$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
  p_relvbcurr <- ggplot(check) +
    geom_ribbon(aes(x = Year, ymin = P5, ymax = P95, fill = RuleType), alpha = 0.5) +
    geom_hline(aes(yintercept = P50, color = RuleType)) +
    geom_line(aes(x = Year, y = RelVB), lwd = 1.2) +
    expand_limits(y = 0) +
    scale_color_tableau() +
    scale_fill_tableau() +
    guides(color = FALSE, fill = FALSE) +
    ylab("Relative vulnerable biomass (VB/VB0now)") +
    facet_grid(~RuleType) +
    coord_cartesian(xlim = c(min(check$Year),max(check$Year)), expand = FALSE) +
    theme_bw(base_size = 20)
  ggsave(file.path(figure_dir, "RelVBcurrent.png"), p_relvbcurr, height = 10, width = 15)

  #######################################
  ## examine variability across filters
  #######################################
  check1 <- cinfo %>%
    dplyr::filter(RuleNum %in% find_max$RuleNum) %>%
    dplyr::left_join(max_info %>% filter(Variable == "Catch")) %>%
    dplyr::select(Iteration, Year, Region, RuleNum, RuleType, Constraint, Catch, RelVB, RelSSB) %>%
    tidyr::pivot_longer(cols = c(Catch,RelVB,RelSSB), names_to = "Variable", values_to = "Value")
  check2 <- check1 %>%
    dplyr::group_by(Year, Region, RuleType, Constraint, Variable) %>%
    dplyr::summarise(P5 = quantile(Value, 0.05),
                    P50 = quantile(Value, 0.50),
                    P95 = quantile(Value, 0.95))
  if(length(unique(check2$RuleType))==3) check2$RuleType <- factor(check2$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
  if(length(unique(check1$RuleType))==3) check1$RuleType <- factor(check1$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
  p_constr <- ggplot() +
    geom_ribbon(data = check2 %>% filter(Variable == "Catch"), aes(x = Year, ymin = P5, ymax = P95, fill = RuleType), color = NA, alpha = 0.5) +
    geom_line(data = check2 %>% filter(Variable == "Catch"), aes(x = Year, y = P50, color = RuleType), lwd = 1.5) +
    geom_line(data = check1 %>% filter(Iteration == 1) %>% filter(Variable == "Catch"), aes(x = Year, y = Value)) +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    expand_limits(y = 0) +
    guides(color = FALSE, fill = FALSE) +
    facet_wrap(RuleType~Constraint) +
    theme_bw(base_size = 20)
  ggsave(file.path(figure_dir, "CompareConstraints_Catch.png"), p_constr, height = 10, width = 15)


  check1 <- cinfo %>%
    dplyr::filter(RuleNum %in% find_max$RuleNum) %>%
    dplyr::left_join(max_info %>% filter(Variable == "Catch")) %>%
    dplyr::select(Iteration, Year, Region, RuleNum, RuleType, Constraint, Catch, RelVB, RelSSB) %>%
    tidyr::pivot_longer(cols = c(Catch,RelVB,RelSSB), names_to = "Variable", values_to = "Value") %>%
    dplyr::filter(RuleType != "CPUE-based") %>%
    dplyr::filter(grepl("5%", Constraint) == FALSE)
  check2 <- check1 %>%
    dplyr::group_by(Year, Region, RuleType, Constraint, Variable) %>%
    dplyr::summarise(P5 = quantile(Value, 0.05),
                    P50 = quantile(Value, 0.50),
                    P95 = quantile(Value, 0.95))
  if(length(unique(check2$RuleType))==3) check2$RuleType <- factor(check2$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
  if(length(unique(check1$RuleType))==3) check1$RuleType <- factor(check1$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
  p_constr_fixed <- ggplot() +
    geom_ribbon(data = check2 %>% filter(Variable == "Catch"), aes(x = Year, ymin = P5, ymax = P95, fill = RuleType), color = NA, alpha = 0.5) +
    geom_line(data = check2 %>% filter(Variable == "Catch"), aes(x = Year, y = P50, color = RuleType), lwd = 1.5) +
    geom_line(data = check1 %>% filter(Iteration == 1) %>% filter(Variable == "Catch"), aes(x = Year, y = Value)) +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    guides(color = FALSE, fill = FALSE) +
    facet_grid(RuleType~Constraint) +
    theme_bw(base_size = 20)
  ggsave(file.path(figure_dir, "CompareConstraints_Fixed.png"), p_constr_fixed, height = 10, width = 15)


  check1 <- cinfo %>%
    dplyr::filter(RuleNum %in% find_max$RuleNum) %>%
    dplyr::left_join(max_info %>% filter(Variable == "Catch")) %>%
    dplyr::select(Iteration, Year, Region, RuleNum, RuleType, Constraint, Catch, RelVB, RelSSB) %>%
    tidyr::pivot_longer(cols = c(Catch,RelVB,RelSSB), names_to = "Variable", values_to = "Value") %>%
    dplyr::filter(RuleType != "FixedCatch") %>%
    dplyr::filter(grepl("5%", Constraint) == FALSE)
  check2 <- check1 %>%
    dplyr::group_by(Year, Region, RuleType, Constraint, Variable) %>%
    dplyr::summarise(P5 = quantile(Value, 0.05),
                    P50 = quantile(Value, 0.50),
                    P95 = quantile(Value, 0.95))
  if(length(unique(check2$RuleType))==3) check2$RuleType <- factor(check2$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
  if(length(unique(check1$RuleType))==3) check1$RuleType <- factor(check1$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
  p_constr_cpue <- ggplot() +
    geom_ribbon(data = check2 %>% filter(Variable == "Catch"), aes(x = Year, ymin = P5, ymax = P95, fill = RuleType), color = NA, alpha = 0.5) +
    geom_line(data = check2 %>% filter(Variable == "Catch"), aes(x = Year, y = P50, color = RuleType), lwd = 1.5) +
    geom_line(data = check1 %>% filter(Iteration == 1) %>% filter(Variable == "Catch"), aes(x = Year, y = Value)) +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    guides(color = FALSE, fill = FALSE) +
    facet_wrap(RuleType~Constraint, ncol = 3) +
    theme_bw(base_size = 20)
  ggsave(file.path(figure_dir, "CompareConstraints_MP.png"), p_constr_cpue, height = 10, width = 15)


  # check <- full_join(dinfo, pinfo) %>%
  #   tidyr::pivot_longer(cols = -c(Iteration, Year, Region, CatchResidual, RuleNum), names_to = "Variable", values_to = "Value") %>%
  #   dplyr::filter(Variable == "VB") %>%
  #   dplyr::group_by(Year, Region, RuleNum, Variable) %>%
  #   dplyr::summarise(P5 = quantile(Value, 0.05),
  #                    P50 = quantile(Value, 0.5),
  #                    P95 = quantile(Value, 0.95)) %>%
  #   filter(RuleNum %in% c(NA, find_msy$RuleNum))
  # check <- info %>% left_join(by = c("Region","RuleNum"), msy_info %>% filter(Variable == "VB") %>% rename(RefVB5 = P5, RefVB50 = P50, RefVB95 = P95) %>% dplyr::select(-Variable)) %>% filter(Variable == "VB")
  # p_vbproj <- ggplot(check) +
  #   geom_ribbon(aes(x = Year, ymin = RefVB5, ymax = RefVB95), fill = "forestgreen", alpha = 0.5) +
  #   geom_hline(aes(yintercept = RefVB50), color = "forestgreen") +
  #   geom_ribbon(aes(x = Year, ymin = P5, ymax = P95), alpha = 0.5) +
  #   geom_line(aes(x = Year, y = P50), lwd = 1.2) +
  #   geom_line(data = info %>% filter(Year <= cutyears[1]), aes(x = Year, y = P50), lwd = 1.2) +
  #   coord_cartesian(xlim = c(min(dinfo$Year), max(check$Year)), expand = FALSE) +
  #   ylab("Vulnerable biomass (VB)") +
  #   expand_limits(y = 0) +
  #   facet_grid(~RuleType) +
  #   theme_bw(base_size = 20)
  # ggsave(file.path(figure_dir, "VBproj.png"), p_vbproj, height = 10, width = 15)


  # check <- full_join(dinfo, pinfo) %>%
  #   tidyr::pivot_longer(cols = -c(Iteration, Year, Region, CatchResidual, RuleNum), names_to = "Variable", values_to = "Value") %>%
  #   dplyr::filter(Variable == "Catch") %>%
  #   dplyr::group_by(Year, Region, RuleNum, Variable) %>%
  #   dplyr::summarise(P5 = quantile(Value, 0.05),
  #                    P50 = quantile(Value, 0.5),
  #                    P95 = quantile(Value, 0.95)) %>%
  #   filter(RuleNum %in% c(NA, find_msy$RuleNum))
  # p_catch <- ggplot(check) +
  #   geom_ribbon(data = check %>% filter(Year <= cutyears[1]), aes(x = Year, ymin = P5, ymax = P95), alpha = 0.5) +
  #   geom_line(data = check %>% filter(Year <= cutyears[1]), aes(x = Year, y = P50), lwd = 1.2) +
  #   geom_ribbon(data = check %>% filter(Year >= cutyears[1]), aes(x = Year, ymin = P5, ymax = P95, fill = factor(RuleNum)), alpha = 0.5) +
  #   geom_line(data = check %>% filter(Year >= cutyears[1]), aes(x = Year, y = P50, color = factor(RuleNum)), lwd = 1.2) +
  #   coord_cartesian(xlim = c(min(check$Year), max(check$Year)), expand = FALSE) +
  #   ylab("Catch") +
  #   expand_limits(y = 0) +
  #   guides(color = FALSE, fill = FALSE) +
  #   theme_bw(base_size = 20)
  # ggsave(file.path(figure_dir, "Project_msy.png"), p_catch, height = 10, width = 15)


  # check <- pinfo %>%
  #   tidyr::pivot_longer(cols = -c(Iteration, Year, Region, CatchResidual, RuleNum), names_to = "Variable", values_to = "Value") %>%
  #   dplyr::filter(Variable == "Catch") %>%
  #   dplyr::group_by(Year, Region, RuleNum, Variable) %>%
  #   dplyr::summarise(P5 = quantile(Value, 0.05),
  #                    P50 = quantile(Value, 0.5),
  #                    P95 = quantile(Value, 0.95)) %>%
  #   filter(RuleNum %in% c(find_max$RuleNum))
  # p_catch <- ggplot(check) +
  #   geom_ribbon(data = check, aes(x = Year, ymin = P5, ymax = P95, fill = factor(RuleNum)), alpha = 0.5) +
  #   geom_line(data = check, aes(x = Year, y = P50, color = factor(RuleNum)), lwd = 1.2) +
  #   geom_line(data = pinfo %>% filter(Iteration == 2) %>% filter(RuleNum %in% find_max$RuleNum), aes(x = Year, y = Catch)) +
  #   coord_cartesian(xlim = c(min(check$Year), max(check$Year)), expand = FALSE) +
  #   ylab("Catch") +
  #   expand_limits(y = 0) +
  #   facet_wrap(~RuleNum, ncol = 1) +
  #   guides(color = FALSE, fill = FALSE) +
  #   theme_bw(base_size = 20)
  # ggsave(file.path(figure_dir, "Project_msy.png"), p_catch, height = 10, width = 15)

  
}
