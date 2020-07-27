#' Calculate reference points and create relevant figures
#'
#' @param object the ref-lsd object
#' @param object1 the lsd object with stock assessment MCMC posterior draws
#' @param figure_dir the directory to save figures to
#' @import dplyr
#' @import ggplot2
#' @import ggthemes
#' @export
#'
plot_refpoints <- function(object, object1, figure_dir){
  
  ##############################
  ## read model output
  ##############################

  ## from stock assessment
  mcmc1 <- object1@mcmc
  data1 <- object1@data
  years1 <- data1$first_yr:data1$last_yr
  pyears1 <- data1$first_yr:data1$last_proj_yr
  n_iter1 <- nrow(mcmc1[[1]])
  regions <- 1:data1$n_area
  seasons <- c("AW","SS")
  if(length(regions) > 1) regions2 <- c(regions, "Total")
  if(length(regions) == 1) regions2 <- regions
  sex <- c("Male","Immature female","Mature female")
  fleets <- c("SL","NSL")


  slcatch <- mcmc1$pred_catch_sl_jryt
  dimnames(slcatch) <- list("Iteration"=1:n_iter1, "RuleNum"=1:dim(slcatch)[2], "Region"=regions, "Year"=pyears1, "Season"=seasons)
  slcatch2 <- reshape2::melt(slcatch, value.name = "Catch") %>% 
    dplyr::group_by(Iteration, Year, Region, RuleNum) %>%
    dplyr::summarise(Catch = sum(Catch)) %>%
    dplyr::mutate("CatchType" = "SL")

  nslcatch <- mcmc1$pred_catch_nsl_jryt
  dimnames(nslcatch) <- list("Iteration"=1:n_iter1, "RuleNum"=1:dim(nslcatch)[2], "Region"=regions, "Year"=pyears1, "Season"=seasons)
  nslcatch2 <- reshape2::melt(nslcatch, value.name = "Catch") %>% 
    dplyr::group_by(Iteration, Year, Region, RuleNum) %>%
    dplyr::summarise(Catch = sum(Catch)) %>%
    dplyr::mutate("CatchType" = "NSL")    
  
  pcatch <- rbind.data.frame(slcatch2, nslcatch2) %>%
    tidyr::pivot_wider(names_from = CatchType, values_from = Catch) %>%
    dplyr::mutate(Catch = SL + NSL)

  rm(slcatch)
  rm(nslcatch)
  gc()

  catch <- pcatch
  gc()
  

  vb <- mcmc1$biomass_vulnref_AW_jyr
  dimnames(vb) <- list("Iteration" = 1:n_iter1, "RuleNum" = 1:dim(vb)[2], "Year" = pyears1, "Region" = regions)
  vb2 <- reshape2::melt(vb) %>% 
    dplyr::rename(VB = value)
  vb2$Region <- factor(vb2$Region)
  
  vb0now <- mcmc1$B0now_r
  dimnames(vb0now) <- list("Iteration" = 1:n_iter1, "Region" = regions2)
  vb0now <- reshape2::melt(vb0now) %>%
    dplyr::rename(VB0now = value)
  vb0now$Region <- factor(vb0now$Region)

  VB0 <- mcmc1$B0_r
  dimnames(VB0) <- list("Iteration" = 1:n_iter1, "Region" = regions2)
  VB0 <- reshape2::melt(VB0) %>%
    dplyr::rename(VB0 = value)
  VB0$Region <- factor(VB0$Region)
  
  ssb <- mcmc1$biomass_ssb_jyr
  dimnames(ssb) <- list("Iteration" = 1:n_iter1, "RuleNum" = 1:dim(ssb)[2], "Year" = pyears1, "Region" = regions)
  ssb2 <- reshape2::melt(ssb) %>% dplyr::rename("SSB"=value) 
  ssb2$Region <- factor(ssb2$Region)
  
  ssb0now <- mcmc1$SSB0now_r
  dimnames(ssb0now) <- list("Iteration" = 1:n_iter1, "Region" = regions2)
  ssb0now <- reshape2::melt(ssb0now) %>%
    dplyr::rename(SSB0now=value) 
  ssb0now$Region <- factor(ssb0now$Region)
  
  SSB0 <- mcmc1$SSB0_r
  dimnames(SSB0) <- list("Iteration" = 1:n_iter1, "Region" = regions2)
  SSB0 <- reshape2::melt(SSB0) %>%
    dplyr::rename(SSB0=value) 
  SSB0$Region <- factor(SSB0$Region)

  if(any(grepl("biomass_total_ytrs", names(mcmc1)))){
    tb <- mcmc1$biomass_total_ytrs
    dimnames(tb) <- list("Iteration" = 1:n_iter1, "Year" = pyears1, "Season" = seasons, "Region" = regions, "Sex" = c(sex, "Total"))
    tb2 <- reshape2::melt(tb) %>% 
      dplyr::rename("TB"=value) %>% 
      dplyr::filter(Sex == "Total") %>%
      dplyr::select(-Sex) %>% 
      dplyr::filter(Season == "AW") %>%
      dplyr::select(-Season) %>%
      dplyr::group_by(Iteration, Year, Region) %>%
      dplyr::summarise(TB = sum(TB))
  }
  if(any(grepl("biomass_total_jytrs", names(mcmc1)))){
    tb <- mcmc1$biomass_total_jytrs
    dimnames(tb) <- list("Iteration" = 1:n_iter1, "RuleNum" = 1:dim(ssb)[2], "Year" = pyears1, "Season" = seasons, "Region" = regions, "Sex" = c(sex, "Total"))
    tb2 <- reshape2::melt(tb) %>% 
      dplyr::rename("TB"=value) %>% 
      dplyr::filter(Sex == "Total") %>%
      dplyr::select(-Sex) %>% 
      dplyr::filter(Season == "AW") %>%
      dplyr::select(-Season) %>%
      dplyr::group_by(Iteration, RuleNum, Year, Region) %>%
      dplyr::summarise(TB = sum(TB))
  }
  tb2$Region <- factor(tb2$Region)

  tb0now <- mcmc1$Btot0now_r
  dimnames(tb0now) <- list("Iteration" = 1:n_iter1, "Region" = regions2)
  tb0now <- reshape2::melt(tb0now) %>%
    dplyr::rename(TB0now=value) 
  tb0now$Region <- factor(tb0now$Region)

  TB0 <- mcmc1$Btot0_r
  dimnames(TB0) <- list("Iteration" = 1:n_iter1, "Region" = regions2)
  TB0 <- reshape2::melt(TB0) %>%
    dplyr::rename(TB0=value) 
  TB0$Region <- factor(TB0$Region)

  rec <- mcmc1$recruits_ry
  dimnames(rec) <- list("Iteration" = 1:n_iter1, "Region" = regions, "Year" = pyears1)
  rec2 <- reshape2::melt(rec) %>%
    dplyr::rename(Recruitment = value)
  rec2$Region <- factor(rec2$Region)
  
  relssb <- inner_join(ssb2, ssb0now) %>%
    dplyr::full_join(SSB0) %>%
    dplyr::mutate(RelSSBnow = SSB/SSB0now,
                  RelSSB = SSB/SSB0)

  relvb <- inner_join(vb2, vb0now) %>%
    dplyr::full_join(VB0) %>%
    dplyr::mutate(RelVBnow = VB/VB0now,
                  RelVB = VB/VB0)

  reltb <- inner_join(tb2, tb0now) %>%
    dplyr::full_join(TB0) %>%
    dplyr::mutate(RelTBnow = TB/TB0now,
                  RelTB = TB/TB0)


  relb <- full_join(relssb, relvb)
  relb1 <- full_join(relb, reltb)
  relb2 <- full_join(relb1, rec2)

  catch$Region <- factor(catch$Region)
  info1 <- full_join(catch, relb2)
  info1 <- info1 %>%
      mutate(Region = paste0("Region ", Region))

  if(length(regions) > 1){
    tinfo1 <- info1 %>%
      dplyr::group_by(Iteration, Year, RuleNum) %>%
      dplyr::summarise(SL = sum(SL),
                       NSL = sum(NSL),
                       Catch = sum(Catch),
                       # CatchResidual = sum(CatchResidual),
                       SSB = sum(SSB),
                       SSB0now = sum(SSB0now),
                       SSB0 = sum(SSB0),
                       VB = sum(VB),
                       VB0now = sum(VB0now),
                       VB0 = sum(VB0),
                       TB = sum(TB),
                       TB0now = sum(TB0now),
                       TB0 = sum(TB0),
                       Recruitment = sum(Recruitment)) %>%
      dplyr::mutate(RelVBnow = VB / VB0now) %>%
      dplyr::mutate(RelSSBnow = SSB / SSB0now) %>%
      dplyr::mutate(RelTBnow = TB / TB0now) %>%
      dplyr::mutate(RelVB = VB / VB0) %>%
      dplyr::mutate(RelSSB = SSB / SSB0) %>%
      dplyr::mutate(RelTB = TB / TB0) %>%
      dplyr::mutate(Region = "Total")

    info1 <- rbind.data.frame(info1, tinfo1)
    info1 <- data.frame(info1)
  }

  status <- info1 %>%
    dplyr::filter(Year == max(years1)+1) %>%
    tidyr::pivot_longer(cols=c(SSB,SSB0now,SSB0,RelSSB,RelSSBnow,VB,VB0now,VB0,RelVB,RelVBnow,TB,TB0now,TB0,RelTB, RelTBnow), names_to = "Variable", values_to = "Value") %>%
    dplyr::group_by(Region, Variable) %>%
    dplyr::summarise(P5 = quantile(Value, 0.05),
                     P50 = quantile(Value, 0.5),
                     P95 = quantile(Value, 0.95))

  write.csv(status, file.path(figure_dir, "Current_status.csv"))
  


  ## from refpoints
  mcmc <- object@mcmc
  data <- object@data
  years <- data$first_yr:data$last_yr
  pyears <- data$first_yr:data$last_proj_yr
  projyears <- (data$first_proj_yr):data$last_proj_yr
  n_iter <- nrow(mcmc[[1]])
  regions <- 1:data$n_area
  seasons <- c("AW","SS")
  if(length(regions) > 1) regions2 <- c(regions, "Total")
  if(length(regions) == 1) regions2 <- regions
  sex <- c("Male","Immature female","Mature female")
  rules <- data$mp_rule_parameters
  n_rules <- nrow(rules)
  fleets <- c("SL","NSL")
  
  projF <- mcmc$proj_F_jytrf
  dimnames(projF) <- list("Iteration"=1:n_iter, "RuleNum"=1:n_rules, "Year"=pyears, "Season"=seasons, "Region"=regions, "Fleet" = fleets)
  projF2 <- reshape2::melt(projF, value.name = "F") %>%
    dplyr::group_by(Iteration, RuleNum, Year, Region) %>%
    dplyr::summarise(F = sum(F))

  # sub <- projF2 %>% filter(RuleNum %in% c(6,37))
  # p <- ggplot(sub %>% filter(Iteration == 1)) +
  # geom_line(aes(x = Year, y = F, color = factor(RuleNum))) +
  # # facet_wrap(~Fleet) +
  # # coord_cartesian(ylim = c(0,quantile(sub$F,0.99))) +
  # theme_bw()

  # qpar <- mcmc$par_q_cpue_qy
  # dimnames(qpar) <- list("Iteration" = 1:n_iter, "qtype" = 1:3, "Year" = pyears)
  # qpar2 <- reshape2::melt(qpar, value.name = "q")

  # p <- ggplot(qpar2) +
  # geom_line(aes(x = Year, y = q)) +
  # facet_wrap(~qtype) +
  # theme_bw()
  
  cpue <- mcmc$mp_offset_cpue_jry
  dimnames(cpue) <- list("Iteration" = 1:n_iter, "RuleNum" = 1:n_rules, "Region" = regions, "Year" = pyears)
  cpue2 <- reshape2::melt(cpue, value.name = "CPUE")
  cpue2 <- tibble(cpue2)
  
  gc()
  
  slcatch <- mcmc$pred_catch_sl_jryt
  dimnames(slcatch) <- list("Iteration"=1:n_iter, "RuleNum"=1:dim(slcatch)[2], "Region"=regions, "Year"=pyears, "Season"=seasons)
  slcatch2 <- reshape2::melt(slcatch, value.name = "Catch") %>% 
    dplyr::group_by(Iteration, Year, Region, RuleNum) %>%
    dplyr::summarise(Catch = sum(Catch)) %>%
    dplyr::mutate("CatchType" = "SL")

  nslcatch <- mcmc$pred_catch_nsl_jryt
  dimnames(nslcatch) <- list("Iteration"=1:n_iter, "RuleNum"=1:dim(nslcatch)[2], "Region"=regions, "Year"=pyears, "Season"=seasons)
  nslcatch2 <- reshape2::melt(nslcatch, value.name = "Catch") %>% 
    dplyr::group_by(Iteration, Year, Region, RuleNum) %>%
    dplyr::summarise(Catch = sum(Catch)) %>%
    dplyr::mutate("CatchType" = "NSL")    
  
  pcatch <- rbind.data.frame(slcatch2, nslcatch2) %>%
    tidyr::pivot_wider(names_from = CatchType, values_from = Catch) %>%
    dplyr::mutate(Catch = SL + NSL)

  # sub <- pcatch %>% filter(RuleNum %in% c(6,37))
  # p <- ggplot(sub %>% filter(Iteration == 1)) +
  # geom_line(aes(x = Year, y = Catch, color = factor(RuleNum))) +
  # # facet_wrap(~Fleet) +
  # theme_bw()
  
  rm(slcatch)
  rm(nslcatch)
  gc()

  catch <- pcatch
  gc()
  

  vb <- mcmc$biomass_vulnref_AW_jyr
  dimnames(vb) <- list("Iteration" = 1:n_iter, "RuleNum" = 1:dim(vb)[2], "Year" = pyears, "Region" = regions)
  vb2 <- reshape2::melt(vb) %>% 
    dplyr::rename(VB = value)
  vb2$Region <- factor(vb2$Region)
  
  vb0now <- mcmc$B0now_r
  dimnames(vb0now) <- list("Iteration" = 1:n_iter, "Region" = regions2)
  vb0now <- reshape2::melt(vb0now) %>%
    dplyr::rename(VB0now = value)
  vb0now$Region <- factor(vb0now$Region)

  VB0 <- mcmc$B0_r
  dimnames(VB0) <- list("Iteration" = 1:n_iter, "Region" = regions2)
  VB0 <- reshape2::melt(VB0) %>%
    dplyr::rename(VB0 = value)
  VB0$Region <- factor(VB0$Region)
  
  ssb <- mcmc$biomass_ssb_jyr
  dimnames(ssb) <- list("Iteration" = 1:n_iter, "RuleNum" = 1:dim(ssb)[2], "Year" = pyears, "Region" = regions)
  ssb2 <- reshape2::melt(ssb) %>% dplyr::rename("SSB"=value) 
  ssb2$Region <- factor(ssb2$Region)
  
  ssb0now <- mcmc$SSB0now_r
  dimnames(ssb0now) <- list("Iteration" = 1:n_iter, "Region" = regions2)
  ssb0now <- reshape2::melt(ssb0now) %>%
    dplyr::rename(SSB0now=value) 
  ssb0now$Region <- factor(ssb0now$Region)
  
  SSB0 <- mcmc$SSB0_r
  dimnames(SSB0) <- list("Iteration" = 1:n_iter, "Region" = regions2)
  SSB0 <- reshape2::melt(SSB0) %>%
    dplyr::rename(SSB0=value) 
  SSB0$Region <- factor(SSB0$Region)

  tb <- mcmc$biomass_total_jytrs
  dimnames(tb) <- list("Iteration" = 1:n_iter, "RuleNum" = 1:dim(ssb)[2], "Year" = pyears, "Season" = seasons, "Region" = regions, "Sex" = c(sex, "Total"))
  tb2 <- reshape2::melt(tb) %>% 
    dplyr::rename("TB"=value) %>% 
    dplyr::filter(Sex == "Total") %>%
    dplyr::select(-Sex) %>% 
    dplyr::filter(Season == "AW") %>%
    dplyr::select(-Season) %>%
    dplyr::group_by(Iteration, RuleNum, Year, Region) %>%
    dplyr::summarise(TB = sum(TB))
  tb2$Region <- factor(tb2$Region)

  tb0now <- mcmc$Btot0now_r
  dimnames(tb0now) <- list("Iteration" = 1:n_iter, "Region" = regions2)
  tb0now <- reshape2::melt(tb0now) %>%
    dplyr::rename(TB0now=value) 
  tb0now$Region <- factor(tb0now$Region)

  TB0 <- mcmc$Btot0_r
  dimnames(TB0) <- list("Iteration" = 1:n_iter, "Region" = regions2)
  TB0 <- reshape2::melt(TB0) %>%
    dplyr::rename(TB0=value) 
  TB0$Region <- factor(TB0$Region)

  rec <- mcmc$recruits_ry
  dimnames(rec) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = pyears)
  rec2 <- reshape2::melt(rec) %>%
    dplyr::rename(Recruitment = value)
  rec2$Region <- factor(rec2$Region)
  
  relssb <- inner_join(ssb2, ssb0now) %>%
    dplyr::full_join(SSB0) %>%
    dplyr::mutate(RelSSBnow = SSB/SSB0now,
                  RelSSB = SSB/SSB0)

  relvb <- inner_join(vb2, vb0now) %>%
    dplyr::full_join(VB0) %>%
    dplyr::mutate(RelVBnow = VB/VB0now,
                  RelVB = VB/VB0)

  reltb <- inner_join(tb2, tb0now) %>%
    dplyr::full_join(TB0) %>%
    dplyr::mutate(RelTBnow = TB/TB0now,
                  RelTB = TB/TB0)


  relb <- full_join(relssb, relvb)
  relb1 <- full_join(relb, reltb)
  relb2 <- full_join(relb1, rec2)

  catch$Region <- factor(catch$Region)
  info <- full_join(catch, relb2)
  cpue2$Region <- factor(cpue2$Region)
  projF2$Region <- factor(projF2$Region)
  info <- full_join(info, cpue2)
  info <- full_join(info, projF2)
  info <- info %>%
      mutate(Region = paste0("Region ", Region))

  if(length(regions) > 1){
    tinfo <- info %>%
      dplyr::group_by(Iteration, Year, RuleNum) %>%
      dplyr::summarise(SL = sum(SL),
                       NSL = sum(NSL),
                       Catch = sum(Catch),
                       # CatchResidual = sum(CatchResidual),
                       SSB = sum(SSB),
                       SSB0now = sum(SSB0now),
                       SSB0 = sum(SSB0),
                       VB = sum(VB),
                       VB0now = sum(VB0now),
                       VB0 = sum(VB0),
                       TB = sum(TB),
                       TB0now = sum(TB0now),
                       TB0 = sum(TB0),
                       Recruitment = sum(Recruitment), 
                       CPUE = mean(CPUE),
                       F = sum(F)) %>%
      dplyr::mutate(RelVB = VB / VB0) %>%
      dplyr::mutate(RelSSB = SSB / SSB0) %>%
      dplyr::mutate(RelTB = TB / TB0) %>%
      dplyr::mutate(RelVBnow = VB / VB0now) %>%
      dplyr::mutate(RelSSBnow = SSB / SSB0now) %>%
      dplyr::mutate(RelTBnow = TB / TB0now) %>%
      dplyr::mutate(Region = "Total")

    info <- rbind.data.frame(info, tinfo)
    info <- data.frame(info)
  }
  
  rm(relb)
  rm(relb2)
  rm(catch)
  gc()
  
  
  rulepar <- paste0("par",1:10)
  colnames(rules) <- rulepar
  ruledf <- data.frame(RuleNum = 1:nrow(rules), rules) %>%
    mutate(RuleType = ifelse(par1 == 0, "FixedF", ifelse(par1 == 1, "FixedCatch", "CPUE-based")))
  
  ##########################
  ## projection time series
  ##########################
    cutyears <- (projyears[1]+10):(data$last_proj_yr)
    pinfo <- info %>% filter(Year %in% (max(years)+1):(min(cutyears)-1))
    cinfo <- info %>% dplyr::filter(Year %in% cutyears)
    dinfo <- info1 %>% ungroup() %>% dplyr::filter(Year %in% years) %>% dplyr::filter(RuleNum == 1) %>% dplyr::select(-c(RuleNum))

  #######################################
  ## risk constraints + flags for catch
  #######################################
    constraints <- cinfo %>%
      dplyr::left_join(ruledf) %>%
      dplyr::filter(Region != "Total") %>%
      dplyr::group_by(Region, RuleNum) %>%
      dplyr::summarise(CV = sd(Catch)/mean(Catch),
                      Prisk = length(which(RelSSBnow <= 0.2))/length(RelSSBnow),
                      RiskConstraint = ifelse(Prisk >= 0.05, 1, 0),
                      ExpectedCatchSL = sum(par2),
                      ObsCatchSL = sum(SL),
                      AvgTotalCatch = sum(Catch)/max(Iteration)) %>%
      dplyr::left_join(ruledf) %>%
      # dplyr::mutate(ExpectedCatchSL = replace(ExpectedCatchSL, RuleType != "FixedCatch", 0)) %>%
      dplyr::mutate(CatchConstraint = ifelse(RuleType == "FixedCatch" & ObsCatchSL < 0.999 * ExpectedCatchSL, 1, 0))
    # constraints <- unique(constraints)

    
  #####################
  ## start summarising
  #####################
    summary <- cinfo %>%
      dplyr::filter(Region != "Total") %>%
      tidyr::pivot_longer(cols=c(SL, NSL, Catch,SSB,SSB0now,SSB0,RelSSB,RelSSBnow,VB,VB0now,VB0,RelVB,RelVBnow,TB,TB0,TB0now, RelTB, RelTBnow,Recruitment,CPUE,F), names_to = "Variable", values_to = "Value") %>%
      dplyr::group_by(Region, RuleNum, Variable) %>%
      dplyr::summarise(P5 = quantile(Value, 0.05),
                       P50 = quantile(Value, 0.5),
                       P95 = quantile(Value, 0.95))
    
    output <- full_join(constraints, summary)
    
    ## identify and label fixed catch rules
    output2 <- output %>% 
      left_join(ruledf) #%>%  
      # mutate(CatchConstraint = replace(CatchConstraint, which(RuleType != "FixedCatch"), 0))  

    output2$Constraint <- sapply(1:nrow(output2), function(x){
      if(output2$CatchConstraint[x] == 1 & output2$RiskConstraint[x] == 1) out <- "Risk + Catch"
      if(output2$CatchConstraint[x] == 1 & output2$RiskConstraint[x] == 0) out <- "Catch"
      if(output2$CatchConstraint[x] == 0 & output2$RiskConstraint[x] == 1) out <- "Risk" #output2$CatchConstraint[x] == 0 & 
      if(output2$CatchConstraint[x] == 0 & output2$RiskConstraint[x] == 0) out <- "Pass"
      return(out)
    })
    output2$Constraint <- factor(output2$Constraint, levels = c("Pass", "Catch", "Risk", "Risk + Catch"))
      
    find_max1 <- output2 %>%
      dplyr::filter(Variable == "Catch") %>%
      dplyr::group_by(Region, RuleType, RuleNum, Constraint) %>%
      dplyr::summarise(P50 = sum(P50)) %>%
      dplyr::group_by(Region, RuleType, Constraint) %>%
      dplyr::filter(P50 == max(P50))  

    
    find_msy1 <- find_max1 %>% filter(Constraint == "Pass")

    msy_info1 <- output2 %>% right_join(find_msy1 %>% dplyr::select(-P50))

  ## filter MP rules where CV < CV(MSY Fixed F)
  CVmaxF <- min(as.numeric(unlist(msy_info1[which(msy_info1$RuleType == "FixedF"),"CV"])))
  # CVx <- unique(as.numeric(unlist(msy_info1[which(msy_info1$RuleType == "CPUE-based"), "CV"])))
  output_mp <- output2 %>% filter(RuleType == "CPUE-based") %>% filter(RiskConstraint == 0)
  CVquants <- quantile(output_mp$CV)

  output2$CVConstraint = 0
  output2 <- output2 %>% 
    mutate(CVConstraint = replace(CVConstraint, which(CV <= CVquants["100%"]), "Max")) %>%
    mutate(CVConstraint = replace(CVConstraint, which(CV <= CVquants["75%"]), "75%")) %>%
    mutate(CVConstraint = replace(CVConstraint, which(CV <= CVquants["50%"]), "Median")) %>%
    mutate(CVConstraint = replace(CVConstraint, which(CV <= CVquants["25%"]), "25%")) %>%
    mutate(CVConstraint = replace(CVConstraint, which(CV <= CVquants["0%"]), "Min")) %>%
    mutate(CVConstraint = replace(CVConstraint, which(RuleType != "CPUE-based"), 0))

  output2$Constraint <- sapply(1:nrow(output2), function(x){
      if(output2$CatchConstraint[x] == 1 & output2$RiskConstraint[x] == 1) out <- "Risk + Catch"
      if(output2$CatchConstraint[x] == 1 & output2$RiskConstraint[x] == 0) out <- "Catch"
      if(output2$CatchConstraint[x] == 0 & output2$RiskConstraint[x] == 1) out <- "Risk" #output2$CatchConstraint[x] == 0 & 
      if(output2$CatchConstraint[x] == 0 & output2$RiskConstraint[x] == 0) out <- "Pass"
    return(out)
  })
  write.csv(output2, file.path(figure_dir, "Summary_byVariable.csv"))
  
  find_max <- output2 %>%
    dplyr::filter(Variable == "Catch") %>%
    dplyr::group_by(Region, RuleType, RuleNum, Constraint, CVConstraint) %>%
    dplyr::summarise(P50 = sum(P50)) %>%
    dplyr::group_by(Region, RuleType, Constraint, CVConstraint) %>%
    dplyr::filter(P50 == max(P50))


  ## remove duplicates of MSY --- choose rule with higher average total catch
  max_info <- output2 %>% right_join(find_max %>% dplyr::select(-P50))
  write.csv(max_info, file.path(figure_dir, "MAX_info.csv"))



  if(length(regions) > 1){
    max_info2 <- cinfo %>%
      dplyr::right_join(max_info %>% dplyr::select(Region, RuleNum, Constraint)) %>%
      dplyr::left_join(ruledf)
    max_info3 <- data.frame(unique(max_info2)) %>%
      dplyr::group_by(Iteration, Year, RuleType, Constraint) %>%
      dplyr::summarise(Catch = sum(Catch),
                       VB = sum(VB),
                       SSB = sum(SSB)) %>%
      dplyr::left_join(vb0now %>% dplyr::filter(Region == "Total")) %>%
      dplyr::left_join(ssb0now %>% dplyr::filter(Region == "Total")) %>%
      dplyr::mutate(RelVB = VB / VB0now) %>%
      dplyr::mutate(RelSSB = SSB / SSB0now) %>%
      tidyr::drop_na()  %>%
      tidyr::pivot_longer(cols = c(Catch, VB, SSB, VB0now, SSB0now, RelVB, RelSSB), names_to = "Variable", values_to = "Value") %>%
      dplyr::group_by(RuleType, Constraint, Variable) %>%
      dplyr::summarise(P5 = quantile(Value, 0.05),
                       P50 = quantile(Value, 0.5),
                       P95 = quantile(Value, 0.95)) %>%
      dplyr::mutate(Region = "Total")
    write.csv(max_info3, file.path(figure_dir, "MAX_info_total.csv"))
  }
 
  #####################################################
  ## MSY info
  find_msy <- find_max %>% filter(Constraint == "Pass")
  ruletypes <- unique(find_msy$RuleType)
  check <- lapply(1:length(regions), function(x){
    check2 <- lapply(1:length(ruletypes), function(y){
      sub <- find_msy %>% filter(Region == paste0("Region ", regions[x])) %>% filter(RuleType == ruletypes[y])
      if(ruletypes[y] == "CPUE-based"){
        if(nrow(sub) == 5) return(sub)
        if(nrow(sub) > 5){
          stop("Multiple rules are associated with the max catch by CV constraint. Need to write code to find the max average total catch by CV constraint.")
        }
      }
      if(nrow(sub) == 1) return(sub)
      if(nrow(sub) > 1){
        nums <- unique(sub$RuleNum)
        byNum <- lapply(1:length(nums), function(z){
          info <- output2 %>% filter(Region == paste0("Region ", regions[x])) %>% filter(RuleNum == nums[z])
          tcatch <- unique(info$AvgTotalCatch)
          return(tcatch)
        })
        byNum <- unlist(byNum)
        sub2 <- sub %>% filter(RuleNum == nums[which(byNum == max(byNum))])
        return(sub2)
      }
    })
    byType <- do.call(rbind, check2)
  })
  find_msy <- do.call(rbind, check)
  
  msy_info <- output2 %>% right_join(find_msy %>% dplyr::select(-P50))
  write.csv(msy_info, file.path(figure_dir, "MSY_info.csv"))

  msy_info2 <- msy_info %>% 
    dplyr::select(-c(P5,P95)) %>%
    tidyr::pivot_wider(names_from = Variable, values_from = P50)

    msy_info3 <- cinfo %>%
      dplyr::right_join(msy_info %>% dplyr::select(Region, RuleNum, Constraint)) %>%
      dplyr::left_join(ruledf)
  if(length(regions) > 1){  
    msy_info4 <- data.frame(unique(msy_info3)) %>%
      dplyr::group_by(Iteration, Year, RuleType, Constraint) %>%
      dplyr::summarise(Catch = sum(Catch),
                       VB = sum(VB),
                       SSB = sum(SSB)) %>%
      dplyr::left_join(vb0now %>% dplyr::filter(Region == "Total")) %>%
      dplyr::left_join(ssb0now %>% dplyr::filter(Region == "Total")) %>%
      dplyr::mutate(RelVB = VB / VB0now) %>%
      dplyr::mutate(RelSSB = SSB / SSB0now) %>%
      tidyr::drop_na()  %>%
      tidyr::pivot_longer(cols = c(Catch, VB, SSB, VB0now, SSB0now, RelVB, RelSSB), names_to = "Variable", values_to = "Value") %>%
      dplyr::group_by(RuleType, Constraint, Variable) %>%
      dplyr::summarise(P5 = quantile(Value, 0.05),
                       P50 = quantile(Value, 0.5),
                       P95 = quantile(Value, 0.95)) %>%
      dplyr::mutate(Region = "Total")
    write.csv(msy_info4, file.path(figure_dir, "MSY_info_total.csv"))
    msy_info <- full_join(msy_info, msy_info4)
  }

  
  #################################
  ## summaries for plotting
  ################################
  output3 <- output2 %>% 
    tidyr::pivot_longer(cols = P5:P95, names_to = "Percentile", values_to = "Value") %>%
    tidyr::pivot_wider(names_from = Variable, values_from = Value)
  
  # write.csv(output3, file.path(figure_dir, "Summary_byPercentile.csv"))
  
  output4 <- output3 %>%
    tidyr::pivot_wider(names_from = Percentile, values_from = Catch:VB0now)
  if(length(unique(output4$RuleType))==3) output4$RuleType <- factor(output4$RuleType, levels = c("FixedCatch","CPUE-based","FixedF"))
  const <- unique(as.character(output4$Constraint))
  const1 <- const[grepl("CV",const)==FALSE]
  const1_1 <- const1[grepl("Catch", const1) == FALSE]
  const1_2 <- const1[grepl("Catch", const1)]
  const1 <- c(const1_1, const1_2)
  const2 <- const[grepl("CV", const)]
  constx <- c(const1,const2)
  const <- c(constx, const[which(const %in% constx == FALSE)])
  output4$Constraint <- factor(output4$Constraint, levels = const)

  output5 <- output4 %>% right_join(find_msy %>% dplyr::select(-P50))
  
  ###############################
  ## all rules tested, compared
  ###############################
    require(ggthemes)

    check <- cinfo %>%
      dplyr::select(Iteration, Year, Region, RuleNum, Catch, CPUE, F, VB) %>%
      tidyr::pivot_longer(cols = c(Catch, CPUE, F, VB), names_to = "Variable", values_to = "Value") %>%
      dplyr::right_join(msy_info %>% dplyr::select(-P50)) %>%
      dplyr::filter(Variable %in% c("Catch", "CPUE", "F", "VB")) %>%
      dplyr::filter(Region != "Total")  %>%
      dplyr::mutate(CVConstraint = replace(CVConstraint, RuleType == "FixedF", "FixedF")) %>%
      dplyr::mutate(CVConstraint = replace(CVConstraint, RuleType == "FixedCatch", "FixedCatch")) %>%
      dplyr::filter(CVConstraint != "Min")
      # dplyr::mutate(CVConstraint = replace(CVConstraint, RuleType == "FixedF", "FixedF")) %>%
      # dplyr::mutate(CVConstraint = replace(CVConstraint, RuleType == "FixedCatch", "FixedCatch"))
    if(length(unique(check$RuleType))==3) check$RuleType <- factor(check$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
    check$CVConstraint <- factor(check$CVConstraint, levels = rev(c("FixedF", "Max", "75%", "Median", "25%", "FixedCatch")))
    p_f_cpue <- ggplot(check %>% filter(Iteration == 1)) +
      geom_line(aes(x = Year, Value, color = CVConstraint), lwd = 1.2) +
      scale_color_brewer(palette = "Spectral") +
      guides(color = guide_legend(title="Rule type\nor CV constraint")) +
      expand_limits(y = 0) +
      theme_bw(base_size = 20)
    if(length(regions) > 1){
      p_f_cpue <- p_f_cpue + facet_wrap(Region~Variable, scales = "free_y", ncol = 4) 
    } else {
      p_f_cpue <- p_f_cpue + facet_wrap(~Variable, scales = "free_y")
    }
    ggsave(file.path(figure_dir, "F_CPUE_Catch_VB_iter1.png"), p_f_cpue, height = 8, width = 15)

    check <- cinfo %>%
      dplyr::select(Iteration, Year, Region, RuleNum, Catch, CPUE, F, VB) %>%
      tidyr::pivot_longer(cols = c(Catch, CPUE, F, VB), names_to = "Variable", values_to = "Value") %>%
      dplyr::right_join(msy_info %>% dplyr::select(-P50)) %>%
      dplyr::filter(Variable %in% c("Catch", "CPUE", "F", "VB")) %>%
      dplyr::filter(Region != "Total") %>%
      dplyr::mutate(CVConstraint = replace(CVConstraint, RuleType == "FixedF", "FixedF")) %>%
      dplyr::mutate(CVConstraint = replace(CVConstraint, RuleType == "FixedCatch", "FixedCatch")) %>%
      dplyr::filter(CVConstraint != "Min")
    check2 <- output2 %>%
      dplyr::right_join(msy_info %>% dplyr::select(-P50)) %>%
      dplyr::filter(Variable %in% c("Catch", "CPUE", "F", "VB")) %>%
      dplyr::filter(Region != "Total") %>%
      dplyr::mutate(CVConstraint = replace(CVConstraint, RuleType == "FixedF", "FixedF")) %>%
      dplyr::mutate(CVConstraint = replace(CVConstraint, RuleType == "FixedCatch", "FixedCatch")) %>%
      dplyr::filter(CVConstraint != "Min")
    # if(length(unique(check2$RuleType))==3) check2$RuleType <- factor(check2$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
    check$CVConstraint <- factor(check$CVConstraint, levels = c("FixedCatch", "25%", "Median", "75%", "Max", "FixedF"))
    check2$CVConstraint <- factor(check2$CVConstraint, levels = c("FixedCatch", "25%", "Median", "75%", "Max", "FixedF"))
    p_f_cpue_v2 <- ggplot(check) +
      stat_summary(aes(x = Year, y = Value, fill = CVConstraint), fun.min = function(x) stats::quantile(x, 0.05), fun.max = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25) +
      stat_summary(aes(x = Year, y = Value, fill = CVConstraint), fun.min = function(x) stats::quantile(x, 0.25), fun.max = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5) +
      stat_summary(aes(x = Year, y = Value, color = CVConstraint), fun = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1.5) +
      geom_hline(data = check2, aes(yintercept = P50), lty = 2, lwd = 1.2) +
      scale_color_viridis_d() +
      scale_fill_viridis_d() +
      guides(color = FALSE, fill = FALSE) +
      expand_limits(y = 0) +
      theme_bw(base_size = 20)
    if(length(regions) > 1){
      p_f_cpue_v2 <- p_f_cpue_v2 + facet_grid(Variable~RuleType+Region, scales = "free_y")
    } else {
      p_f_cpue_v2 <- p_f_cpue_v2 + facet_grid(Variable~CVConstraint, scales = "free_y")
    }
  ggsave(file.path(figure_dir, "F_CPUE_Catch_VB_intervals.png"), p_f_cpue_v2, height = 8, width = 20)

# #   ## plots of rules over time
#   info2 <- info %>%
#     dplyr::select(Iteration, Region, RuleNum, Year, Catch, RelSSB, RelVB, Recruitment) %>%
#     tidyr::pivot_longer(-c(Iteration, Region, RuleNum, Year), names_to = "Variable", values_to = "Value") %>%
#     dplyr::group_by(Year, Region, RuleNum, Variable) %>%
#     dplyr::summarise(P5 = quantile(Value, 0.05),
#                      P50 = quantile(Value, 0.50),
#                      P95 = quantile(Value, 0.95))

# # 
#   check <- info2 %>% filter(Variable %in% c("Catch","RelSSB", "RelVB")) %>%
#     left_join(ruledf)
#   plot_msy <- check %>% filter(RuleNum %in% find_msy$RuleNum)
#   p_time <- ggplot(check) +
#     geom_line(aes(x = Year, y = P50, col = factor(RuleNum)), alpha = 0.5) +
#     geom_line(data = plot_msy, aes(x = Year, y = P50), lwd = 2) +
#     guides(color = FALSE) +
#     ylab("Median value by rule across iterations") +
#     theme_bw(base_size = 20)
#   if(length(regions)==1){
#     p_time <- p_time + facet_wrap(RuleType~Variable, scales = "free_y", nrow = 3)
#   } else {
#     p_time <- p_time + facet_wrap(Variable~Region + RuleType, scales = "free_y", nrow = 3)
#   }
#   ggsave(file.path(figure_dir, "Projections.png"), p_time, height = 8, width = 15)

  # check <- info2 %>% filter(Variable == "Recruitment") %>%
  #   left_join(ruledf)
  # p_rec <- ggplot(check) +
  #   geom_ribbon(aes(x = Year, ymin = P5, ymax = P95), alpha = 0.5) +
  #   geom_line(aes(x = Year, y = P50)) + 
  #   facet_wrap(Region~RuleType, scales = "free_y", nrow = 3) +
  #   theme_bw(base_size = 20)
  # ggsave(file.path(figure_dir, "Recruitment_check.png"), p_rec, height = 8, width = 15)
  
  ## plotF
  # projF2$Region <- factor(projF2$Region)
  # plot_F1 <- projF2 %>% 
  #     dplyr::filter(Year <= 2019)
  # plot_F2 <- projF2 %>% 
  #     dplyr::filter(Year > 2019) %>% 
  #     dplyr::filter(RuleNum %in% find_msy$RuleNum)
  # plot_F <- rbind.data.frame(plot_F1, plot_F2) %>%
  #   dplyr::left_join(ruledf)
  # p <- ggplot(plot_F) +
  # geom_line(aes(x = Year, y = F)) +
  # facet_grid(RuleType ~ Fleet) + 
  # theme_bw()



  p_cv <- ggplot(output4) +
    geom_segment(aes(x = CV, xend = CV, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_point(aes(x = CV, y = Catch_P50, fill = Constraint), pch = 21, cex = 4) +
    expand_limits(y = 0, x = 0) +
    xlab("CV of catch over time and iteration") + ylab("Average annual catch") +
    scale_fill_tableau() +
    scale_color_tableau() +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    p_cv <- p_cv + facet_grid(Region~RuleType, scales = "free_x") 
  } else {
    p_cv <- p_cv + facet_grid(~RuleType, scales = "free_x")
  }
  ggsave(file.path(figure_dir, "CV_vs_Catch.png"), p_cv, height = 8, width = 20)

  p_relssb <- ggplot(output4) +
    geom_segment(aes(x = RelSSB_P5, xend = RelSSB_P95, y = Catch_P50, yend = Catch_P50, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_segment(aes(x = RelSSB_P50, xend = RelSSB_P50, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_point(aes(x = RelSSB_P50, y = Catch_P50, fill = Constraint), pch = 21, cex = 4) +
    expand_limits(y = 0, x = 0) +
    xlab("Relative spawning biomass (SSB/SSB0now)") + ylab("Average annual catch") +
    scale_fill_tableau() +
    scale_color_tableau() +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    p_relssb <- p_relssb + facet_grid(Region~RuleType, scales = "free_x") 
  } else {
    p_relssb <- p_relssb + facet_grid(~RuleType)
  }
  ggsave(file.path(figure_dir, "RelSSB_vs_Catch_byConstraint.png"), p_relssb, height = 8, width = 20)
  
  p_relvb <- ggplot(output4) +
    geom_segment(aes(x = RelVB_P5, xend = RelVB_P95, y = Catch_P50, yend = Catch_P50, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_segment(aes(x = RelVB_P50, xend = RelVB_P50, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_point(aes(x = RelVB_P50, y = Catch_P50, fill = Constraint), pch = 21, cex = 3) +
    expand_limits(y = 0, x = 0) +
    xlab("Relative vulnerable biomass (VB/VB0now)") + ylab("Average annual catch") +
    scale_fill_tableau() +
    scale_color_tableau() +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    p_relvb <- p_relvb + facet_grid(Region~RuleType) 
  } else {
    p_relvb <- p_relvb + facet_grid(~RuleType)
  }
  ggsave(file.path(figure_dir, "RelVB_vs_Catch_byConstraint.png"), p_relvb, height = 8, width = 20)
  
  output5$RuleType <- factor(output5$RuleType, levels = levels(output4$RuleType))
  p_relvb_v2 <- p_relvb +
    geom_vline(data = output5, aes(xintercept = RelVB_P50), linetype = 2, lwd = 1.5) +
    geom_hline(data = output5, aes(yintercept = Catch_P50), linetype = 2, lwd = 1.5)
  ggsave(file.path(figure_dir, "RelVB_vs_Catch_byConstraint_wTarget.png"), p_relvb_v2, height = 8, width = 20)
  
  p_vb <- ggplot(output4) +
    geom_segment(aes(x = VB_P5, xend = VB_P95, y = Catch_P50, yend = Catch_P50, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_segment(aes(x = VB_P50, xend = VB_P50, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_point(aes(x = VB_P50, y = Catch_P50, fill = Constraint), pch = 21, cex = 3) +
    expand_limits(y = 0, x = 0) +
    xlab("Vulnerable biomass") + ylab("Average annual catch") +
    scale_fill_tableau() +
    scale_color_tableau() +
    theme_bw(base_size = 20) 
  if(length(regions) > 1){
    p_vb <- p_vb + facet_wrap(Region~RuleType, scales = "free_x", nrow = length(regions)) 
  } else {
    p_vb <- p_vb + facet_wrap(~RuleType, scales = "free_x", nrow = length(regions))
  } 
  ggsave(file.path(figure_dir, "VB_vs_Catch_byConstraint.png"), p_vb, height = 8, width = 20)
  
  output5$RuleType <- factor(output5$RuleType, levels = levels(output4$RuleType))
  p_vb_v2 <- p_vb +
    geom_vline(data = output5, aes(xintercept = VB_P50), linetype = 2, lwd = 1.5) +
    geom_hline(data = output5, aes(yintercept = Catch_P50), linetype = 2, lwd = 1.5)
  ggsave(file.path(figure_dir, "VB_vs_Catch_byConstraint_wTarget.png"), p_vb_v2, height = 8, width = 20)
  

  p_reltb <- ggplot(output4) +
    geom_segment(aes(x = RelTB_P5, xend = RelTB_P95, y = Catch_P50, yend = Catch_P50, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_segment(aes(x = RelTB_P50, xend = RelTB_P50, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_point(aes(x = RelTB_P50, y = Catch_P50, fill = Constraint), pch = 21, cex = 3) +
    expand_limits(y = 0, x = 0) +
    xlab("Relative total biomass (TB/TB0now)") + ylab("Average annual catch") +
    scale_fill_tableau() +
    scale_color_tableau() +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    p_reltb <- p_reltb + facet_grid(Region~RuleType) 
  } else {
    p_reltb <- p_reltb + facet_grid(~RuleType)
  }
  ggsave(file.path(figure_dir, "RelTB_vs_Catch_byConstraint.png"), p_reltb, height = 8, width = 20)
  
  output5$RuleType <- factor(output5$RuleType, levels = levels(output4$RuleType))
  p_reltb_v2 <- p_reltb +
    geom_vline(data = output5, aes(xintercept = RelTB_P50), linetype = 2, lwd = 1.5) +
    geom_hline(data = output5, aes(yintercept = Catch_P50), linetype = 2, lwd = 1.5)
  ggsave(file.path(figure_dir, "RelTB_vs_Catch_byConstraint_wTarget.png"), p_reltb_v2, height = 8, width = 20)
  
  p_tb <- ggplot(output4) +
    geom_segment(aes(x = TB_P5, xend = TB_P95, y = Catch_P50, yend = Catch_P50, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_segment(aes(x = TB_P50, xend = TB_P50, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
    geom_point(aes(x = TB_P50, y = Catch_P50, fill = Constraint), pch = 21, cex = 3) +
    expand_limits(y = 0, x = 0) +
    xlab("Total biomass") + ylab("Average annual catch") +
    scale_fill_tableau() +
    scale_color_tableau() +
    theme_bw(base_size = 20) 
  if(length(regions) > 1){
    p_tb <- p_tb + facet_wrap(Region~RuleType, scales = "free_x", nrow = length(regions)) 
  } else {
    p_tb <- p_tb + facet_wrap(~RuleType, scales = "free_x", nrow = length(regions))
  } 
  ggsave(file.path(figure_dir, "TB_vs_Catch_byConstraint.png"), p_tb, height = 8, width = 20)
  
  output5$RuleType <- factor(output5$RuleType, levels = levels(output4$RuleType))
  p_tb_v2 <- p_tb +
    geom_vline(data = output5, aes(xintercept = TB_P50), linetype = 2, lwd = 1.5) +
    geom_hline(data = output5, aes(yintercept = Catch_P50), linetype = 2, lwd = 1.5)
  ggsave(file.path(figure_dir, "TB_vs_Catch_byConstraint_wTarget.png"), p_tb_v2, height = 8, width = 20)
  

  # p_vbcatch <- ggplot(output4) +
  #   geom_segment(aes(x = VB_P5, xend = VB_P95, y = Catch_P50, yend = Catch_P50, color = factor(CatchConstraint)), lwd = 1.2, alpha = 0.8) +
  #   geom_segment(aes(x = VB_P50, xend = VB_P50, y = Catch_P5, yend = Catch_P95, color = factor(CatchConstraint)), lwd = 1.2, alpha = 0.8) +
  #   geom_point(aes(x = VB_P50, y = Catch_P50, fill = factor(CatchConstraint)), pch = 21, cex = 3) +
  #   expand_limits(y = 0, x = 0) +
  #   xlab("Vulnerable biomass") + ylab("Average annual catch") +
  #   scale_fill_tableau() +
  #   scale_color_tableau() +
  #   guides(fill=guide_legend(title="Catch constraint"), color = FALSE) +
  #   theme_bw(base_size = 20)
  # if(length(regions) > 1){
  #   p_vbcatch <- p_vbcatch + facet_wrap(Region~RuleType, scales = "free_x", nrow = length(regions)) 
  # } else {
  #   p_vbcatch <- p_vbcatch + facet_wrap(~RuleType, scales = "free_x", nrow = length(regions))
  # } 
  # ggsave(file.path(figure_dir, "VB_vs_Catch_CatchConstraint.png"), p_vbcatch, height = 8, width = 20)
  
  # p_vbrisk <- ggplot(output4) +
  #   geom_segment(aes(x = VB_P5, xend = VB_P95, y = Catch_P50, yend = Catch_P50, color = factor(RiskConstraint)), lwd = 1.2, alpha = 0.8) +
  #   geom_segment(aes(x = VB_P50, xend = VB_P50, y = Catch_P5, yend = Catch_P95, color = factor(RiskConstraint)), lwd = 1.2, alpha = 0.8) +
  #   geom_point(aes(x = VB_P50, y = Catch_P50, fill = factor(RiskConstraint)), pch = 21, cex = 3) +
  #   expand_limits(y = 0, x = 0) +
  #   xlab("Vulnerable biomass") + ylab("Average annual catch") +
  #   scale_fill_tableau() +
  #   scale_color_tableau() +
  #   guides(fill=guide_legend(title="Risk constraint"), color = FALSE) +
  #   theme_bw(base_size = 20) 
  # if(length(regions) > 1){
  #   p_vbrisk <- p_vbrisk + facet_wrap(Region~RuleType, scales = "free_x", nrow = length(regions)) 
  # } else {
  #   p_vbrisk <- p_vbrisk + facet_wrap(~RuleType, scales = "free_x", nrow = length(regions))
  # }
  # ggsave(file.path(figure_dir, "VB_vs_Catch_RiskConstraint.png"), p_vbrisk, height = 8, width = 20)
  
  # p_vbcv <- ggplot(output4) +
  #   geom_segment(aes(x = VB_P5, xend = VB_P95, y = Catch_P50, yend = Catch_P50, color = factor(CVConstraint)), lwd = 1.2, alpha = 0.8) +
  #   geom_segment(aes(x = VB_P50, xend = VB_P50, y = Catch_P5, yend = Catch_P95, color = factor(CVConstraint)), lwd = 1.2, alpha = 0.8) +
  #   geom_point(aes(x = VB_P50, y = Catch_P50, fill = factor(CVConstraint)), pch = 21, cex = 3) +
  #   expand_limits(y = 0, x = 0) +
  #   xlab("Vulnerable biomass") + ylab("Average annual catch") +
  #   scale_fill_tableau() +
  #   scale_color_tableau() +
  #   guides(fill=guide_legend(title="CV constraint"), color = FALSE) +
  #   theme_bw(base_size = 20) 
  # if(length(regions) > 1){
  #   p_vbcv <- p_vbcv + facet_wrap(Region~RuleType, scales = "free_x", nrow = length(regions)) 
  # } else {
  #   p_vbcv <- p_vbcv + facet_wrap(~RuleType, scales = "free_x", nrow = length(regions))
  # }
  # ggsave(file.path(figure_dir, "VB_vs_Catch_CVConstraint.png"), p_vbcv, height = 8, width = 20)
  

  ###################################################
  ## Status relative to reference level by rule type
  ###################################################
  check <- dinfo %>% left_join(msy_info %>% filter(Variable == "VB"))
  if(length(unique(check$RuleType))==3) check$RuleType <- factor(check$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
  p_vbcurr <- ggplot(check) +
    geom_ribbon(aes(x = Year, ymin = P5, ymax = P95, fill = RuleType), alpha = 0.5) +
    geom_hline(aes(yintercept = P50, color =RuleType), lwd = 1.2) +
    # geom_line(aes(x = Year, y = VB), lwd = 1.2) +
    stat_summary(aes(x = Year, y = VB), fun.min = function(x) stats::quantile(x, 0.25), fun.max = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.25) +
    stat_summary(aes(x = Year, y = VB), fun = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1.2) +
    expand_limits(y = 0) +
    scale_fill_tableau() +
    scale_color_tableau() +
    ylab("Vulnerable biomass (VB)") +
    guides(color = FALSE, fill = FALSE) +
    coord_cartesian(xlim = c(min(check$Year),max(check$Year)), expand = FALSE) +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    p_vbcurr <- p_vbcurr + facet_grid(Region~RuleType, scales = "free_y")
  } else {
    p_vbcurr <- p_vbcurr + facet_grid(~RuleType, scales = "free_y")
  }
  ggsave(file.path(figure_dir, "VBcurrent.png"), p_vbcurr, height = 10, width = 20)
  
  check <- dinfo %>% left_join(msy_info %>% filter(Variable == "RelVB")) 
  if(length(unique(check$RuleType))==3) check$RuleType <- factor(check$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
  p_relvbcurr <- ggplot(check) +
    geom_ribbon(aes(x = Year, ymin = P5, ymax = P95, fill = RuleType), alpha = 0.5) +
    geom_hline(aes(yintercept = P50, color = RuleType), lwd = 1.2) +
    stat_summary(aes(x = Year, y = RelVB), fun.min = function(x) stats::quantile(x, 0.25), fun.max = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.25) +
    stat_summary(aes(x = Year, y = RelVB), fun = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1.2) +
    expand_limits(y = 0) +
    scale_color_tableau() +
    scale_fill_tableau() +
    guides(color = FALSE, fill = FALSE) +
    ylab("Relative vulnerable biomass (VB/VB0now)") +
    coord_cartesian(xlim = c(min(check$Year),max(check$Year)), expand = FALSE) +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    p_relvbcurr <- p_relvbcurr + facet_grid(Region~RuleType)
  } else {
    p_relvbcurr <- p_relvbcurr + facet_grid(~RuleType)
  }
  ggsave(file.path(figure_dir, "RelVBcurrent.png"), p_relvbcurr, height = 10, width = 20)

  #######################################
  ## examine variability across filters
  #######################################
  ## maximum F that passes risk constraints
  maxF <- output4 %>% 
    dplyr::filter(RuleType == "FixedF") %>%
    dplyr::right_join(max_info %>% filter(RuleType == "FixedF") %>% select(-c(Variable, P5, P50, P95)))
  maxF2 <- unique(maxF)

  mp_options <- output4 %>%
    dplyr::filter(RuleType == "CPUE-based") %>%
    dplyr::filter(Constraint %in% c("Pass", "CV"))
  mp_options2 <- unique(mp_options)

  options <- rbind.data.frame(maxF2, mp_options2)
  write.csv(options, file.path(figure_dir, "Options_summary.csv"))

  ## examples from find_max
  reg <- unique(find_max$Region)
  find_max_sub <- lapply(1:length(reg), function(x){
    sub <- find_max %>% filter(Region == reg[x])
    types <- unique(sub$RuleType)
    byType <- lapply(1:length(types), function(y){
      sub2 <- sub %>% filter(RuleType == types[y])
      con <- unique(sub2$Constraint)
      byCon <- lapply(1:length(con), function(z){
        sub3 <- sub2 %>% filter(Constraint == con[z])
        out <- sub3[nrow(sub3),]
        return(out)
      })
      byCon <- do.call(rbind, byCon)
      return(byCon)
    })
    byType <- do.call(rbind, byType)
    return(byType)
  })
  find_max_sub <- do.call(rbind, find_max_sub)
  
  check1 <- cinfo %>%
    dplyr::right_join(find_max_sub %>% dplyr::select(-P50)) %>%
    dplyr::left_join(max_info %>% filter(Variable == "Catch")) %>%
    dplyr::mutate(RelVB = VB / VB0now) %>%
    dplyr::mutate(RelSSB = SSB / SSB0now) %>%
    dplyr::select(Iteration, Year, Region, RuleNum, RuleType, Constraint, Catch, RelVB, RelSSB) %>%
    tidyr::pivot_longer(cols = c(Catch,RelVB,RelSSB), names_to = "Variable", values_to = "Value") %>%
    dplyr::filter(RuleType == "FixedCatch")
  if(nrow(check1) > 0){
    check2 <- check1 %>%
      dplyr::group_by(Year, Region, RuleType, Constraint, Variable) %>%
      dplyr::summarise(P5 = quantile(Value, 0.05),
                       P50 = quantile(Value, 0.50),
                       P95 = quantile(Value, 0.95))
    if(length(unique(check2$RuleType))==3) check2$RuleType <- factor(check2$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
    if(length(unique(check1$RuleType))==3) check1$RuleType <- factor(check1$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
      const <- unique(as.character(output4$Constraint))
  const1 <- const[grepl("CV",const)==FALSE]
  const1_1 <- const1[grepl("Catch", const1) == FALSE]
  const1_2 <- const1[grepl("Catch", const1)]
  const1 <- c(const1_1, const1_2)
  const2 <- const[grepl("CV", const)]
  constx <- c(const1,const2)
  const <- c(constx, const[which(const %in% constx == FALSE)])
  check1$Constraint <- factor(check1$Constraint, levels = const)
    const <- rev(c(constx, const[which(const %in% constx == FALSE)]))
    check1$Constraint <- factor(check1$Constraint, levels = const)
    check2$Constraint <- factor(check2$Constraint, levels = const)
    p_constr1 <- ggplot() +
      geom_ribbon(data = check2 %>% filter(Variable %in% c("Catch","RelSSB","RelVB")), aes(x = Year, ymin = P5, ymax = P95, fill = Constraint), color = NA, alpha = 0.5) +
      geom_line(data = check2 %>% filter(Variable %in% c("Catch","RelSSB","RelVB")), aes(x = Year, y = P50, color = Constraint), lwd = 1.5) +
      geom_line(data = check1 %>% filter(Iteration == 1) %>% filter(Variable %in% c("Catch","RelSSB","RelVB")), aes(x = Year, y = Value)) +
      scale_color_tableau() +
      scale_fill_tableau() +
      expand_limits(y = 0) +
      ylab("Value") + xlab("Projection Year") +
      guides(color = FALSE, fill = FALSE) +
      theme_bw(base_size = 20)
    if(length(regions)>1){
      p_constr1 <- p_constr1 + facet_grid(Variable~Region+Constraint, scales = "free_y") 
    } else {
      p_constr1 <- p_constr1 + facet_grid(Variable~Constraint, scales = "free_y")
    }
    ggsave(file.path(figure_dir, "CompareConstraints_FixedCatch.png"), p_constr1, height = 10, width = 15)
  }
  

  check1 <- cinfo %>%
    dplyr::mutate(RelVB = VB / VB0now) %>%
    dplyr::mutate(RelSSB = SSB / SSB0now) %>%
    dplyr::right_join(find_max_sub %>% dplyr::select(-P50)) %>%
    dplyr::left_join(max_info %>% filter(Variable == "Catch")) %>%
    dplyr::select(Iteration, Year, Region, RuleNum, RuleType, Constraint, Catch, RelVB, RelSSB) %>%
    tidyr::pivot_longer(cols = c(Catch,RelVB,RelSSB), names_to = "Variable", values_to = "Value") %>%
    dplyr::filter(RuleType == "FixedF")
  if(nrow(check1) > 0){
    check2 <- check1 %>%
      dplyr::group_by(Year, Region, RuleType, Constraint, Variable) %>%
      dplyr::summarise(P5 = quantile(Value, 0.05),
                       P50 = quantile(Value, 0.50),
                       P95 = quantile(Value, 0.95))
    if(length(unique(check2$RuleType))==3) check2$RuleType <- factor(check2$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
    if(length(unique(check1$RuleType))==3) check1$RuleType <- factor(check1$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
    const <- unique(as.character(check1$Constraint))
  const <- unique(as.character(check1$Constraint))
  const1 <- const[grepl("CV",const)==FALSE]
  const1_1 <- const1[grepl("Catch", const1) == FALSE]
  const1_2 <- const1[grepl("Catch", const1)]
  const1 <- c(const1_1, const1_2)
  const2 <- const[grepl("CV", const)]
  constx <- c(const1,const2)
  const <- c(constx, const[which(const %in% constx == FALSE)])
  check1$Constraint <- factor(check1$Constraint, levels = const)
    const <- rev(c(constx, const[which(const %in% constx == FALSE)]))
    check1$Constraint <- factor(check1$Constraint, levels = const)
    check2$Constraint <- factor(check2$Constraint, levels = const)
    p_constr2 <- ggplot() +
      geom_ribbon(data = check2 %>% filter(Variable %in% c("Catch","RelSSB","RelVB")), aes(x = Year, ymin = P5, ymax = P95, fill = Constraint), color = NA, alpha = 0.5) +
      geom_line(data = check2 %>% filter(Variable %in% c("Catch","RelSSB","RelVB")), aes(x = Year, y = P50, color = Constraint), lwd = 1.5) +
      geom_line(data = check1 %>% filter(Iteration == 1) %>% filter(Variable %in% c("Catch","RelSSB","RelVB")), aes(x = Year, y = Value)) +
      scale_color_tableau() +
      scale_fill_tableau() +
      ylab("Value") + xlab("Projection Year") +
      expand_limits(y = 0) +
      guides(color = FALSE, fill = FALSE) +
      theme_bw(base_size = 20)
    if(length(regions)>1){
      p_constr2 <- p_constr2 + facet_grid(Variable~Region+Constraint, scales = "free_y") 
    } else {
      p_constr2 <- p_constr2 + facet_grid(Variable~Constraint, scales = "free_y")
    }
    ggsave(file.path(figure_dir, "CompareConstraints_FixedF.png"), p_constr2, height = 10, width = 15)
  }

  check1 <- cinfo %>%
    dplyr::right_join(find_max_sub %>% dplyr::select(-P50)) %>%
    dplyr::left_join(max_info %>% filter(Variable == "Catch")) %>%
    dplyr::select(Iteration, Year, Region, RuleNum, RuleType, Constraint, Catch, RelVB, RelSSB) %>%
    tidyr::pivot_longer(cols = c(Catch,RelVB,RelSSB), names_to = "Variable", values_to = "Value") %>%
    dplyr::filter(RuleType == "CPUE-based")
  if(nrow(check1) > 0){
    check2 <- check1 %>%
      dplyr::group_by(Year, Region, RuleType, Constraint, Variable) %>%
      dplyr::summarise(P5 = quantile(Value, 0.05),
                       P50 = quantile(Value, 0.50),
                       P95 = quantile(Value, 0.95))
    if(length(unique(check2$RuleType))==3) check2$RuleType <- factor(check2$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
    if(length(unique(check1$RuleType))==3) check1$RuleType <- factor(check1$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
    const <- unique(as.character(check1$Constraint))
  const <- unique(as.character(check1$Constraint))
  const1 <- const[grepl("CV",const)==FALSE]
  const1_1 <- const1[grepl("Catch", const1) == FALSE]
  const1_2 <- const1[grepl("Catch", const1)]
  const1 <- c(const1_1, const1_2)
  const2 <- const[grepl("CV", const)]
  constx <- c(const1,const2)
  const <- c(constx, const[which(const %in% constx == FALSE)])
  check1$Constraint <- factor(check1$Constraint, levels = const)
    const <- rev(c(constx, const[which(const %in% constx == FALSE)]))
    check1$Constraint <- factor(check1$Constraint, levels = const)
    check2$Constraint <- factor(check2$Constraint, levels = const)
    p_constr3 <- ggplot() +
      geom_ribbon(data = check2 %>% filter(Variable %in% c("Catch","RelSSB","RelVB")), aes(x = Year, ymin = P5, ymax = P95, fill = Constraint), color = NA, alpha = 0.5) +
      geom_line(data = check2 %>% filter(Variable %in% c("Catch","RelSSB","RelVB")), aes(x = Year, y = P50, color = Constraint), lwd = 1.5) +
      geom_line(data = check1 %>% filter(Iteration == 1) %>% filter(Variable %in% c("Catch","RelSSB","RelVB")), aes(x = Year, y = Value)) +
      scale_color_tableau() +
      scale_fill_tableau() +
      expand_limits(y = 0) +
      ylab("Value") + xlab("Projection Year") +
      guides(color = FALSE, fill = FALSE) +
      theme_bw(base_size = 20)
    if(length(regions)>1){
      p_constr3 <- p_constr3 + facet_grid(Variable~Region+Constraint, scales = "free_y") 
    } else {
      p_constr3 <- p_constr3 + facet_grid(Variable~Constraint, scales = "free_y")
    }
    ggsave(file.path(figure_dir, "CompareConstraints_MP.png"), p_constr3, height = 10, width = 15)
  }
  
  check1 <- cinfo %>%
    dplyr::right_join(find_max_sub %>% dplyr::select(-P50)) %>%
    dplyr::left_join(max_info %>% filter(Variable == "Catch")) %>%
    dplyr::select(Iteration, Year, Region, RuleNum, RuleType, Constraint, Catch, RelVB, RelSSB) %>%
    tidyr::pivot_longer(cols = c(Catch,RelVB,RelSSB), names_to = "Variable", values_to = "Value") %>%
    dplyr::filter(Constraint == "Pass")
  if(nrow(check1) > 0){
    check2 <- check1 %>%
      dplyr::group_by(Year, Region, RuleType, Constraint, Variable) %>%
      dplyr::summarise(P5 = quantile(Value, 0.05),
                       P50 = quantile(Value, 0.50),
                       P95 = quantile(Value, 0.95))
    if(length(unique(check2$RuleType))==3) check2$RuleType <- factor(check2$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
    if(length(unique(check1$RuleType))==3) check1$RuleType <- factor(check1$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
    const <- unique(as.character(check1$Constraint))
  const <- unique(as.character(check1$Constraint))
  const1 <- const[grepl("CV",const)==FALSE]
  const1_1 <- const1[grepl("Catch", const1) == FALSE]
  const1_2 <- const1[grepl("Catch", const1)]
  const1 <- c(const1_1, const1_2)
  const2 <- const[grepl("CV", const)]
  constx <- c(const1,const2)
  const <- c(constx, const[which(const %in% constx == FALSE)])
  check1$Constraint <- factor(check1$Constraint, levels = const)
    const <- rev(c(constx, const[which(const %in% constx == FALSE)]))
    check1$Constraint <- factor(check1$Constraint, levels = const)
    check2$Constraint <- factor(check2$Constraint, levels = const)
    p_constr3 <- ggplot() +
      geom_ribbon(data = check2 %>% filter(Variable %in% c("Catch","RelSSB","RelVB")), aes(x = Year, ymin = P5, ymax = P95, fill = RuleType), color = NA, alpha = 0.5) +
      geom_line(data = check2 %>% filter(Variable %in% c("Catch","RelSSB","RelVB")), aes(x = Year, y = P50, color = RuleType), lwd = 1.5) +
      geom_line(data = check1 %>% filter(Iteration == 1) %>% filter(Variable %in% c("Catch","RelSSB","RelVB")), aes(x = Year, y = Value)) +
      scale_color_tableau() +
      scale_fill_tableau() +
      expand_limits(y = 0) +
      ylab("Value") + xlab("Projection Year") +
      guides(color = FALSE, fill = FALSE) +
      theme_bw(base_size = 20)
    if(length(regions)>1){
      p_constr3 <- p_constr3 + facet_grid(Variable~Region+RuleType, scales = "free_y") 
    } else {
      p_constr3 <- p_constr3 + facet_grid(Variable~RuleType, scales = "free_y")
    }
    ggsave(file.path(figure_dir, "CompareRuleTypes_Pass.png"), p_constr3, height = 10, width = 15)
  }
  
  check <- output4 %>%
    filter(RuleType == "CPUE-based")
  if(nrow(check) > 0) {
    p_cv <- ggplot(check) +
      geom_segment(aes(x = CV, xend = CV, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
      geom_point(aes(x = CV, y = Catch_P50, fill = Constraint), pch = 21, cex = 4) +
      expand_limits(y = 0, x = 0) +
      xlab("CV of catch over time and iteration") + ylab("Average annual catch") +
      scale_fill_tableau() +
      scale_color_tableau() +
      theme_bw(base_size = 20)
    if(length(regions) > 1){
      p_cv <- p_cv + facet_grid(Region~., scales = "free_x") 
    } 
    ggsave(file.path(figure_dir, "CV_vs_Catch_MP.png"), p_cv, height = 8, width = 12)
    
    p_cv <- ggplot(check %>% filter(Constraint == "Pass")) +
      geom_segment(aes(x = CV, xend = CV, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
      geom_point(aes(x = CV, y = Catch_P50, fill = Constraint), pch = 21, cex = 4) +
      expand_limits(y = 0, x = 0) +
      xlab("CV of catch over time and iteration") + ylab("Average annual catch") +
      scale_fill_tableau() +
      scale_color_tableau() +
      theme_bw(base_size = 20)
    if(length(regions) > 1){
      p_cv <- p_cv + facet_grid(Region~., scales = "free_x") 
    } 
    ggsave(file.path(figure_dir, "CV_vs_Catch_MP_Pass.png"), p_cv, height = 8, width = 12)
  }

    check1 <- cinfo %>%
      dplyr::left_join(output2 %>% filter(Variable == "Catch")) %>%
      dplyr::select(Iteration, Year, Region, RuleNum, RuleType, Constraint, Catch, CPUE) %>%
      dplyr::filter(RuleType == "CPUE-based")
    if(nrow(check1) > 0){
    const <- unique(as.character(check1$Constraint))
  const <- unique(as.character(check1$Constraint))
  const1 <- const[grepl("CV",const)==FALSE]
  const1_1 <- const1[grepl("Catch", const1) == FALSE]
  const1_2 <- const1[grepl("Catch", const1)]
  const1 <- c(const1_1, const1_2)
  const2 <- const[grepl("CV", const)]
  constx <- c(const1,const2)
  const <- c(constx, const[which(const %in% constx == FALSE)])
    const <- rev(c(constx, const[which(const %in% constx == FALSE)]))
    check1$Constraint <- factor(check1$Constraint, levels = const)
      p_cpue_mp <- ggplot(check1) +
        geom_point(data = check1 %>% filter(Constraint != "Pass"), aes(x = CPUE, y = Catch, color = Constraint), cex = 2, alpha = 0.5) +
        geom_point(data = check1 %>% filter(Constraint == "Pass"), aes(x = CPUE, y = Catch, color = Constraint), cex = 2, alpha = 0.5) +
       scale_color_tableau() +
        xlab("Offset-year CPUE") +
        theme_bw(base_size = 20)
      if(length(regions) > 1){
        p_cpue_mp <- p_cpue_mp + facet_grid(Region~., scales = "free_x") 
      } 
      ggsave(file.path(figure_dir, "CPUE_vs_Catch_MP.png"), p_cpue_mp, height = 10, width = 12)

      p_cpue_mp_v2 <- p_cpue_mp +
        geom_vline(data = msy_info2 %>% filter(RuleType == "CPUE-based"), aes(xintercept = CPUE), lty = 2) +
        geom_hline(data = msy_info2 %>% filter(RuleType == "CPUE-based"), aes(yintercept = Catch), lty = 2)
      ggsave(file.path(figure_dir, "CPUE_vs_Catch_MP_v2.png"), p_cpue_mp_v2, height = 10, width = 12)
    } 

    check1 <- cinfo %>%
      dplyr::left_join(output2 %>% filter(Variable == "Catch")) %>%
      dplyr::select(Iteration, Year, Region, RuleNum, RuleType, Constraint, Catch, CPUE)
    const <- unique(as.character(check1$Constraint))
  const <- unique(as.character(check1$Constraint))
  const1 <- const[grepl("CV",const)==FALSE]
  const1_1 <- const1[grepl("Catch", const1) == FALSE]
  const1_2 <- const1[grepl("Catch", const1)]
  const1 <- c(const1_1, const1_2)
  const2 <- const[grepl("CV", const)]
  constx <- c(const1,const2)
  const <- c(constx, const[which(const %in% constx == FALSE)])
    check1$Constraint <- factor(check1$Constraint, levels = const)    
    if(length(unique(check1$RuleType))==3) check1$RuleType <- factor(check1$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
    p_cpue_all <- ggplot(check1) +
      geom_point(data = check1 %>% filter(Constraint != "Pass"), aes(x = CPUE, y = Catch, color = Constraint), cex = 2, alpha = 0.5) +
      geom_point(data = check1 %>% filter(Constraint == "Pass"), aes(x = CPUE, y = Catch, color = Constraint), cex = 2, alpha = 0.5) +
      scale_color_tableau() +
      xlab("Offset-year CPUE") +
      theme_bw(base_size = 20)
    if(length(regions) > 1){
      p_cpue_all <- p_cpue_all + facet_wrap(Region~RuleType)
    } else {
      p_cpue_all <- p_cpue_all + facet_wrap(~RuleType)
    }
    ggsave(file.path(figure_dir, "CPUE_vs_Catch.png"), p_cpue_all, height = 10, width = 12)
    
    if(length(unique(msy_info2$RuleType == 3))) msy_info2$RuleType <- factor(msy_info2$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
    p_cpue_all_v2 <- p_cpue_all +
        geom_vline(data = msy_info2, aes(xintercept = CPUE), lty = 2, lwd = 2) +
        geom_hline(data = msy_info2, aes(yintercept = Catch), lty = 2, lwd = 2)
    ggsave(file.path(figure_dir, "CPUE_vs_Catch_v2.png"), p_cpue_all, height = 10, width = 12)

    check1 <- cinfo %>%
      dplyr::left_join(max_info %>% filter(Variable == "Catch")) %>%
      dplyr::select(Iteration, Year, Region, RuleNum, RuleType, Constraint, Catch, CPUE) %>%
      dplyr::filter(RuleType == "CPUE-based")
    if(nrow(check1) > 0){
    const <- unique(as.character(check1$Constraint))
  const <- unique(as.character(check1$Constraint))
  const1 <- const[grepl("CV",const)==FALSE]
  const1_1 <- const1[grepl("Catch", const1) == FALSE]
  const1_2 <- const1[grepl("Catch", const1)]
  const1 <- c(const1_1, const1_2)
  const2 <- const[grepl("CV", const)]
  constx <- c(const1,const2)
  const <- c(constx, const[which(const %in% constx == FALSE)])
    const <- rev(c(constx, const[which(const %in% constx == FALSE)]))
    check1$Constraint <- factor(check1$Constraint, levels = const)
      p_cpue_mp <- ggplot(check1) +
        geom_point(data = check1 %>% filter(Constraint != "Pass"), aes(x = CPUE, y = Catch, color = Constraint), cex = 2, alpha = 0.5) +
        geom_point(data = check1 %>% filter(Constraint == "Pass"), aes(x = CPUE, y = Catch, color = Constraint), cex = 2, alpha = 0.5) +
       scale_color_tableau() +
        xlab("Offset-year CPUE") +
        theme_bw(base_size = 20)
      if(length(regions) > 1){
        p_cpue_mp <- p_cpue_mp + facet_wrap(Region~.)
      }
      ggsave(file.path(figure_dir, "CPUE_vs_Catch_MP_Max.png"), p_cpue_mp, height = 10, width = 12)

      p_cpue_mp_v2 <- p_cpue_mp +
        geom_vline(data = msy_info2 %>% filter(RuleType == "CPUE-based"), aes(xintercept = CPUE), lty = 2, lwd = 1.5) +
        geom_hline(data = msy_info2 %>% filter(RuleType == "CPUE-based"), aes(yintercept = Catch), lty = 2, lwd = 1.5)
      ggsave(file.path(figure_dir, "CPUE_vs_Catch_MP_Max_v2.png"), p_cpue_mp_v2, height = 10, width = 12)
    } 

    check1 <- cinfo %>%
      dplyr::right_join(msy_info %>% filter(Variable == "Catch")) %>%
      dplyr::select(Iteration, Year, Region, RuleNum, RuleType, Constraint, Catch, CPUE)  %>%
      dplyr::filter(Region != "Total")
    if(length(unique(check1$RuleType))==3) check1$RuleType <- factor(check1$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
    p_cpue_all <- ggplot(check1) +
      geom_point(data = check1, aes(x = CPUE, y = Catch, color = RuleType), cex = 2, alpha = 0.5) +
      scale_color_tableau() +
      xlab("Offset-year CPUE") +
      theme_bw(base_size = 20)
    if(length(regions) > 1){
      p_cpue_all <- p_cpue_all + facet_wrap(Region~.)
    }
    ggsave(file.path(figure_dir, "CPUE_vs_Catch.png"), p_cpue_all, height = 10, width = 12)
    
    if(length(unique(msy_info2$RuleType == 3))) msy_info2$RuleType <- factor(msy_info2$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
    p_cpue_all_v2 <- p_cpue_all +
        geom_vline(data = msy_info2, aes(xintercept = CPUE, color = RuleType), lty = 2, lwd = 1.5) +
        geom_hline(data = msy_info2, aes(yintercept = Catch, color = RuleType), lty = 2, lwd = 1.5)
    ggsave(file.path(figure_dir, "CPUE_vs_Catch_v2.png"), p_cpue_all_v2, height = 10, width = 12)
    
    sub <- output4 %>% filter(RuleType == "CPUE-based") %>% filter(Region != "Total")
    const <- unique(as.character(sub$Constraint))
  const1 <- const[grepl("CV",const)==FALSE]
  const1_1 <- const1[grepl("Catch", const1) == FALSE]
  const1_2 <- const1[grepl("Catch", const1)]
  const1 <- c(const1_1, const1_2)
  const2 <- const[grepl("CV", const)]
  constx <- c(const1,const2)
  const <- c(constx, const[which(const %in% constx == FALSE)])
    sub$Constraint <- factor(sub$Constraint, levels = const)
    msy <- unique(msy_info %>% filter(RuleType == "CPUE-based") %>% dplyr::select(par1:par10)) %>% mutate(Constraint = "Pass")
    msy$Constraint <- factor(msy$Constraint, levels = const)
    msy <- msy %>% filter(Region != "Total")
    p_rule <- ggplot(sub) +
      geom_segment(aes(x = par2, xend = par3, y = 0, yend = par5, color = Constraint), lwd = 1.5) +
      geom_segment(aes(x = par3, xend = par4, y = par5, yend = par5, color = Constraint ), lwd = 1.5) +
      geom_segment(aes(x = par4, xend = par4, y = par5, yend = par5 + (par5 * par7), color = Constraint), lwd = 1.5) +
      geom_segment(aes(x = par4, xend = par4 + par6, y = par5 + (par5 * par7), yend = par5 + (par5 * par7), color = Constraint), lwd = 1.5) +
      geom_segment(aes(x = par4 + par6, xend = par4 + par6, y = par5 + (par5 * par7), yend = par5 + 2*(par5 * par7), color = Constraint), lwd = 1.5) +
      geom_segment(aes(x = par4 + par6, xend = par4 + 2*par6, y = par5 + 2*(par5 * par7), yend = par5 + 2*(par5 * par7), color = Constraint), lwd = 1.5) +
      geom_segment(aes(x = par4 + 2*par6, xend = par4 + 2*par6, y = par5 + 2*(par5 * par7), yend = par5 + 3*(par5 * par7), color = Constraint), lwd = 1.5) +
      geom_segment(aes(x = par4 + 2*par6, xend = par4 + 3*par6, y = par5 + 3*(par5 * par7), yend = par5 + 3*(par5 * par7), color = Constraint), lwd = 1.5) +
      geom_segment(aes(x = par4 + 3*par6, xend = par4 + 3*par6, y = par5 + 3*(par5 * par7), yend = par5 + 4*(par5 * par7), color = Constraint), lwd = 1.5) +
      geom_segment(aes(x = par4 + 3*par6, xend = par4 + 4*par6, y = par5 + 4*(par5 * par7), yend = par5 + 4*(par5 * par7), color = Constraint), lwd = 1.5) +
      geom_segment(data = msy, aes(x = par2, xend = par3, y = 0, yend = par5 ), lwd = 1.5) +
      geom_segment(data = msy, aes(x = par3, xend = par4, y = par5, yend = par5  ), lwd = 1.5) +
      geom_segment(data = msy, aes(x = par4, xend = par4, y = par5, yend = par5 + (par5 * par7) ), lwd = 1.5) +
      geom_segment(data = msy, aes(x = par4, xend = par4 + par6, y = par5 + (par5 * par7), yend = par5 + (par5 * par7) ), lwd = 1.5) +
      geom_segment(data = msy, aes(x = par4 + par6, xend = par4 + par6, y = par5 + (par5 * par7), yend = par5 + 2*(par5 * par7) ), lwd = 1.5) +
      geom_segment(data = msy, aes(x = par4 + par6, xend = par4 + 2*par6, y = par5 + 2*(par5 * par7), yend = par5 + 2*(par5 * par7) ), lwd = 1.5) +
      geom_segment(data = msy, aes(x = par4 + 2*par6, xend = par4 + 2*par6, y = par5 + 2*(par5 * par7), yend = par5 + 3*(par5 * par7) ), lwd = 1.5) +
      geom_segment(data = msy, aes(x = par4 + 2*par6, xend = par4 + 3*par6, y = par5 + 3*(par5 * par7), yend = par5 + 3*(par5 * par7) ), lwd = 1.5) +
      geom_segment(data = msy, aes(x = par4 + 3*par6, xend = par4 + 3*par6, y = par5 + 3*(par5 * par7), yend = par5 + 4*(par5 * par7) ), lwd = 1.5) +
      geom_segment(data = msy, aes(x = par4 + 3*par6, xend = par4 + 4*par6, y = par5 + 4*(par5 * par7), yend = par5 + 4*(par5 * par7) ), lwd = 1.5) +
      scale_color_tableau() +
      coord_cartesian(xlim = c(0,4)) +
      xlab("Offset-year CPUE") + ylab("Catch") +
      theme_bw(base_size = 14)
    if(length(regions) > 1){
      p_rule <- p_rule + facet_wrap(Region+Constraint~par5)
    } else {
      p_rule <- p_rule + facet_wrap(Constraint~par5)
    }
    ggsave(file.path(figure_dir, "CPUE_rules.png"), p_rule, height = 15, width = 17)
    
  
}
