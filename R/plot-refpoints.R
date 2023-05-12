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

      require(ggthemes)

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
  seasons <- c("AW", "SS")
  if (length(regions) > 1) regions2 <- c(regions, "Total")
  if (length(regions) == 1) regions2 <- regions
  sex <- c("Male", "Immature female", "Mature female")
  fleets <- c("SL", "NSL")


 if ("pred_catch_sl_jryt" %in% names(mcmc1)) {
  slcatch <- mcmc1$pred_catch_sl_jryt
  dimnames(slcatch) <- list("Iteration" = 1:n_iter1, "RuleNum" = 1:dim(slcatch)[2], "Region" = regions, "Year" = pyears1, "Season" = seasons)
  slcatch2 <- reshape2::melt(slcatch, value.name = "Catch") %>%
    group_by(Iteration, Year, Region, RuleNum) %>%
    summarise(Catch = sum(Catch)) %>%
    mutate("CatchType" = "SL")

  slcatch2_t <- reshape2::melt(slcatch, value.name = "Catch") %>%
    group_by(Iteration, Year, Region, Season, RuleNum) %>%
    summarise(Catch = sum(Catch)) %>%
    mutate("CatchType" = "SL")

  nslcatch <- mcmc1$pred_catch_nsl_jryt
  dimnames(nslcatch) <- list("Iteration"=1:n_iter1, "RuleNum"=1:dim(nslcatch)[2], "Region"=regions, "Year"=pyears1, "Season"=seasons)
  nslcatch2 <- reshape2::melt(nslcatch, value.name = "Catch") %>%
    group_by(Iteration, Year, Region, RuleNum) %>%
    summarise(Catch = sum(Catch)) %>%
    mutate("CatchType" = "NSL")

  nslcatch2_t <- reshape2::melt(nslcatch, value.name = "Catch") %>%
    group_by(Iteration, Year, Region, Season, RuleNum) %>%
    summarise(Catch = sum(Catch)) %>%
    mutate("CatchType" = "NSL")

 } else {
  slcatch <- mcmc1$pred_catch_sl_ryt
  dimnames(slcatch) <- list("Iteration"=1:n_iter1, "Region"=regions, "Year"=pyears1, "Season"=seasons)
  slcatch2 <- reshape2::melt(slcatch, value.name = "Catch") %>%
    group_by(Iteration, Year, Region) %>%
    summarise(Catch = sum(Catch)) %>%
    mutate("CatchType" = "SL")

  slcatch2_t <- reshape2::melt(slcatch, value.name = "Catch") %>%
    group_by(Iteration, Year, Region, Season) %>%
    summarise(Catch = sum(Catch)) %>%
    mutate("CatchType" = "SL")

  nslcatch <- mcmc1$pred_catch_nsl_ryt
  dimnames(nslcatch) <- list("Iteration"=1:n_iter1, "Region"=regions, "Year"=pyears1, "Season"=seasons)
  nslcatch2 <- reshape2::melt(nslcatch, value.name = "Catch") %>%
    group_by(Iteration, Year, Region) %>%
    summarise(Catch = sum(Catch)) %>%
    mutate("CatchType" = "NSL")

  nslcatch2_t <- reshape2::melt(nslcatch, value.name = "Catch") %>%
    group_by(Iteration, Year, Region, Season) %>%
    summarise(Catch = sum(Catch)) %>%
    mutate("CatchType" = "NSL")
 }

   pcatch <- rbind.data.frame(slcatch2, nslcatch2) %>%
    tidyr::pivot_wider(names_from = CatchType, values_from = Catch) %>%
    mutate(Catch = SL + NSL)

  pcatch_t <- rbind.data.frame(slcatch2_t, nslcatch2_t) %>%
    tidyr::pivot_wider(names_from = CatchType, values_from = Catch) %>%
    mutate(Catch = SL + NSL)

  rm(slcatch)
  rm(nslcatch)
  gc()

  catch <- pcatch
  catch_t <- pcatch_t %>%
    filter(Season == "AW") %>%
    ungroup() %>%
    select(-Season) %>%
    rename(SL_AW = SL, NSL_AW = NSL, Catch_AW = Catch)
  gc()

  vb <- mcmc1$biomass_vulnref_AW_jyr
  dimnames(vb) <- list("Iteration" = 1:n_iter1, "RuleNum" = 1:dim(vb)[2], "Year" = pyears1, "Region" = regions)
  vb2 <- reshape2::melt(vb) %>%
    rename(VB = value)
  vb2$Region <- factor(vb2$Region)

 if(any(grepl("B0now_r", names(mcmc1)))){
  vb0now <- mcmc1$B0now_r
  dimnames(vb0now) <- list("Iteration" = 1:n_iter1, "Region" = regions2)
  vb0now <- reshape2::melt(vb0now) %>%
    rename(VB0now = value)
  vb0now$Region <- factor(vb0now$Region)

  VB0 <- mcmc1$B0_r
  dimnames(VB0) <- list("Iteration" = 1:n_iter1, "Region" = regions2)
  VB0 <- reshape2::melt(VB0) %>%
    rename(VB0 = value)
  VB0$Region <- factor(VB0$Region)

  ssb0now <- mcmc1$SSB0now_r
  dimnames(ssb0now) <- list("Iteration" = 1:n_iter1, "Region" = regions2)
  ssb0now <- reshape2::melt(ssb0now) %>%
    rename(SSB0now=value)
  ssb0now$Region <- factor(ssb0now$Region)

  tb0now <- mcmc1$Btot0now_r
  dimnames(tb0now) <- list("Iteration" = 1:n_iter1, "Region" = regions2)
  tb0now <- reshape2::melt(tb0now) %>%
    rename(TB0now=value)
  tb0now$Region <- factor(tb0now$Region)
 }

  ssb <- mcmc1$biomass_ssb_jyr
  dimnames(ssb) <- list("Iteration" = 1:n_iter1, "RuleNum" = 1:dim(ssb)[2], "Year" = pyears1, "Region" = regions)
  ssb2 <- reshape2::melt(ssb) %>% rename("SSB"=value)
  ssb2$Region <- factor(ssb2$Region)

  SSB0 <- mcmc1$SSB0_r
  dimnames(SSB0) <- list("Iteration" = 1:n_iter1, "Region" = regions2)
  SSB0 <- reshape2::melt(SSB0) %>%
    rename(SSB0=value)
  SSB0$Region <- factor(SSB0$Region)

  if(any(grepl("biomass_total_ytrs", names(mcmc1)))){
    tb <- mcmc1$biomass_total_ytrs
    dimnames(tb) <- list("Iteration" = 1:n_iter1, "Year" = pyears1, "Season" = seasons, "Region" = regions, "Sex" = c(sex, "Total"))
    tb2 <- reshape2::melt(tb) %>%
      rename("TB"=value) %>%
      filter(Sex == "Total") %>%
      select(-Sex) %>%
      filter(Season == "AW") %>%
      select(-Season) %>%
      group_by(Iteration, Year, Region) %>%
      summarise(TB = sum(TB))
  }
  if(any(grepl("biomass_total_jytrs", names(mcmc1)))){
    tb <- mcmc1$biomass_total_jytrs
    dimnames(tb) <- list("Iteration" = 1:n_iter1, "RuleNum" = 1:dim(ssb)[2], "Year" = pyears1, "Season" = seasons, "Region" = regions, "Sex" = c(sex, "Total"))
    tb2 <- reshape2::melt(tb) %>%
      rename("TB"=value) %>%
      filter(Sex == "Total") %>%
      select(-Sex) %>%
      filter(Season == "AW") %>%
      select(-Season) %>%
      group_by(Iteration, RuleNum, Year, Region) %>%
      summarise(TB = sum(TB))
  }
  tb2$Region <- factor(tb2$Region)

  TB0 <- mcmc1$Btot0_r
  dimnames(TB0) <- list("Iteration" = 1:n_iter1, "Region" = regions2)
  TB0 <- reshape2::melt(TB0) %>%
    rename(TB0=value)
  TB0$Region <- factor(TB0$Region)

  rec <- mcmc1$recruits_ry
  dimnames(rec) <- list("Iteration" = 1:n_iter1, "Region" = regions, "Year" = pyears1)
  rec2 <- reshape2::melt(rec) %>%
    rename(Recruitment = value)
  rec2$Region <- factor(rec2$Region)

if(any(grepl("B0now_r", names(mcmc1)))){
  relssb <- full_join(ssb2, ssb0now) %>%
    full_join(SSB0) %>%
    mutate(RelSSBdata = SSB/SSB0now,
                  RelSSB = SSB/SSB0)

  relvb <- full_join(vb2, vb0now) %>%
    full_join(VB0) %>%
    mutate(RelVBdata = VB/VB0now,
                  RelVB = VB/VB0)

  reltb <- full_join(tb2, tb0now) %>%
    full_join(TB0) %>%
    mutate(RelTBdata = TB/TB0now,
                  RelTB = TB/TB0)

  relb <- full_join(relssb, relvb)
  relb1 <- full_join(relb, reltb)
  relb2 <- full_join(relb1, rec2)

 } else {
  relssb <- full_join(ssb2, SSB0) %>%
    mutate(RelSSB = SSB/SSB0)

  reltb <- full_join(tb2, TB0) %>%
    mutate(RelTB = TB/TB0)

  relb1 <- full_join(relssb, reltb)
  relb2 <- full_join(relb1, rec2)
  relb2 <- full_join(relb2, vb2)
 }



  catch$Region <- factor(catch$Region)
  catch_t$Region <- factor(catch_t$Region)
  info1x <- full_join(catch, relb2) %>%
    full_join(catch_t) %>%
    select(-c(SL_AW, NSL_AW))
  info1 <- info1x %>%
      filter(Region %in% regions) %>%
      mutate(Region = paste0("Region ", Region)) %>%
      mutate(U = Catch_AW / VB)

  if(length(regions) > 1){
    b0_total <- info1x %>%
      filter(Region == "Total") %>%
      ungroup() %>%
      select(Iteration, Region, SSB0now, SSB0, VB0now, VB0, TB0now, TB0)
    tinfo1 <- info1 %>%
      group_by(Iteration, Year, RuleNum) %>%
      summarise(SL = sum(SL),
                       NSL = sum(NSL),
                       Catch = sum(Catch),
                       Catch_AW = sum(Catch_AW),
                       # CatchResidual = sum(CatchResidual),
                       SSB = sum(SSB),
                       VB = sum(VB),
                       TB = sum(TB),
                       Recruitment = sum(Recruitment),
                       U = Catch_AW / VB) %>%
      mutate(Region = "Total") %>%
      full_join(b0_total) %>%
      mutate(RelVBdata = VB / VB0now) %>%
      mutate(RelSSBdata = SSB / SSB0now) %>%
      mutate(RelTBdata = TB / TB0now) %>%
      mutate(RelVB = VB / VB0) %>%
      mutate(RelSSB = SSB / SSB0) %>%
      mutate(RelTB = TB / TB0)

    info1 <- rbind.data.frame(info1, tinfo1)
    info1 <- data.frame(info1)
  }

  ## status in last year of model estimates
 if(any(grepl("B0now_r", names(mcmc1)))){
  status_check <- info1 %>%
    filter(Year == max(years1)+1) %>%
    tidyr::pivot_longer(cols=c(Catch, Catch_AW, SSB,SSB0now,SSB0,RelSSB,RelSSBdata,VB,VB0now,VB0,RelVB,RelVBdata,TB,TB0now,TB0,RelTB, RelTBdata, U), names_to = "Variable", values_to = "Value") %>%
    group_by(RuleNum, Region, Variable) %>%
    summarise(P5 = quantile(Value, 0.05),
                     P50 = quantile(Value, 0.5),
                     Mean = mean(Value),
                     P95 = quantile(Value, 0.95))

  proj_check <- info1 %>%
    filter(Year == max(years1)+5) %>%
    tidyr::pivot_longer(cols=c(Catch, Catch_AW, SSB,SSB0now,SSB0,RelSSB,RelSSBdata,VB,VB0now,VB0,RelVB,RelVBdata,TB,TB0now,TB0,RelTB, RelTBdata, U), names_to = "Variable", values_to = "Value") %>%
    group_by(RuleNum, Region, Variable) %>%
    summarise(Proj_P5 = quantile(Value, 0.05),
                     Proj_P50 = quantile(Value, 0.5),
                     Proj_Mean = mean(Value),
                     Proj_P95 = quantile(Value, 0.95))
 } else {
  status_check <- info1 %>%
    filter(Year == max(years1)+1) %>%
    tidyr::pivot_longer(cols=c(Catch, Catch_AW, SSB,SSB0,RelSSB,VB,TB,TB0,RelTB, U), names_to = "Variable", values_to = "Value") %>%
    group_by(RuleNum, Region, Variable) %>%
    summarise(P5 = quantile(Value, 0.05),
                     P50 = quantile(Value, 0.5),
                     Mean = mean(Value),
                     P95 = quantile(Value, 0.95))

  proj_check <- info1 %>%
    filter(Year == max(years1)+5) %>%
    tidyr::pivot_longer(cols=c(Catch, Catch_AW, SSB,SSB0,RelSSB,VB,TB,TB0,RelTB, U), names_to = "Variable", values_to = "Value") %>%
    group_by(RuleNum, Region, Variable) %>%
    summarise(Proj_P5 = quantile(Value, 0.05),
                     Proj_P50 = quantile(Value, 0.5),
                     Proj_Mean = mean(Value),
                     Proj_P95 = quantile(Value, 0.95))
 }

  # write.csv(status_check, file.path(figure_dir, "Current_status_check.csv"))
  recdev <- mcmc1$par_rec_dev_ry
  ryears <- years[-c((length(years)-1):length(years))]
  dimnames(recdev) <- list("Iteration" = 1:n_iter1, "Region" = regions, "Year" = ryears)
  recdev2 <- reshape2::melt(recdev) %>%
    rename(Recruitment = value)
  recdev2$Region <- factor(recdev2$Region)

  r0 <- mcmc1$par_R0_r
  dimnames(r0) <- list("Iteration" = 1:n_iter1, "Region" = regions)
  r0 <- reshape2::melt(r0) %>%
  rename(R0 = value)
  r0$Region <- factor(r0$Region)


if(length(object) == 1){
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
  rule_type <- data.frame(RuleType1 = rules[,1], RuleNum = 1:n_rules) %>%
  mutate(RuleType = ifelse(RuleType1 == 1, "FixedCatch", "FixedF")) %>%
  select(-RuleType1)
  fleets <- c("SL","NSL")

  gc()
  
  cpue <- mcmc$mp_offset_cpue_jry
  dimnames(cpue) <- list("Iteration" = 1:n_iter, "RuleNum" = 1:n_rules, "Region" = regions, "Year" = pyears)
  cpue2 <- reshape2::melt(cpue, value.name = "CPUE")
  cpue2 <- tibble(cpue2)

  gc()

  slcatch <- mcmc$pred_catch_sl_jryt
  dimnames(slcatch) <- list("Iteration"=1:n_iter, "RuleNum"=1:dim(slcatch)[2], "Region"=regions, "Year"=pyears, "Season"=seasons)
  slcatch2 <- reshape2::melt(slcatch, value.name = "Catch") %>%
    group_by(Iteration, Year, Region, RuleNum) %>%
    summarise(Catch = sum(Catch)) %>%
    mutate("CatchType" = "SL")

  slcatch2_t <- reshape2::melt(slcatch, value.name = "Catch") %>%
    group_by(Iteration, Year, Region, Season, RuleNum) %>%
    summarise(Catch = sum(Catch)) %>%
    mutate("CatchType" = "SL")

  gc()

  nslcatch <- mcmc$pred_catch_nsl_jryt
  dimnames(nslcatch) <- list("Iteration"=1:n_iter, "RuleNum"=1:dim(nslcatch)[2], "Region"=regions, "Year"=pyears, "Season"=seasons)
  nslcatch2 <- reshape2::melt(nslcatch, value.name = "Catch") %>%
    group_by(Iteration, Year, Region, RuleNum) %>%
    summarise(Catch = sum(Catch)) %>%
    mutate("CatchType" = "NSL")

  nslcatch2_t <- reshape2::melt(nslcatch, value.name = "Catch") %>%
    group_by(Iteration, Year, Region, Season, RuleNum) %>%
    summarise(Catch = sum(Catch)) %>%
    mutate("CatchType" = "NSL")

  gc()

  pcatch <- rbind.data.frame(slcatch2, nslcatch2) %>%
    tidyr::pivot_wider(names_from = CatchType, values_from = Catch) %>%
    mutate(Catch = SL + NSL)

  pcatch_t <- rbind.data.frame(slcatch2_t, nslcatch2_t) %>%
    tidyr::pivot_wider(names_from = CatchType, values_from = Catch) %>%
    mutate(Catch = SL + NSL)
  rm(slcatch)
  rm(nslcatch)
  gc()

  catch <- pcatch
  catch_t <- pcatch_t %>%
    filter(Season == "AW") %>%
    ungroup() %>%
    select(-Season) %>%
    rename(SL_AW = SL, NSL_AW = NSL, Catch_AW = Catch)


  rm(pcatch)
  rm(pcatch_t)
  gc()


  vb <- mcmc$biomass_vulnref_AW_jyr
  dimnames(vb) <- list("Iteration" = 1:n_iter, "RuleNum" = 1:dim(vb)[2], "Year" = pyears, "Region" = regions)
  vb2 <- reshape2::melt(vb) %>%
    rename(VB = value)
  vb2$Region <- factor(vb2$Region)

  vb0now <- mcmc$B0now_r
  dimnames(vb0now) <- list("Iteration" = 1:n_iter, "Region" = regions2)
  vb0now <- reshape2::melt(vb0now) %>%
    rename(VB0now = value)
  vb0now$Region <- factor(vb0now$Region)

  VB0 <- mcmc$B0_r
  dimnames(VB0) <- list("Iteration" = 1:n_iter, "Region" = regions2)
  VB0 <- reshape2::melt(VB0) %>%
    rename(VB0 = value)
  VB0$Region <- factor(VB0$Region)

  gc()

  ssb <- mcmc$biomass_ssb_jyr
  dimnames(ssb) <- list("Iteration" = 1:n_iter, "RuleNum" = 1:dim(ssb)[2], "Year" = pyears, "Region" = regions)
  ssb2 <- reshape2::melt(ssb) %>% rename("SSB"=value)
  ssb2$Region <- factor(ssb2$Region)

  ssb0now <- mcmc$SSB0now_r
  dimnames(ssb0now) <- list("Iteration" = 1:n_iter, "Region" = regions2)
  ssb0now <- reshape2::melt(ssb0now) %>%
    rename(SSB0now=value)
  ssb0now$Region <- factor(ssb0now$Region)

  SSB0 <- mcmc$SSB0_r
  dimnames(SSB0) <- list("Iteration" = 1:n_iter, "Region" = regions2)
  SSB0 <- reshape2::melt(SSB0) %>%
    rename(SSB0=value)
  SSB0$Region <- factor(SSB0$Region)

  gc()

  tb <- mcmc$biomass_total_jytrs
  dimnames(tb) <- list("Iteration" = 1:n_iter, "RuleNum" = 1:dim(ssb)[2], "Year" = pyears, "Season" = seasons, "Region" = regions, "Sex" = c(sex, "Total"))
  tb2 <- reshape2::melt(tb) %>%
    rename("TB"=value) %>%
    filter(Sex == "Total") %>%
    select(-Sex) %>%
    filter(Season == "AW") %>%
    select(-Season) %>%
    group_by(Iteration, RuleNum, Year, Region) %>%
    summarise(TB = sum(TB))
  tb2$Region <- factor(tb2$Region)

  tb0now <- mcmc$Btot0now_r
  dimnames(tb0now) <- list("Iteration" = 1:n_iter, "Region" = regions2)
  tb0now <- reshape2::melt(tb0now) %>%
    rename(TB0now=value)
  tb0now$Region <- factor(tb0now$Region)

  TB0 <- mcmc$Btot0_r
  dimnames(TB0) <- list("Iteration" = 1:n_iter, "Region" = regions2)
  TB0 <- reshape2::melt(TB0) %>%
    rename(TB0=value)
  TB0$Region <- factor(TB0$Region)

  gc()


  rec <- mcmc$recruits_ry
  ryears <- years[-c((length(years)-1):length(years))]
  dimnames(rec) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = pyears)
  rec2 <- reshape2::melt(rec) %>%
    rename(Recruitment = value)
  rec2$Region <- factor(rec2$Region)

  rec3 <- rec2 %>%
  group_by(Region, Year) %>%
  summarise(P5 = quantile(Recruitment, 0.05),
            P50 = quantile(Recruitment, 0.5),
            P95 = quantile(Recruitment, 0.95))

  recdev3 <- recdev2 %>%
  left_join(r0) %>%
  filter(Year %in% min(data$data_lf_year_i):(max(years)-2)) %>%
  group_by(Region) %>%
  summarise(R0 = median(R0), DataYears = median(R0) * exp(median(Recruitment) - 0.5 * data$fpar_rec_sd ^ 2))

  recdev4 <- recdev2 %>%
  left_join(r0) %>%
  filter(Year %in% (max(years) - 9 - 2):(max(years)-2)) %>%
  group_by(Region) %>%
  summarise(Last10Years = median(R0) * exp(median(Recruitment) - 0.5 * data$fpar_rec_sd ^ 2))


  rec3 <- rec3 %>%
  left_join(recdev3) %>%
  left_join(recdev4) %>%
  tidyr::pivot_longer(R0:Last10Years, names_to = "Type", values_to = "Average")


  gc()

  relssb <- inner_join(ssb2, ssb0now) %>%
    left_join(SSB0) %>%
    mutate(RelSSBdata = SSB/SSB0now,
                  RelSSB = SSB/SSB0)
  gc()

  relvb <- inner_join(vb2, vb0now) %>%
    left_join(VB0) %>%
    mutate(RelVBdata = VB/VB0now,
                  RelVB = VB/VB0)
  gc()

  reltb <- inner_join(tb2, tb0now) %>%
    left_join(TB0) %>%
    mutate(RelTBdata = TB/TB0now,
                  RelTB = TB/TB0)
  gc()


  relb <- full_join(relssb, relvb)
  gc()
  relb1 <- full_join(relb, reltb)
  gc()
  # relb2 <- full_join(relb1, rec2)
  # gc()

  rm(relssb)
  rm(relvb)
  rm(reltb)

  catch$Region <- factor(catch$Region)
  catch_t$Region <- factor(catch_t$Region)
  info <- full_join(catch, relb1) %>%
    full_join(catch_t) %>%
    select(-c(SL_AW, NSL_AW))
  gc()

  cpue2$Region <- factor(cpue2$Region)
  # projF2$Region <- factor(projF2$Region)
  info <- full_join(info, cpue2)
  info$Region <- factor(info$Region)
  gc()

  # infox <- full_join(info, projF2)
  infox <- info
  rm(info)
  gc()

  info <- infox %>%
    filter(Region %in% regions) %>%
    mutate(Region = paste0("Region ", Region)) %>%
    mutate(U = Catch_AW / VB)
  gc()


  rm(relb)
  # rm(relb2)
  rm(catch)
  gc()

  rulepar <- paste0("par",1:10)
  colnames(rules) <- rulepar
  ruledf <- data.frame(RuleNum = 1:nrow(rules), rules) %>%
    mutate(RuleType = ifelse(par1 == 0, "FixedF", ifelse(par1 == 1, "FixedCatch", NA)))


  sub <- catch %>% left_join(rule_type) %>% filter(Iteration == 1)
  p <- ggplot(sub %>% filter(Iteration == 1)) +
    geom_line(aes(x = Year, y = Catch, color = factor(RuleNum))) +
    facet_grid(RuleType~Region, scales = "free_y") +
    theme_bw(base_size = 20)
  ggsave(file.path(figure_dir, "Catch_check.png"), p, height = 10, width = 15)

  p <- ggplot(rec3) +
    geom_ribbon(aes(x = Year, ymin = P5, ymax = P95), alpha = 0.3) +
    geom_line(aes(x = Year, y = P50)) +
    geom_vline(aes(xintercept = min(data1$data_lf_year_i)), lty = 2) +
    geom_vline(aes(xintercept = projyears[1]), lty = 2) +
    ylab("Recruitment") +
    geom_line(aes(x = Year, y = Average, color = Type), lwd = 0.9) +
    guides(color=guide_legend(title="Compare average\nrecruitment")) +
    facet_wrap(~Region) +
    expand_limits(y = 0) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    scale_color_brewer(palette = "Set1") +
    theme_bw(base_size = 20)
  ggsave(file.path(figure_dir, "Recruitment_proj.png"), p, height = 8, width = 15)

    cutyears <- (data$last_proj_yr-19):(data$last_proj_yr)
}
  

if(length(object) > 1){

  data_list <- lapply(1:length(object), function(x){
    out <- object[[x]]@data
    return(out)
  })
  mcmc_list <- lapply(1:length(object), function(x){
    out <- object[[x]]@mcmc
    return(out)
  })
  years <- data_list[[1]]$first_yr:data_list[[1]]$last_yr
  pyears <- data_list[[1]]$first_yr:data_list[[1]]$last_proj_yr
  projyears <- (data_list[[1]]$first_proj_yr):data_list[[1]]$last_proj_yr
  n_iter <- nrow(mcmc_list[[1]][[1]])
  regions <- 1:data_list[[1]]$n_area
  seasons <- c("AW","SS")
  if(length(regions) > 1) regions2 <- c(regions, "Total")
  if(length(regions) == 1) regions2 <- regions
  sex <- c("Male","Immature female","Mature female")
  nrules_list <- sapply(1:length(object), function(x){ nrow(data_list[[x]]$mp_rule_parameters)})
  rules_list <- lapply(1:length(object), function(x){
    out <- data.frame(data_list[[x]]$mp_rule_parameters) %>%
      mutate(N = x) %>%
      mutate(RuleNum = 1:nrow(.))
    return(out)
  })
  rules <- do.call(rbind, rules_list)
  n_rules <- nrow(rules)
  rule_type <- data.frame(RuleType1 = rules[,1], RuleNumAll = 1:n_rules, N = rules[,"N"], RuleNum = rules[,"RuleNum"]) %>%
  mutate(RuleType = ifelse(RuleType1 == 1, "FixedCatch", "FixedF")) %>%
  select(-RuleType1)
  fleets <- c("SL","NSL")

  gc()
  
  cpue_list <- lapply(1:length(object), function(x){
    cpue <- mcmc_list[[x]]$mp_offset_cpue_jry
    dimnames(cpue) <- list("Iteration" = 1:n_iter, "RuleNum" = 1:nrules_list[x], "Region" = regions, "Year" = pyears)
    cpue2 <- reshape2::melt(cpue, value.name = "CPUE")
    cpue2 <- tibble(cpue2) %>%
      mutate(N = x) %>%
      left_join(rule_type) %>%
      select(-c(RuleNum,N,RuleType)) %>%
      rename(RuleNum = RuleNumAll)
    return(cpue2)
  })
  cpue2 <- do.call(rbind, cpue_list)

  gc()

  slcatch_list <- lapply(1:length(object), function(x){
    slcatch <- mcmc_list[[x]]$pred_catch_sl_jryt
    dimnames(slcatch) <- list("Iteration"=1:n_iter, "RuleNum"=1:dim(slcatch)[2], "Region"=regions, "Year"=pyears, "Season"=seasons)
    slcatch2 <- reshape2::melt(slcatch, value.name = "Catch") %>%
      group_by(Iteration, Year, Region, RuleNum) %>%
      summarise(Catch = sum(Catch)) %>%
      mutate("CatchType" = "SL")  %>%
      mutate(N = x) %>%
      left_join(rule_type) %>%
      select(-c(RuleNum, N, RuleType)) %>%
      rename(RuleNum = RuleNumAll)
    return(slcatch2)
  })
  slcatch2 <- do.call(rbind, slcatch_list)

  slcatcht_list <- lapply(1:length(object), function(x){
    slcatch <- mcmc_list[[x]]$pred_catch_sl_jryt
    dimnames(slcatch) <- list("Iteration"=1:n_iter, "RuleNum"=1:dim(slcatch)[2], "Region"=regions, "Year"=pyears, "Season"=seasons)
    slcatch2_t <- reshape2::melt(slcatch, value.name = "Catch") %>%
      group_by(Iteration, Year, Region, Season, RuleNum) %>%
      summarise(Catch = sum(Catch)) %>%
      mutate("CatchType" = "SL")  %>%
      mutate(N = x) %>%
      left_join(rule_type) %>%
      select(-c(RuleNum, N, RuleType)) %>%
      rename(RuleNum = RuleNumAll)
    return(slcatch2_t)
  })
  slcatch2_t <- do.call(rbind, slcatcht_list)

  gc()

  nslcatch_list <- lapply(1:length(object), function(x){
    nslcatch <- mcmc_list[[x]]$pred_catch_nsl_jryt
    dimnames(nslcatch) <- list("Iteration"=1:n_iter, "RuleNum"=1:dim(nslcatch)[2], "Region"=regions, "Year"=pyears, "Season"=seasons)
    nslcatch2 <- reshape2::melt(nslcatch, value.name = "Catch") %>%
      group_by(Iteration, Year, Region, RuleNum) %>%
      summarise(Catch = sum(Catch)) %>%
      mutate("CatchType" = "NSL") %>%
      mutate(N = x) %>%
      left_join(rule_type) %>%
      select(-c(RuleNum, N, RuleType)) %>%
      rename(RuleNum = RuleNumAll)
    return(nslcatch2)
  })
  nslcatch2 <- do.call(rbind, nslcatch_list)

  nslcatcht_list <- lapply(1:length(object), function(x){
    nslcatch <- mcmc_list[[x]]$pred_catch_nsl_jryt
    dimnames(nslcatch) <- list("Iteration"=1:n_iter, "RuleNum"=1:dim(nslcatch)[2], "Region"=regions, "Year"=pyears, "Season"=seasons)
    nslcatch2_t <- reshape2::melt(nslcatch, value.name = "Catch") %>%
      group_by(Iteration, Year, Region, Season, RuleNum) %>%
      summarise(Catch = sum(Catch)) %>%
      mutate("CatchType" = "NSL") %>%
      mutate(N = x) %>%
      left_join(rule_type) %>%
      select(-c(RuleNum, N, RuleType)) %>%
      rename(RuleNum = RuleNumAll)
    return(nslcatch2_t)
  })
  nslcatch2_t <- do.call(rbind, nslcatcht_list)

  gc()

  pcatch <- rbind.data.frame(slcatch2, nslcatch2) %>%
    tidyr::pivot_wider(names_from = CatchType, values_from = Catch) %>%
    mutate(Catch = SL + NSL)

  pcatch_t <- rbind.data.frame(slcatch2_t, nslcatch2_t) %>%
    tidyr::pivot_wider(names_from = CatchType, values_from = Catch) %>%
    mutate(Catch = SL + NSL)

  gc()

  catch <- pcatch
  catch_t <- pcatch_t %>%
    filter(Season == "AW") %>%
    ungroup() %>%
    select(-Season) %>%
    rename(SL_AW = SL, NSL_AW = NSL, Catch_AW = Catch)


  rm(pcatch)
  rm(pcatch_t)
  gc()

  vb_list <- lapply(1:length(object), function(x){
    vb <- mcmc_list[[x]]$biomass_vulnref_AW_jyr
    dimnames(vb) <- list("Iteration" = 1:n_iter, "RuleNum" = 1:dim(vb)[2], "Year" = pyears, "Region" = regions)
    vb2 <- reshape2::melt(vb) %>%
      rename(VB = value) %>%
      mutate(N = x) %>%
      left_join(rule_type) %>%
      select(-c(RuleNum, N, RuleType)) %>%
      rename(RuleNum = RuleNumAll)
    vb2$Region <- factor(vb2$Region)
    return(vb2)
  })
  vb2 <- do.call(rbind, vb_list)


    vb0now <- mcmc_list[[1]]$B0now_r
    dimnames(vb0now) <- list("Iteration" = 1:n_iter, "Region" = regions2)
    vb0now <- reshape2::melt(vb0now) %>%
      rename(VB0now = value) 
    vb0now$Region <- factor(vb0now$Region)


    VB0 <- mcmc_list[[1]]$B0_r
    dimnames(VB0) <- list("Iteration" = 1:n_iter, "Region" = regions2)
    VB0 <- reshape2::melt(VB0) %>%
      rename(VB0 = value)
    VB0$Region <- factor(VB0$Region)


  gc()

  ssb_list <- lapply(1:length(object), function(x){
   ssb <- mcmc_list[[x]]$biomass_ssb_jyr
   dimnames(ssb) <- list("Iteration" = 1:n_iter, "RuleNum" = 1:dim(ssb)[2], "Year" = pyears, "Region" = regions)
   ssb2 <- reshape2::melt(ssb) %>% rename("SSB"=value)  %>%
      mutate(N = x) %>%
      left_join(rule_type) %>%
      select(-c(RuleNum, N, RuleType)) %>%
      rename(RuleNum = RuleNumAll)
   ssb2$Region <- factor(ssb2$Region)   
   return(ssb2)
  })
  ssb2 <- do.call(rbind, ssb_list)

   ssb0now <- mcmc_list[[1]]$SSB0now_r
   dimnames(ssb0now) <- list("Iteration" = 1:n_iter, "Region" = regions2)
   ssb0now <- reshape2::melt(ssb0now) %>%
     rename(SSB0now=value)
  ssb0now$Region <- factor(ssb0now$Region)

    SSB0 <- mcmc_list[[1]]$SSB0_r
    dimnames(SSB0) <- list("Iteration" = 1:n_iter, "Region" = regions2)
    SSB0 <- reshape2::melt(SSB0) %>%
      rename(SSB0=value)
    SSB0$Region <- factor(SSB0$Region)



  gc()


  tb_list <- lapply(1:length(object), function(x){
    tb <- mcmc_list[[x]]$biomass_total_jytrs
    dimnames(tb) <- list("Iteration" = 1:n_iter, "RuleNum" = 1:dim(tb)[2], "Year" = pyears, "Season" = seasons, "Region" = regions, "Sex" = c(sex, "Total"))
    tb2 <- reshape2::melt(tb) %>%
      rename("TB"=value) %>%
      filter(Sex == "Total") %>%
      select(-Sex) %>%
      filter(Season == "AW") %>%
      select(-Season) %>%
      group_by(Iteration, RuleNum, Year, Region) %>%
      summarise(TB = sum(TB)) %>%
      ungroup() %>%
      mutate(N = x) %>%
      left_join(rule_type) %>%
      select(-c(RuleNum, N, RuleType)) %>%
      rename(RuleNum = RuleNumAll)
    tb2$Region <- factor(tb2$Region)
    return(tb2)
  })
  tb2 <- do.call(rbind, tb_list)


    tb0now <- mcmc_list[[1]]$Btot0now_r
    dimnames(tb0now) <- list("Iteration" = 1:n_iter, "Region" = regions2)
    tb0now <- reshape2::melt(tb0now) %>%
      rename(TB0now=value)
    tb0now$Region <- factor(tb0now$Region)


    TB0 <- mcmc_list[[1]]$Btot0_r
    dimnames(TB0) <- list("Iteration" = 1:n_iter, "Region" = regions2)
    TB0 <- reshape2::melt(TB0) %>%
      rename(TB0=value)
    TB0$Region <- factor(TB0$Region)   



  gc()

   rec <- mcmc_list[[1]]$recruits_ry
  ryears <- years[-c((length(years)-1):length(years))]
  dimnames(rec) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = pyears)
  rec2 <- reshape2::melt(rec) %>%
    rename(Recruitment = value)
  rec2$Region <- factor(rec2$Region)

  rec3 <- rec2 %>%
  group_by(Region, Year) %>%
  summarise(P5 = quantile(Recruitment, 0.05),
            P50 = quantile(Recruitment, 0.5),
            P95 = quantile(Recruitment, 0.95))

  recdev3 <- recdev2 %>%
  left_join(r0) %>%
  filter(Year %in% min(data_list[[1]]$data_lf_year_i):(max(years)-2)) %>%
  group_by(Region) %>%
  summarise(R0 = median(R0), DataYears = median(R0) * exp(median(Recruitment) - 0.5 * data_list[[1]]$fpar_rec_sd ^ 2))

  recdev4 <- recdev2 %>%
  left_join(r0) %>%
  filter(Year %in% (max(years) - 9 - 2):(max(years)-2)) %>%
  group_by(Region) %>%
  summarise(Last10Years = median(R0) * exp(median(Recruitment) - 0.5 * data_list[[1]]$fpar_rec_sd ^ 2))

  rec3 <- rec3 %>%
  left_join(recdev3) %>%
  left_join(recdev4) %>%
  tidyr::pivot_longer(R0:Last10Years, names_to = "Type", values_to = "Average")

  gc()

  relssb <- inner_join(ssb2, ssb0now) %>%
    left_join(SSB0) %>%
    mutate(RelSSBdata = SSB/SSB0now,
                  RelSSB = SSB/SSB0)
  gc()

  relvb <- inner_join(vb2, vb0now) %>%
    left_join(VB0) %>%
    mutate(RelVBdata = VB/VB0now,
                  RelVB = VB/VB0)
  gc()

  reltb <- inner_join(tb2, tb0now) %>%
    left_join(TB0) %>%
    mutate(RelTBdata = TB/TB0now,
                  RelTB = TB/TB0)
  gc()


  relb <- full_join(relssb, relvb)
  gc()
  relb1 <- full_join(relb, reltb)
  gc()
  # relb2 <- full_join(relb1, rec2)
  # gc()

  rm(relssb)
  rm(relvb)
  rm(reltb)

  catch$Region <- factor(catch$Region)
  catch_t$Region <- factor(catch_t$Region)
  info <- full_join(catch, relb1) %>%
    full_join(catch_t) %>%
    select(-c(SL_AW, NSL_AW))
  gc()

  cpue2$Region <- factor(cpue2$Region)
  # projF2$Region <- factor(projF2$Region)
  info <- full_join(info, cpue2)
  info$Region <- factor(info$Region)
  gc()

  # infox <- full_join(info, projF2)
  infox <- info
  rm(info)
  gc()

  info <- infox %>%
    filter(Region %in% regions) %>%
    mutate(Region = paste0("Region ", Region)) %>%
    mutate(U = Catch_AW / VB)
  gc()


  rm(relb)
  # rm(relb2)
  # rm(catch)
  gc()

  rulepar <- paste0("par",1:10)
  colnames(rules) <- c(rulepar,"N","RuleNumInit")
  ruledf <- data.frame(RuleNum = 1:nrow(rules), rules) %>%
    mutate(RuleType = ifelse(par1 == 0, "FixedF", ifelse(par1 == 1, "FixedCatch", NA)))

  sub <- catch %>% 
      left_join(rule_type %>% select(-c(RuleNum,N)) %>% rename(RuleNum = RuleNumAll)) %>% 
      filter(Iteration == 1)
  p <- ggplot(sub %>% filter(Iteration == 1)) +
    geom_line(aes(x = Year, y = Catch, color = factor(RuleNum))) +
    facet_grid(RuleType~Region, scales = "free_y") +
    theme_bw(base_size = 20)
  ggsave(file.path(figure_dir, "Catch_check.png"), p, height = 10, width = 15)

  p <- ggplot(rec3) +
    geom_ribbon(aes(x = Year, ymin = P5, ymax = P95), alpha = 0.3) +
    geom_line(aes(x = Year, y = P50)) +
    geom_vline(aes(xintercept = min(data1$data_lf_year_i)), lty = 2) +
    geom_vline(aes(xintercept = projyears[1]), lty = 2) +
    ylab("Recruitment") +
    geom_line(aes(x = Year, y = Average, color = Type), lwd = 0.9) +
    guides(color=guide_legend(title="Compare average\nrecruitment")) +
    facet_wrap(~Region) +
    expand_limits(y = 0) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    scale_color_brewer(palette = "Set1") +
    theme_bw(base_size = 20)
  ggsave(file.path(figure_dir, "Recruitment_proj.png"), p, height = 8, width = 15)

    cutyears <- (data_list[[1]]$last_proj_yr-19):(data_list[[1]]$last_proj_yr)
}




  ##########################
  ## projection time series
  ##########################
    syears <- projyears[5]
    pinfo <- info %>% filter(Year %in% (max(years)+1):(min(cutyears)-1))
    cinfo <- info %>% filter(Year %in% cutyears)
    sinfo <- info %>% filter(Year %in% syears)
    dinfo <- info1 %>% ungroup() %>% filter(Year %in% years) %>% filter(RuleNum == 1) %>% select(-c(RuleNum))

    gc()
  #######################################
  ## risk constraints + flags for catch
  #######################################
    constraints <- cinfo %>%
      left_join(ruledf) %>%
      filter(Region != "Total") %>%
      group_by(Region, RuleNum) %>%
      summarise(CV = sd(Catch)/mean(Catch),
                      Prisk = length(which(RelSSBdata <= 0.2))/length(RelSSBdata),
                      RiskConstraint = ifelse(Prisk >= 0.05, 1, 0),
                      ExpectedCatchSL = sum(par2),
                      ObsCatchSL = sum(SL),
                      Pcatch = length(which(SL < 0.99 * par2))/length(SL),
                      AvgTotalCatch = sum(Catch)/max(Iteration),
                      Catch5 = quantile(Catch,0.05),
                      Catch95 = quantile(Catch,0.95)) %>%
      left_join(ruledf) %>%
      # mutate(ExpectedCatchSL = replace(ExpectedCatchSL, RuleType != "FixedCatch", 0)) %>%
      mutate(CatchConstraint = ifelse(RuleType == "FixedCatch" & Pcatch > 0.05, 1, 0))
      # mutate(CatchConstraint = ifelse(RuleType == "FixedCatch" & (Catch95-Catch5)>1, 1, 0))
    # constraints <- unique(constraints)

      gc()

  #####################
  ## start summarising
  #####################
    summary <- cinfo %>%
      filter(Region != "Total") %>%
      tidyr::pivot_longer(cols=c(Catch,Catch_AW, CPUE,SSB,SSB0now,SSB0,RelSSB,RelSSBdata,VB,VB0now,VB0,RelVB,RelVBdata,TB,TB0,TB0now, RelTB, RelTBdata, U), names_to = "Variable", values_to = "Value") %>% #CPUE,F)
      group_by(Region, RuleNum, Variable) %>%
      summarise(P5 = quantile(Value, 0.05),
                       P50 = quantile(Value, 0.5),
                       Mean = mean(Value),
                       P95 = quantile(Value, 0.95))

    gc()

    output <- full_join(constraints, summary)

    ## identify and label fixed catch rules
    output2 <- output %>%
      left_join(ruledf) #%>%
      # mutate(CatchConstraint = replace(CatchConstraint, which(RuleType != "FixedCatch"), 0))

    output2$Constraint <- sapply(1:nrow(output2), function(x){
      if(output2$CatchConstraint[x] == 1 & output2$RiskConstraint[x] == 1) out <- "Fail: Risk + Catch"
      if(output2$CatchConstraint[x] == 1 & output2$RiskConstraint[x] == 0) out <- "Fail: Catch"
      if(output2$CatchConstraint[x] == 0 & output2$RiskConstraint[x] == 1) out <- "Fail: Risk" #output2$CatchConstraint[x] == 0 &
      if(output2$CatchConstraint[x] == 0 & output2$RiskConstraint[x] == 0) out <- "Pass"
      return(out)
    })
    output2$Constraint <- factor(output2$Constraint, levels = c("Pass", "Fail: Catch", "Fail: Risk", "Fail: Risk + Catch"))

    gc()

    find_max1 <- output2 %>%
      filter(Variable == "Catch") %>%
      group_by(Region, RuleType, RuleNum, Constraint) %>%
      summarise(P50 = sum(P50),
                       Mean = sum(Mean)) %>%
      group_by(Region, RuleType, Constraint) %>%
      # filter(P50 == max(P50))
      filter(Mean == max(Mean))

    find_msy1 <- find_max1 %>% filter(Constraint == "Pass")

    msy_info1 <- output2 %>% right_join(find_msy1 %>% select(-P50))

  ## filter MP rules where CV < CV(MSY Fixed F)
  CVmaxF <- min(as.numeric(unlist(msy_info1[which(msy_info1$RuleType == "FixedF"),"CV"])))
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

  gc()

  output2$Constraint <- sapply(1:nrow(output2), function(x){
      if(output2$CatchConstraint[x] == 1 & output2$RiskConstraint[x] == 1) out <- "Fail: Risk + Catch"
      if(output2$CatchConstraint[x] == 1 & output2$RiskConstraint[x] == 0) out <- "Fail: Catch"
      if(output2$CatchConstraint[x] == 0 & output2$RiskConstraint[x] == 1) out <- "Fail: Risk" #output2$CatchConstraint[x] == 0 &
      if(output2$CatchConstraint[x] == 0 & output2$RiskConstraint[x] == 0) out <- "Pass"
    return(out)
  })
  write.csv(output2, file.path(figure_dir, "Summary_byVariable.csv"))

  gc()

  find_max <- output2 %>%
    filter(Variable == "Catch") %>%
    group_by(Region, RuleType, RuleNum, Constraint, CVConstraint) %>%
    summarise(P50 = sum(P50),
                     Mean = sum(Mean)) %>%
    group_by(Region, RuleType, Constraint, CVConstraint) %>%
    # filter(P50 == max(P50))
    filter(Mean == max(Mean))

  ## examples from find_max
  reg <- unique(find_max$Region)
  find_max_sub <- lapply(1:length(reg), function(x){
    sub <- find_max %>% filter(Region == reg[x])
    types <- unique(sub$RuleType)
    byType <- lapply(1:length(types), function(y){
      sub2 <- sub %>% filter(RuleType == types[y])
      con <- unique(sub2$Constraint)
      con2 <- unique(sub2$CVConstraint)
      byCon <- lapply(1:length(con), function(z){
        sub3 <- sub2 %>% filter(Constraint == con[z])
        if(types[y] == "CPUE-based"){
          byCon2 <- lapply(1:length(con2), function(zz){
            sub4 <- sub3 %>% filter(CVConstraint == con2[zz])
            if(nrow(sub4) == 1) out <- sub4
            if(nrow(sub4) > 1){
              subinfo <- output2 %>% right_join(sub4)
              out <- sub4[which(subinfo$AvgTotalCatch == max(subinfo$AvgTotalCatch)),]
            }
            return(out)
          })
          out <- do.call(rbind,byCon2)
        } else {
          if(nrow(sub3) == 1) out <- sub3
          if(nrow(sub3) > 1){
            subinfo <- output2 %>% right_join(sub3)
            out <- sub3[which(subinfo$AvgTotalCatch == max(subinfo$AvgTotalCatch)),]
          }
        }
        return(out)
      })
      byCon <- do.call(rbind, byCon)
      return(byCon)
    })
    byType <- do.call(rbind, byType)
    return(byType)
  })
  find_max <- do.call(rbind, find_max_sub)

  gc()

  ## remove duplicates of MSY --- choose rule with higher average total catch
  max_info <- output2 %>% right_join(find_max %>% select(-c(P50,Mean)))
  # write.csv(max_info, file.path(figure_dir, "MAX_info.csv"))

  if(length(regions) > 1){
    max_info2 <- cinfo %>%
      right_join(unique(max_info %>% select(Region, RuleNum, Constraint, CVConstraint))) %>%
      left_join(ruledf)
    gc()
    max_info3 <- data.frame(max_info2) %>%
      group_by(Iteration, Year, RuleType, Constraint, CVConstraint) %>%
      summarise(Catch = sum(Catch),
                       Catch_AW = sum(Catch_AW),
                       VB = sum(VB),
                       SSB = sum(SSB),
                       TB = sum(TB),
                       CPUE = mean(CPUE),
                       U = Catch_AW / VB) %>%
      left_join(vb0now %>% filter(Region == "Total")) %>%
      left_join(ssb0now %>% filter(Region == "Total")) %>%
      left_join(tb0now %>% filter(Region == "Total")) %>%
      left_join(VB0 %>% filter(Region == "Total")) %>%
      left_join(SSB0 %>% filter(Region == "Total")) %>%
      left_join(TB0 %>% filter(Region == "Total")) %>%
      mutate(RelVBdata = VB / VB0now) %>%
      mutate(RelSSBdata = SSB / SSB0now) %>%
      mutate(RelTBdata = TB / TB0now) %>%
      mutate(RelVB = VB / VB0) %>%
      mutate(RelSSB = SSB / SSB0) %>%
      mutate(RelTB = TB / TB0) %>%
      tidyr::drop_na()  %>%
      tidyr::pivot_longer(cols = c(Catch, CPUE, VB, SSB, TB, VB0now, SSB0now, TB0now, VB0, SSB0, TB0, RelVBdata, RelSSBdata, RelTBdata, RelVB, RelSSB, RelTB), names_to = "Variable", values_to = "Value") %>%
      group_by(RuleType, Constraint, CVConstraint, Variable) %>%
      summarise(P5 = quantile(Value, 0.05),
                       P50 = quantile(Value, 0.5),
                       Mean = mean(Value),
                       P95 = quantile(Value, 0.95)) %>%
      mutate(Region = "Total")
    # write.csv(max_info3, file.path(figure_dir, "MAX_info_total.csv"))
  }

  #####################################################
  ## MSY info
  find_msy <- find_max %>% filter(Constraint == "Pass")
  ruletypes <- unique(find_msy$RuleType)
  check <- lapply(1:length(regions), function(x){
    check2 <- lapply(1:length(ruletypes), function(y){
      sub <- find_msy %>% filter(Region == paste0("Region ", regions[x])) %>% filter(RuleType == ruletypes[y])
      if(ruletypes[y] == "CPUE-based"){
        ncon <- unique(sub$CVConstraint)
        if(nrow(sub) == length(ncon)) return(sub)
        if(nrow(sub) > length(ncon)){
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

  msy_info <- output2 %>% right_join(find_msy %>% select(-c(P50,Mean)))
  # write.csv(msy_info, file.path(figure_dir, "MSY_info.csv"))

  average_info <- cinfo %>%
    right_join(find_msy %>% select(-c(P50,Mean))) %>%
    filter(RuleType %in% c("FixedCatch", "FixedF")) %>%
    # tidyr::pivot_longer(cols = c(Catch,VB,TB,SSB,SSB0now, TB0now, VB0now, RelSSBdata, RelTBdata, RelVBdata), names_to = "Variable", values_to = "Value") %>%
    select(Iteration, Year, Region, RuleType, unique(summary$Variable))



  if(length(regions) > 1){
    average_info_t <- average_info %>%
      group_by(Iteration, Year, RuleType) %>%
      summarise(Catch = sum(Catch),
                       Catch_AW = sum(Catch_AW),
                       VB = sum(VB),
                       SSB = sum(SSB),
                       TB = sum(TB),
                       CPUE = mean(CPUE),
                       U = Catch_AW / VB) %>%
      left_join(vb0now %>% filter(Region == "Total")) %>%
      left_join(ssb0now %>% filter(Region == "Total")) %>%
      left_join(tb0now %>% filter(Region == "Total")) %>%
      left_join(VB0 %>% filter(Region == "Total")) %>%
      left_join(SSB0 %>% filter(Region == "Total")) %>%
      left_join(TB0 %>% filter(Region == "Total")) %>%
      mutate(RelSSBdata = SSB / SSB0now) %>%
      mutate(RelTBdata = TB / TB0now) %>%
      mutate(RelVBdata = VB / VB0now) %>%
      mutate(RelSSB = SSB / SSB0) %>%
      mutate(RelTB = TB / TB0) %>%
      mutate(RelVB = VB / VB0)

    average_info <- full_join(average_info, average_info_t)
  }

  average_sum <- average_info %>%
    tidyr::pivot_longer(cols = c(unique(summary$Variable)), names_to = "Variable", values_to = "Value") %>%
    group_by(Region, Variable) %>%
    summarise(P5 = quantile(Value, 0.05),
                     P50 = quantile(Value, 0.5),
                     Mean = mean(Value),
                     P95 = quantile(Value, 0.95)) %>%
    mutate(RuleType = "Average")

  average_info2 <- average_info %>%
    group_by(Iteration, Year, Region) %>%
    summarise(VB = mean(VB)) %>%
    mutate(RuleType = "Average") %>%
    full_join(average_info %>% select(Iteration, Year, RuleType, Region, VB))
  average_info2$RuleType <- factor(average_info2$RuleType, levels = c("Average", "FixedCatch", "FixedF"))


  rule_sum <- average_info %>%
    tidyr::pivot_longer(cols = c(unique(summary$Variable)), names_to = "Variable", values_to = "Value") %>%
    group_by(Region, Variable, RuleType) %>%
    summarise(P5 = quantile(Value, 0.05),
                     P50 = quantile(Value, 0.5),
                     Mean = mean(Value),
                     P95 = quantile(Value, 0.95))

  sum <- full_join(average_sum, rule_sum)
  # write.csv(sum, file.path(figure_dir, "Summary_reference_average.csv"))

  p <- ggplot(average_info2) +
    geom_density(aes(x = VB, fill = RuleType), alpha = 0.5) +
    geom_vline(data = sum %>% filter(RuleType == "Average", Variable == "VB"), aes(xintercept = Mean, color = RuleType), lwd = 1.5) +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    guides(color = FALSE) +
    expand_limits(y = 0) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    xlab("AW adjusted vulnerable biomass (B; tonnes)") + ylab("Density") +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    p <- p + facet_grid(Region~., scales = "free_x")
    ggsave(file.path(figure_dir, "VB_Distributions.png"), p, height = 8, width = 10)

  } else {
    ggsave(file.path(figure_dir, "VB_Distributions.png"), p, height = 8, width = 10)
  }


  ## last assessment year
  curr <- info1 %>%
    filter(RuleNum == 1) %>%
    select(-RuleNum) %>%
    filter(Year == max(years)+1) %>%
    tidyr::pivot_longer(cols = c(unique(status_check$Variable)), names_to = "Variable", values_to = "Value") %>%
    ungroup() %>%
    select(Region, Variable, Value)  %>%
    rename(Current = Value) %>%
    left_join(sum %>% select(Region, RuleType, Variable, Mean, P50)) %>%
    group_by(Region, Variable, RuleType) %>%
    summarise(P_above_mean = length(which(Current >= Mean))/length(Current),
                     P_above_med = length(which(Current >= P50))/length(Current))

  proj <- info1 %>%
    filter(RuleNum == 1) %>%
    select(-RuleNum) %>%
    filter(Year == max(years)+5) %>%
    tidyr::pivot_longer(cols = c(unique(status_check$Variable)), names_to = "Variable", values_to = "Value") %>%
    ungroup() %>%
    select(Region, Variable, Value)  %>%
    rename(Proj = Value) %>%
    left_join(sum %>% select(Region, RuleType, Variable, Mean, P50)) %>%
    group_by(Region, Variable, RuleType) %>%
    summarise(P_above_mean_proj = length(which(Proj >= Mean))/length(Proj),
                     P_above_med_proj = length(which(Proj >= P50))/length(Proj))

  # write.csv(curr, file.path(figure_dir, "P_above_ref.csv"))

  # if(any(grepl("B0now_r", names(mcmc1)))){
  #  curr <- pinfo %>%
  #   filter(Year == max(years)+1) %>%
  #   tidyr::pivot_longer(cols = c(unique(summary$Variable)), names_to = "Variable", values_to = "Value") %>%
  #   ungroup() %>%
  #   select(Region, Variable, Value) %>%
  #   rename(Current = Value) %>%
  #   left_join(sum %>% select(Region, RuleType, Variable, Mean, P50)) %>%
  #   group_by(Region, Variable, RuleType) %>%
  #   summarise(P_above_mean = length(which(Current >= Mean))/length(Current),
  #                    P_above_med = length(which(Current >= P50))/length(Current))
  # }
    sum_curr <- full_join(sum, curr) %>%
      full_join(proj)
    status_check <- status_check %>%
      ungroup() %>%
      filter(RuleNum == 1) %>%
      select(-RuleNum) %>%
      rename(Current_P5 = P5, Current_P50 = P50, Current_Mean = Mean, Current_P95 = P95)
    proj_check <- proj_check %>%
      ungroup() %>%
      filter(RuleNum == 1) %>%
      select(-RuleNum)

    sum_status <- full_join(sum_curr, status_check) %>%
    left_join(proj_check)

    msy_info_sub <- msy_info %>%
      select(Region, RuleNum, CV, Prisk, Pcatch, AvgTotalCatch, par2, RuleType) %>%
      unique()

    sum_info <- full_join(sum_status, msy_info_sub)
    write.csv(sum_info, file.path(figure_dir, "Reference_level_info.csv"), row.names = FALSE)

  msy_toUse <- max_info %>% select(RuleType, Constraint, CVConstraint, Variable, P5, P50, Mean, P95, Region)
  if(length(regions) > 1){
    msy_toUse <- rbind.data.frame(msy_toUse, max_info3)
  }
  max_all <- data.frame(msy_toUse) %>% filter(Constraint == "Pass")


  gc()
  # msy_info2 <- msy_info %>%
  #   select(-c(P5,P95)) %>%
  #   tidyr::pivot_wider(names_from = Variable, values_from = P50)
  #

  #################################################
  ## convert fixed catch to short-term projections
  ##//// deprecated -- didn't make sense without the correct NSL/rec catch
  #################################################
  # sinfo2 <- sinfo %>%
  #   left_join(ruledf) %>%
  #   filter(RuleType == "FixedCatch") %>%
  #   select(Iteration, Year, Region, Catch, VB, RelSSB, RelSSBdata, par2, RuleType) %>%
  #   rename(TACC = par2)

  # ref <- average_sum %>%
  #   filter(Variable == "VB") %>%
  #   select(Region, Mean) %>%
  #   rename(RefVB = Mean)

  # sinfo2 <- full_join(sinfo2, ref)

  # ssum <- sinfo2 %>%
  #   group_by(Year, Region, TACC, RefVB) %>%
  #   summarise(MedVB = median(VB),
  #                    P_g_ref = length(which(VB > RefVB))/length(VB),
  #                    P_g_softlim = length(which(RelSSBdata > 0.2))/length(RelSSBdata))
  # write.csv(ssum, file.path(figure_dir, "Proj_5year_probs.csv"), row.names = FALSE)

  #################################
  ## summaries for plotting
  ################################
  output3 <- output2 %>%
    tidyr::pivot_longer(cols = P5:P95, names_to = "Percentile", values_to = "Value") %>%
    tidyr::pivot_wider(names_from = Variable, values_from = Value)

  gc()
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

  output5 <- output4 %>% right_join(find_msy %>% select(-c(P50,Mean)))

  gc()

  p_cpue <- ggplot(output4) +
    # geom_segment(aes(x = RelSSB_P5, xend = RelSSB_P95, y = Catch_P50, yend = Catch_P50, color = Constraint), lwd = 1.2, alpha = 0.8) +
    # geom_segment(aes(x = RelSSB_P50, xend = RelSSB_P50, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
    # geom_point(aes(x = RelSSB_P50, y = Catch_P50, fill = Constraint), pch = 21, cex = 4) +
    geom_segment(aes(x = CPUE_P5, xend = CPUE_P95, y = Catch_Mean, yend = Catch_Mean, color = Constraint), lwd = 1.2, alpha = 0.25) +
    geom_segment(aes(x = CPUE_Mean, xend = CPUE_Mean, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.25) +
    geom_point(aes(x = CPUE_Mean, y = Catch_Mean, fill = Constraint), pch = 21, cex = 4, alpha = 0.5) +
    expand_limits(y = 0, x = 0) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    scale_x_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    xlab("Offset-year CPUE") + ylab("Average annual catch (tonnes)") +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    p_cpue <- p_cpue + facet_grid(Region~RuleType, scales = "free_x")
  } else {
    p_cpue <- p_cpue + facet_grid(~RuleType)
  }
  ggsave(file.path(figure_dir, "CPUE_vs_Catch_byConstraint.png"), p_cpue, height = 8, width = 20)

  p_cpue_v2 <- p_cpue +
    # geom_vline(data = output5, aes(xintercept = RelVB_P50), linetype = 2, lwd = 1.5) +
    # geom_hline(data = output5, aes(yintercept = Catch_P50), linetype = 2, lwd = 1.5)
    geom_vline(data = output5, aes(xintercept = CPUE_Mean), linetype = 2, lwd = 1.5) +
    geom_hline(data = output5, aes(yintercept = Catch_Mean), linetype = 2, lwd = 1.5)
  ggsave(file.path(figure_dir, "CPUE_vs_Catch_wTarget.png"), p_cpue_v2, height = 8, width = 20)

  p_cpue_b <- ggplot(output4) +
    # geom_segment(aes(x = RelSSB_P5, xend = RelSSB_P95, y = Catch_P50, yend = Catch_P50, color = Constraint), lwd = 1.2, alpha = 0.8) +
    # geom_segment(aes(x = RelSSB_P50, xend = RelSSB_P50, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
    # geom_point(aes(x = RelSSB_P50, y = Catch_P50, fill = Constraint), pch = 21, cex = 4) +
    geom_segment(aes(x = VB_P5, xend = VB_P95, y = CPUE_Mean, yend = CPUE_Mean, color = Constraint), lwd = 1.2, alpha = 0.25) +
    geom_segment(aes(x = VB_Mean, xend = VB_Mean, y = CPUE_P5, yend = CPUE_P95, color = Constraint), lwd = 1.2, alpha = 0.25) +
    geom_point(aes(x = VB_Mean, y = CPUE_Mean, fill = Constraint), pch = 21, cex = 4, alpha = 0.5) +
    expand_limits(y = 0, x = 0) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    scale_x_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    ylab("Offset-year CPUE") + xlab("AW adjusted vulnerable biomass (B; tonnes)") +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    p_cpue_b <- p_cpue_b + facet_grid(Region~RuleType, scales = "free_x")
  } else {
    p_cpue_b <- p_cpue_b + facet_grid(~RuleType)
  }
  ggsave(file.path(figure_dir, "CPUE_vs_VB_byConstraint.png"), p_cpue_b, height = 8, width = 20)

  p_cpue_b_v2 <- p_cpue_b +
    # geom_vline(data = output5, aes(xintercept = RelVB_P50), linetype = 2, lwd = 1.5) +
    # geom_hline(data = output5, aes(yintercept = Catch_P50), linetype = 2, lwd = 1.5)
    geom_vline(data = output5, aes(xintercept = VB_Mean), linetype = 2, lwd = 1.5) +
    geom_hline(data = output5, aes(yintercept = CPUE_Mean), linetype = 2, lwd = 1.5)
  ggsave(file.path(figure_dir, "CPUE_vs_VB_wTarget.png"), p_cpue_b_v2, height = 8, width = 20)


  p_cv <- ggplot(output4) +
    geom_segment(aes(x = CV, xend = CV, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.25) +
    # geom_point(aes(x = CV, y = Catch_P50, fill = Constraint), pch = 21, cex = 4) +
    geom_point(aes(x = CV, y = Catch_Mean, fill = Constraint), pch = 21, cex = 4, alpha = 0.5) +
    expand_limits(y = 0, x = 0) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    scale_x_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    xlab("CV of catch over time and iteration") + ylab("Average annual catch (tonnes)") +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    p_cv <- p_cv + facet_grid(Region~RuleType, scales = "free_x")
  } else {
    p_cv <- p_cv + facet_grid(~RuleType, scales = "free_x")
  }
  ggsave(file.path(figure_dir, "CV_vs_Catch.png"), p_cv, height = 8, width = 20)

  p_cv_v2 <- p_cv +
    # geom_vline(data = output5, aes(xintercept = RelVB_P50), linetype = 2, lwd = 1.5) +
    # geom_hline(data = output5, aes(yintercept = Catch_P50), linetype = 2, lwd = 1.5)
    geom_vline(data = output5, aes(xintercept = CV), linetype = 2, lwd = 1.5) +
    geom_hline(data = output5, aes(yintercept = Catch_Mean), linetype = 2, lwd = 1.5)
  ggsave(file.path(figure_dir, "CV_vs_Catch_wTarget.png"), p_cv_v2, height = 8, width = 20)


  p_relssb <- ggplot(output4) +
    # geom_segment(aes(x = RelSSB_P5, xend = RelSSB_P95, y = Catch_P50, yend = Catch_P50, color = Constraint), lwd = 1.2, alpha = 0.8) +
    # geom_segment(aes(x = RelSSB_P50, xend = RelSSB_P50, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
    # geom_point(aes(x = RelSSB_P50, y = Catch_P50, fill = Constraint), pch = 21, cex = 4) +
    geom_segment(aes(x = RelSSBdata_P5, xend = RelSSBdata_P95, y = Catch_Mean, yend = Catch_Mean, color = Constraint), lwd = 1.2, alpha = 0.25) +
    geom_segment(aes(x = RelSSBdata_Mean, xend = RelSSBdata_Mean, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.25) +
    geom_point(aes(x = RelSSBdata_Mean, y = Catch_Mean, fill = Constraint), pch = 21, cex = 4, alpha = 0.5) +
    expand_limits(y = 0, x = 0) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    scale_x_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    xlab(expression("Relative spawning biomass (SSB/SSB"["0_data"]*")")) + ylab("Average annual catch (tonnes)") +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    p_relssb <- p_relssb + facet_grid(Region~RuleType, scales = "free_x")
  } else {
    p_relssb <- p_relssb + facet_grid(~RuleType)
  }
  ggsave(file.path(figure_dir, "RelSSB_vs_Catch_byConstraint.png"), p_relssb, height = 8, width = 20)

  p_relvb <- ggplot(output4) +
    # geom_segment(aes(x = RelVB_P5, xend = RelVB_P95, y = Catch_P50, yend = Catch_P50, color = Constraint), lwd = 1.2, alpha = 0.8) +
    # geom_segment(aes(x = RelVB_P50, xend = RelVB_P50, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
    # geom_point(aes(x = RelVB_P50, y = Catch_P50, fill = Constraint), pch = 21, cex = 3) +
    geom_segment(aes(x = RelVB_P5, xend = RelVB_P95, y = Catch_Mean, yend = Catch_Mean, color = Constraint), lwd = 1.2, alpha = 0.25) +
    geom_segment(aes(x = RelVB_Mean, xend = RelVB_Mean, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.25) +
    geom_point(aes(x = RelVB_Mean, y = Catch_Mean, fill = Constraint), pch = 21, cex = 3, alpha = 0.5) +
    expand_limits(y = 0, x = 0) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    scale_x_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    xlab(expression("Relative AW adjusted vulnerable biomass (B/B"[0]*")")) + ylab("Average annual catch (tonnes)") +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    expand_limits(y = 0) +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    p_relvb <- p_relvb + facet_grid(Region~RuleType)
  } else {
    p_relvb <- p_relvb + facet_grid(~RuleType)
  }
  ggsave(file.path(figure_dir, "RelVB_vs_Catch_byConstraint.png"), p_relvb, height = 8, width = 20)

  # output5$RuleType <- factor(output5$RuleType, levels = levels(output5$RuleType))
  p_relvb_v2 <- p_relvb +
    # geom_vline(data = output5, aes(xintercept = RelVB_P50), linetype = 2, lwd = 1.5) +
    # geom_hline(data = output5, aes(yintercept = Catch_P50), linetype = 2, lwd = 1.5)
    geom_vline(data = output5, aes(xintercept = RelVB_Mean), linetype = 2, lwd = 1.5) +
    geom_hline(data = output5, aes(yintercept = Catch_Mean), linetype = 2, lwd = 1.5)
  ggsave(file.path(figure_dir, "RelVB_vs_Catch_byConstraint_wTarget.png"), p_relvb_v2, height = 8, width = 20)

  p_vb <- ggplot(output4) +
    # geom_segment(aes(x = VB_P5, xend = VB_P95, y = Catch_P50, yend = Catch_P50, color = Constraint), lwd = 1.2, alpha = 0.8) +
    # geom_segment(aes(x = VB_P50, xend = VB_P50, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
    # geom_point(aes(x = VB_P50, y = Catch_P50, fill = Constraint), pch = 21, cex = 3) +
    geom_segment(aes(x = VB_P5, xend = VB_P95, y = Catch_Mean, yend = Catch_Mean, color = Constraint), lwd = 1.2, alpha = 0.25) +
    geom_segment(aes(x = VB_Mean, xend = VB_Mean, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.25) +
    geom_point(aes(x = VB_Mean, y = Catch_Mean, fill = Constraint), pch = 21, cex = 3, alpha = 0.5) +
    expand_limits(y = 0, x = 0) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    scale_x_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    xlab("AW adjusted vulnerable biomass (B; tonnes)") + ylab("Average annual catch (tonnes)") +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    p_vb <- p_vb + facet_wrap(Region~RuleType, scales = "free_x", nrow = length(regions))
  } else {
    p_vb <- p_vb + facet_wrap(~RuleType, scales = "free_x", nrow = length(regions))
  }
  ggsave(file.path(figure_dir, "VB_vs_Catch_byConstraint.png"), p_vb, height = 8, width = 20)

  # output5$RuleType <- factor(output5$RuleType, levels = levels(output4$RuleType))
  p_vb_v2 <- p_vb +
    # geom_vline(data = output5, aes(xintercept = VB_P50), linetype = 2, lwd = 1.5) +
    # geom_hline(data = output5, aes(yintercept = Catch_P50), linetype = 2, lwd = 1.5)
    geom_vline(data = output5, aes(xintercept = VB_Mean), linetype = 2, lwd = 1.5) +
    geom_hline(data = output5, aes(yintercept = Catch_Mean), linetype = 2, lwd = 1.5)
  ggsave(file.path(figure_dir, "VB_vs_Catch_byConstraint_wTarget.png"), p_vb_v2, height = 8, width = 20)


  p_reltb <- ggplot(output4) +
    # geom_segment(aes(x = RelTB_P5, xend = RelTB_P95, y = Catch_P50, yend = Catch_P50, color = Constraint), lwd = 1.2, alpha = 0.8) +
    # geom_segment(aes(x = RelTB_P50, xend = RelTB_P50, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
    # geom_point(aes(x = RelTB_P50, y = Catch_P50, fill = Constraint), pch = 21, cex = 3) +
    geom_segment(aes(x = RelTB_P5, xend = RelTB_P95, y = Catch_Mean, yend = Catch_Mean, color = Constraint), lwd = 1.2, alpha = 0.25) +
    geom_segment(aes(x = RelTB_Mean, xend = RelTB_Mean, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.25) +
    geom_point(aes(x = RelTB_Mean, y = Catch_Mean, fill = Constraint), pch = 21, cex = 3, alpha = 0.5) +
    expand_limits(y = 0, x = 0) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    scale_x_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    xlab(expression("Relative total biomass (B"["tot"]*"/B"["tot_0"]*")")) + ylab("Average annual catch (tonnes)") +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    p_reltb <- p_reltb + facet_grid(Region~RuleType)
  } else {
    p_reltb <- p_reltb + facet_grid(~RuleType)
  }
  ggsave(file.path(figure_dir, "RelTB_vs_Catch_byConstraint.png"), p_reltb, height = 8, width = 20)

  # output5$RuleType <- factor(output5$RuleType, levels = levels(output5$RuleType))
  p_reltb_v2 <- p_reltb +
    # geom_vline(data = output5, aes(xintercept = RelTB_P50), linetype = 2, lwd = 1.5) +
    # geom_hline(data = output5, aes(yintercept = Catch_P50), linetype = 2, lwd = 1.5)
    geom_vline(data = output5, aes(xintercept = RelTB_Mean), linetype = 2, lwd = 1.5) +
    geom_hline(data = output5, aes(yintercept = Catch_Mean), linetype = 2, lwd = 1.5)
  ggsave(file.path(figure_dir, "RelTB_vs_Catch_byConstraint_wTarget.png"), p_reltb_v2, height = 8, width = 20)

  p_tb <- ggplot(output4) +
    # geom_segment(aes(x = TB_P5, xend = TB_P95, y = Catch_P50, yend = Catch_P50, color = Constraint), lwd = 1.2, alpha = 0.8) +
    # geom_segment(aes(x = TB_P50, xend = TB_P50, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.8) +
    # geom_point(aes(x = TB_P50, y = Catch_P50, fill = Constraint), pch = 21, cex = 3) +
    geom_segment(aes(x = TB_P5, xend = TB_P95, y = Catch_Mean, yend = Catch_Mean, color = Constraint), lwd = 1.2, alpha = 0.25) +
    geom_segment(aes(x = TB_Mean, xend = TB_Mean, y = Catch_P5, yend = Catch_P95, color = Constraint), lwd = 1.2, alpha = 0.25) +
    geom_point(aes(x = TB_Mean, y = Catch_Mean, fill = Constraint), pch = 21, cex = 3, alpha = 0.5) +
    expand_limits(y = 0, x = 0) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    scale_x_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    xlab(expression("Total biomass (B"["tot"]*"; tonnes)")) + ylab("Average annual catch (tonnes)") +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    p_tb <- p_tb + facet_wrap(Region~RuleType, scales = "free_x", nrow = length(regions))
  } else {
    p_tb <- p_tb + facet_wrap(~RuleType, scales = "free_x", nrow = length(regions))
  }
  ggsave(file.path(figure_dir, "TB_vs_Catch_byConstraint.png"), p_tb, height = 8, width = 20)

  # output5$RuleType <- factor(output5$RuleType, levels = levels(output4$RuleType))
  p_tb_v2 <- p_tb +
    # geom_vline(data = output5, aes(xintercept = TB_P50), linetype = 2, lwd = 1.5) +
    # geom_hline(data = output5, aes(yintercept = Catch_P50), linetype = 2, lwd = 1.5)
    geom_vline(data = output5, aes(xintercept = TB_Mean), linetype = 2, lwd = 1.5) +
    geom_hline(data = output5, aes(yintercept = Catch_Mean), linetype = 2, lwd = 1.5)
  ggsave(file.path(figure_dir, "TB_vs_Catch_byConstraint_wTarget.png"), p_tb_v2, height = 8, width = 20)



  ###################################################
  ## Status relative to reference level by rule type
  ###################################################
  check <- max_all %>%
    filter(Variable %in% c("TB","SSB","SSB0","VB")) %>%
    select(Region, RuleType, Variable, Mean)
  check2 <- average_sum %>%
    filter(Variable %in% c("TB","SSB","VB","SSB0")) %>%
    select(Region, Variable, Mean, RuleType)
  limits <- check %>%
    select(-RuleType) %>%
    filter(Variable == "SSB0")
  limits <- unique(limits) %>%
    group_by(Region) %>%
    summarise(SoftLimit = 0.2 * Mean,
                    HardLimit = 0.1 * Mean) %>%
    mutate(Variable = "SSB")
  check3 <- full_join(check, check2) %>%
    left_join(limits) %>%
    filter(Variable != "SSB0")  %>%
    mutate(Variable = replace(Variable, Variable == "TB", "Btot"),
           Variable = replace(Variable, Variable == "VB", "B"))
  checkt <- dinfo %>%
    select(Iteration, Year, Region, SSB, VB, TB) %>%
    tidyr::pivot_longer(cols = c(SSB,VB,TB), names_to = "Variable", values_to = "Value") %>%
    mutate(Variable = replace(Variable, Variable == "TB", "Btot"),
           Variable = replace(Variable, Variable == "VB", "B"))
  check3$RuleType <- factor(check3$RuleType, levels = c("FixedCatch", "Average", "FixedF"))
  pb <- ggplot(check3) +
    stat_summary(data = checkt, aes(x = Year, y = Value), fun.min = function(x) stats::quantile(x, 0.05), fun.max = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25) +
    stat_summary(data = checkt, aes(x = Year, y = Value), fun = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1.2) +
    geom_hline(aes(yintercept = HardLimit), color = "red", lwd = 1.4) +
    geom_hline(aes(yintercept = SoftLimit), color = "darkorange2", lwd = 1.4) +
    geom_label(label = "Soft limit", x = min(dinfo$Year)+10, y = check3$SoftLimit, color = "darkorange2", size = 5) +
    geom_label(label = "Hard limit", x = min(dinfo$Year)+10, y = check3$HardLimit, color = "red", size = 5) +
    geom_hline(aes(yintercept = Mean, color = RuleType), lwd = 1.4) +
    scale_color_manual(values = rev(c("goldenrod", "forestgreen","steelblue")))+
    guides(color = guide_legend(title="Reference level")) +
    expand_limits(y = 0) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    pb <- pb + facet_wrap(Region~Variable, scales = "free_y", ncol = 3)
    ggsave(file.path(figure_dir, "SSB_VB_Ref.png"), pb, height = 15, width = 15)
  } else {
    pb <- pb + facet_wrap(~Variable, scales = "free_y", ncol = 3)
    ggsave(file.path(figure_dir, "SSB_VB_Ref.png"), pb, height = 8, width = 20)
  }

  pb <- ggplot(check3 %>% filter(Variable == "B")) +
    stat_summary(data = checkt %>% filter(Variable == "B"), aes(x = Year, y = Value), fun.min = function(x) stats::quantile(x, 0.05), fun.max = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25) +
    stat_summary(data = checkt %>% filter(Variable == "B"), aes(x = Year, y = Value), fun = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1.2) +
    geom_hline(aes(yintercept = Mean, color = RuleType), lwd = 1.4) +
    scale_color_manual(values = rev(c("goldenrod", "forestgreen","steelblue")))+
    guides(color = guide_legend(title="Reference level")) +
    expand_limits(y = 0) +
    scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
    ylab("AW adjusted vulnerable biomass (B; tonnes)") +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    pb <- pb + facet_wrap(Region~., scales = "free_y", ncol = 3)
    ggsave(file.path(figure_dir, "VB_Ref.png"), pb, height = 8, width = 20)
  } else {
    ggsave(file.path(figure_dir, "VB_Ref.png"), pb, height = 8, width = 10)
  }


  check <- dinfo %>% left_join(max_all %>% filter(Variable == "VB")) %>%
    mutate(CVConstraint = replace(CVConstraint, RuleType == "FixedCatch", "FixedCatch")) %>%
    mutate(CVConstraint = replace(CVConstraint, RuleType == "FixedF", "FixedF")) %>%
    filter(CVConstraint != "Min")
  if(length(unique(check$RuleType))==3) check$RuleType <- factor(check$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
  check$CVConstraint <- factor(check$CVConstraint, levels = c("FixedCatch", "25%", "Median", "75%", "Max", "FixedF"))
  p_vbcurr <- ggplot(check) +
    geom_ribbon(aes(x = Year, ymin = P5, ymax = P95, fill = CVConstraint), alpha = 0.5) +
    # geom_hline(aes(yintercept = P50, color = CVConstraint), lwd = 1.2) +
    geom_hline(aes(yintercept = Mean, color = CVConstraint), lwd = 1.2) +
    # geom_line(aes(x = Year, y = VB), lwd = 1.2) +
    stat_summary(aes(x = Year, y = VB), fun.min = function(x) stats::quantile(x, 0.05), fun.max = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25) +
    stat_summary(aes(x = Year, y = VB), fun = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1.2) +
    expand_limits(y = 0) +
    # scale_fill_brewer(palette = "Spectral") +
    # scale_color_brewer(palette = "Spectral") +
    scale_fill_tableau() +
    scale_color_tableau() +
    ylab("AW adjusted vulnerable biomass (B; tonnes)") +
    guides(color = FALSE, fill = FALSE) +
    coord_cartesian(xlim = c(min(check$Year),max(check$Year)), expand = FALSE) +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    p_vbcurr <- p_vbcurr + facet_grid(Region~CVConstraint, scales = "free_y")
  } else {
    p_vbcurr <- p_vbcurr + facet_grid(~CVConstraint, scales = "free_y")
  }
  ggsave(file.path(figure_dir, "VBcurrent.png"), p_vbcurr, height = 10, width = 20)


  msyx <- msy_info %>% select(Region, RuleNum) %>% unique()
  vbcheck <- check %>% 
    select(Iteration, Year, Region, RuleType, VB) %>% 
    filter(Region != "Total") %>%
    unique()
  sub <- info %>% 
    filter(RuleNum %in% msy_info$RuleNum, Year %in% (min(projyears)-1):max(projyears)) %>% 
    left_join(rule_type) %>% 
    inner_join(msyx) %>%
    select(Iteration, Year, Region, RuleType, VB) %>%
    filter(Region != "Total") %>%
    unique()
  p_vbcheck <- ggplot() +
    stat_summary(data = vbcheck, aes(x = Year, y = VB), fun.min = function(x) stats::quantile(x, 0.05), fun.max = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25) +
    stat_summary(data = vbcheck, aes(x = Year, y = VB), fun = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1.2) +
    stat_summary(data = sub, aes(x = Year, y = VB, fill = RuleType), fun.min = function(x) stats::quantile(x, 0.05), fun.max = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25) +
    stat_summary(data = sub, aes(x = Year, y = VB, color = RuleType), fun = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1.2) +    
    expand_limits(y = 0) +
    scale_fill_tableau() +
    scale_color_tableau() +
    ylab("AW adjusted vulnerable biomass (B; tonnes)") +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    p_vbcheck <- p_vbcheck + facet_grid(~Region)
  }
  ggsave(file.path(figure_dir, "VBcheck.png"), p_vbcheck, height = 10, width = 20)




  check_avg <- average_sum %>%
    select(Region, Variable, P5, Mean, P95) %>%
    filter(Variable == "VB") %>%
    # rename(P50 = Mean) %>%
    mutate(RuleType = "Average") %>%
    mutate(CVConstraint = 0)
  check_avg$CVConstraint <- factor(check_avg$CVConstraint)
  max_sub <- max_all %>%
    select(-Constraint) %>%
    filter(Variable == "VB") %>%
    full_join(check_avg)
  check <- dinfo %>% left_join(max_sub) %>%
    filter(RuleType %in% c("FixedCatch", "FixedF", "Average"))
  check$RuleType <- factor(check$RuleType, levels = c("FixedCatch", "Average", "FixedF"))
  p_vbcurr <- ggplot(check) +
    geom_ribbon(aes(x = Year, ymin = P5, ymax = P95, fill = RuleType), alpha = 0.5) +
    # geom_hline(aes(yintercept = P50, color =RuleType), lwd = 1.2) +
    geom_hline(aes(yintercept = Mean, color =RuleType), lwd = 1.2) +
    # geom_line(aes(x = Year, y = VB), lwd = 1.2) +
    stat_summary(aes(x = Year, y = VB), fun.min = function(x) stats::quantile(x, 0.05), fun.max = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25) +
    stat_summary(aes(x = Year, y = VB), fun = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1.2) +
    expand_limits(y = 0) +
    # scale_fill_brewer(palette = "Spectral") +
    # scale_color_brewer(palette = "Spectral") +
    scale_fill_tableau() +
    scale_color_tableau() +
    ylab("AW adjusted vulnerable biomass (B; tonnes)") +
    guides(color = FALSE, fill = FALSE) +
    coord_cartesian(xlim = c(min(check$Year),max(check$Year)), expand = FALSE) +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    p_vbcurr <- p_vbcurr + facet_grid(Region~RuleType, scales = "free_y")
  } else {
    p_vbcurr <- p_vbcurr + facet_grid(~RuleType, scales = "free_y")
  }
  ggsave(file.path(figure_dir, "VBcurrent_AVG.png"), p_vbcurr, height = 10, width = 20)

  gc()

 if(any(grepl("B0now_r", names(mcmc1)))){
  check <- dinfo %>% left_join(max_all %>% filter(Variable == "RelVBdata")) %>%
    mutate(CVConstraint = replace(CVConstraint, RuleType == "FixedCatch", "FixedCatch")) %>%
    mutate(CVConstraint = replace(CVConstraint, RuleType == "FixedF", "FixedF")) %>%
    filter(CVConstraint != "Min")
  if(length(unique(check$RuleType))==3) check$RuleType <- factor(check$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
  check$CVConstraint <- factor(check$CVConstraint, levels = c("FixedCatch", "25%", "Median", "75%", "Max", "FixedF"))
  p_relvbcurr <- ggplot(check) +
    geom_ribbon(aes(x = Year, ymin = P5, ymax = P95, fill = CVConstraint), alpha = 0.5) +
    # geom_hline(aes(yintercept = P50, color = CVConstraint), lwd = 1.2) +
    geom_hline(aes(yintercept = Mean, color = CVConstraint), lwd = 1.2) +
    stat_summary(aes(x = Year, y = RelVB), fun.min = function(x) stats::quantile(x,0.05), fun.max = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25) +
    stat_summary(aes(x = Year, y = RelVB), fun = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1.2) +
    expand_limits(y = 0) +
    # scale_color_brewer(palette = "Spectral") +
    # scale_fill_brewer(palette = "Spectral") +
    scale_fill_tableau() +
    scale_color_tableau() +
    guides(color = FALSE, fill = FALSE) +
    ylab(expression("Relative AW adjusted vulnerable biomass (B/B"["0"]*")")) +
    coord_cartesian(xlim = c(min(check$Year),max(check$Year)), expand = FALSE) +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    p_relvbcurr <- p_relvbcurr + facet_grid(Region~CVConstraint)
  } else {
    p_relvbcurr <- p_relvbcurr + facet_grid(~CVConstraint)
  }
  ggsave(file.path(figure_dir, "RelVBcurrent.png"), p_relvbcurr, height = 10, width = 20)

  check_avg <- average_sum %>%
    select(Region, Variable, P5, Mean, P95) %>%
    filter(Variable == "RelVB") %>%
    # rename(P50 = Mean) %>%
    mutate(RuleType = "Average") %>%
    mutate(CVConstraint = 0)
  check_avg$CVConstraint <- factor(check_avg$CVConstraint)
  max_sub <- max_all %>%
    select(-Constraint) %>%
    filter(Variable == "RelVB") %>%
    full_join(check_avg)
  check <- dinfo %>% left_join(max_sub) %>%
    filter(RuleType %in% c("FixedCatch", "FixedF", "Average"))
  check$RuleType <- factor(check$RuleType, levels = c("FixedCatch", "Average", "FixedF"))
  p_relvbcurr <- ggplot(check) +
    geom_ribbon(aes(x = Year, ymin = P5, ymax = P95, fill = RuleType), alpha = 0.5) +
    # geom_hline(aes(yintercept = P50, color =RuleType), lwd = 1.2) +
    geom_hline(aes(yintercept = Mean, color =RuleType), lwd = 1.2) +
    # geom_line(aes(x = Year, y = VB), lwd = 1.2) +
    stat_summary(aes(x = Year, y = RelVB), fun.min = function(x) stats::quantile(x, 0.05), fun.max = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25) +
    stat_summary(aes(x = Year, y = RelVB), fun = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1.2) +
    expand_limits(y = 0) +
    # scale_fill_brewer(palette = "Spectral") +
    # scale_color_brewer(palette = "Spectral") +
    scale_fill_tableau() +
    scale_color_tableau() +
    ylab(expression("Relative AW adjusted vulnerable biomass (B/B"["0"]*")")) +
    guides(color = FALSE, fill = FALSE) +
    coord_cartesian(xlim = c(min(check$Year),max(check$Year)), expand = FALSE) +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    p_relvbcurr <- p_relvbcurr + facet_grid(Region~RuleType, scales = "free_y")
  } else {
    p_relvbcurr <- p_relvbcurr + facet_grid(~RuleType, scales = "free_y")
  }
  ggsave(file.path(figure_dir, "RelVBcurrent_AVG.png"), p_relvbcurr, height = 10, width = 20)

  gc()

  check <- dinfo %>% left_join(max_all %>% filter(Variable == "RelTBdata")) %>%
    mutate(CVConstraint = replace(CVConstraint, RuleType == "FixedCatch", "FixedCatch")) %>%
    mutate(CVConstraint = replace(CVConstraint, RuleType == "FixedF", "FixedF")) %>%
    filter(CVConstraint != "Min")
  if(length(unique(check$RuleType))==3) check$RuleType <- factor(check$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
  check$CVConstraint <- factor(check$CVConstraint, levels = c("FixedCatch", "25%", "Median", "75%", "Max", "FixedF"))
  p_reltbcurr <- ggplot(check) +
    geom_ribbon(aes(x = Year, ymin = P5, ymax = P95, fill = CVConstraint), alpha = 0.5) +
    # geom_hline(aes(yintercept = P50, color = CVConstraint), lwd = 1.2) +
    geom_hline(aes(yintercept = Mean, color = CVConstraint), lwd = 1.2) +
    stat_summary(aes(x = Year, y = RelTB), fun.min = function(x) stats::quantile(x, 0.05), fun.max = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25) +
    stat_summary(aes(x = Year, y = RelTB), fun = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1.2) +
    expand_limits(y = 0) +
    # scale_color_brewer(palette = "Spectral") +
    # scale_fill_brewer(palette = "Spectral") +
    scale_fill_tableau() +
    scale_color_tableau() +
    guides(color = FALSE, fill = FALSE) +
    ylab(expression("Relative total biomass (B"["tot"]*"/B"["tot_0"]*")")) +
    coord_cartesian(xlim = c(min(check$Year),max(check$Year)), expand = FALSE) +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    p_reltbcurr <- p_reltbcurr + facet_grid(Region~CVConstraint)
  } else {
    p_reltbcurr <- p_reltbcurr + facet_grid(~CVConstraint)
  }
  ggsave(file.path(figure_dir, "RelTBcurrent.png"), p_reltbcurr, height = 10, width = 20)

  check_avg <- average_sum %>%
    select(Region, Variable, P5, Mean, P95) %>%
    filter(Variable == "RelTB") %>%
    # rename(P50 = Mean) %>%
    mutate(RuleType = "Average") %>%
    mutate(CVConstraint = 0)
  check_avg$CVConstraint <- factor(check_avg$CVConstraint)
  max_sub <- max_all %>%
    select(-Constraint) %>%
    filter(Variable == "RelTB") %>%
    full_join(check_avg)
  check <- dinfo %>% left_join(max_sub) %>%
    filter(RuleType %in% c("FixedCatch", "FixedF", "Average"))
  check$RuleType <- factor(check$RuleType, levels = c("FixedCatch", "Average", "FixedF"))
  p_reltbcurr <- ggplot(check) +
    geom_ribbon(aes(x = Year, ymin = P5, ymax = P95, fill = RuleType), alpha = 0.5) +
    # geom_hline(aes(yintercept = P50, color =RuleType), lwd = 1.2) +
    geom_hline(aes(yintercept = Mean, color =RuleType), lwd = 1.2) +
    # geom_line(aes(x = Year, y = VB), lwd = 1.2) +
    stat_summary(aes(x = Year, y = RelTB), fun.min = function(x) stats::quantile(x, 0.05), fun.max = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25) +
    stat_summary(aes(x = Year, y = RelTB), fun = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1.2) +
    expand_limits(y = 0) +
    # scale_fill_brewer(palette = "Spectral") +
    # scale_color_brewer(palette = "Spectral") +
    scale_fill_tableau() +
    scale_color_tableau() +
    ylab(expression("Relative total biomass (B"["tot"]*"/B"["tot_0"]*")")) +
    guides(color = FALSE, fill = FALSE) +
    coord_cartesian(xlim = c(min(check$Year),max(check$Year)), expand = FALSE) +
    theme_bw(base_size = 20)
  if(length(regions) > 1){
    p_reltbcurr <- p_reltbcurr + facet_grid(Region~RuleType, scales = "free_y")
  } else {
    p_reltbcurr <- p_reltbcurr + facet_grid(~RuleType, scales = "free_y")
  }
  ggsave(file.path(figure_dir, "RelTBcurrent_AVG.png"), p_reltbcurr, height = 10, width = 20)
 }
  # #######################################
  # ## examine variability across filters
  # #######################################
  ## examples from find_max
  # reg <- unique(find_max$Region)
  # find_max_sub <- lapply(1:length(reg), function(x){
  #   sub <- find_max %>% filter(Region == reg[x])
  #   types <- unique(sub$RuleType)
  #   byType <- lapply(1:length(types), function(y){
  #     sub2 <- sub %>% filter(RuleType == types[y])
  #     if(types[y] != "CPUE-based") con <- unique(sub2$Constraint)
  #     if(types[y] == "CPUE-based"){
  #       con <- unique(sub2$Constraint)
  #       con2 <- unique(sub2$CVConstraint)
  #     }
  #     byCon <- lapply(1:length(con), function(z){
  #       sub3 <- sub2 %>% filter(Constraint == con[z])
  #       if(types[y] == "CPUE-based"){
  #         byCon2 <- lapply(1:length(con2), function(zz){
  #           sub4 <- sub3 %>% filter(CVConstraint == con2[zz])
  #           out <- sub4[nrow(sub4),]
  #           return(out)
  #         })
  #           out <- do.call(rbind,byCon2)
  #       } else {
  #         out <- sub3[nrow(sub3),]
  #       }
  #       return(out)
  #     })
  #     byCon <- do.call(rbind, byCon)
  #     return(byCon)
  #   })
  #   byType <- do.call(rbind, byType)
  #   return(byType)
  # })
  # find_max_sub <- do.call(rbind, find_max_sub)

  check1 <- cinfo %>%
    right_join(find_max %>% select(-c(P50,Mean))) %>%
    left_join(max_info %>% filter(Variable == "Catch")) %>%
    mutate(RelVB = VB / VB0) %>%
    mutate(RelSSBdata = SSB / SSB0now) %>%
    select(Iteration, Year, Region, RuleNum, RuleType, Constraint, Catch, RelVB, RelSSBdata) %>%
    tidyr::pivot_longer(cols = c(Catch,RelVB,RelSSBdata), names_to = "Variable", values_to = "Value") %>%
    filter(RuleType == "FixedCatch")
  if(nrow(check1) > 0){
    check2 <- check1 %>%
      group_by(Year, Region, RuleType, Constraint, Variable) %>%
      summarise(P5 = quantile(Value, 0.05),
                       P50 = quantile(Value, 0.50),
                       Mean = mean(Value),
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
      geom_ribbon(data = check2 %>% filter(Variable %in% c("Catch","RelSSBdata","RelVB")), aes(x = Year, ymin = P5, ymax = P95, fill = Constraint), color = NA, alpha = 0.5) +
      # geom_line(data = check2 %>% filter(Variable %in% c("Catch","RelSSB","RelVB")), aes(x = Year, y = P50, color = Constraint), lwd = 1.5) +
      geom_line(data = check2 %>% filter(Variable %in% c("Catch","RelSSBdata","RelVB")), aes(x = Year, y = Mean, color = Constraint), lwd = 1.5) +
      geom_line(data = check1 %>% filter(Iteration == 1) %>% filter(Variable %in% c("Catch","RelSSBdata","RelVB")), aes(x = Year, y = Value)) +
      geom_line(data = check1 %>% filter(Iteration == 2) %>% filter(Variable %in% c("Catch","RelSSBdata","RelVB")), aes(x = Year, y = Value)) +
      geom_line(data = check1 %>% filter(Iteration == 3) %>% filter(Variable %in% c("Catch","RelSSBdata","RelVB")), aes(x = Year, y = Value)) +
      geom_line(data = check1 %>% filter(Iteration == 4) %>% filter(Variable %in% c("Catch","RelSSBdata","RelVB")), aes(x = Year, y = Value)) +
      geom_line(data = check1 %>% filter(Iteration == 5) %>% filter(Variable %in% c("Catch","RelSSBdata","RelVB")), aes(x = Year, y = Value)) +
      # scale_color_brewer(palette = "Spectral") +
      # scale_fill_brewer(palette = "Spectral") +
      scale_fill_tableau() +
      scale_color_tableau() +
      expand_limits(y = 0) +
      scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
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
    mutate(RelVB = VB / VB0) %>%
    mutate(RelSSBdata = SSB / SSB0now) %>%
    right_join(find_max %>% select(-c(P50,Mean))) %>%
    left_join(max_info %>% filter(Variable == "Catch")) %>%
    select(Iteration, Year, Region, RuleNum, RuleType, Constraint, Catch, RelVB, RelSSBdata) %>%
    tidyr::pivot_longer(cols = c(Catch,RelVB,RelSSBdata), names_to = "Variable", values_to = "Value") %>%
    filter(RuleType == "FixedF")
  if(nrow(check1) > 0){
    check2 <- check1 %>%
      group_by(Year, Region, RuleType, Constraint, Variable) %>%
      summarise(P5 = quantile(Value, 0.05),
                       P50 = quantile(Value, 0.50),
                       Mean = mean(Value),
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
      geom_ribbon(data = check2 %>% filter(Variable %in% c("Catch","RelSSBdata","RelVB")), aes(x = Year, ymin = P5, ymax = P95, fill = Constraint), color = NA, alpha = 0.5) +
      # geom_line(data = check2 %>% filter(Variable %in% c("Catch","RelSSBdata","RelVB")), aes(x = Year, y = P50, color = Constraint), lwd = 1.5) +
      geom_line(data = check2 %>% filter(Variable %in% c("Catch","RelSSBdata","RelVB")), aes(x = Year, y = Mean, color = Constraint), lwd = 1.5) +
      geom_line(data = check1 %>% filter(Iteration == 1) %>% filter(Variable %in% c("Catch","RelSSBdata","RelVB")), aes(x = Year, y = Value)) +
      geom_line(data = check1 %>% filter(Iteration == 2) %>% filter(Variable %in% c("Catch","RelSSBdata","RelVB")), aes(x = Year, y = Value)) +
      geom_line(data = check1 %>% filter(Iteration == 3) %>% filter(Variable %in% c("Catch","RelSSBdata","RelVB")), aes(x = Year, y = Value)) +
      geom_line(data = check1 %>% filter(Iteration == 4) %>% filter(Variable %in% c("Catch","RelSSBdata","RelVB")), aes(x = Year, y = Value)) +
      geom_line(data = check1 %>% filter(Iteration == 5) %>% filter(Variable %in% c("Catch","RelSSBdata","RelVB")), aes(x = Year, y = Value)) +
      # scale_color_brewer(palette = "Spectral") +
      # scale_fill_brewer(palette = "Spectral") +
      scale_color_tableau() +
      scale_fill_tableau() +
      ylab("Value") + xlab("Projection Year") +
      expand_limits(y = 0) +
      scale_y_continuous(limits = c(0,NA), expand = expansion(mult = c(0, 0.1))) +
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
    right_join(find_max %>% select(-c(P50,Mean))) %>%
    left_join(max_info %>% filter(Variable == "Catch")) %>%
    select(Iteration, Year, Region, RuleNum, RuleType, Constraint, CVConstraint, Catch, RelVB, RelSSBdata) %>%
    tidyr::pivot_longer(cols = c(Catch,RelVB,RelSSBdata), names_to = "Variable", values_to = "Value") %>%
    filter(RuleType == "CPUE-based") %>%
    filter(CVConstraint != "Min") %>%
    mutate(CVConstraint = replace(CVConstraint, Constraint == "Fail: Risk" & CVConstraint != "Max", 0)) %>%
    mutate(CVConstraint = replace(CVConstraint, Constraint == "Fail: Risk" & CVConstraint == "Max", "Fail: Risk")) %>%
    filter(CVConstraint != 0)
  if(any(grepl("Fail: Risk", check1$Constraint))) check1$CVConstraint <- factor(check1$CVConstraint, levels = c("Fail: Risk","Max","75%","Median","25%"))
  if(any(grepl("Fail: Risk", check1$Constraint)) == FALSE) check1$CVConstraint <- factor(check1$CVConstraint, levels = c("Max","75%","Median","25%"))
  if(nrow(check1) > 0){
    check2 <- check1 %>%
      group_by(Year, Region, RuleType, Constraint, CVConstraint, Variable) %>%
      summarise(P5 = quantile(Value, 0.05),
                       P50 = quantile(Value, 0.50),
                       Mean = mean(Value),
                       P95 = quantile(Value, 0.95))
    if(any(grepl("Fail: Risk", check2$Constraint))) check2$CVConstraint <- factor(check2$CVConstraint, levels = c("Fail: Risk","Max","75%","Median","25%"))
    if(any(grepl("Fail: Risk", check2$Constraint)) == FALSE) check2$CVConstraint <- factor(check2$CVConstraint, levels = c("Max","75%","Median","25%"))
    # if(length(unique(check2$RuleType))==3) check2$RuleType <- factor(check2$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
    # if(length(unique(check1$RuleType))==3) check1$RuleType <- factor(check1$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
    #
    p_constr3 <- ggplot() +
      geom_ribbon(data = check2 %>% filter(Variable %in% c("Catch","RelSSBdata","RelVB")), aes(x = Year, ymin = P5, ymax = P95, fill = CVConstraint), color = NA, alpha = 0.5) +
      # geom_line(data = check2 %>% filter(Variable %in% c("Catch","RelSSBdata","RelVB")), aes(x = Year, y = P50, color = CVConstraint), lwd = 1.5) +
      geom_line(data = check2 %>% filter(Variable %in% c("Catch","RelSSBdata","RelVB")), aes(x = Year, y = Mean, color = CVConstraint), lwd = 1.5) +
      geom_line(data = check1 %>% filter(Iteration == 1) %>% filter(Variable %in% c("Catch","RelSSBdata","RelVB")), aes(x = Year, y = Value)) +
      # scale_color_brewer(palette = "Spectral") +
      # scale_fill_brewer(palette = "Spectral") +
      scale_color_tableau() +
      scale_fill_tableau() +
      expand_limits(y = 0) +
      ylab("Value") + xlab("Projection Year") +
      guides(color = FALSE, fill = FALSE) +
      theme_bw(base_size = 20)
    if(length(regions)>1){
      p_constr3 <- p_constr3 + facet_grid(Variable~Region+CVConstraint, scales = "free_y")
    } else {
      p_constr3 <- p_constr3 + facet_grid(Variable~CVConstraint, scales = "free_y")
    }
    ggsave(file.path(figure_dir, "CompareConstraints_MP.png"), p_constr3, height = 10, width = 15)
  }
  #
  # check1 <- cinfo %>%
  #   right_join(find_max_sub %>% select(-P50)) %>%
  #   left_join(max_info %>% filter(Variable == "Catch")) %>%
  #   select(Iteration, Year, Region, RuleNum, RuleType, Constraint, Catch, RelVB, RelSSB) %>%
  #   tidyr::pivot_longer(cols = c(Catch,RelVB,RelSSB), names_to = "Variable", values_to = "Value") %>%
  #   filter(Constraint == "Pass")
  # if(nrow(check1) > 0){
  #   check2 <- check1 %>%
  #     group_by(Year, Region, RuleType, Constraint, Variable) %>%
  #     summarise(P5 = quantile(Value, 0.05),
  #                      P50 = quantile(Value, 0.50),
  #                      P95 = quantile(Value, 0.95))
  #   if(length(unique(check2$RuleType))==3) check2$RuleType <- factor(check2$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
  #   if(length(unique(check1$RuleType))==3) check1$RuleType <- factor(check1$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
  #   const <- unique(as.character(check1$Constraint))
  # const <- unique(as.character(check1$Constraint))
  # const1 <- const[grepl("CV",const)==FALSE]
  # const1_1 <- const1[grepl("Catch", const1) == FALSE]
  # const1_2 <- const1[grepl("Catch", const1)]
  # const1 <- c(const1_1, const1_2)
  # const2 <- const[grepl("CV", const)]
  # constx <- c(const1,const2)
  # const <- c(constx, const[which(const %in% constx == FALSE)])
  # check1$Constraint <- factor(check1$Constraint, levels = const)
  #   const <- rev(c(constx, const[which(const %in% constx == FALSE)]))
  #   check1$Constraint <- factor(check1$Constraint, levels = const)
  #   check2$Constraint <- factor(check2$Constraint, levels = const)
  #   p_constr3 <- ggplot() +
  #     geom_ribbon(data = check2 %>% filter(Variable %in% c("Catch","RelSSB","RelVB")), aes(x = Year, ymin = P5, ymax = P95, fill = RuleType), color = NA, alpha = 0.5) +
  #     geom_line(data = check2 %>% filter(Variable %in% c("Catch","RelSSB","RelVB")), aes(x = Year, y = P50, color = RuleType), lwd = 1.5) +
  #     geom_line(data = check1 %>% filter(Iteration == 1) %>% filter(Variable %in% c("Catch","RelSSB","RelVB")), aes(x = Year, y = Value)) +
  #     scale_color_brewer(palette = "Spectral") +
  #     scale_fill_brewer(palette = "Spectral") +
  #     expand_limits(y = 0) +
  #     ylab("Value") + xlab("Projection Year") +
  #     guides(color = FALSE, fill = FALSE) +
  #     theme_bw(base_size = 20)
  #   if(length(regions)>1){
  #     p_constr3 <- p_constr3 + facet_grid(Variable~Region+RuleType, scales = "free_y")
  #   } else {
  #     p_constr3 <- p_constr3 + facet_grid(Variable~RuleType, scales = "free_y")
  #   }
  #   ggsave(file.path(figure_dir, "CompareRuleTypes_Pass.png"), p_constr3, height = 10, width = 15)
  # }
  #
  # check <- output4 %>%
  #   filter(RuleType == "CPUE-based") %>%
  #   filter(Constraint == "Pass")
  # if(nrow(check) > 0) {
  #   check$CVConstraint <- factor(check$CVConstraint, levels = c("Max","75%", "Median","25%","Min"))
  #   p_cv <- ggplot(check) +
  #     geom_segment(aes(x = CV, xend = CV, y = Catch_P5, yend = Catch_P95, color = CVConstraint), lwd = 1.2, alpha = 0.8) +
  #     geom_point(aes(x = CV, y = Catch_P50, fill = CVConstraint), pch = 21, cex = 4) +
  #     expand_limits(y = 0, x = 0) +
  #     xlab("CV of catch over time and iteration") + ylab("Average annual catch") +
  #     # scale_fill_brewer(palette = "Spectral") +
  #     # scale_color_brewer(palette = "Spectral") +
  #     scale_fill_tableau() +
  #     scale_color_tableau() +
  #     theme_bw(base_size = 20)
  #   if(length(regions) > 1){
  #     p_cv <- p_cv + facet_grid(Region~., scales = "free_x")
  #   }
  #   ggsave(file.path(figure_dir, "CV_vs_Catch_Pass.png"), p_cv, height = 8, width = 12)
  # }

  #   check1 <- cinfo %>%
  #     left_join(output2 %>% filter(Variable == "Catch")) %>%
  #     select(Iteration, Year, Region, RuleNum, RuleType, Constraint, CVConstraint, Catch, CPUE) %>%
  #     filter(RuleType == "CPUE-based")
  #   if(nrow(check1) > 0){
  #   const <- unique(as.character(check1$Constraint))
  # const <- unique(as.character(check1$Constraint))
  # const1 <- const[grepl("CV",const)==FALSE]
  # const1_1 <- const1[grepl("Catch", const1) == FALSE]
  # const1_2 <- const1[grepl("Catch", const1)]
  # const1 <- c(const1_1, const1_2)
  # const2 <- const[grepl("CV", const)]
  # constx <- c(const1,const2)
  # const <- c(constx, const[which(const %in% constx == FALSE)])
  #   const <- rev(c(constx, const[which(const %in% constx == FALSE)]))
  #   check1$Constraint <- factor(check1$Constraint, levels = const)
  #     p_cpue_mp <- ggplot(check1) +
  #       geom_point(data = check1 %>% filter(Constraint != "Pass"), aes(x = CPUE, y = Catch, color = Constraint), cex = 2, alpha = 0.5) +
  #       geom_point(data = check1 %>% filter(Constraint == "Pass"), aes(x = CPUE, y = Catch, color = Constraint), cex = 2, alpha = 0.5) +
  #      # scale_color_brewer(palette = "Spectral") +
  #       scale_color_tableau() +
  #       xlab("Offset-year CPUE") +
  #       theme_bw(base_size = 20)
  #     if(length(regions) > 1){
  #       p_cpue_mp <- p_cpue_mp + facet_grid(Region~., scales = "free_x")
  #     }
  #     ggsave(file.path(figure_dir, "CPUE_vs_Catch_MP.png"), p_cpue_mp, height = 10, width = 12)
  #
  #     check2 <- check1 %>% filter(Constraint == "Pass")
  #     check2$CVConstraint <- factor(check2$CVConstraint, levels = c("Max", "75%", "Median", "25%", "Min"))
  #     p_cpue_mp <- ggplot(check2) +
  #       geom_point(aes(x = CPUE, y = Catch, color = CVConstraint), cex = 2, alpha = 0.5) +
  #       # scale_color_brewer(palette = "Spectral") +
  #       scale_color_tableau() +
  #       xlab("Offset-year CPUE") +
  #       theme_bw(base_size = 20)
  #     if(length(regions) > 1){
  #       p_cpue_mp <- p_cpue_mp + facet_grid(Region~., scales = "free_x")
  #     }
  #     ggsave(file.path(figure_dir, "CPUE_vs_Catch_MP_Pass.png"), p_cpue_mp, height = 10, width = 12)
  #   }

  #     p_cpue_mp_v2 <- p_cpue_mp +
  #       geom_vline(data = msy_info2 %>% filter(RuleType == "CPUE-based"), aes(xintercept = CPUE), lty = 2) +
  #       geom_hline(data = msy_info2 %>% filter(RuleType == "CPUE-based"), aes(yintercept = Catch), lty = 2)
  #     ggsave(file.path(figure_dir, "CPUE_vs_Catch_MP_v2.png"), p_cpue_mp_v2, height = 10, width = 12)
  #   }
  #
  #   check1 <- cinfo %>%
  #     left_join(output2 %>% filter(Variable == "Catch")) %>%
  #     select(Iteration, Year, Region, RuleNum, RuleType, Constraint, Catch, CPUE)
  #   const <- unique(as.character(check1$Constraint))
  # const <- unique(as.character(check1$Constraint))
  # const1 <- const[grepl("CV",const)==FALSE]
  # const1_1 <- const1[grepl("Catch", const1) == FALSE]
  # const1_2 <- const1[grepl("Catch", const1)]
  # const1 <- c(const1_1, const1_2)
  # const2 <- const[grepl("CV", const)]
  # constx <- c(const1,const2)
  # const <- c(constx, const[which(const %in% constx == FALSE)])
  #   check1$Constraint <- factor(check1$Constraint, levels = const)
  #   if(length(unique(check1$RuleType))==3) check1$RuleType <- factor(check1$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
  #   p_cpue_all <- ggplot(check1) +
  #     geom_point(data = check1 %>% filter(Constraint != "Pass"), aes(x = CPUE, y = Catch, color = Constraint), cex = 2, alpha = 0.5) +
  #     geom_point(data = check1 %>% filter(Constraint == "Pass"), aes(x = CPUE, y = Catch, color = Constraint), cex = 2, alpha = 0.5) +
  #     scale_color_brewer(palette = "Spectral") +
  #     xlab("Offset-year CPUE") +
  #     theme_bw(base_size = 20)
  #   if(length(regions) > 1){
  #     p_cpue_all <- p_cpue_all + facet_wrap(Region~RuleType)
  #   } else {
  #     p_cpue_all <- p_cpue_all + facet_wrap(~RuleType)
  #   }
  #   ggsave(file.path(figure_dir, "CPUE_vs_Catch.png"), p_cpue_all, height = 10, width = 12)
  #
  #   if(length(unique(msy_info2$RuleType == 3))) msy_info2$RuleType <- factor(msy_info2$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
  #   p_cpue_all_v2 <- p_cpue_all +
  #       geom_vline(data = msy_info2, aes(xintercept = CPUE), lty = 2, lwd = 2) +
  #       geom_hline(data = msy_info2, aes(yintercept = Catch), lty = 2, lwd = 2)
  #   ggsave(file.path(figure_dir, "CPUE_vs_Catch_v2.png"), p_cpue_all, height = 10, width = 12)
  #
  #   check1 <- cinfo %>%
  #     left_join(max_info %>% filter(Variable == "Catch")) %>%
  #     select(Iteration, Year, Region, RuleNum, RuleType, Constraint, Catch, CPUE) %>%
  #     filter(RuleType == "CPUE-based")
  #   if(nrow(check1) > 0){
  #   const <- unique(as.character(check1$Constraint))
  # const <- unique(as.character(check1$Constraint))
  # const1 <- const[grepl("CV",const)==FALSE]
  # const1_1 <- const1[grepl("Catch", const1) == FALSE]
  # const1_2 <- const1[grepl("Catch", const1)]
  # const1 <- c(const1_1, const1_2)
  # const2 <- const[grepl("CV", const)]
  # constx <- c(const1,const2)
  # const <- c(constx, const[which(const %in% constx == FALSE)])
  #   const <- rev(c(constx, const[which(const %in% constx == FALSE)]))
  #   check1$Constraint <- factor(check1$Constraint, levels = const)
  #     p_cpue_mp <- ggplot(check1) +
  #       geom_point(data = check1 %>% filter(Constraint != "Pass"), aes(x = CPUE, y = Catch, color = Constraint), cex = 2, alpha = 0.5) +
  #       geom_point(data = check1 %>% filter(Constraint == "Pass"), aes(x = CPUE, y = Catch, color = Constraint), cex = 2, alpha = 0.5) +
  #      scale_color_brewer(palette = "Spectral") +
  #       xlab("Offset-year CPUE") +
  #       theme_bw(base_size = 20)
  #     if(length(regions) > 1){
  #       p_cpue_mp <- p_cpue_mp + facet_wrap(Region~.)
  #     }
  #     ggsave(file.path(figure_dir, "CPUE_vs_Catch_MP_Max.png"), p_cpue_mp, height = 10, width = 12)
  #
  #     p_cpue_mp_v2 <- p_cpue_mp +
  #       geom_vline(data = msy_info2 %>% filter(RuleType == "CPUE-based"), aes(xintercept = CPUE), lty = 2, lwd = 1.5) +
  #       geom_hline(data = msy_info2 %>% filter(RuleType == "CPUE-based"), aes(yintercept = Catch), lty = 2, lwd = 1.5)
  #     ggsave(file.path(figure_dir, "CPUE_vs_Catch_MP_Max_v2.png"), p_cpue_mp_v2, height = 10, width = 12)
  #   }
  #
  #   check1 <- cinfo %>%
  #     right_join(msy_info %>% filter(Variable == "Catch")) %>%
  #     select(Iteration, Year, Region, RuleNum, RuleType, Constraint, Catch, CPUE)  %>%
  #     filter(Region != "Total")
  #   if(length(unique(check1$RuleType))==3) check1$RuleType <- factor(check1$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
  #   p_cpue_all <- ggplot(check1) +
  #     geom_point(data = check1, aes(x = CPUE, y = Catch, color = RuleType), cex = 2, alpha = 0.5) +
  #     scale_color_brewer(palette = "Spectral") +
  #     xlab("Offset-year CPUE") +
  #     theme_bw(base_size = 20)
  #   if(length(regions) > 1){
  #     p_cpue_all <- p_cpue_all + facet_wrap(Region~.)
  #   }
  #   ggsave(file.path(figure_dir, "CPUE_vs_Catch.png"), p_cpue_all, height = 10, width = 12)
  #
  #   if(length(unique(msy_info2$RuleType == 3))) msy_info2$RuleType <- factor(msy_info2$RuleType, levels = c("FixedCatch", "CPUE-based", "FixedF"))
  #   p_cpue_all_v2 <- p_cpue_all +
  #       geom_vline(data = msy_info2, aes(xintercept = CPUE, color = RuleType), lty = 2, lwd = 1.5) +
  #       geom_hline(data = msy_info2, aes(yintercept = Catch, color = RuleType), lty = 2, lwd = 1.5)
  #   ggsave(file.path(figure_dir, "CPUE_vs_Catch_v2.png"), p_cpue_all_v2, height = 10, width = 12)
  #
    sub <- output4 %>% filter(RuleType == "CPUE-based") %>% filter(Region != "Total")
  #   const <- unique(as.character(sub$Constraint))
  # const1 <- const[grepl("CV",const)==FALSE]
  # const1_1 <- const1[grepl("Catch", const1) == FALSE]
  # const1_2 <- const1[grepl("Catch", const1)]
  # const1 <- c(const1_1, const1_2)
  # const2 <- const[grepl("CV", const)]
  # constx <- c(const1,const2)
  # const <- c(constx, const[which(const %in% constx == FALSE)])
  #   sub$Constraint <- factor(sub$Constraint, levels = const)
  if(nrow(sub)>0){
    msy <- unique(msy_info %>% filter(RuleType == "CPUE-based") %>% select(par1:par10,CVConstraint)) %>% mutate(Constraint = "Pass")
    msy <- msy %>% filter(Region != "Total")
    msy$CVConstraint <- factor(msy$CVConstraint, levels = c("Max", "75%", "Median", "25%", "Min"))
    p_rule <- ggplot(sub) +
      geom_segment(aes(x = par2, xend = par3, y = 0, yend = par5), lwd = 1.5) +
      geom_segment(aes(x = par3, xend = par4, y = par5, yend = par5 ), lwd = 1.5) +
      geom_segment(aes(x = par4, xend = par4, y = par5, yend = par5 + (par5 * par7)), lwd = 1.5) +
      geom_segment(aes(x = par4, xend = par4 + par6, y = par5 + (par5 * par7), yend = par5 + (par5 * par7)), lwd = 1.5) +
      geom_segment(aes(x = par4 + par6, xend = par4 + par6, y = par5 + (par5 * par7), yend = par5 + 2*(par5 * par7)), lwd = 1.5) +
      geom_segment(aes(x = par4 + par6, xend = par4 + 2*par6, y = par5 + 2*(par5 * par7), yend = par5 + 2*(par5 * par7)), lwd = 1.5) +
      geom_segment(aes(x = par4 + 2*par6, xend = par4 + 2*par6, y = par5 + 2*(par5 * par7), yend = par5 + 3*(par5 * par7)), lwd = 1.5) +
      geom_segment(aes(x = par4 + 2*par6, xend = par4 + 3*par6, y = par5 + 3*(par5 * par7), yend = par5 + 3*(par5 * par7)), lwd = 1.5) +
      geom_segment(aes(x = par4 + 3*par6, xend = par4 + 3*par6, y = par5 + 3*(par5 * par7), yend = par5 + 4*(par5 * par7)), lwd = 1.5) +
      geom_segment(aes(x = par4 + 3*par6, xend = par4 + 4*par6, y = par5 + 4*(par5 * par7), yend = par5 + 4*(par5 * par7)), lwd = 1.5) +
      geom_segment(data = msy, aes(color = CVConstraint, x = par2, xend = par3, y = 0, yend = par5 ), lwd = 1.5) +
      geom_segment(data = msy, aes(color = CVConstraint, x = par3, xend = par4, y = par5, yend = par5  ), lwd = 1.5) +
      geom_segment(data = msy, aes(color = CVConstraint, x = par4, xend = par4, y = par5, yend = par5 + (par5 * par7) ), lwd = 1.5) +
      geom_segment(data = msy, aes(color = CVConstraint, x = par4, xend = par4 + par6, y = par5 + (par5 * par7), yend = par5 + (par5 * par7) ), lwd = 1.5) +
      geom_segment(data = msy, aes(color = CVConstraint, x = par4 + par6, xend = par4 + par6, y = par5 + (par5 * par7), yend = par5 + 2*(par5 * par7) ), lwd = 1.5) +
      geom_segment(data = msy, aes(color = CVConstraint, x = par4 + par6, xend = par4 + 2*par6, y = par5 + 2*(par5 * par7), yend = par5 + 2*(par5 * par7) ), lwd = 1.5) +
      geom_segment(data = msy, aes(color = CVConstraint, x = par4 + 2*par6, xend = par4 + 2*par6, y = par5 + 2*(par5 * par7), yend = par5 + 3*(par5 * par7) ), lwd = 1.5) +
      geom_segment(data = msy, aes(color = CVConstraint, x = par4 + 2*par6, xend = par4 + 3*par6, y = par5 + 3*(par5 * par7), yend = par5 + 3*(par5 * par7) ), lwd = 1.5) +
      geom_segment(data = msy, aes(color = CVConstraint, x = par4 + 3*par6, xend = par4 + 3*par6, y = par5 + 3*(par5 * par7), yend = par5 + 4*(par5 * par7) ), lwd = 1.5) +
      geom_segment(data = msy, aes(color = CVConstraint, x = par4 + 3*par6, xend = par4 + 4*par6, y = par5 + 4*(par5 * par7), yend = par5 + 4*(par5 * par7) ), lwd = 1.5) +
      # scale_color_brewer(palette = "Spectral") +
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



}
