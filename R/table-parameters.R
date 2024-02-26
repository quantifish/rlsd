#' Create residuals table
#'
#' @param object and LSD object
#' @param figure_dir the directory to save to
#' @param save_table TRUE or FALSE
#' @import dplyr
#' @importFrom tidyr pivot_longer pivot_wider
#' @export
table_parameters <- function(object, figure_dir = "figure/", save_table = TRUE)
{
  mcmc <- object@mcmc_pars
  names(mcmc) <- c("Iteration", "Chain", "Parameter", "Estimate")

  ## sdnr and mar
  # map <- object@map
  # map2 <- map %>%
  # reshape2::melt() %>%
  # rename(Parameter = L1, Estimate = value) %>%
  # filter(grepl("sdnr", Parameter) | grepl("MAR", Parameter)) %>%
  # select(Parameter, Estimate) %>%
  # mutate(Dummy = 1) %>%
  # group_by(Parameter) %>%
  # mutate(N = cumsum(Dummy)) %>%
  # mutate(Parameter = paste0(Parameter, " [", N, "]")) %>%
  # select(Parameter, Estimate) %>%
  # mutate(Type = case_when(grepl('sdnr', Parameter) ~ 1,
  #                          grepl("MAR", Parameter) ~ 2)) %>%
  # mutate(Order = case_when(grepl('sexr', Parameter) ~ 1,
  #                          grepl("cpue", Parameter) ~ 2)) %>%
  # arrange(Type, Order) %>%
  # select(-c(Type, Order)) %>%
  # mutate(P5 = NA, P95 = NA)


  ## weights
  data <- object@data
  data2 <- data %>%
  reshape2::melt() %>%
  rename(Parameter = L1, Estimate = value) %>%
  filter(grepl("_wt", Parameter)) %>%
  select(Parameter, Estimate) %>%
  mutate(Dummy = 1) %>%
  group_by(Parameter) %>%
  mutate(N = cumsum(Dummy)) %>%
  mutate(Parameter = paste0(Parameter, " [", N, "]")) %>%
  select(Parameter, Estimate) %>%
  mutate(P5 = NA, P95 = NA)


  pars <- mcmc %>%
    group_by(Parameter) %>%
    summarise(P5 = quantile(Estimate, 0.05),
              P95 = quantile(Estimate, 0.95),
              Estimate = median(Estimate))
  
  sdnr_mar <- pars %>%
    filter(grepl("sdnr", Parameter) | grepl("MAR", Parameter)) %>%
    mutate(Dummy = 1) %>%
    group_by(Parameter) %>%
    mutate(N = cumsum(Dummy)) %>%
    mutate(Parameter = paste0(Parameter, " [", N, "]")) %>%
    select(-c(Dummy, N)) %>%
    mutate(Type = case_when(grepl('sdnr', Parameter) ~ 1,
                            grepl("MAR", Parameter) ~ 2)) %>%
    mutate(Order = case_when(grepl('sexr', Parameter) ~ 1,
                             grepl("cpue", Parameter) ~ 2)) %>%
    arrange(Type, Order) %>%
    select(-c(Type, Order))

  likes <- pars %>% filter(grepl("lp_", Parameter)) %>%
  mutate(Order = case_when(grepl("lp__", Parameter) ~ 1,
                           grepl("total", Parameter) ~ 2,
                           grepl("prior", Parameter) ~ 3,
                           grepl('tag', Parameter) ~ 4,
                           grepl("sexr", Parameter) ~ 5,
                           grepl("lf", Parameter) ~ 6,
                           grepl('cpue', Parameter) ~ 7, 
                           grepl("puerulus", Parameter) ~ 8)) %>%
  arrange(Order) %>%
  select(-Order)

  par1 <- pars %>%
      filter(grepl("R0", Parameter) | grepl("_M_", Parameter) | grepl("_mat_", Parameter))

  growth <- pars %>%
      filter(grepl("grow", Parameter)) %>%
      mutate(Order = case_when(grepl("[1]", Parameter) ~ 1,
                               grepl("[2]", Parameter) ~ 2)) %>%
      mutate(Order = ifelse(grepl("_sd_", Parameter), 3, Order)) %>%
      arrange(Order) %>%
      select(-Order)

  mu <- pars %>% 
    filter(grepl("init_erate", Parameter))

  q <- pars %>%
    filter(grepl("q_cpue", Parameter))

  vuln <- pars %>% 
    filter(grepl('vuln', Parameter))

  sel <- pars %>%
    filter(grepl("_sel_", Parameter)) %>%
    tidyr::separate(Parameter, c("Par", "Sex"), "i", remove = FALSE) %>%
    arrange(Sex, Par) %>%
    select(-c(Par, Sex))

  b_rep <- pars %>%
    filter(grepl("par", Parameter) == FALSE) %>%
    filter(grepl('lp_', Parameter) == FALSE) %>%
    tidyr::separate(Parameter, c("Par", "Type"), "[\\[\\]]", remove = FALSE) %>%
    filter(grepl("B", Par)) %>%
    filter(grepl("SSB", Par) == FALSE) %>%
    filter(grepl("Btot", Par) == FALSE) %>%
    filter(substr(Parameter, 1, 1) != "n") %>%
    filter(grepl("Bmoney", Par) == FALSE) %>%
    filter(grepl("Bmsy", Par) == FALSE) %>%
    filter(grepl("male", Par) == FALSE) %>%
    mutate(Order = case_when(Par == "B0_r" ~ 1,
                             Par == "B0now_r" ~ 2, 
                             Par == "Bmin_jr" ~ 3,
                             Par == "Bref_jr" ~ 4,
                             Par == "Bcurr_jr" ~ 5,
                             Par == "Bproj_jr" ~ 6,
                             Par == "Bref_B0_jr" ~ 7,
                             Par == "Bref_B0now_jr" ~ 8,
                             Par == "Bcurr_B0_jr" ~ 9,
                             Par == "Bproj_B0_jr" ~ 10,
                             Par == "Bcurr_B0now_jr" ~ 11,
                             Par == "Bproj_B0now_jr" ~ 12,
                             Par == "Bproj_Bcurr_jr" ~ 13,
                             Par == "Bcurr_Bref_jr" ~ 14,
                             Par == "Bproj_Bref_jr" ~ 15)) %>%
    arrange(Order) %>%
    select(-c(Par, Type, Order))

  expl <- pars %>%
  filter(substr(Parameter, 1, 1) == "U") %>%
  filter(grepl("money", Parameter) == FALSE) %>%
  tidyr::separate(Parameter, c("Par", "Type"), "[\\[\\]]", remove = FALSE) %>%
  mutate(Order = case_when(Par == "Uref_jr" ~ 1,
                           Par == "Ucurr_jtr" ~ 2,
                           Par ==  "Uproj_jtr" ~ 3,
                           grepl("Ucurr_Uref", Par) ~ 4,
                           grepl("Uproj_Uref", Par) ~ 5,
                           grepl("Uproj_Ucurr", Par) ~ 6)) %>%
  arrange(Order) %>%
  select(-c(Order, Par, Type))

  ssb <- pars %>%
    filter(grepl("par", Parameter) == FALSE) %>%
    filter(grepl('lp_', Parameter) == FALSE) %>%
    tidyr::separate(Parameter, c("Par", "Type"), "[\\[\\]]", remove = FALSE) %>%
    filter(grepl("SSB", Par)) %>%
    filter(grepl('n_', Par) == FALSE) %>%
    filter(grepl("msy", Par) == FALSE) %>%
    mutate(Order = case_when(Par == "SSB0_r" ~ 1,
                             Par == "SSB0now_r" ~ 2, 
                             Par == "SSBcurr_jr" ~ 5,
                             Par == "SSBproj_jr" ~ 6,
                             Par == "SSBcurr_SSB0_jr" ~ 9,
                             Par == "SSBproj_SSB0_jr" ~ 10,
                             Par == "SSBcurr_SSB0now_jr" ~ 11,
                             Par == "SSBproj_SSB0now_jr" ~ 12,
                             Par == "SSBproj_SSBcurr_jr" ~ 13)) %>%
    arrange(Order) %>%
    select(-c(Par, Type, Order))

  tb <- pars %>%
    filter(grepl("par", Parameter) == FALSE) %>%
    filter(grepl('lp_', Parameter) == FALSE) %>%
    tidyr::separate(Parameter, c("Par", "Type"), "[\\[\\]]", remove = FALSE) %>%
    filter(grepl("Btot", Par)) %>%
    filter(grepl('n_', Par) == FALSE) %>%
    filter(grepl("msy", Par) == FALSE) %>%
    mutate(Order = case_when(Par == "Btot0_r" ~ 1,
                             Par == "Btot0now_r" ~ 2, 
                             Par == "Btot_curr_jr" ~ 5,
                             Par == "Btot_proj_jr" ~ 6,
                             Par == "Btot_curr_Btot0_jr" ~ 9,
                             Par == "Btot_proj_Btot0_jr" ~ 10,
                             Par == "Btot_curr_Btot0now_jr" ~ 11,
                             Par == "Btot_proj_Btot0now_jr" ~ 12,
                             Par == "Btot_proj_Btot_curr_jr" ~ 13)) %>%
    arrange(Order) %>%
    select(-c(Par, Type, Order)) 

  other <- pars %>%
  filter(substr(Parameter, 1, 1) %in% c("H", "C", "R")) %>%
  mutate(Order = case_when(grepl("R", Parameter) ~ 1,
                           grepl("H", Parameter) ~ 2)) %>%
  select(-Order)

  ratio <- pars %>%
  filter(grepl("male", Parameter))

  probs <- mcmc %>%
  filter(substr(Parameter, 1, 1) == "n") %>%
  filter(grepl("money", Parameter) == FALSE) %>%
  filter(grepl("msy", Parameter) == FALSE) %>%
  group_by(Parameter) %>%
  summarise(Estimate = mean(Estimate)) %>%
  mutate(P5 = NA, P95 = NA) %>%
  mutate(Order = case_when(grepl("min", Parameter) ~ 1,
                           grepl("_Bcurr", Parameter) ~ 2,
                           grepl("_Bproj", Parameter) ~ 3,
                           grepl("SSB0_", Parameter) ~ 4,
                           grepl("SSB0now_", Parameter) ~ 5,
                           grepl("SSBproj_g_SSBcurr_", Parameter) ~ 6,
                           grepl("Btot", Parameter) ~ 7,
                           grepl("_U", Parameter) ~ 8)) %>%
  arrange(Order) %>%
  select(-Order)
 
  # if (length(object@regions) == 1) {
  #   pars <- pars %>% filter(.data$Parameter != "Bmsy_r[1]",
  #                           .data$Parameter != "MSY_r[1]",
  #                           .data$Parameter != "Fmult_r[1]",
  #                           .data$Parameter != "Fmsy_r[1]",
  #                           .data$Parameter != "SSBmsy_r[1]",
  #                           .data$Parameter != "n_Bcurr_g_Bmsy_r[1]",
  #                           .data$Parameter != "n_Bref_g_Bmsy_r[1]",
  #                           .data$Parameter != "n_SSBcurr_g_SSBmsy_r[1]")
  # } else {
  #   for (i in 1:(length(object@regions) + 1)) {
  #     #remove Fmult and MSY derived quants
  #     pars <- pars %>% filter(.data$Parameter != paste0("Bmsy_r", "[", i, "]"),
  #                             .data$Parameter != paste0("MSY_r","[", i, "]"),
  #                             .data$Parameter != paste0("Fmult_r","[", i, "]"),
  #                             .data$Parameter != paste0("Fmsy_r","[", i, "]"),
  #                             .data$Parameter != paste0("SSBmsy_r","[", i, "]"),
  #                             .data$Parameter != paste0("n_Bcurr_g_Bmsy_r","[", i, "]"),
  #                             .data$Parameter != paste0("n_Bref_g_Bmsy_r","[", i, "]"),
  #                             .data$Parameter != paste0("n_SSBcurr_g_SSBmsy_r","[", i, "]"))
  #   }
  # }

  out <- bind_rows(data2, sdnr_mar, likes, par1, growth, mu, q, vuln, sel, b_rep, expl, ssb, tb, other, ratio, probs)

  extra <- pars %>% filter(Parameter %in% out$Parameter == FALSE) %>% filter(grepl("sdnr", Parameter) == FALSE, grepl("MAR", Parameter) == FALSE)

  out2 <- bind_rows(out, extra) 
  out2 <- cbind.data.frame(out2[,"Parameter"], out2[,"P5"], out2[,"Estimate"], out2[,"P95"])

  if (save_table == TRUE) {
    write.csv(out2, file.path(figure_dir, "Parameters_summary.csv"), row.names = FALSE)
  } else {
    return(out2)
  }
}
