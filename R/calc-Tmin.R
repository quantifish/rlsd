#' Calculate Tmin
#'
#' @export
#'
calc_Tmin <- function()
{
    modlist <- c("CRA2_base", "CRA2_2Xrecce", "CRA2_qdrift")
    datalist <- lapply(1:length(modlist), function(x) rstan::read_rdump(file.path(modlist[x], "lsd.dat")))
    outlist <- lapply(1:length(modlist), function(x) stanUtils::read_stan_mcmc(file.path(modlist[x], "constant_mpe.csv")))

    do_extract <- c("Bref_jr", "biomass_vulnref_AW_jyr")
    mcmclist <- lapply(1:length(modlist), function(x) stanUtils::extract(outlist[[x]], pars = do_extract, permuted = TRUE, include = TRUE))

    n_iter <- lapply(1:length(modlist), function(x) nrow(mcmclist[[x]][[1]]))
    pyears <- lapply(1:length(modlist), function(x) datalist[[x]]$first_yr:datalist[[x]]$last_proj_yr)
    n_rules <- lapply(1:length(modlist), function(x) datalist[[x]]$n_rules)
    regions <- lapply(1:length(modlist), function(x) 1:datalist[[x]]$n_area)

    vulnreflist <- lapply(1:length(modlist), function(x){
        vb <- mcmclist[[x]]$biomass_vulnref_AW_jyr
        dimnames(vb) <- list("Iteration" = 1:n_iter[[x]], "Rules" = 1:n_rules[[x]], "Year" = pyears[[x]], "Region" = regions[[x]])
        vb <- reshape2::melt(vb) %>%
            dplyr::filter(Rules == 1) %>%
            dplyr::rename("VB" = value) %>%
            dplyr::mutate("Model" = modlist[x])
        return(vb)
    })
    vulnref <- do.call(rbind, vulnreflist)

    Breflist <- lapply(1:length(modlist), function(x){
        bref <- mcmclist[[x]]$Bref_jr
        dimnames(bref) <- list("Iteration" = 1:n_iter[[x]], "Rules" = 1:n_rules[[x]], "Region" = regions[[x]])
        bref <- reshape2::melt(bref) %>%
                dplyr::filter(Rules == 1) %>%
                dplyr::rename("Bref" = value) %>%
                dplyr::mutate("Model" = modlist[x])
    })
    Bref <- do.call(rbind, Breflist)

    df <- full_join(vulnref, Bref) %>%
        dplyr::mutate("B_Bref" = ifelse(VB > Bref, 1, 0))

    pdf <- df %>%
        dplyr::group_by(Year, Model) %>%
        dplyr::summarise("PBref" = mean(B_Bref))

    now <- 2018
    tmin_yr <- sapply(1:length(modlist), function(x) pdf$Year[which(pdf$Model == modlist[x] & pdf$Year > now & pdf$PBref > 0.5)[1]])
    tmin <- tmin_yr - now
    ptmin <- sapply(1:length(modlist), function(x) pdf$PBref[which(pdf$Model == modlist[x] & pdf$Year > now & pdf$PBref > 0.5)[1]])
    names(ptmin) <- modlist

    pdf2 <- data.frame("Model" = modlist, "TminYr" = tmin_yr, "Tmin" = tmin, "Tmin2" = 2*tmin)

    df <- left_join(df, pdf2, by=c("Model")) %>%
        dplyr::group_by(Iteration, Rules, Year, Region, VB, Model, Bref, B_Bref, TminYr, Tmin, Tmin2)


    p <- ggplot(df, aes(x = Year)) + 
        geom_vline(aes(xintercept = datalist[[1]]$first_ref_yr), linetype = "dashed", colour = "gray") + 
        geom_vline(aes(xintercept = datalist[[1]]$last_ref_yr), linetype = "dashed", colour = "gray") +


        stat_summary(aes(y = Bref), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, fill = "orange") +
        stat_summary(aes(y = Bref), fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA, fill = "orange") +
        stat_summary(aes(y = Bref), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, colour = "orange") +

        stat_summary(aes(y = VB), fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA, fill = "steelblue") +
        stat_summary(aes(y = VB), fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA, fill = "steelblue") +
        stat_summary(aes(y = VB), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, colour = "steelblue") +

        geom_vline(aes(xintercept = now), linetype = "dashed") + 
        geom_vline(aes(xintercept = TminYr), linetype = "dashed") +
        geom_vline(aes(xintercept = now + Tmin2), linetype = "dashed") +

        facet_grid(Model~.) +
        expand_limits(y = 0) +
        xlab("Fishing year") + ylab("Reference biomass (tonnes)") +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd()

    ggsave("Tmin_calc.png", p, width = 10)

}
