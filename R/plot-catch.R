#' Plot catch
#'
#' Plot the catch data and fit to the data.
#'
#' @param object and LSD object
#' @param show_proj show projection or not
#' @param scales free or fixed
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param figure_dir the directory to save to
#' @import dplyr
#' @import ggplot2
#' @import ggrepel
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#' 
plot_catch <- function(object,
                       show_proj = FALSE,
                       scales = "free",
                       xlab = "Fishing year",
                       ylab = "Catch (tonnes)",
                       figure_dir = "figure/")
{
    data <- object@data
    mcmc <- object@mcmc
    
    YR <- "YR" # label for the season before the season change year
    
    n_iter <- nrow(mcmc[[1]])
    years <- data$first_yr:data$last_yr
    pyears <- data$first_yr:data$last_proj_yr
    seasons <- c("AW", "SS")
    regions <- 1:data$n_area
    rules <- 1:data$n_rules

    # Fit to recreational catch
    pcatch <- mcmc$pred_catch_recreational_ryt
    dimnames(pcatch) <- list("Iteration" = 1:n_iter, Region = regions, Year = years, Season = seasons)
    d <- reshape2::melt(pcatch, value.name = "Catch") %>%
        dplyr::group_by(Iteration, Region, Year) %>%
        dplyr::summarise(Catch = sum(Catch))

    #dcatch <- data.frame(Year = c(1994, 1996, 2011), Catch = c(95, 149, 42))
    # dcatch <- data.frame(Year = c(1994, 1996, 2011), Catch = c(142000, 223000, 60100))
    
    # p <- ggplot(d, aes(x = Year, y = Catch)) +
    #     stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
    #     stat_summary(fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
    #     stat_summary(fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
    #     geom_point(data = dcatch, colour = 'red') +
    #     expand_limits(y = 0) +
    #     theme_lsd()
    # ggsave(paste0(figure_dir, "recreational_catch.png"), p)
    
    # Catch
    comm <- data$data_catch_commercial_ryt
    dimnames(comm) <- list("Region" = regions, "Year" = years, "Season" = seasons)
    comm <- reshape2::melt(comm, value.name = "Catch") %>%
                dplyr::mutate(Sector = "Commercial")

    rec <- data$data_catch_recreational_ryt
    dimnames(rec) <- list("Region" = regions, "Year" = years, "Season" = seasons)
    rec <- reshape2::melt(rec, value.name = "Catch") %>%
                dplyr::mutate(Sector = "Recreational")

    dsl <- rbind(comm, rec)
    dsl <- dplyr::mutate(dsl, Iteration = NA, Type = "SL", Data = "Observed")
    
    dnsl <- data$data_catch_nsl_ryt
    dimnames(dnsl) <- list("Region" = regions, "Year" = years, "Season" = seasons)
    dnsl <- reshape2::melt(dnsl, value.name = "Catch") %>%
        dplyr::mutate(Iteration = NA, Type = "NSL", Data = "Observed", Sector = "Customary + illegal")

    psl <- mcmc$pred_catch_sl_jryt
    dimnames(psl) <- list("Rule"=rules, "Iteration" = 1:n_iter, "Region" = regions, "Year" = pyears, "Season" = seasons)
    psl <- reshape2::melt(psl, value.name = "Catch") %>%
        dplyr::mutate(Type = "SL", Data = "Expected")
    
    pnsl <- mcmc$pred_catch_nsl_jryt
    dimnames(pnsl) <- list("Rule"=rules, "Iteration" = 1:n_iter, "Region" = regions, "Year" = pyears, "Season" = seasons)
    pnsl <- reshape2::melt(pnsl, value.name = "Catch") %>%
        dplyr::mutate(Type = "NSL", Data = "Expected")
    
    ph <- mcmc$pred_death_handling_ryt
    dimnames(ph) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = pyears, "Season" = seasons)
    ph <- reshape2::melt(ph, value.name = "Catch") %>%
        dplyr::mutate(Type = "Handling mortality", Data = "Expected")
    phlist <- lapply(rules, function(x){
        out <- ph %>% mutate(Rule = x)
        return(out)
    })
    ph <- do.call(rbind, phlist)

    
    # Observed catch
    dcatch <- rbind(dsl, dnsl) %>%
        dplyr::filter(!(Year < data$season_change_yr & Season == "SS")) %>%
        dplyr::mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR))

    # Predicted catch
    pcatch <- rbind(psl, pnsl, ph) %>%
        dplyr::filter(!(Year < data$season_change_yr & Season == "SS")) %>%
        dplyr::mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR))
    if (!show_proj) {
        pcatch <- dplyr::filter(pcatch, Year <= data$last_yr)
    }

    # This simply sets up factor order in plot
    ord1 <- c("NSL", "SL", "Handling mortality")
    ord2 <- c(YR, "AW", "SS")
    pcatch$Type <- factor(pcatch$Type, levels = ord1)
    pcatch$Season <- factor(pcatch$Season, levels = ord2)
    dcatch$Type <- factor(dcatch$Type, levels = ord1)
    dcatch$Season <- factor(dcatch$Season, levels = ord2)

    dcatch1 <- dcatch %>%
        dplyr::group_by(Region, Year, Season, Iteration, Type, Data) %>%
        dplyr::summarise(Catch = sum(Catch)) 
    
    p <- ggplot(data = pcatch, aes(x = Year, y = Catch))
    if (show_proj) p <- p + geom_vline(aes(xintercept = data$last_yr), linetype = "dashed")
    p <- p + stat_summary(fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
        geom_point(data = dcatch1, color = "red") +
        #expand_limits(y = 0) +
        xlab(xlab) + ylab(ylab) +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd()
    if (data$n_area > 1) {
        if(data$n_rules==1){
          p <- p + facet_grid(Region + Type ~ Season, scales = "free")
        } else {
          p <- p + facet_wrap(Region~Rule + Type ~ Season, scales="free") 
        }
    } else {
        if(data$n_rules==1){
          p <- p + facet_grid(Type ~ Season, scales = "free")
        } else {
          p <- p + facet_wrap(Rule+Type ~ Season, scales="free")
        }
    }
    ggsave(paste0(figure_dir, "catch.png"), p, width = 8)


    # Plot of catch summed over seasons and drop handling mortality
    pcatch_sum <- dplyr::group_by(pcatch, Rule, Iteration, Region, Year, Type, Data) %>%
        dplyr::summarise(Catch = sum(Catch)) %>%
        dplyr::filter(Type != "Handling mortality")
    dcatch_sum <- dplyr::group_by(dcatch, Iteration, Region, Year, Type, Data) %>%
        dplyr::summarise(Catch = sum(Catch)) %>%
        dplyr::filter(Type != "Handling mortality")
    pcatch_sum$Type <- factor(pcatch_sum$Type, levels = c("SL", "NSL"))

    p <- ggplot(data = pcatch_sum, aes(x = Year, y = Catch))
    if (show_proj) geom_vline(aes(xintercept = data$last_yr), linetype = "dashed")
    p <- p + stat_summary(fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
        geom_point(data = dcatch_sum, aes(x = Year, y = Catch), color = "red") +
        expand_limits(y = 0) +
        xlab(xlab) + ylab(ylab) +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd()
    if (data$n_area > 1) {
        if(data$n_rules == 1){
          p <- p + facet_grid(Region ~ Type, scales = scales)
        } else{
          p <- p + facet_wrap(Region+Rule~Type, scales=scales)
        }
    } else {
        if(data$n_rules==1){
          p <- p + facet_grid(Type ~ ., scales = scales)
        } else {
          p <- p + facet_wrap(Type ~ Rule, scales=scales)
        }
    }
    ggsave(paste0(figure_dir, "catch_sums.png"), p)


    # Catch residuals
    rsl <- mcmc$resid_catch_sl_jryt
    dimnames(rsl) <- list("Rule"=rules, "Iteration" = 1:n_iter, "Region" = regions,
                          "Year" = pyears, "Season" = seasons)
    rsl <- reshape2::melt(rsl, value.name = "Catch") %>%
        dplyr::mutate(Type = "SL", Data = "Residual")
    rnsl <- mcmc$resid_catch_nsl_jryt
    dimnames(rnsl) <- list("Rule"=rules, "Iteration" = 1:n_iter, "Region" = regions,
                           "Year" = pyears, "Season" = seasons)
    rnsl <- reshape2::melt(rnsl, value.name = "Catch") %>%
        dplyr::mutate(Type = "NSL", Data = "Residual")

    rcatch <- rbind(rsl, rnsl)
    rcatch$Type <- factor(rcatch$Type, levels = c("SL","NSL"))
    
    p <- ggplot(data = dplyr::filter(rcatch), aes(x = Year, y = Catch)) +
        stat_summary(fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
        #geom_violin() +
        expand_limits(y = 0) +
        xlab(xlab) + ylab("Residual") +
        theme_lsd()
    if(data$n_rules==1){
      p <- p + facet_grid(Region + Type ~ Season, scales = "free")
    } else {
        p <- p + facet_grid(Region + Type ~ Season + Rule)
    }
    ggsave(paste0(figure_dir, "catch_resid.png"), p)

    ## catch area
    msy <- mcmc$MSY_r
    msy <- reshape2::melt(msy) %>%
            dplyr::rename("Iteration" = iterations) %>%
            dplyr::select(-Var2) %>%
            dplyr::group_by(Iteration, value) %>%
            dplyr::rename("MSY" = value)

    dcatch2 <- dcatch %>% 
            dplyr::group_by(Year, Sector, Iteration) %>%
            dplyr::summarise(Catch = sum(Catch))
    dcatch2$Iteration <- 1
    dcatch2 <- dplyr::left_join(dcatch2, msy) %>% 
        dplyr::mutate(Label = "") %>%
        dplyr::mutate(Label = ifelse(Iteration == 1 & Year == max(years) & Sector == "Commercial", "MSY", ""))

    p <- ggplot(dcatch2) + 
            geom_area(aes(x = Year, y = Catch, colour = Sector, fill = Sector), position = "stack") +
            xlab(xlab) + ylab(ylab) +
            theme_lsd() 

    ## add msy
    p <- p + stat_summary(aes(x = Year, y = MSY), fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.125, colour = "black", fill="black" ) +
            stat_summary(aes(x = Year, y = MSY), fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.25, colour = NA) +
            stat_summary(aes(x = Year, y = MSY), fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) + 
            ggrepel::geom_label_repel(data = dcatch2, aes(x = Year, y = MSY, label = Label), fill = "black", size = 5, color = 'white', force = 10, segment.color = '#bbbbbb', min.segment.length = unit(0, "lines"))

    ggsave(paste0(figure_dir, "catch_type.png"), p, width = 10)
}
