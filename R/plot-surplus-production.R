#' Plot surplus production
#' 
#' @param object an LSD object
#' @param xlab the a axis label
#' @param figure_dir the directory to save the figure to
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @export
#' 
plot_surplus_production <- function(object, xlab = "Fishing year", figure_dir = "figure/")
{
	data <- object@data
    map <- object@map
    mcmc <- object@mcmc
    
    years <- data$first_yr:data$last_yr
    pyears <- data$first_yr:data$last_proj_yr
    sex <- c("Male","Immature female","Mature female")
    seasons <- c("AW","SS")
    regions <- 1:data$n_area
    YR <- "YR" # label for the season before the season change year

    if (length(mcmc) > 0) {
        n_iter <- nrow(mcmc[[1]])
        
        biomass_total_ytrs2 <- mcmc$biomass_total_ytrs
        dimnames(biomass_total_ytrs2) <- list("Iteration" = 1:n_iter, "Year" = pyears, "Season" = seasons, "Region" = regions, "Sex" = c(sex,"Total"))
        biomass_total_ytrs2 <- reshape2::melt(biomass_total_ytrs2) %>%
            dplyr::filter(value > 0)
        
        biomass_total_yts2 <- biomass_total_ytrs2 %>%
            dplyr::group_by(Iteration, Year) %>%
            dplyr::filter(Sex == "Total") %>%
            dplyr::summarise(Biomass = sum(value)) %>%
            dplyr::filter(Year <= max(years))

	    # Catch
	    dsl <- data$data_catch_commercial_ryt + data$data_catch_recreational_ryt
	    dimnames(dsl) <- list("Region" = regions, "Year" = years, "Season" = seasons)
	    dsl <- reshape2::melt(dsl, value.name = "Catch") %>%
	        dplyr::mutate(Iteration = NA, Type = "SL", Data = "Observed")
	    
	    dnsl <- data$data_catch_nsl_ryt
	    dimnames(dnsl) <- list("Region" = regions, "Year" = years, "Season" = seasons)
	    dnsl <- reshape2::melt(dnsl, value.name = "Catch") %>%
	        dplyr::mutate(Iteration = NA, Type = "NSL", Data = "Observed")

	    # Observed catch
	    dcatch <- rbind(dsl, dnsl) %>%
	        dplyr::filter(!(Year < data$season_change_yr & Season == "SS")) %>%
	        dplyr::mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR))

	    dcatch_sum <- dplyr::group_by(dcatch, Iteration, Year) %>%
	        dplyr::summarise(Catch = sum(Catch))
    }

    df <- cbind.data.frame(biomass_total_yts2, "Catch"=dcatch_sum$Catch)
    if(nrow(df)!=length(unique(df$Year))) print("Error in surplus production calculation: more than one catch or biomass observations per year.")
    sp <- sapply(1:nrow(df), function(x){
    	if(x==1) return(0)
    	if(x!=1){
    		spx <- df[x,"Biomass"] - df[(x-1),"Biomass"] + df[x,"Catch"]
    		return(spx)
    	}
    })
	df$SP <- sp

	# surplus production
    p <- ggplot(data = df, aes(x = Year, y = SP)) +
    	geom_hline(aes(yintercept = 0), linetype = "dashed") + 
        stat_summary(fun.ymin = function(x) stats::quantile(x, 0.05), fun.ymax = function(x) stats::quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
        stat_summary(fun.ymin = function(x) stats::quantile(x, 0.25), fun.ymax = function(x) stats::quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
        stat_summary(fun.y = function(x) stats::quantile(x, 0.5), geom = "line", lwd = 1) +
        expand_limits(y = 0) +
        xlab("Fishing year") + ylab("Surplus production (tonnes)") +
        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
        theme_lsd()
    if (data$n_area > 1) {
        p <- p + facet_wrap(Region ~ .)
    }
    ggsave(paste0(figure_dir, "surplus_production.png"), p, width = 12)

}