#' Plot retrospective analysis
#' 
#' Plots retrospective analysis for vulnerable biomass and recruitment
#'
#' @param object an LSD object
#' @param scales the scales
#' @param show_map show the MAP or not
#' @param figure_dir the directory to save the figure to
#' @import dplyr
#' @import ggplot2
#' @importFrom reshape2 melt
#' @export
#' 
plot_retro <- function(object, 
	                   scales = "free_x", 
	                   show_map = TRUE, 
	                   figure_dir = "figure/")
{
	if(any(grepl("retro", list.files()))){

		files <- list.files()
		retro_names_raw <- files[grepl("retro", files)]
		retro_yrs_raw <- as.numeric(sapply(1:length(retro_names_raw), function(x) strsplit(retro_names_raw[x],"retro")[[1]][2]))
		retro_yrs <- retro_yrs_raw[order(retro_yrs_raw)]
		retro_names <- retro_names_raw[order(retro_yrs_raw)]

		### BASE MODEL 
		data <- object@data
		mcmc <- object@mcmc

		years <- data$first_yr:data$last_yr
		pyears <- data$first_yr:data$last_proj_yr
		sex <- c("Male","Immature female","Mature female")
		seasons <- c("AW","SS")
		regions <- 1:data$n_area
		YR <- "YR" # label for the season before the season change year	

		### Base, non-retrospective	
	     if (length(mcmc) > 0) {
	        n_iter <- nrow(mcmc[[1]])

	        #### vulnerable biomass
        	biomass_vuln_ytrs2 <- mcmc$biomass_vuln_ytrs
	        dimnames(biomass_vuln_ytrs2) <- list("Iteration" = 1:n_iter, "Year" = pyears, "Season" = seasons, "Region" = regions, Sex = sex)
	        vuln <- reshape2::melt(biomass_vuln_ytrs2) %>%
	            dplyr::filter(value > 0) %>%
	            dplyr::mutate(Season = as.character(Season), Season = ifelse(Year > 1978, Season, YR)) %>%
	            dplyr::filter(Season %in% c("YR","AW")) %>%
	            dplyr::group_by(Iteration, Year, Season, Region) %>%
	            dplyr::summarise(value = sum(value)) %>%
	            dplyr::mutate(LastFitYr_num = as.numeric(data$last_fit_yr))

	        ## catch
		    dsl <- data$data_catch_commercial_ryt + data$data_catch_recreational_ryt
		    dimnames(dsl) <- list("Region" = regions, "Year" = years, "Season" = seasons)
		    dsl <- reshape2::melt(dsl, value.name = "Catch") %>%
		        dplyr::mutate(Iteration = NA, Type = "SL", Data = "Observed")
		    
		    dnsl <- data$data_catch_nsl_ryt
		    dimnames(dnsl) <- list("Region" = regions, "Year" = years, "Season" = seasons)
		    dnsl <- reshape2::melt(dnsl, value.name = "Catch") %>%
		        dplyr::mutate(Iteration = NA, Type = "NSL", Data = "Observed")		

		    psl <- mcmc$pred_catch_sl_ryt
		    dimnames(psl) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = pyears, "Season" = seasons)
		    psl <- reshape2::melt(psl, value.name = "Catch") %>%
		        dplyr::mutate(Type = "SL", Data = "Expected")
		    
		    pnsl <- mcmc$pred_catch_nsl_ryt
		    dimnames(pnsl) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = pyears, "Season" = seasons)
		    pnsl <- reshape2::melt(pnsl, value.name = "Catch") %>%
		        dplyr::mutate(Type = "NSL", Data = "Expected")
		    
		    ph <- mcmc$pred_death_handling_ryt
		    dimnames(ph) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = pyears, "Season" = seasons)
		    ph <- reshape2::melt(ph, value.name = "Catch") %>%
		        dplyr::mutate(Type = "Handling mortality", Data = "Expected")
		    
		    # Observed catch
		    dcatch <- rbind(dsl, dnsl) %>%
		        dplyr::filter(!(Year < data$season_change_yr & Season == "SS")) %>%
		        dplyr::mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR)) %>%
		        dplyr::mutate(LastFitYr_num = as.numeric(data$last_fit_yr))

		    # Predicted catch
		    pcatch <- rbind(psl, pnsl, ph) %>%
		        dplyr::filter(!(Year < data$season_change_yr & Season == "SS")) %>%
		        dplyr::mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR)) %>%
		        dplyr::mutate(LastFitYr_num = as.numeric(data$last_fit_yr))

	        # Catch summed over seasons and types
		    pcatch <- dplyr::group_by(pcatch, Iteration, Region, Year, Data, LastFitYr_num)  %>%
		        dplyr::summarise(Catch = sum(Catch))
		    dcatch <- dplyr::group_by(dcatch, Iteration, Region, Year, Data, LastFitYr_num) %>%
		        dplyr::summarise(Catch = sum(Catch))

			## recruitment
		    ny <- dim(mcmc$recruits_ry)[3]
		    ryears <- data$first_yr:(data$first_yr + ny - 1)
		    regions <- 1:data$n_area

	        recruits <- mcmc$recruits_ry
	        dimnames(recruits) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = ryears)
	        recruits <- reshape2::melt(recruits) %>%
	        	dplyr::group_by(Iteration, Region, Year, value) %>%
	            dplyr::mutate(LastFitYr_num = as.numeric(data$last_fit_yr))	

	        R0 <- mcmc$par_R0_r
	        dimnames(R0) <- list("Iteration" = 1:n_iter, "Region" = regions)
	        R0 <- reshape2::melt(R0) %>%
	            dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = ryears), by = "Iteration") %>%
	            dplyr::group_by(Iteration, Region, value, Year) %>%
	            dplyr::mutate(LastFitYr_num = as.numeric(data$last_fit_yr))

	     }

	    ### FIND RETROSPECTIVES
		for(i in 1:length(retro_names)){

			object2 <- readRDS(paste0("./", retro_names[i], "/lsd.rds"))

			data <- object2@data
			mcmc <- object2@mcmc

			years <- data$first_yr:data$last_yr
			pyears <- data$first_yr:data$last_proj_yr
			sex <- c("Male","Immature female","Mature female")
			seasons <- c("AW","SS")
			regions <- 1:data$n_area
			YR <- "YR" # label for the season before the season change year			

		     if (length(mcmc) > 0) {
		       	    n_iter <- nrow(mcmc[[1]])

	            #### vulnerable biomass
             	bv_ytrs2 <- mcmc$biomass_vuln_ytrs
     	        dimnames(bv_ytrs2) <- list("Iteration" = 1:n_iter, "Year" = pyears, "Season" = seasons, "Region" = regions, Sex = sex)
     	        rvuln <- reshape2::melt(bv_ytrs2) %>%
     	            dplyr::filter(value > 0) %>%
     	            dplyr::mutate(Season = as.character(Season), Season = ifelse(Year > 1978, Season, YR)) %>%
     	            dplyr::filter(Season %in% c("YR","AW")) %>%
     	            dplyr::group_by(Iteration, Year, Season, Region) %>%
     	            dplyr::summarise(value = sum(value)) %>%
     	            dplyr::mutate(LastFitYr_num = as.numeric(data$last_fit_yr))     

     	        ## catch
     		    rdsl <- data$data_catch_commercial_ryt + data$data_catch_recreational_ryt
     		    dimnames(rdsl) <- list("Region" = regions, "Year" = years, "Season" = seasons)
     		    rdsl <- reshape2::melt(rdsl, value.name = "Catch") %>%
     		        dplyr::mutate(Iteration = NA, Type = "SL", Data = "Observed")
     		    
     		    rdnsl <- data$data_catch_nsl_ryt
     		    dimnames(rdnsl) <- list("Region" = regions, "Year" = years, "Season" = seasons)
     		    rdnsl <- reshape2::melt(rdnsl, value.name = "Catch") %>%
     		        dplyr::mutate(Iteration = NA, Type = "NSL", Data = "Observed")		     

     		    rpsl <- mcmc$pred_catch_sl_ryt
     		    dimnames(rpsl) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = pyears, "Season" = seasons)
     		    rpsl <- reshape2::melt(rpsl, value.name = "Catch") %>%
     		        dplyr::mutate(Type = "SL", Data = "Expected")
     		    
     		    rpnsl <- mcmc$pred_catch_nsl_ryt
     		    dimnames(rpnsl) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = pyears, "Season" = seasons)
     		    rpnsl <- reshape2::melt(rpnsl, value.name = "Catch") %>%
     		        dplyr::mutate(Type = "NSL", Data = "Expected")
     		    
     		    rph <- mcmc$pred_death_handling_ryt
     		    dimnames(rph) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = pyears, "Season" = seasons)
     		    rph <- reshape2::melt(rph, value.name = "Catch") %>%
     		        dplyr::mutate(Type = "Handling mortality", Data = "Expected")
     		    
     		    # Observed catch
     		    rdcatch <- rbind(rdsl, rdnsl) %>%
     		        dplyr::filter(!(Year < data$season_change_yr & Season == "SS")) %>%
     		        dplyr::mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR)) %>%
     		     	dplyr::mutate(LastFitYr_num = as.numeric(data$last_fit_yr))


     		    # Predicted catch
     		    rpcatch <- rbind(rpsl, rpnsl, rph) %>%
     		        dplyr::filter(!(Year < data$season_change_yr & Season == "SS")) %>%
     		        dplyr::mutate(Season = as.character(Season), Season = ifelse(Year >= data$season_change_yr, Season, YR)) %>%
     		        dplyr::mutate(LastFitYr_num = as.numeric(data$last_fit_yr))



		        # Catch summed over seasons and types
			    rpcatch_sum <- dplyr::group_by(rpcatch, Iteration, Region, Year, Data, LastFitYr_num)  %>%
			        dplyr::summarise(Catch = sum(Catch))
			    rdcatch_sum <- dplyr::group_by(rdcatch, Iteration, Region, Year, Data, LastFitYr_num) %>%
			    	dplyr::summarise(Catch = sum(Catch))

     					    
     			## recruitment
     		    ny <- dim(mcmc$recruits_ry)[3]
     		    ryears <- data$first_yr:(data$first_yr + ny - 1)
     		    regions <- 1:data$n_area     

     	        rrecruits <- mcmc$recruits_ry
     	        dimnames(rrecruits) <- list("Iteration" = 1:n_iter, "Region" = regions, "Year" = ryears)
     	        rrecruits <- reshape2::melt(rrecruits) %>%
     	        	dplyr::group_by(Iteration, Region, Year, value) %>%
     	            dplyr::mutate(LastFitYr_num = as.numeric(data$last_fit_yr))	     

     	        rR0 <- mcmc$par_R0_r
     	        dimnames(rR0) <- list("Iteration" = 1:n_iter, "Region" = regions)
     	        rR0 <- reshape2::melt(rR0) %>%
     	            dplyr::left_join(expand.grid(Iteration = 1:n_iter, Year = ryears), by = "Iteration") %>%
     	    	    dplyr::group_by(Iteration, Region, value, Year) %>%
     	            dplyr::mutate(LastFitYr_num = as.numeric(data$last_fit_yr))

		    }

		    vuln <- rbind(vuln, rvuln)
		    pcatch <- rbind(pcatch, rpcatch_sum)
		    dcatch <- rbind(dcatch, rdcatch_sum)
		    recruits <- rbind(recruits, rrecruits)
		    R0 <- rbind(R0, rR0)
		}

		    vuln <- dplyr::filter(vuln, Year <= (max(years)+1))

		    pcatch <- dplyr::filter(pcatch, Year <= (max(years)+1))

		    dcatch <- dplyr::filter(dcatch, Year <= (max(years)+1))

		    recruits <- dplyr::filter(recruits, Year <= (max(years)+1))

		    R0 <- dplyr::filter(R0, Year <= (max(years)+1))

		# ## order the years to peel off from base model
		vuln$LastFitYr <- factor(vuln$LastFitYr_num, levels = unique(vuln$LastFitYr_num[rev(order(as.numeric(vuln$LastFitYr_num)))]))
		pcatch$LastFitYr <- factor(pcatch$LastFitYr_num, levels = unique(pcatch$LastFitYr_num[rev(order(as.numeric(pcatch$LastFitYr_num)))]))
		dcatch$LastFitYr <- factor(dcatch$LastFitYr_num, levels = unique(dcatch$LastFitYr_num[rev(order(as.numeric(dcatch$LastFitYr_num)))]))
		recruits$LastFitYr <- factor(recruits$LastFitYr_num, levels = unique(recruits$LastFitYr_num[rev(order(as.numeric(recruits$LastFitYr_num)))]))
		R0$LastFitYr <- factor(R0$LastFitYr_num, levels = unique(R0$LastFitYr_num[rev(order(as.numeric(R0$LastFitYr_num)))]))


        colfun <- colorRampPalette(c("goldenrod","darkorange","darkred"))
        cols <- colfun(length(retro_names))

		##### plot vulnerable biomass
   		p <- ggplot(data = vuln, aes(x = Year, y = value, colour = LastFitYr)) +
   		 		scale_colour_manual(values=c(gray(0.3),cols), name="Model fit to data\nthrough year") +
		        #geom_vline(aes(xintercept = data$last_yr), linetype = "dashed") +
		        #stat_summary(fun.ymin = function(x) quantile(x, 0.05), fun.ymax = function(x) quantile(x, 0.95), geom = "ribbon", alpha = 0.25, colour = NA) +
		        #stat_summary(fun.ymin = function(x) quantile(x, 0.25), fun.ymax = function(x) quantile(x, 0.75), geom = "ribbon", alpha = 0.5, colour = NA) +
		        stat_summary(data=vuln %>% dplyr::filter(Year <= LastFitYr_num), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
		        stat_summary(data=vuln %>% dplyr::filter(Year==LastFitYr_num), fun.y=function(x) quantile(x, 0.5), geom="point", pch=19, cex=3) +
		        stat_summary(data=vuln %>% dplyr::filter(Year >= LastFitYr_num), fun.y= function(x) quantile(x, 0.5), geom="line", lwd=1, alpha=0.5) +
		        expand_limits(y = 0) +
		        xlab("Fishing year") + ylab("Vulnerable biomass (tonnes)") +
		        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
		        theme_lsd()


		    if (data$n_area > 1) {
		        p <- p + facet_wrap(~Region)
		    }
		    ggsave(paste0(figure_dir, "retro_biomass_vuln.png"), p, width = 10)

    	####plot catch

	    p <- ggplot(data = pcatch, aes(x = Year, y = Catch, colour = LastFitYr)) +
	    	scale_colour_manual(values=c(gray(0.3),cols), name="Model fit to data\nthrough year") + 
	        stat_summary(data=pcatch %>% dplyr::filter(Year <= LastFitYr_num), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
	        stat_summary(data=pcatch %>% dplyr::filter(Year == LastFitYr_num), fun.y=function(x) quantile(x, 0.5), geom="point", pch=19, cex=3) +
	        stat_summary(data=pcatch %>% dplyr::filter(Year >= LastFitYr_num), fun.y=function(x) quantile(x, 0.5), geom="line", lwd=1, alpha=0.5) +
	        stat_summary(data=dcatch %>% dplyr::filter(Year >= LastFitYr_num), aes(y=Catch), fun.y=function(x) quantile(x, 0.5), geom="line", linetype="dotted") +
	        expand_limits(y = 0) +
	        xlab("Fishing year") + ylab("Catch (tonnes)") +
	        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
	       	theme_lsd()

	    ggsave(paste0(figure_dir, "retro_catch.png"), p, width=10)


	    p2 <- ggplot(data = pcatch, aes(x = Year, y = Catch, colour = LastFitYr)) +
	    	scale_colour_manual(values=c(gray(0.3),cols), name="Model fit to data\nthrough year") + 
	        stat_summary(data=pcatch %>% dplyr::filter(Year <= LastFitYr_num), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
	        stat_summary(data=pcatch %>% dplyr::filter(Year == LastFitYr_num), fun.y=function(x) quantile(x, 0.5), geom="point", pch=19, cex=3) +
	        stat_summary(data=pcatch %>% dplyr::filter(Year >= LastFitYr_num), fun.y=function(x) quantile(x, 0.5), geom="line", lwd=1, alpha=0.5) +
	        stat_summary(data=dcatch %>% dplyr::filter(Year >= LastFitYr_num), aes(y=Catch), fun.y=function(x) quantile(x, 0.5), geom="line", linetype="dotted") +
	        expand_limits(y = 0) +
	        xlab("Fishing year") + ylab("Catch (tonnes)") +
	        scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +  
	        facet_grid(LastFitYr ~ ., scales="free") +
	       	theme_lsd()

	    ggsave(paste0(figure_dir, "retro_catch_year.png"), p2, width=10)
    

	    ### plot recruitment
	    xmin <- min(recruits$Year)
	    xmax <- max(recruits$Year)	
	    
	    p <- ggplot(data = recruits, aes(x = Year, y = value, colour = LastFitYr)) +
	    	    scale_colour_manual(values=c(gray(0.3),cols), name="Model fit to data\nthrough year") + 
	            stat_summary(data = R0, aes(x = Year, y = value/1e+6), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, linetype=2) +
	            stat_summary(data = recruits %>% dplyr::filter(Year <= LastFitYr_num), aes(x = Year, y = value/1e+6), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
	            stat_summary(data=recruits %>% dplyr::filter(Year == LastFitYr_num), fun.y=function(x) quantile(x, 0.5)/1e+6, geom="point", pch=19, cex=3) +
	            stat_summary(data=recruits %>% dplyr::filter(Year >= LastFitYr_num), fun.y= function(x) quantile(x, 0.5)/1e+6, geom="line", lwd=1, alpha=0.5) +
	    		expand_limits(y = 0) +
	        	xlab("Year") + ylab("Recruitment (millions of individuals)") +
	        	scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
	        	theme_lsd()

	    ggsave(paste0(figure_dir, "retro_recruitment.png"), p, width=10)

	    p2 <- ggplot(data = recruits, aes(x = Year, y = value, colour = LastFitYr)) +
	    	    scale_colour_manual(values=c(gray(0.3),cols), name="Model fit to data\nthrough year") + 
	            stat_summary(data = R0, aes(x = Year, y = value/1e+6), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1, linetype=2) +
	            stat_summary(data = recruits %>% dplyr::filter(Year <= LastFitYr_num), aes(x = Year, y = value/1e+6), fun.y = function(x) quantile(x, 0.5), geom = "line", lwd = 1) +
	            stat_summary(data=recruits %>% dplyr::filter(Year == LastFitYr_num), fun.y=function(x) quantile(x, 0.5)/1e+6, geom="point", pch=19, cex=3) +
	            stat_summary(data=recruits %>% dplyr::filter(Year >= LastFitYr_num), fun.y= function(x) quantile(x, 0.5)/1e+6, geom="line", lwd=1, alpha=0.5) +
	    		expand_limits(y = 0) +
	        	xlab("Year") + ylab("Recruitment (millions of individuals)") +
	        	scale_x_continuous(breaks = seq(0, 1e6, 10), minor_breaks = seq(0, 1e6, 1)) +
	        	facet_grid(LastFitYr ~ ., scales="free") +
	        	theme_lsd()	

	    ggsave(paste0(figure_dir, "retro_recruitment_year.png"), p2, width=10)    

	}

}
