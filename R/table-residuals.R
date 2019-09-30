#' Create residuals table
#' 
#' @param object and LSD object
#' @param figure_dir the directory to save to
#' @importFrom reshape2 melt
#' @export
#'
table_residuals <- function(object, 
							figure_dir = "figure/")
{
	data <- object@data
	map <- object@map
	mcmc <- object@mcmc
	
	##########
	### tags
	##########
    g <- length(data$cov_grow_morph_g)
    n_iter <- nrow(mcmc[[1]])
    bins <- data$size_midpoint_l
    n_morph <- data$n_growth_subset
    pyears <- data$first_yr:data$last_proj_yr
    sex <- c("Male", "Female")

    #w <- data$which_growth_rsy
    #dimnames(w) <- list("Region" = object@regions, "Sex" = sex, "Year" = pyears)
    #w <- reshape2::melt(w, value.name = "Morph") %>%
    #    dplyr::distinct(Region, Sex, Morph, .keep_all = TRUE)

    morph <- data.frame("Morph" = data$cov_grow_morph_g,
                        "Size" = data$data_grow_size_capture_g,
                        "Area" = data$cov_grow_release_area_g,
                        "Sex" = sex[data$cov_grow_sex_g],
                        "Release" = data$cov_grow_release_no_g,
                        "Year" = data$cov_grow_release_yr_g) %>%
        dplyr::mutate(I = 1:g)
    
    resid <- mcmc$resid_grow_g
    dimnames(resid) <- list("Iteration" = 1:n_iter, "I" = 1:g)
    resid <- reshape2::melt(resid) %>%
        dplyr::inner_join(morph, by = "I")
        #dplyr::inner_join(w, by = "I") %>%
        #dplyr::distinct(Iteration, I, Region, .keep_all = TRUE)


    df_tag <- data.frame("data"="tags", "sumresid"=sum(abs(resid$value)), "avgresid"=sum(abs(resid$value))/nrow(resid))

    #########
    ### LFs
    #########
    n_iter <- nrow(mcmc[[1]])
    years <- data$first_yr:data$last_yr
    sex <- c("Male","Immature female","Mature female")
    seasons <- c("AW","SS")
    bins <- data$size_midpoint_l
    regions <- 1:data$n_area
    sources <- c("LB","CS")

    w <- data.frame(LF = 1:data$n_lf, Year = data$data_lf_year_i, Season = data$data_lf_season_i,
                    Source = data$data_lf_source_i, Region = data$data_lf_area_i,
                    Weight = data$data_lf_weight_i[,1], N = data$data_lf_N_is)

    resid <- mcmc$resid_lf_isl
    dimnames(resid) <- list("Iteration" = 1:n_iter, "LF" = 1:data$n_lf, "Sex"= sex, "Size" = bins)
    resid <- reshape2::melt(resid) %>%
        dplyr::left_join(w, by = "LF") %>%
        dplyr::mutate(Source = sources[Source], Season = seasons[Season])
    head(resid)

    df_lf <- data.frame("data"="LF", "sumresid"=sum(abs(resid$value)), "avgresid"=sum(abs(resid$value))/nrow(resid))

    #########
    ## CPUE
    #########
    n_iter <- nrow(mcmc[[1]])
    seasons <- c("AW", "SS")
    n_q <- data$n_q
    years <- data$first_yr:data$last_yr
    pyears <- data$first_yr:data$last_proj_yr
    poffset <- data$data_offset_cpue_ry

    pq <- mcmc$par_q_cpue_qy
    dimnames(pq) <- list("Iteration" = 1:n_iter, "qtype" = 1:n_q, "Year" = pyears)
    pq <- reshape2::melt(pq, value.name = "q") %>%
      dplyr::filter(Year <= max(years))

    rcpue <- mcmc$resid_cpue_i
    dimnames(rcpue) <- list("Iteration" = 1:n_iter, "I" = 1:data$n_cpue)
    rcpue <- reshape2::melt(rcpue, value.name = "CPUE") %>%
        dplyr::select(Iteration, CPUE) %>%
        dplyr::mutate(Data = "Residual", Type = "CPUE", Region = rep(data$data_cpue_area_i, each = n_iter), Year = rep(data$data_cpue_year_i, each = n_iter), Season = seasons[rep(data$data_cpue_season_i, each = n_iter)]) %>%
        dplyr::full_join(pq, by=c("Iteration", "Year")) %>%
        dplyr::select(-q) %>%
        dplyr::filter(Data == "Residual")

    df_cpue <- data.frame("data"="cpue", "sumresid"=sum(abs(rcpue$CPUE)), "avgresid"=sum(abs(rcpue$CPUE))/nrow(rcpue))

    df <- rbind.data.frame(df_tag, df_lf, df_cpue)

    write.csv(df, file.path(figure_dir, "Residual_summaries.csv"))

    return(df)

}