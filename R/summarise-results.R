

# object <- readRDS('lsd.rds')
# mcmc <- object@mcmc


# table_outputs <- c("sdnr_lfs", 
# 				"MAR_lfs", 
# 				"lp_lf", 
# 				"sdnr_tag", 
# 				"MAR_tag", 
# 				"lp_tag", 
# 				"sdnr_cpue", 
# 				"MAR_cpue", 
# 				"lp_cpue", 
# 				"sdnr_sexr",
# 				"MAR_sexr",
# 				"lp_sexr",
# 				"lp_prior",
# 				"lp__",
# 				"par_R0_r",
# 				"par_M_i",
# 				"par_mat_50_i",
# 				"par_mat_95_i",
# 				"par_grow_alpha_i",
# 				"par_grow_diff_i",
# 				"par_grow_shape_i",
# 				"par_grow_cv_i",
# 				"par_grow_sd_i",
# 				"par_vuln_i", 
# 				"par_sel_1_i",
# 				"par_sel_3_i",
# 				"par_init_erate_i",
# 				"SSB0_r",
# 				"Bref_r",
# 				"Bmsy_r",
# 				"Bcurr_r",
# 				"SSBcurr_r",
# 				"MSY_r", 
# 				"Fmult_r") ## change to Fmsy


# out <- lapply(1:length(table_outputs), function(x){
# 	if(table_outputs[x] %in% names(mcmc)==FALSE) return(rep(NA, 3))
# 	sub <- mcmc[[table_outputs[x]]]
# 	if(length(dim(sub))==1){
# 		quants <- t(as.matrix(quantile(sub, c(0.05, 0.5, 0.95))))
# 	}
# 	if(length(dim(sub))==2){
# 		if(grepl("Fmult", table_outputs[x])){
# 			quants <- t(sapply(1:ncol(sub), function(x) quantile(sub[,x]^(-1), c(0.05, 0.5, 0.95))))
# 		} else {
# 			quants <- t(sapply(1:ncol(sub), function(x) quantile(sub[,x], c(0.05, 0.5, 0.95))))		
# 		}
# 	}
# 	if(length(dim(sub))==3){
# 		quants <- t(as.matrix(quantile(sub[,1,], c(0.05, 0.5, 0.95))))
# 	}
# 	if(grepl("Fmult", table_outputs[x])){
# 		rownames(quants) <- rep("Fmsy", nrow(quants))
# 	} else {
# 		rownames(quants) <- rep(table_outputs[x], nrow(quants))
# 	}
# 	return(quants)
# })
# out <- do.call(rbind, out)




# ### non-automated outputs
# # sub <- mcmc[["SSBcurr_r"]] / mcmc[["SSB0_r"]]
# # quants <- t(as.matrix(quantile(sub, c(0.05, 0.5, 0.95))))
# # rownames(quants) <- "SSB2017/SSB0"
# # out <- rbind(out, quants)

# sub <- mcmc[["Bcurr_r"]] / mcmc[["Bref_r"]]
# quants <- t(as.matrix(quantile(sub, c(0.05, 0.5, 0.95))))
# rownames(quants) <- "B2017/Bref"
# out <- rbind(out, quants)

# sub <- mcmc[["Bcurr_r"]] / mcmc[["Bmsy_r"]]
# quants <- t(as.matrix(quantile(sub, c(0.05, 0.5, 0.95))))
# rownames(quants) <- "B2017/Bmsy"
# out <- rbind(out, quants)


# write.csv(out, "output_table.csv")


