#' Plot model fits
#'
#' @param object_list list of 'lsd.rds' files from multiple models
#' @param object_names vector of model names associated with each of the output files in object_list
#' @param single_stock TRUE for comparing models within a single stock assessment, FALSE for comparing multiple stocks and models (e.g. reference points study)
#' @param figure_dir the directory to save figures to
#' @export
#'
do_plot_compare <- function(object_list, object_names, single_stock, figure_dir = "compare_figure/")
{
	dir.create(figure_dir, showWarnings=FALSE)
	if(single_stock==TRUE){
	    plot_compare_vb(object_list = object_list, object_names = object_names, figure_dir = figure_dir)
	    plot_compare_recruitment(object_list = object_list, object_names = object_names, figure_dir = figure_dir)
	    plot_compare_q(object_list = object_list, object_names = object_names, figure_dir = figure_dir)
	    plot_compare_ssb(object_list = object_list, object_names = object_names, figure_dir = figure_dir)
		plot_compare_selectivity(object_list = object_list, object_names = object_names, figure_dir = figure_dir)
	}
	if(single_stock==FALSE){
		plot_rules(object_list = object_list, object_names = object_names, figure_dir = figure_dir)
		risk_summary <- summary_fun(object_list = object_list, object_names = object_names, figure_dir = figure_dir)
		msy <- find_msy(risk_summary = risk_summary, figure_dir = figure_dir)
		msy_new <- explore_rules(risk_summary=risk_summary, msy=msy)
		plot_curves(risk_summary=risk_summary, msy=msy_new, figure_dir = figure_dir)
		# plot_compare_catch_bio(object_list = object_list, object_names = object_names, figure_dir = figure_dir)
		# compare_multi_risk(object_list = object_list, object_names = object_names, figure_dir = figure_dir)
	 #    # plot_compare_multi_vb(object_list = object_list, object_names = object_names, figure_dir = figure_dir)
	 #    plot_compare_multi_recruitment(object_list = object_list, object_names = object_names, figure_dir = figure_dir)
	 #    # plot_compare_multi_q(object_list = object_list, object_names = object_names, figure_dir = figure_dir)
	 #    # plot_compare_multi_ssb(object_list = object_list, object_names = object_names, figure_dir = figure_dir)
	 #    plot_compare_multi_relssb(object_list = object_list, object_names = object_names, figure_dir = figure_dir)
	}
}
