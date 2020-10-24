.make_plot_data <- function(object, pars, include = TRUE, inc_warmup = FALSE, unconstrain = FALSE) {
  
  window <- NULL
  if (is.stanreg(object)) {
    sim <- object$stanfit@sim
  }
  else sim <- object@sim
  
  nopars <- missing(pars)
  if (is.stanfit(object) && !nopars) {
    if ("log-posterior" %in% pars)
      pars[which(pars == "log-posterior")] <- "lp__"
  }
  if (!include) {
    if (nopars) stop("pars must be specified if include=FALSE.", call. = FALSE)
    else {
      if (is.stanreg(object)) 
        pars <- setdiff(sim$fnames_oi, pars)
      else 
        pars <- setdiff(sim$pars_oi, pars)
    }
  }
  if (nopars) {
    if (is.stanreg(object)) 
      pars <- names(object$coefficients)
    else 
      pars <- setdiff(sim$pars_oi, c("lp__", "log-posterior"))
  }
  else {
    if (!is.stanreg(object)) 
      pars <- check_pars_second(sim, pars)
  }
  
  pars <- remove_empty_pars(pars, sim$dims_oi)
  if (unconstrain && "lp__" %in% pars) {
    if (length(pars) == 1L) stop("Can't unconstrain lp__", call. = FALSE)
    pars <- pars[-which(pars == "lp__")]
  }
  tidx <- pars_total_indexes(sim$pars_oi,
                             sim$dims_oi,
                             sim$fnames_oi,
                             pars)
  tidx <- lapply(tidx, function(x) attr(x, "row_major_idx"))
  tidx <- unlist(tidx, use.names = FALSE)
  num_plots <- length(tidx)
  
  if (nopars && num_plots > 10) {
    # if pars is not specified then default to showing 10 of the parameters
    tidx <- tidx[1:10]
    message("'pars' not specified. Showing first 10 parameters by default.")
  }
  
  if (!is.null(window)) {
    window <- sort(window)
    if (window[1] < 1) window[1] <- 1
    if (window[1] > sim$iter[1])
      stop("wrong specification of argument window", call. = FALSE)
    if (is.na(window[2]) || window[2] > sim$iter[1])
      window[2] <- sim$iter[1]
  } else {
    window <- c(1, sim$iter[1])
  }
  if ((sim$warmup2 == 0 || !inc_warmup) && window[1] <= sim$warmup[1]) {
    window[1] <- sim$warmup[1] + 1
  }
  if (window[1] > window[2]) {
    stop("the given window does not include sample")
  }
  if (window[1] > sim$warmup[1]) inc_warmup <- FALSE
  
  thin <- sim$thin
  warmup2 <- sim$warmup2[1]
  warmup <- sim$warmup
  start_i <- window[1]
  window_size <- (window[2] - start_i) %/% thin
  id <- seq.int(start_i, by = thin, length.out = window_size)
  start_idx <- (if (warmup2 == 0) (start_i - warmup) else start_i) %/% thin
  if (start_idx < 1)  start_idx <- 1
  idx <- seq.int(start_idx, by = 1, length.out = window_size)
  if (unconstrain) sim$samples <- .upars(object)
  samp_use <- lapply(sim$samples, function(chain) {
    out <- lapply(chain[tidx], function(x) x[idx])
    names(out) <- sim$fnames_oi[tidx]
    out
  })
  nchains <- length(samp_use)
  
  if (unconstrain) {
    if (is.stanreg(object)) 
      object <- object$stanfit
    pblock <- .get_stan_params(object)
    pars2 <- unlist(lapply(strsplit(pars, "\\["), "[[", 1))
    not_pblock <- length(setdiff(unique(pars2), pblock))
    if (not_pblock)
      stop("If 'unconstrain' is TRUE only variables declared in the ",
           "'parameters' block can be included in 'pars'.", 
           call. = FALSE)
  }
  
  dat <- .reshape_sample(samp_use)
  dat$iteration <- idx
  dat$chain <- factor(dat$chain)
  fnames <- sim$fnames_oi[tidx]
  lp <- which(dat$parameter == "lp__")
  if (!identical(lp, integer(0))) {
    dat$parameter[lp] <- "log-posterior"
    fnames[which(fnames == "lp__")] <- "log-posterior"
  }
  dat$parameter <- factor(dat$parameter, levels = fnames)
  list(samp = dat,
       nchains = nchains,
       nparams = length(tidx),
       warmup = warmup)
}


check_pars <- function(allpars, pars) {
  pars_wo_ws <- gsub('\\s+', '', pars) 
  m <- which(match(pars_wo_ws, allpars, nomatch = 0) == 0)
  if (length(m) > 0) 
    stop("no parameter ", paste(pars[m], collapse = ', ')) 
  if (length(pars_wo_ws) == 0) 
    stop("no parameter specified (pars is empty)")
  unique(pars_wo_ws) 
} 


check_pars_first <- function(object, pars) {
  # Check if all parameters in pars are valid parameters of the model 
  # Args:
  #   object: a stanfit object 
  #   pars: a character vector of parameter names
  # Returns:
  #   pars without white spaces, if any, if all are valid
  #   otherwise stop reporting error
  allpars <- cbind(object@model_pars, flatnames(object@model_pars))
  check_pars(allpars, pars) 
} 


check_pars_second <- function(sim, pars) {
  #
  # Check if all parameters in pars are parameters for which we saved
  # their samples 
  # 
  # Args:
  #   sim: The sim slot of class stanfit 
  #   pars: a character vector of parameter names
  # 
  # Returns:
  #   pars without white spaces, if any, if all are valid
  #   otherwise stop reporting error
  if (missing(pars)) return(sim$pars_oi) 
  allpars <- c(sim$pars_oi, sim$fnames_oi) 
  check_pars(allpars, pars)
}


remove_empty_pars <- function(pars, model_dims) {
  # 
  # Remove parameters that are actually empty, which
  # could happen when for exmample a user specify the
  # following stan model code: 
  # 
  # transformed data { int n; n <- 0; }
  # parameters { real y[n]; } 
  # 
  # Args:
  #   pars: a character vector of parameters names
  #   model_dims: a named list of the parameter dimension
  # 
  # Returns:
  #   A character vector of parameter names with empty parameter 
  #   being removed. 
  # 
  ind <- rep(TRUE, length(pars))
  model_pars <- names(model_dims)
  if (is.null(model_pars)) stop("model_dims need be a named list")
  for (i in seq_along(pars)) {
    p <- pars[i]
    m <- match(p, model_pars)
    if (!is.na(m) && prod(model_dims[[p]]) == 0)  ind[i] <- FALSE
  } 
  pars[ind]
}


pars_total_indexes <- function(names, dims, fnames, pars) {
  # Obtain the total indexes for parameters (pars) in the 
  # whole sequences of names that is order by 'column major.' 
  # Args: 
  #   names: all the parameters names specifying the sequence of parameters
  #   dims:  the dimensions for all parameters, the order for all parameters 
  #          should be the same with that in 'names'
  #   fnames: all the parameter names specified by names and dims 
  #   pars:  the parameters of interest. This function assumes that
  #     pars are in names.   
  # Note: inside each parameter (vector or array), the sequence is in terms of
  #   col-major. That means if we have parameter alpha and beta, the dims
  #   of which are [2,2] and [2,3] respectively.  The whole parameter sequence
  #   are alpha[1,1], alpha[2,1], alpha[1,2], alpha[2,2], beta[1,1], beta[2,1],
  #   beta[1,2], beta[2,2], beta[1,3], beta[2,3]. In addition, for the col-majored
  #   sequence, an attribute named 'row_major_idx' is attached, which could
  #   be used when row major index is favored.

  starts <- calc_starts(dims) 
  par_total_indexes <- function(par) {
    # for just one parameter
    #
    p <- match(par, fnames)
    # note that here when `par' is a scalar, it would
    # match one of `fnames'
    if (!is.na(p)) {
      names(p) <- par 
      attr(p, "row_major_idx") <- p 
      return(p) 
    } 
    p <- match(par, names)
    np <- num_pars(dims[[p]])
    if (np == 0) return(NULL)
    idx <- starts[p] + seq(0, by = 1, length.out = np) 
    names(idx) <- fnames[idx] 
    attr(idx, "row_major_idx") <- starts[p] + idx_col2rowm(dims[[p]]) - 1 
    idx
  } 
  idx <- lapply(pars, FUN = par_total_indexes)
  nulls <- sapply(idx, is.null)
  idx <- idx[!nulls]
  names(idx) <- pars[!nulls] 
  idx
}


calc_starts <- function(dims) {
  len <- length(dims) 
  s <- sapply(unname(dims), function(d)  num_pars(d), USE.NAMES = FALSE) 
  cumsum(c(1, s))[1:len] 
} 

num_pars <- function(d) prod(d) 


idx_col2rowm <- function(d) {
  # Suppose an iteration of samples for an array parameter is ordered by
  # col-major. This function generates the indexes that can be used to change
  # the sequences to row-major. 
  # Args:
  #   d: the dimension of the parameter 
  len <- length(d) 
  if (0 == len) return(1)  
  if (1 == len) return(1:d)  
  idx <- aperm(array(1:prod(d), dim = d)) 
  return(as.vector(idx)) 
}

.reshape_sample <- function(x) {
  res <- lapply(seq_along(x), function(i) {
    data.frame(value = unlist(x[[i]], use.names = FALSE),
               parameter = rep(names(x[[i]]), each = length(x[[i]][[1L]])),
               chain = i)
  })
  res <- do.call(rbind, res)
  res$parameter <- as.character(res$parameter)
  res
}

is.stanreg <- function(x) inherits(x, "stanreg")
is.stanfit <- function(x) inherits(x, "stanfit")


#' Plot dens
#' 
#' @param object and LSD object
#' @param xlab the x axis label
#' @param ylab the y axis label
#' @param figure_dir the directory to save to
#' @export
#' 
plot_dens <- function(object, 
                      xlab = "Size (mm)", 
                      ylab = "Maturation", 
                      figure_dir = "figure/")
{
    data <- object@data
    mcmc <- object@mcmc
    
    pars <- c("par_R0_r","par_M_i",
              "par_grow_alpha_si","par_grow_beta_si","par_grow_diff_si","par_grow_shape_si",
              "par_grow_cv_si","par_grow_sd",
              "par_mat_50_i","par_mat_95_i",
              "par_sel_1_i","par_sel_2_i","par_sel_3_i","par_vuln_i",
              "par_q_cpue_i","par_q_catch_rate_i","par_q_puerulus_i")
    i <- apply(as.matrix(pars), 1, FUN = function(x) {any(grepl(x, names(object@mcmc)))})
    pars <- pars[i]

    plot_data <- .make_plot_data(object@mcmc_raw, pars = pars, include = TRUE, inc_warmup = FALSE, unconstrain = FALSE)
    # This only seems to return 2 of the 3 samples

    bnd <- data[grepl("bnd_", names(data))]
    pri <- data[grepl("pri_", names(data))]

    #old_names <- gsub("bnd_", "", names(bnd))
    #k <- unique(as.character(plot_data$samp$parameter))
    #which(grepl(old_names[2], k))
    #vapply(as.matrix(old_names), 1, FUN = function(x) { which(grepl(x, k)) })
    
    p <- ggplot(data = plot_data$samp) +
        geom_density(aes(x = value)) +
        facet_wrap(~parameter, scales = "free_x") +
        theme_lsd()
    ggsave(paste0(figure_dir, "par_dens2.png"), p)
}
