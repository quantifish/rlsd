# This file is part of RStan
# Copyright (C) 2012, 2013, 2014, 2015 Jiqiang Guo and Benjamin Goodrich
#
# RStan is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.
#
# RStan is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

#' Read CSV files created by CmdStan or RStan
#' 
#' @param csvfiles A character vector of paths to CSV files of MCMC output
#' @return An object of \code{\link{StanFitMCMC-class}}
#' @seealso \code{\link{StanFitMCMC-class}}
#' @examples
#' csvfiles <- dir(system.file('misc', package = 'rstan'),
#'                 pattern = 'rstan_doc_ex_[0-9].csv', full.names = TRUE)
#' fit <- read_stan_csv(csvfiles)
#' fit
#'
#' @export
#'
read_stan_map <- function(csvfiles) {

    col_major <- TRUE

    dotfnames_to_sqrfnames <- function(fnames) {
  fnames <- sapply(fnames, 
                   function(i) { 
                     if (!grepl("\\.", i)) return(i)
                     i <- sub("\\.", "[", i)
                     i <- sub("\\s*$", "]", i)
                     i }, USE.NAMES = FALSE)
  gsub("\\.\\s*", ",", fnames)
    }
    
  paridx_fun <- function(names) {
    # Args:
    #   names: names (character vector) such as lp__, treedepth__, stepsize__,
    #          alpha, beta.1, 
    # Returns: 
    #   The indexes in the names that are parameters other than lp__,
    #   treedepth__, or stepsize__. The vector has attribute meta
    #   with the indexes of 'treedepth__', 'lp__', and 'stepsize__'
    #   if available. 
    
    sampler_param_names <- c('lp__', 'accept_stat__', 'treedepth__', 'stepsize__', 'divergent__', 'n_leapfrog__')
    metaidx <- match(sampler_param_names, names)
    names(metaidx) <- sampler_param_names
    paridx <- setdiff(seq_along(names), metaidx)
    attr(paridx, "meta") <- metaidx[!sapply(metaidx, is.na)]
    paridx
  }

  read_comments <- function(f, n = -1) {
    # Read comments beginning with `#`
    # Args:
    #   f: the filename
    #   n: max number of line; -1 means all
    # Returns:
    #   a vector of strings
    con <- file(f, 'r')
    comments <- list()
    iter <- 0
    while (length(input <- readLines(con, n = 1)) > 0) {
      if (n > 0 && n <= iter) break;
      if (grepl("#", input)) {
        comments <- c(comments, gsub("^.*#", "#", input))
        iter <- iter + 1
      }
    }
    close(con)
    do.call(c, comments)
  }

  read_csv_header <- function(f, comment.char = '#') {
    # Read the header of a csv file (the first line not beginning with
    # comment.char). And the line number is return as attribute of name 'lineno'.
    con <- file(f, 'r')
    niter <- 0
    while (length(input <- readLines(con, n = 1)) > 0) {
      niter <- niter + 1
      if (!grepl(comment.char, input)) break;
    }
    header <- input
    attr(header, "lineno") <- niter
    close(con)
    header
  }
  
  parse_stancsv_comments <- function(comments) {
    # Parse the comments in Stan CSV files to get information such as
    # iter, thin, seed, etc. This is specific to the CSV files
    # generated from Stan
    
    adapt_term_lineno <- which(grepl("Adaptation terminated", comments))
    time_lineno <- which(grepl("Elapsed Time", comments))
    has_time <- length(time_lineno) > 0
    len <- length(comments)
    if (length(adapt_term_lineno) < 1) 
      adapt_term_lineno <- len
    
    if (adapt_term_lineno == len)
      adaptation_info <- ''
    else {
      if (has_time)
        adaptation_info <- paste(comments[(adapt_term_lineno+1):(time_lineno-1)], collapse = '\n')
      else
        adaptation_info <- paste(comments[(adapt_term_lineno+1):len], collapse = '\n')
    }
    if (has_time)
      time_info <- comments[time_lineno:len]
    else
      time_info <- ''
    comments <- comments[1:(adapt_term_lineno - 1)]
    
    has_eq <- sapply(comments, function(i) grepl('=', i))
    comments <- comments[has_eq] 
    comments <- gsub('^#+\\s*|\\s*|\\(Default\\)', '', comments)
    eq_pos <- regexpr("=", comments, fixed = TRUE)
    names0 <- substr(comments, 0, eq_pos - 1)
    values <- as.list(substring(comments, eq_pos + 1))
    
    id_idx <- which("id" == names0)
    if (length(id_idx) > 0) 
      names0[id_idx] <- "chain_id"
    
    compute_iter <- FALSE
    id_warmup <- which("num_warmup" == names0)
    if (length(id_warmup) > 0) {
      names0[id_warmup] <- "warmup"
      compute_iter <- TRUE
    }   
    
    id_numsamples <- which("num_samples" == names0)
    if (length(id_numsamples) > 0) {
      names0[id_numsamples] <- "iter"
    }
    names(values) <- names0;
    
    add_lst <- list(adaptation_info = adaptation_info,
                    has_time = has_time,
                    time_info = time_info)
    
    sampler_t <- NULL
    if (!is.null(values$algorithm) && is.null(values$sampler_t)) {
      if (values$algorithm == 'rwm' || values$algorithm == 'Metropolis')  
        sampler_t <- "Metropolis"
      else if (values$algorithm == 'hmc') {
        if (values$engine == 'static')  sampler_t <- "HMC"
        else {
          if (values$metric == 'unit_e') sampler_t <- "NUTS(unit_e)"
          else if (values$metric == 'diag_e') sampler_t <- "NUTS(diag_e)"
          else if (values$metric == 'dense_e') sampler_t <- "NUTS(dense_e)"
        } 
      } 
      add_lst <- c(add_lst, sampler_t = sampler_t)
    } 
    names1 <- intersect(c("thin", "iter", "warmup", "chain_id", "max_depth", 
                          "num_samples", "num_warmup", "id",
                          "max_treedepth", "save_warmup"), names0)
    names2 <- intersect(c("stepsize", "stepsize_jitter", "adapt_gamma", "adapt_kappa", 
                          "adapt_delta", "gamma", "kappa", "delta", "t0",
                          "adapt_t0"), names0) 
    for (z in names1) values[[z]] <- as.integer(values[[z]])
    for (z in names2) values[[z]] <- as.numeric(values[[z]])
    if (compute_iter) values[["iter"]] <- values[["iter"]] + values[["warmup"]]
    c(values, add_lst)  
  }
  
  unique_par <- function(fnames) {
    # obtain parameters from flat names in format of say alpha.1,
    # alpha.2, beta.1.1, ..., beta.3.4, --- in this case, return
    # c('alpha', 'beta')
    unique(gsub('\\..*', '', fnames))
  }
  
  get_dims_from_fnames <- function(fnames, pname) {
    # Get the dimension for a parameter from
    # the flatnames such as "alpha.1.1", ..., "alpha.3.4", the
    # format of names in the CSV files generated by Stan.
    # Currently, this function assume fnames are correctly given.
    # Args:
    #   fnames: a character of names for one (vector/array) parameter
    #   pname: the name for this vector/array parameter such as "alpha"
    #     for the above example
    
    if (missing(pname)) pname <- gsub('\\..*', '', fnames[1])
    
    if (length(fnames) == 1 && fnames == pname)
      return(integer(0)) # a scalar
    
    idxs <- sub(pname, '', fnames, fixed = TRUE)
    lp <- gregexpr('\\d+', idxs)
    
    tfun <- function(name, start, i) {
      last <- attr(start, 'match.length')[i] + start[i]
      as.integer(substr(name, start[i], last))
    }
    
    dim_len <- length(lp[[1]])
    dims <- integer(dim_len)
    for (i in 1:dim_len) {
      dimi <- mapply(tfun, idxs, lp, MoreArgs = list(i = i), USE.NAMES = FALSE)
      dims[i] <- max(dimi)
    }
    dims
  }

  get_time_from_csv <- function(tlines) {
    # get the warmup time and sample time from the commented lines
    # about time in the CSV files
    # Args:
    #  tlines: character vector of length 3 (or 2 since the last one is not used)
    #          from the CSV File. For example, it could be
    #          # Elapsed Time: 0.005308 seconds (Warm-up)
    #                          0.003964 seconds (Sampling)
    #                          0.009272 seconds (Total)
    t <- rep(NA, 2)
    names(t) <- c("warmup", "sample")
    if (length(tlines) < 2) return(t)
    warmupt <- gsub(".*#\\s*Elapsed.*:\\s*", "", tlines[1])
    warmupt <- gsub("\\s*seconds.*$", "", warmupt)
    samplet <- gsub(".*#\\s*", "", tlines[2])
    samplet <- gsub("\\s*seconds.*$", "", samplet)
    t[1] <- as.double(warmupt)
    t[2] <- as.double(samplet)
    t
  }

  all_int_eq <- function(is) {
    # tell if all integers in 'is' are the same
    if (!all(is.integer(is)))
      stop("not all are integers")
    min(is) == max(is)
  }
  
  if (length(csvfiles) < 1) 
    stop("csvfiles does not contain any CSV file name")

  g_skip <- 10
  g_max_comm <- -1 # to read all 

  cs_lst <- lapply(csvfiles, function(csv) read_comments(csv, n = g_max_comm))
  cs_lst2 <- lapply(cs_lst, parse_stancsv_comments)
  
  ss_lst <- vector("list", length(csvfiles))
  for (i in seq_along(csvfiles)) {
    header <- read_csv_header(csvfiles[i])
    lineno <- attr(header, 'lineno')
    vnames <- strsplit(header, ",")[[1]]
    m <- matrix(scan(csvfiles[i], skip = lineno, comment.char = '#', sep = ',', quiet = TRUE),
                ncol = length(vnames), byrow = TRUE)
    ss_lst[[i]] <- as.data.frame(m)
    colnames(ss_lst[[i]]) <- vnames 
  } 

  ## read.csv is slow for large files 
  ##ss_lst <- lapply(csvfiles, function(csv) read.csv(csv, header = TRUE, skip = 10, comment.char = '#'))
  # use the first CSV file name as model name
  m_name <- sub("(_\\d+)*$", '', sub("\\.[^.]*$", "", basename(csvfiles[1])))

  sdate <- do.call(max, lapply(csvfiles, function(csv) file.info(csv)$mtime))
  sdate <- format(sdate, "%a %b %d %X %Y") # same format as date() 

  chains <- length(ss_lst)
  fnames <- names(ss_lst[[1]])
  n_save <- nrow(ss_lst[[1]])
  paridx <- paridx_fun(fnames)
  lp__idx <- attr(paridx, 'meta')["lp__"]
  par_fnames <- c(fnames[paridx], "lp__")
  pars_oi <- unique_par(par_fnames)
  dims_oi <- lapply(pars_oi, 
                    function(i) {
                      pat <- paste('^', i, '(\\.\\d+)*$', sep = '')
                      i_fnames <- par_fnames[grepl(pat, par_fnames)]
                      get_dims_from_fnames(i_fnames, i) 
                    })
  names(dims_oi) <- pars_oi
  midx <- if (!col_major) multi_idx_row2colm(dims_oi) else 1:length(par_fnames)
  if (chains > 1) {
    if (!all(sapply(ss_lst[-1], function(i) identical(names(i), fnames))))
      stop('the CSV files do not have same parameters')
    if (!all(sapply(ss_lst[-1], function(i) identical(length(i[[1]]), n_save)))) 
      stop('the number of iterations are not the same in all CSV files')
  } 
  mode <- 0L
  
  samples <- lapply(ss_lst, 
                    function(df) {
                      ss <- df[c(paridx, lp__idx)[midx]]
                      attr(ss, "sampler_params") <- df[setdiff(attr(paridx, 'meta'), lp__idx)] 
                      ss
                    })
  par_fnames <- par_fnames[midx]
  for (i in seq_along(samples)) {
    attr(samples[[i]], "adaptation_info") <- cs_lst2[[i]]$adaptation_info 
    attr(samples[[i]], "args") <- 
      list(sampler_t = cs_lst2[[i]]$sampler_t,
           chain_id = cs_lst2[[i]]$chain_id)
    if (cs_lst2[[i]]$has_time)
      attr(samples[[i]], "elapsed_time") <- get_time_from_csv(cs_lst2[[i]]$time_info)
  } 

  save_warmup <- 0
  warmup <- 0
  thin <- 1
  #iter <- sapply(cs_lst2, function(i) i$iter)
  iter <- 1
  #if (!all_int_eq(warmup) || !all_int_eq(thin) || !all_int_eq(iter)) 
  #  stop("not all iter/warmups/thin are the same in all CSV files")
  n_kept0 <- 1 + (iter - warmup - 1) %/% thin
  warmup2 <- 0
  if (max(save_warmup) == 0L) { # all equal to 0L
    n_kept <- n_save
  } else if (min(save_warmup) == 1L) { # all equals to 1L 
    warmup2 <- 1 + (warmup[1] - 1) %/% thin[1]
    n_kept <- n_save - warmup2 
  } 
  
  if (n_kept0[1] != n_kept) {
    warning("the number of iterations after warmup found (", n_kept, 
            ") does not match iter/warmup/thin from CSV comments (",
            paste(n_kept0, collapse = ','), ")")

    if (n_kept < 0) {
      warmup <- warmup + n_kept
      n_kept <- 0
      mode <- 2L
    }
    n_kept0 <- n_save
    iter <- n_save

    for (i in 1:length(cs_lst2)) {
      cs_lst2[[i]]$warmup <- warmup
      cs_lst2[[i]]$iter <- iter
    }
  }

  idx_kept <- if (warmup2 == 0) 1:n_kept else -(1:warmup2)
  for (i in seq_along(samples)) {
    m <- apply(samples[[i]][idx_kept,], 2, mean)
    attr(samples[[i]], "mean_pars") <- m[-length(m)]
    attr(samples[[i]], "mean_lp__") <- m["lp__"]
  }

  perm_lst <- lapply(1:chains, function(id) sample.int(n_kept))

  # old output
  sim = list(samples = samples, 
             iter = iter[1], 
             thin = thin[1], 
             warmup = warmup[1], 
             chains = chains, 
             n_save = rep(n_save, chains),
             warmup2 = rep(warmup2, chains),
             permutation = perm_lst,
             pars_oi = pars_oi, 
             dims_oi = dims_oi,
             fnames_oi = dotfnames_to_sqrfnames(par_fnames), 
             n_flatnames = length(par_fnames))
  null_dso <- new("cxxdso", sig = list(character(0)), dso_saved = FALSE, dso_filename = character(0), 
                  modulename = character(0), system = R.version$system, cxxflags = character(0), 
                 .CXXDSOMISC = new.env(parent = emptyenv()))
  null_sm <- new("stanmodel", model_name = m_name, model_code = character(0), 
                 model_cpp = list(), dso = null_dso)

  nfit <- new("stanfit", 
              model_name = m_name,
              model_pars = pars_oi,
              par_dims = dims_oi, 
              mode = mode,
              sim = sim,
              inits = list(), 
              stan_args = cs_lst2,
              stanmodel = null_sm,
              date = sdate, # not the time of sampling
              .MISC = new.env(parent = emptyenv()))
  return(nfit)
}
