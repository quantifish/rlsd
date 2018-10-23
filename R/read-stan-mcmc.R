#' Read MCMC comma delimited files produced by \code{cmdStan}
#'#' 
#' @param mcmcfiles character vector of the complete path to csv output files produced by \code{cmdStan}
#'
#' @export
read_stan_mcmc <- function(mcmcfiles) {
    # Read the csv files saved from Stan (or RStan) to a stanfit object
    # Args:
    #   mcmcfiles: csv files fitted for the same model; each file contains 
    #     the sample of one chain 
    #   col_major: the order for array parameters. 
    # 
    
    if (length(mcmcfiles) < 1) 
        stop("mcmcfiles does not contain any CSV file name")
    #
    #g_skip <- 10
    #g_max_comm <- -1 # to read all 
    #
    #cs_lst <- lapply(mcmcfiles, function(csv) read_comments(csv, n = g_max_comm))
    #cs_lst2 <- lapply(cs_lst, parse_stancsv_comments)
    
    #mcmcfiles <- c("modelB1.mcmc", "modelB2.mcmc")
    
    ss_lst <- ss_lst2 <- vector("list", length(mcmcfiles))
    
    for (i in seq_along(mcmcfiles)) {
        header <- .read_csv_header(mcmcfiles[i])
        lineno <- attr(header, 'lineno')
        vnames <- strsplit(header, ",")[[1]]
        m <- matrix(scan(mcmcfiles[i], skip = lineno, comment.char = '#', sep = ',', quiet = TRUE), ncol = length(vnames), byrow = TRUE)
        colnames(m) <- vnames
        ss_lst[[i]] <- as.data.frame(m)
    }
    
    n_samples <- unlist(lapply(ss_lst, nrow))
    #n_headers <- unlist(lapply(ss_lst, ncol))
    #
    #if (length(n_samples) > 1) {
    #    warning("mcmcfiles contain a different number of samples")
    #    #n_samples <- max(n_samples)
    #}
    #if (length(n_headers) > 1) {
    #    
    #    warning("mcmcfiles contain a different number of parameters")
        
    fnames <- lapply(ss_lst, names)
    fnames <- Reduce(intersect, fnames)
        
    ss_lst <- lapply(ss_lst , "[", fnames)
    
    #} else {
    #    fnames <- names(ss_lst[[1]])    
    #}
    
    #for (i in 1:length(ss_lst)) {
    #    
    #    ss_lst2[[i]] <- matrix(NA, ncol = n_headers[i], nrow = n_samples[i])
    #    
    #    ss_lst2[[i]][1:nrow(ss_lst[[i]]),] <- ss_lst[[i]]
    #    
    #    colnames(ss_lst2[[i]]) <- colnames(ss_lst[[i]])
    #    
    #} 
    
    
    
    ## read.csv is slow for large files 
    ##ss_lst <- lapply(mcmcfiles, function(csv) read.csv(csv, header = TRUE, skip = 10, comment.char = '#'))
    # use the first CSV file name as model name
    #m_name <- sub("(_\\d+)*$", '', filename_rm_ext(basename(mcmcfiles[1])))
    
    #sdate <- do.call(max, lapply(mcmcfiles, function(csv) file.info(csv)$mtime))
    #sdate <- format(sdate, "%a %b %d %X %Y") # same format as date() 
    
    # get number of chains
    chains <- length(ss_lst)

    # organise relevant information
    paridx  <- .paridx_fun(fnames)
    lp__idx <- attr(paridx, 'meta')["lp__"]
    par_fnames <- c(fnames[paridx], "lp__")
    pars_oi <- .unique_par(par_fnames)
    dims_oi <- lapply(pars_oi, 
                      function(i) {
                          pat <- paste('^', i, '(\\.\\d+)*$', sep = '')
                          i_fnames <- par_fnames[grepl(pat, par_fnames)]
                          .get_dims_from_fnames(i_fnames, i) 
                      })
    names(dims_oi) <- pars_oi
    #midx <- if (!col_major) multi_idx_row2colm(dims_oi) else 1:length(par_fnames)
    midx <- 1:length(par_fnames)
    #if (chains > 1) {
    #    if (!all(sapply(ss_lst[-1], function(i) identical(names(i), fnames))))
    #        stop('the CSV files do not have same parameters')
    #    if (!all(sapply(ss_lst[-1], function(i) identical(length(i[[1]]), n_save)))) 
    #        stop('the number of iterations are not the same in all CSV files')
    #} 
    
    samples <- lapply(ss_lst, 
                      function(df) {
                          ss <- df[c(paridx, lp__idx)[midx]]
                          attr(ss, "sampler_params") <- df[setdiff(attr(paridx, 'meta'), lp__idx)] 
                          ss
                      })
    par_fnames <- par_fnames[midx]
    #for (i in seq_along(samples)) {
    #    attr(samples[[i]], "adaptation_info") <- cs_lst2[[i]]$adaptation_info 
    #    attr(samples[[i]], "args") <- 
    #        list(sampler_t = cs_lst2[[i]]$sampler_t,
    #             chain_id = cs_lst2[[i]]$chain_id)
    #    if (cs_lst2[[i]]$has_time)
    #        attr(samples[[i]], "elapsed_time") <- get_time_from_csv(cs_lst2[[i]]$time_info)
    #} 
    
    #save_warmup <- sapply(cs_lst2, function(i) i$save_warmup)
    #warmup <- sapply(cs_lst2, function(i) i$warmup)
    #thin <- sapply(cs_lst2, function(i) i$thin)
    #iter <- sapply(cs_lst2, function(i) i$iter)
    #if (!all_int_eq(warmup) || !all_int_eq(thin) || !all_int_eq(iter)) 
    #    stop("not all iter/warmups/thin are the same in all CSV files")
    #n_kept0 <- 1 + (iter - warmup - 1) %/% thin
    #warmup2 <- 0
    #if (max(save_warmup) == 0L) { # all equal to 0L
    #    n_kept <- n_save
    #} else if (min(save_warmup) == 1L) { # all equals to 1L 
    #    warmup2 <- 1 + (warmup[1] - 1) %/% thin[1]
    #    n_kept <- n_save - warmup2 
    #} 
    
    for (i in seq_along(samples)) {
        m <- apply(samples[[i]][1:n_samples[i],], 2, mean)
        attr(samples[[i]], "mean_pars") <- m[-length(m)]
        attr(samples[[i]], "mean_lp__") <- m["lp__"]
    }
    
    perm_lst <- lapply(1:chains, function(id) sample.int(n_samples[id]))
    
    sim = list(samples = samples, 
               iter = n_samples, 
               chains = chains, 
               permutation = perm_lst,
               pars_oi = pars_oi, 
               dims_oi = dims_oi,
               fnames_oi = .dotfnames_to_sqrfnames(par_fnames), 
               file_names = .filename_rm_ext(basename(mcmcfiles)))
    
    
    #null_dso <- new("cxxdso", sig = list(character(0)), dso_saved = FALSE, dso_filename = character(0), 
    #                modulename = character(0), system = R.version$system, cxxflags = character(0), 
    #                .CXXDSOMISC = new.env(parent = emptyenv()))
    #null_sm <- new("stanmodel", model_name = mcmcfiles[1], model_code = character(0), 
    #               model_cpp = list(), dso = null_dso)
    #
    #nfit <- new("stanfit", 
    #            file_names = .filename_rm_ext(basename(mcmcfiles)),
    #            model_pars = pars_oi,
    #            par_dims = dims_oi, 
    #            mode = 0L,
    #            sim = sim,
    #            inits = list(), 
    #            stan_args = list(),#cs_lst2,
    #            stanmodel = null_sm,
    #            date = date(), # not the time of sampling
    #            .MISC = new.env(parent = emptyenv()))
    #return(nfit)
    
    return(sim)
}


.paridx_fun <- function(names) {
    # Args:
    #   names: names (character vector) such as lp__, treedepth__, stepsize__,
    #          alpha, beta.1, 
    # Returns: 
    #   The indexes in the names that are parameters other than lp__,
    #   treedepth__, or stepsize__. The vector has attribute meta
    #   with the indexes of 'treedepth__', 'lp__', and 'stepsize__'
    #   if available. 
    
    # copied from rstan package
    
    sampler_param_names <- c('lp__', 'accept_stat__', 'treedepth__', 'stepsize__', 
                             'divergent__', 'n_leapfrog__', "energy__")
    metaidx <- match(sampler_param_names, names)
    names(metaidx) <- sampler_param_names
    paridx <- setdiff(seq_along(names), metaidx)
    attr(paridx, "meta") <- metaidx[!sapply(metaidx, is.na)]
    paridx
}

.dotfnames_to_sqrfnames <- function(fnames) {
    
    # copied from rstan package
    
    fnames <- sapply(fnames,
                     function(i) {
                         if (!grepl("\\.", i)) return(i)
                         i <- sub("\\.", "[", i)
                         i <- sub("\\s*$", "]", i)
                         i }, USE.NAMES = FALSE)
    gsub("\\.\\s*", ",", fnames)
}

.get_dims_from_fnames <- function(fnames, pname) {
    # Get the dimension for a parameter from
    # the flatnames such as "alpha.1.1", ..., "alpha.3.4", the
    # format of names in the CSV files generated by Stan.
    # Currently, this function assume fnames are correctly given.
    # Args:
    #   fnames: a character of names for one (vector/array) parameter
    #   pname: the name for this vector/array parameter such as "alpha"
    #     for the above example
    
    # copied from rstan package
    
    if (missing(pname)) pname <- gsub('\\..*', '', fnames[1])
    
    if (length(fnames) == 1 && fnames == pname)
        return(integer(0)) # a scalar
    
    idxs <- sub(pname, '', fnames, fixed = TRUE)
    lp <- gregexpr('\\d+', idxs)
    
    tfun <- function(name, start, i) {
        last <- attr(start, 'match.length')[i] + start[i]
        # cat('name=', name, ', start=', start[i], ', last=', last, '.\n', sep = '')
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

.unique_par <- function(fnames) {
    # obtain parameters from flat names in format of say alpha.1,
    # alpha.2, beta.1.1, ..., beta.3.4, --- in this case, return
    # c('alpha', 'beta')
    
    # copied from rstan package
    
    unique(gsub('\\..*', '', fnames))
}

.read_csv_header <- function(f, comment.char = '#') {
    # Read the header of a csv file (the first line not beginning with
    # comment.char). And the line number is return as attribute of name 'lineno'.
    
    # copied from rstan package
    
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

.filename_rm_ext <- function(x) {
    # remove the filename's extension
    
    # copied from rstan package
    
    sub("\\.[^.]*$", "", x)
}

