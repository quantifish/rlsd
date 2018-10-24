#' Extract MCMC samples
#' 
#' Extract MCMC samples from a list produced by \code{read_stan_mcmc}
#' 
#' @param object list object produced by \code{read_stan_mcmc}
#' @param pars character vector of parameters to be extracted (or not)
#' @param permute should the MCMC sample chains be permuted and combined?
#' @param include should pars be extracted (TRUE) or everything except the pars (FALSE) 
#' @importMethodsFrom rstan extract
#' @import methods
#' 
setGeneric("extract", getGeneric("extract", package = "rstan"))

#' Extract thingy
#' 
#' @import methods
#' @export
#' 
setMethod("extract", signature = "list", definition = function(object, pars, permuted = TRUE, include = TRUE) {
    
              # Extract the samples in different forms for different parameters. 
              #
              # Args:
              #   object: the object of "stanfit" class 
              #   pars: the names of parameters (including other quantiles) 
              #   permuted: if TRUE, the returned samples are permuted without
              #     warming up. And all the chains are merged. 
              #   inc_warmup: if TRUE, warmup samples are kept; otherwise, 
              #     discarded. If permuted is TRUE, inc_warmup is ignored. 
              #   include: if FALSE interpret pars as those to exclude
              #
              # Returns:
              #   If permuted is TRUE, return an array (matrix) of samples with each
              #   column being the samples for a parameter. 
              #   If permuted is FALSE, return array with dimensions
              #   (# of iter (with or w.o. warmup), # of chains, # of flat parameters). 
              
              #if (object@mode == 1L) {
              #    cat("Stan model '", object@model_name, "' is of mode 'test_grad';\n",
              #        "sampling is not conducted.\n", sep = '')
              #    return(invisible(NULL)) 
              #} else if (object@mode == 2L) {
              #    cat("Stan model '", object@model_name, "' does not contain samples.\n", sep = '') 
              #    return(invisible(NULL)) 
              #} 
    
                #object <- sim
              
              if(!include) pars <- setdiff(object$pars_oi, pars)
              pars <- if (missing(pars)) object$pars_oi else pars #check_pars_second(object@sim, pars) 
              #pars <- remove_empty_pars(pars, object@sim$dims_oi)
              tidx <- .pars_total_indexes(object$pars_oi, 
                                         object$dims_oi, 
                                         object$fnames_oi, 
                                         pars) 
              
              n_kept <- object$iter
              
              fun1 <- function(par_i) {
                  # sss <- sapply(tidx[[par_i]], get_kept_samples2, object@sim)
                  # if (is.list(sss))  sss <- do.call(c, sss)
                  # the above two lines are slower than the following line of code
                  sss <- do.call(cbind, lapply(tidx[[par_i]], .get_samples_permute, object)) 
                  dim(sss) <- c(sum(n_kept), object$dims_oi[[par_i]]) 
                  dimnames(sss) <- list(iterations = NULL)
                  sss 
              } 
              
              if (permuted) {
                  
                  slist <- lapply(pars, fun1) 
                  names(slist) <- pars 
                  return(slist) 
                  
              } else {
              
                  tidx <- unlist(tidx, use.names = FALSE) 
                  tidxnames <- object$fnames_oi[tidx] 
                  
                  sss  <- lapply(tidx, .get_samples, object) 
                  
                  max_iter <- max(unlist(lapply(sss[[1]], length)))
                  
                  sss2 <- array(NA, dim = c(max_iter, object$chains, length(tidx)))
                  
                  #sss2 <- vector('list', length(sss))
                  #sss2 <- lapply(sss2, function(x) matrix(NA, ncol = object$chains, nrow = max_iter))
                  
                  for (i in 1:length(sss))
                      for (j in 1:length(sss[[i]]))
                          sss2[1:length(sss[[i]][[j]]),j,i] <- sss[[i]][[j]]
                          #sss2[[i]][1:length(sss[[i]][[j]]),j] <- sss[[i]][[j]]
                  
                  dimnames(sss2) <- list(iterations = NULL, chains = paste0("chain:", 1:object$chains), parameters = tidxnames)
                  
                  #names(sss2) <- tidxnames
                  
                  #sss2 <- lapply(sss2, function(x){ dimnames(x) <- list(iterations = NULL, chains = paste0("chain:", 1:object$chains)); x})
                  
                  #sss2 <- lapply(sss, function(x) do.call(c, x))  # concatenate samples from different chains
                  #sssf <- unlist(sss2, use.names = FALSE) 
                  #
                  #n2 <- object$iter[1]  ## assuming all the chains have equal iter 
                  #
                  #dim(sssf) <- c(n2, object$chains, length(tidx)) 
                  #dimnames(sssf) <- list(iterations = NULL, chains = paste0("chain:", 1:object$chains), parameters = tidxnames)
                  #sssf 
                  
                  return(sss2)
              
              }
})  

.num_pars <- function(d) prod(d)

.calc_starts <- function(dims) {
    
    len <- length(dims)
    s <- sapply(unname(dims), function(d)  .num_pars(d), USE.NAMES = FALSE)
    cumsum(c(1, s))[1:len]
}

.pars_total_indexes <- function(names, dims, fnames, pars) {
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
    
    starts <- .calc_starts(dims)
    
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
        np <- .num_pars(dims[[p]])
        if (np == 0) return(NULL)
        idx <- starts[p] + seq(0, by = 1, length.out = np)
        names(idx) <- fnames[idx]
        attr(idx, "row_major_idx") <- starts[p] + .idx_col2rowm(dims[[p]]) - 1
        idx
    }
    
    idx <- lapply(pars, FUN = par_total_indexes)
    nulls <- sapply(idx, is.null)
    idx <- idx[!nulls]
    names(idx) <- pars[!nulls]
    idx
}

.idx_col2rowm <- function(d) {
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

.get_samples <- function(n, sim) {
    
    lst <- vector("list", sim$chains)
    for (ic in 1:sim$chains) { 
        lst[[ic]] <- sim$samples[[ic]][[n]]
    } 
    lst
}

.get_samples_permute <- function(n, sim) {
    
    lst <- vector("list", sim$chains)
    for (ic in 1:sim$chains) { 
        lst[[ic]] <- sim$samples[[ic]][[n]][sim$permutation[[ic]]]
    } 
    do.call(c, lst)
}
