#' lsdOutput object class
#' 
#' lsdOutput object class for storing outputs from a particular lsd model run
#' 
#' @export
#' @import methods
#' @importClassesFrom rstan stanfit
#'
setClass("lsdOutput", slots = list(
                          data = "list",            # data object or list
                          map = "list",
                          mcmc_pars = "data.frame",
                          mcmc_priors = "data.frame",
                          mcmc = "list",
                          variational = "list",
                          sim = "list",
                          sex = "character",
                          seasons = "character",
                          regions = "character",
                          inits = "list",           # initial values populated by initialisation function
                          model = "character"         # optional label for this particular run
                      )
)


#' Set up the object class
#' 
#' @param .Object no idea
#' @param model.name a model name
#' @import methods
#' @export
#' 
setMethod("initialize", signature = "lsdOutput", definition = function(.Object, model.name)
{
  if (!missing(model.name)) {
    .Object@model <- as.character(model.name)
  }
  .Object@sex     <- c("Male","Female","Mature female")
  .Object@seasons <- c("AW","SS")
    
  return(.Object)    
})


#' Dunno what this does
#' 
#' @param object an LSD object
#' @import methods
#' @export
#' 
setMethod("show", "lsdOutput",
          function(object) {
            cat("lsdOutput S4 object class for model '", object@model, "':\n" ,sep = '') 
            cat("@data:")
            print(object@data)
            cat("@map:")
            print(object@map)
            cat("@mcmc:")
            print(object@mcmc)
            cat("@variational:")
            print(object@variational)
            cat("@sex:")
            print(object@sex)
            cat("@seasons:")
            print(object@seasons)
            cat("@regions:")
            print(object@regions)
            cat("@inits:")
            print(object@inits)
            cat("@model:")
            print(object@model)
})
