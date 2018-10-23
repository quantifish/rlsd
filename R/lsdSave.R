#'
#' Save lsdOutput object
#' 
#' @export
lsdSave <- function(object, save.dir = ".") {
    
    model.name  <- object@model
    object.path <- file.path(save.dir, paste0(model.name, ".rds"))
    
    message("saving model as: ", object.path)
    
    # Save object
    saveRDS(object, file = object.path)
}


