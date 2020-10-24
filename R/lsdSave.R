#' Save lsdOutput object
#'
#' @param object an LSD object
#' @param save.dir the directory to save to
#' @export
#'
lsdSave <- function(object, save.dir = ".") {
  model.name  <- object@model
  object.path <- file.path(save.dir, paste0(model.name, ".rds"))
  message("saving model as: ", object.path)
  saveRDS(object, file = object.path)
}
