#' Save r objects as individual rds files.
#'
#' @param ... object names to be saved individually as .rds files.
#' @param folder folder where objects should be stored.
#' @export

obj_save <- function(..., folder) {
  objects <- list(...)
  object_names <- sapply(substitute(list(...))[-1], deparse)
  sapply(1:length(objects), function(i) {
    filename = paste0(folder, "/", object_names[i], ".rds")
    saveRDS(objects[i], filename)
  })
}
