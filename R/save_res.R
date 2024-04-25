#' Save workspace and significant results table from NGS analysis
#'
#' @param save logical indicating whether workspace and significant results table should be saved. Default is TRUE.
#' @param results data.frame containing results with differentially expressed genes.
#' @param path path where data should be saved. Must end with "<<filename>>.csv".
#' @param file.name name of the exported csv and rdata file.
#' @param rm.batch second factor of a design formula. If present the batch effect introduced by this covariate is removed and the corresponding
#' files are saved with the suffix "adjusted".
#' @export

save_res <- function(save = TRUE, results = NULL, path = NULL, file.name, rm.batch = NULL){
  if(save == TRUE){
    warning("The current DSC_NGS file has been overwritten. Please check carefully if results are still valid.")
    if(!is.null(rm.batch)) save.image(file = paste(file.name, "_adjusted.RData", sep = "")) else save.image(file = paste(file.name, ".RData", sep = ""))
    if(!is.null(path) & !is.null(results)){
      if(!is.null(rm.batch)) write.csv(results, paste(path, "_adjusted.csv", sep = "")) else write.csv(results, paste(path, ".csv", sep = ""))
    } else {
      warning("No results file or path has been provided. Results table will not be saved.")
    }
  } else if(save == FALSE) {
    warning("Data has not been saved as RData. Please check carefully if this was desired.")
  }
}
