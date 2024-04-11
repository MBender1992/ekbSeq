#' Save workspace and significant results table from NGS analysis
#'
#' @param save logical indicating whether workspace and significant results table should be saved. Default is TRUE.
#' @param results data.frame containing results with differentially expressed genes. 
#' @param path path where data should be saved. Must end with "<<filename>>.csv".
#' @export

save_res <- function(save = TRUE, results = NULL, path = NULL){
  if(save == TRUE){
    warning("The current DSC_NGS file has been overwritten. Please check carefully if results are still valid.")
    save.image(file = "DSC_NGS.RData")
    if(!is.null(path) & !is.null(results)){
      write.csv(results, path)
    } else {
      warning("No results file or path has been provided. Results table will not be saved.")
    }
  } else if(save == FALSE) {
    warning("Data has not been saved as RData. Please check carefully if this was desired.")
  }
}