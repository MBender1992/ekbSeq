#' Save workspace and significant sig.res table from NGS analysis
#'
#' @param save logical indicating whether workspace and significant results table should be saved. Default is TRUE.
#' @param all.res data.frame containing results with expression data.
#' @param sig.res data.frame containing results with differentially expressed genes.
#' @param results.name name of csv file where differential expression results are stored.
#' @param rdata.name name of the exported rdata file.
#' @param rm.batch second factor of a design formula. If present the batch effect introduced by this covariate is removed and the corresponding
#' files are saved with the suffix "adjusted".
#' @export

save_res <- function(save = TRUE, all.res = NULL, sig.res = NULL, results.name = NULL, rdata.name, rm.batch = NULL) {
  if (save == TRUE) {
    warning("The current DSC_NGS file has been overwritten. Please check carefully if results are still valid.")

    ## if else statement 1
    if (!is.null(rm.batch)) {
      save.image(file = paste(rdata.name, "_adjusted.RData", sep = ""))
    } else {
      save.image(file = paste(rdata.name, ".RData", sep = ""))
    }

    ## if else statement 2
    if (!is.null(results.name) & !is.null(sig.res)) {
      if (!is.null(rm.batch)) {
        write.csv(sig.res, paste(results.name, "_significant_genes_adjusted.csv", sep = ""))
      } else {
        write.csv(sig.res, paste(results.name, "_significant_genes.csv", sep = ""))
      }
    } else {
      warning("No results file or path has been provided. Results table will not be saved.")
    }

    ## if else statement 3
    if (!is.null(results.name) & !is.null(all.res)) {
      allRes <- allRes[, c(1, 8, 3, 5, 6)]
      colnames(allRes) <- c("GeneID", "GeneName", "log2FC", "P.Value", "adj.P.Value")
      readr::write_delim(allRes, paste(results.name, "txt", sep = ""))
    } else {
      warning("No expression file or path has been provided. Expression table will not be saved.")
    }
  } else if (save == FALSE) {
    warning("Data has not been saved as RData. Please check carefully if this was desired.")
  }
}



