#' Save workspace and significant sig.res table from NGS analysis
#'
#' @param save logical indicating whether workspace and significant results table should be saved. Default is TRUE.
#' @param all.res data.frame containing results with expression data.
#' @param sig.res data.frame containing results with differentially expressed genes.
#' @param pathway.res data.frame containing results of pathfindR using wiki pathways.
#' @param results.name name of csv file where differential expression results are stored.
#' @param rdata.name name of the exported rdata file.
#' @param rm.batch second factor of a design formula. If present the batch effect introduced by this covariate is removed and the corresponding
#' files are saved with the suffix "adjusted".
#' @export

save_res <- function(save = TRUE, all.res = NULL, sig.res = NULL, pathway.res = NULL,
                     results.name = NULL, rdata.name, rm.batch = NULL) {
  if (save == TRUE) {
    warning("The current DSC_NGS file has been overwritten. Please check carefully if results are still valid.")

    ## if else statement 1 (save Rdata object)
    if (!is.null(rm.batch)) {
      save.image(file = paste(rdata.name, "_adjusted.RData", sep = ""))
    } else {
      save.image(file = paste(rdata.name, ".RData", sep = ""))
    }

    ## if else statement 2 (save significant genes as xlsx)
    if (!is.null(results.name) & !is.null(sig.res)) {
      if (!is.null(rm.batch)) {
        writexl::write_xlsx(sig.res, paste(results.name, "_significant_genes_adjusted.xlsx", sep = ""))
      } else {
        writexl::write_xlsx(sig.res, paste(results.name, "_significant_genes.xlsx", sep = ""))
      }
    } else {
      warning("No results file or path has been provided. Results table will not be saved.")
    }

    ## if else statement 3 (saving all results, as xlsx and input for PathVisio)
    if (!is.null(results.name) & !is.null(all.res)) {
      resPathVisio <- all.res[, c(1, 8, 3, 5, 6)]
      colnames(resPathVisio) <- c("GeneID", "GeneName", "log2FC", "P.Value", "adj.P.Value")
      readr::write_delim(resPathVisio, paste(results.name, "_all_PathVisio.txt", sep = ""))
      writexl::write_xlsx(all.res, paste(results.name, "_all_genes.xlsx", sep = ""))
    } else {
      warning("No expression file or path has been provided. Expression table will not be saved.")
    }

    if (!is.null(results.name) & !is.null(sig.res)) {
      resPathVisio <- sig.res[, c(1, 8, 3, 5, 6)]
      colnames(resPathVisio) <- c("GeneID", "GeneName", "log2FC", "P.Value", "adj.P.Value")
      readr::write_delim(resPathVisio, paste(results.name, "_signif_PathVisio.txt", sep = ""))
    } else {
      warning("No expression file or path has been provided. Expression table will not be saved.")
    }

    ## if else statement 4 (save enriched wikipathways)
    if (!is.null(results.name) & !is.null(pathway.res)) {
      writexl::write_xlsx(pathway.res, paste(results.name, "_enriched_wikiPW.xlsx", sep = ""))
    } else {
      warning("No pathway analysis or path has been provided. Pathway table will not be saved.")
    }

  } else if (save == FALSE) {
    warning("Data has not been saved as RData. Please check carefully if this was desired.")
  }
}
