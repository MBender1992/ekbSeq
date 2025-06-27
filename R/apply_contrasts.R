#' Extract and Annotate DESeq2 Contrast Results with Optional LFC Shrinkage
#'
#' This function extracts differential expression results for a specified contrast from a global
#' DESeq2 results object (\code{dds}). It constructs the contrast using a helper function
#' (\code{contraster}), and optionally applies log2 fold change shrinkage via \code{lfcShrink} (using
#' the "ashr" method). If an annotation object is provided, the function merges the annotation with
#' the results and removes any Ensembl IDs containing a period. Additionally, when shrinkage is enabled,
#' the annotated results are saved as an Excel file (.xlsx) to the specified path.
#'
#' @param dds DESeq object.
#' @param trt Character. The treatment group label to be contrasted.
#' @param ctrl Character. The control group label to serve as the baseline.
#' @param lfcThres log2FoldChange thresholds used in the results function.
#' @param pThres P-value threshold used in the results function.
#' @param condition Character. The factor name in \code{dds} defining the experimental condition.
#'   Default is \code{"condition"}.
#' @param annObj A data frame containing annotation information for the genes. Must have the same
#'   number of rows as the result object. If provided, the annotation is merged with the DE results.
#'   Default is \code{NULL}.
#' @param shrink Logical. If \code{TRUE}, log2 fold change shrinkage is performed using \code{lfcShrink}.
#'   Default is \code{FALSE}.
#' @param path Character. The directory path where the Excel file (of shrunken and annotated results)
#'   will be saved. This argument is used only when \code{shrink = TRUE}.
#'
#' @return A data frame containing the differential expression results for the specified contrast.
#'   If an annotation object is provided, the results include the merged annotation columns. When
#'   \code{shrink = TRUE} and an annotation object is provided, the annotated results are also saved
#'   as an Excel file in the specified path.
#'
#' @details
#' The function uses a global DESeq2 object \code{dds} along with preset thresholds \code{lfcThres}
#' and \code{pThres}. It builds the contrast with \code{contraster(dds, group1 = list(c(condition, trt)),
#' group2 = list(c(condition, ctrl)))}. When \code{shrink = TRUE}, the function applies the \code{lfcShrink}
#' function using the "ashr" method to obtain shrunken fold changes. If an annotation data frame
#' (\code{annObj}) is supplied, the function checks that its dimensions match the results, merges the
#' annotation via \code{left_join}, and removes rows with Ensembl IDs that contain a period. Finally,
#' if shrinkage is applied, the annotated results are saved as an Excel file (using \code{write.xlsx})
#' with a filename constructed from the treatment and control labels.
#'
#' @note
#' The variables \code{dds}, \code{lfcThres}, \code{pThres}, and the helper function \code{contraster}
#' must be defined in the global environment prior to using this function.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' # Assuming dds, lfcThres, pThres, and contraster are properly defined in the global environment,
#' # and annotation_df is a data frame with gene annotations:
#' res <- apply_contrasts(trt = "UVB", ctrl = "control", condition = "treatment",
#'                        annObj = annotation_df, shrink = TRUE, path = "results/")
#' }
#'
#' @export

apply_contrasts <- function(dds, trt, ctrl, lfcThres = 0, pThres = 0.05, condition = "condition", annObj = NULL, shrink = FALSE, path = NULL) {
  res <- results(dds, lfcThreshold = lfcThres, alpha = pThres,
                 contrast = contraster(dds,
                                       group1 = list(c(condition, trt)),
                                       group2 = list(c(condition, ctrl))))

  if (shrink == TRUE) {
    message("Output contains shrunken log fold changes.")
    res <- lfcShrink(dds, res = res, contrast = c(condition, trt, ctrl), type = "ashr")
  } else {
    message("Output contains original fold changes.")
  }

  if (!is.null(annObj)) {
    allRes <- cbind(ENSEMBL = rownames(res), res)
    allRes <- if (dim(annObj)[1] == dim(allRes)[1]) {
      dplyr::left_join(as.data.frame(allRes), annObj)
    } else {
      stop("Dimensions of annotation object and result object are different.")
    }
    allRes <- allRes[!str_detect(allRes$ENSEMBL, "\\."), ]

    if (shrink == TRUE) {
      res_print <- as.data.frame(allRes)
      write.xlsx(res_print, paste0(path, trt, "_vs_", ctrl, "_shrunken_LFC.xlsx"))
    }
    res <- allRes
  }
  res
}
