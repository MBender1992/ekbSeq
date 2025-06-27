#' Plot a Volcano Plot Highlighting Top and Custom Genes
#'
#' Generates a volcano plot from differential expression results using the \pkg{EnhancedVolcano} package.
#' It labels the top up- and downregulated genes, and optionally any user-specified genes.
#'
#' @param results A `data.frame` or tibble containing differential expression results.
#'   Must include columns `log2FoldChange`, `padj`, and `SYMBOL`.
#' @param nlabel Integer. Number of top upregulated and downregulated genes to label. Default is 30.
#' @param title Character. Plot title. Default is an empty string.
#' @param pointsize Numeric. Size of points in the plot. Default is 2.
#' @param labSize Numeric. Size of gene labels. Default is 4.
#' @param xlim Numeric vector of length 2. X-axis limits (log2 fold change). Default is `c(-8, 8)`.
#' @param ylim Numeric vector of length 2. Y-axis limits (–log10 p-value). Default is `c(-0.5, 100)`.
#' @param show.genes Optional character vector. Additional gene symbols (from the `SYMBOL` column) to label,
#' @param pThres P-value threshold for plotting. Default is 0.05.
#' @param lfcThres Log2-fold change threshold for plotting. Default is 1.
#'   in addition to the top up- and downregulated genes.
#'
#' @return A ggplot2 object representing the volcano plot.
#'
#' @details
#' The function selects the top `nlabel` upregulated (log2FC > 0) and downregulated (log2FC < 0)
#' genes by adjusted p-value. You can also specify additional genes to label via `show.genes`.
#'
#' @examples
#' \dontrun{
#' plot_volcano(res, nlabel = 20, title = "IR vs Control", show.genes = c("CDKN1A", "GADD45A"))
#' }
#' @export
#'
plot_volcano <- function(results, nlabel = 30, title = "", pointsize = 2, labSize = 4,
                         xlim = NULL, ylim = NULL, show.genes = NULL, pThres = 0.05, lfcThres = 1) {
  log2FoldChange <- NULL
  padj <- NULL

  down <- results %>%
    dplyr::filter(log2FoldChange < 0) %>%
    dplyr::arrange(padj, log2FoldChange) %>%
    head(nlabel)

  up <- results %>%
    dplyr::filter(log2FoldChange > 0) %>%
    dplyr::arrange(padj, log2FoldChange) %>%
    head(nlabel)

  if(is.null(xlim)){
    max_x <- max(results$log2FoldChange, na.rm = T)
    xlim <- c(-max_x, max_x)
  }

  if(is.null(ylim)){
    max_y <- max(-log10(min(results$padj, na.rm = T)))
    ylim <- c(-0.5, max_y)
  }

  EnhancedVolcano::EnhancedVolcano(
    as.data.frame(results),
    lab = results$SYMBOL,
    selectLab = c(down$SYMBOL, up$SYMBOL, show.genes),
    x = 'log2FoldChange',
    y = 'padj',
    gridlines.major = FALSE,
    gridlines.minor = FALSE,
    xlim = xlim,
    ylim = ylim,
    title = title,
    pCutoff = pThres,
    FCcutoff = lfcThres,
    pointSize = pointsize,
    labSize = labSize
  )
}
