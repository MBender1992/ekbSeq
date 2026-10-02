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
#' @param xlim Optional numeric vector of length 2 defining the x-axis limits.
#'   If `NULL`, symmetric limits are determined automatically from finite log2 fold changes
#'   with valid adjusted p-values.
#' @param ylim Optional numeric vector of length 2 defining the y-axis limits.
#'   If `NULL`, limits are determined automatically from finite positive adjusted p-values.
#' @param show.genes Optional character vector. Additional gene symbols (from the `SYMBOL` column) to label,
#' @param plot.signif Logical indicating whether significant results should be highlighted. Default is TRUE.
#'   Can be specified together with show.genes to show significant results AND selected genes.
#' @param pThres P-value threshold for plotting. Default is 0.05.
#' @param lfcThres Log2-fold change threshold for plotting. Default is 1.
#' @param ... Additional arguments passed to the EnhancedVolcano function.
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
#'
#' @export
plot_volcano <- function(results, nlabel = 30, title = "", pointsize = 2, labSize = 4,
                         xlim = NULL, ylim = NULL, plot.signif = TRUE, show.genes = NULL,
                         pThres = 0.05, lfcThres = 1, ...) {

  .check_columns(results, c("log2FoldChange", "padj", "SYMBOL"))
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

  ## automatic x-axis based only on points with valid adjusted p-values
  if (is.null(xlim)) {
    plot_idx <- is.finite(results$log2FoldChange) & is.finite(results$padj) & results$padj > 0
    finite_fc <- results$log2FoldChange[plot_idx]

    if (!length(finite_fc)) {
      stop("No finite log2 fold changes with valid adjusted p-values are available for the volcano plot.")
    }

    max_x <- max(abs(finite_fc))
    if (max_x == 0) max_x <- 1

    max_x <- ceiling(max_x)
    xlim <- c(-max_x, max_x)
  }

  ## automatic y-axis with modest headroom for labels
  if (is.null(ylim)) {
    finite_p <- results$padj[is.finite(results$padj) & results$padj > 0]

    if (!length(finite_p)) {
      stop("No finite positive adjusted p-values are available for the volcano plot.")
    }

    max_y <- -log10(min(finite_p))
    if (max_y <= 0) max_y <- 1

    ylim <- c(0, ceiling(max_y * 1.1))
  }

  if (plot.signif) {
    show.genes <- c(down$SYMBOL, up$SYMBOL, show.genes)
  }

  EnhancedVolcano::EnhancedVolcano(
    as.data.frame(results),
    lab = results$SYMBOL,
    selectLab = show.genes,
    x = "log2FoldChange",
    y = "padj",
    gridlines.major = FALSE,
    gridlines.minor = FALSE,
    xlim = xlim,
    ylim = ylim,
    title = title,
    pCutoff = pThres,
    FCcutoff = lfcThres,
    pointSize = pointsize,
    labSize = labSize,
    ...
  )
}
