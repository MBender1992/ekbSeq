#' Construct a DESeq2 model matrix contrast
#' @param dds A DESeqDataSet.
#' @param group1,group2 Lists of factor/value selections. Positive coefficients
#'   represent group1 relative to group2.
#' @param weighted Retain sample-frequency weighting. FALSE compares unique
#'   design rows, exactly as historical `contraster()`.
#' @return A numeric contrast vector in design-matrix coefficient order.
#' @export
make_deseq_contrast <- function(dds, group1, group2, weighted = FALSE) {
  .require_package("DESeq2")
  .require_package("SummarizedExperiment")
  meta <- as.data.frame(SummarizedExperiment::colData(dds))
  mat <- stats::model.matrix(DESeq2::design(dds), meta)
  select_rows <- function(groups) {
    if (!is.list(groups) || !length(groups)) stop("Groups must be nonempty lists.")
    flags <- lapply(groups, function(g) {
      if (length(g) < 2L || !g[1L] %in% names(meta)) stop("Invalid group factor/value selection.")
      !is.na(meta[[g[1L]]]) & meta[[g[1L]]] %in% g[-1L]
    })
    Reduce(`&`, flags)
  }
  first <- mat[select_rows(group1), , drop = FALSE]
  second <- mat[select_rows(group2), , drop = FALSE]
  if (!nrow(first) || !nrow(second)) stop("Both contrast groups must contain samples.")
  if (!weighted) {
    first <- first[!duplicated(first), , drop = FALSE]
    second <- second[!duplicated(second), , drop = FALSE]
  }
  colMeans(first) - colMeans(second)
}

#' Compute a DESeq2 contrast without writing files
#' @param dds Fitted DESeqDataSet.
#' @param treatment,control Values of `condition`; log2 fold changes represent
#'   treatment divided by control.
#' @param condition Metadata factor name.
#' @param lfc_threshold,p_threshold Thresholds passed to `DESeq2::results()`.
#' @param annotation Optional annotation data frame, joined as in the legacy
#'   workflow when it has one row per result.
#' @param shrink Use `ashr` shrinkage as in `apply_contrasts()`.
#' @return DESeqResults, or an annotated data frame when `annotation` is given.
#' @export
deseq_contrast <- function(dds, treatment, control, condition = "condition",
                           lfc_threshold = 0, p_threshold = 0.05,
                           annotation = NULL, shrink = FALSE) {
  .require_package("DESeq2")
  contrast <- make_deseq_contrast(dds, list(c(condition, treatment)), list(c(condition, control)))
  res <- DESeq2::results(dds, lfcThreshold = lfc_threshold, alpha = p_threshold,
                         contrast = contrast)
  if (shrink) {
    .require_package("ashr")
    res <- DESeq2::lfcShrink(dds, res = res,
                             contrast = c(condition, treatment, control), type = "ashr")
  }
  if (is.null(annotation)) return(res)
  if (nrow(annotation) != nrow(res)) stop("Dimensions of annotation object and result object are different.")
  .require_package("dplyr")
  result <- cbind(ENSEMBL = rownames(res), as.data.frame(res))
  result <- dplyr::left_join(result, annotation)
  result[!grepl("\\.", result$ENSEMBL), , drop = FALSE]
}

#' Compare expressed differential genes across arbitrary groups
#' @param counts Numeric gene-by-sample matrix.
#' @param results Differential expression table ordered like `counts`, with
#'   `padj` and optionally `ENSEMBL` and biotype columns.
#' @param groups Named list mapping group labels to sample column names.
#' @param p_threshold,expression_threshold Strict adjusted p-value and average
#'   count thresholds, respectively.
#' @param biotypes Optional allowed biotypes.
#' @param biotype_col Biotype column in `results`.
#' @return List `sets`, `exclusive`, and `shared`; no files are written.
#' @export
compare_expression_sets <- function(counts, results, groups, p_threshold = 0.05,
                                    expression_threshold = 1, biotypes = NULL,
                                    biotype_col = "GENETYPE_biomaRt") {
  counts <- as.matrix(counts)
  .check_columns(results, "padj")
  if (!identical(rownames(counts), rownames(results))) {
    if ("ENSEMBL" %in% names(results) && !anyDuplicated(results$ENSEMBL)) {
      results <- results[match(rownames(counts), results$ENSEMBL), , drop = FALSE]
    } else stop("Result rows must align with count rows or provide unique ENSEMBL IDs.")
  }
  keep <- !is.na(results$padj) & results$padj < p_threshold
  if (!is.null(biotypes)) {
    .check_columns(results, biotype_col)
    keep <- keep & !is.na(results[[biotype_col]]) & results[[biotype_col]] %in% biotypes
  }
  if (!is.list(groups) || length(groups) != 2L || is.null(names(groups))) {
    stop("Provide exactly two named groups with sample column names.")
  }
  selected <- lapply(groups, function(samples) {
    if (!length(samples) || !all(samples %in% colnames(counts))) stop("Unknown or empty sample group.")
    rownames(counts)[keep & rowMeans(counts[, samples, drop = FALSE]) > expression_threshold]
  })
  list(sets = selected, exclusive = lapply(seq_along(selected), function(i) {
    setdiff(selected[[i]], selected[[3L - i]])
  }), shared = intersect(selected[[1L]], selected[[2L]]))
}

#' Plot an overlap of two gene sets
#' @param sets Named list of two gene vectors.
#' @param title Optional plot title.
#' @param fill_color Set fill colours.
#' @param stroke_size,set_name_size,text_size Historical ggvenn styling.
#' @param caption Optional caption.
#' @return ggplot object from `ggvenn`.
#' @export
plot_gene_overlap <- function(sets, title = NULL,
                              fill_color = c("#CD534CFF", "#0073C2FF"),
                              stroke_size = 0.5, set_name_size = 7,
                              text_size = 7, caption = NULL) {
  .require_package("ggvenn")
  if (!is.list(sets) || length(sets) != 2L) stop("Provide exactly two sets.")
  ggvenn::ggvenn(sets, fill_color = fill_color, stroke_size = stroke_size,
                set_name_size = set_name_size, text_size = text_size) +
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 12)) +
    ggplot2::labs(title = title, caption = caption)
}
