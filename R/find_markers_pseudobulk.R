#' Identify marker genes using pseudobulk differential expression
#'
#' Aggregates raw single-cell counts into pseudobulk samples and performs
#' differential expression analysis using DESeq2. Cell identities are obtained
#' from \code{SeuratObject::Idents(object)}.
#'
#' If the object contains exactly two identity classes, the first factor level
#' is compared with the second. If more than two identity classes are present,
#' each identity is compared with all remaining identities combined.
#'
#' @param object A Seurat object containing raw counts and cell-level metadata.
#'
#' @param assay Character string specifying the assay to use. Defaults to the
#'   default assay of \code{object}.
#'
#' @param features Optional character vector of genes to include. If
#'   \code{NULL}, all genes in the selected assay are used.
#'
#' @param group.by Character vector defining the variables used to construct
#'   pseudobulk samples. This should contain at least one biological sample
#'   variable, such as \code{"donor"}. The reserved value \code{"ident"} refers
#'   to the active identities of the Seurat object.
#'
#' @param design A model formula passed to
#'   \code{DESeq2::DESeqDataSetFromMatrix()}. The formula must contain
#'   \code{ident}. All other variables in the formula must be included in
#'   \code{group.by}.
#'
#' @param only.pos Logical. If \code{TRUE}, only genes with a positive
#'   log2 fold change of at least \code{logfc.threshold} are returned.
#'   If \code{FALSE}, genes are retained based on the absolute log2 fold
#'   change. Defaults to \code{FALSE}, following Seurat's
#'   \code{FindMarkers()}.
#'
#' @param logfc.threshold Numeric. Minimum absolute DESeq2 log2 fold change
#'   required for a gene to be returned. When \code{only.pos = TRUE}, the
#'   threshold is applied only in the positive direction. Defaults to
#'   \code{0.1}.
#'
#' @param min.pct Numeric between 0 and 1. A gene must be detected in at least
#'   this fraction of cells in either comparison group. Detection is defined
#'   as a raw count greater than zero. Defaults to \code{0.01}.
#'
#' @param min.cells Integer. Minimum number of cells required to retain an
#'   individual pseudobulk sample. Defaults to \code{20}.
#'
#' @param verbose Logical. If \code{TRUE}, the current comparison is reported.
#'   Internal output from \code{PseudobulkExpression()} and DESeq2 is
#'   suppressed. Defaults to \code{TRUE}.
#'
#' @details
#' Raw counts are summed using
#' \code{Seurat::PseudobulkExpression(method = "aggregate")}. Each pseudobulk
#' sample represents one unique combination of the sample variables supplied
#' through \code{group.by} and the identity class used in the current
#' comparison.
#'
#' For comparisons involving more than two identity classes, all non-target
#' cells belonging to the same combination of sample variables are aggregated
#' into a single \code{rest} pseudobulk sample.
#'
#' The columns \code{pct.1} and \code{pct.2} are calculated from the original
#' single-cell count matrix, restricted to cells represented in retained
#' pseudobulk samples. They are descriptive measures and are not part of the
#' DESeq2 model.
#'
#' Positive \code{log2FoldChange} values indicate higher expression in the
#' target identity. Negative values indicate higher expression in the
#' reference identity or in the combined \code{rest} population.
#'
#' The function does not perform log2 fold-change shrinkage. Adjusted p-values
#' are calculated by DESeq2 using the Benjamini-Hochberg procedure separately
#' for each comparison.
#'
#' @return A data frame containing one row per retained gene and comparison,
#'   with the following columns:
#'
#' \describe{
#'   \item{\code{gene}}{Gene identifier.}
#'   \item{\code{cluster}}{Target identity class.}
#'   \item{\code{comparison}}{Comparison represented by the row.}
#'   \item{\code{pct.1}}{Fraction of target cells expressing the gene.}
#'   \item{\code{pct.2}}{Fraction of reference cells expressing the gene.}
#'   \item{\code{baseMean}}{Mean normalized count across pseudobulk samples.}
#'   \item{\code{log2FoldChange}}{Estimated target-versus-reference log2 fold
#'     change.}
#'   \item{\code{lfcSE}}{Standard error of the log2 fold-change estimate.}
#'   \item{\code{stat}}{DESeq2 Wald statistic.}
#'   \item{\code{pvalue}}{Raw DESeq2 p-value.}
#'   \item{\code{padj}}{Benjamini-Hochberg-adjusted p-value.}
#' }
#'
#' @examples
#' \dontrun{
#' # Marker genes for all active identities
#' SeuratObject::Idents(object) <- "cell_state"
#'
#' markers <- find_markers_pseudobulk(
#'   object = object,
#'   assay = "RNA",
#'   group.by = c("donor", "ident"),
#'   design = ~ donor + ident,
#'   only.pos = TRUE,
#'   logfc.threshold = 0.25,
#'   min.pct = 0.1
#' )
#'
#' # For two groups, the first factor level is tested against the second
#' SeuratObject::Idents(object) <- factor(
#'   SeuratObject::Idents(object),
#'   levels = c(
#'     "DLR fate committed",
#'     "Melanocyte fate committed"
#'   )
#' )
#'
#' fate_markers <- find_markers_pseudobulk(
#'   object = object,
#'   group.by = c("donor", "ident"),
#'   design = ~ donor + ident
#' )
#' }
#'
#' @seealso
#' \code{\link[Seurat]{PseudobulkExpression}},
#' \code{\link[Seurat]{FindMarkers}},
#' \code{\link[DESeq2]{DESeq}}
#'
#' @export

find_markers_pseudobulk <- function(
    object,
    assay = SeuratObject::DefaultAssay(object),
    features = NULL,
    group.by,
    design,
    only.pos = FALSE,
    logfc.threshold = 0.1,
    min.pct = 0.01,
    min.cells = 20,
    verbose = TRUE
) {

  # Retrieve the active identity levels. Their order determines the direction
  # of the comparison when exactly two identities are present.
  groups <- levels(droplevels(SeuratObject::Idents(object)))

  if (length(groups) < 2) {
    stop("At least two identity classes are required.")
  }

  # "ident" is generated from Idents(object); all remaining variables must
  # correspond to columns in the Seurat metadata.
  sample_vars <- setdiff(group.by, "ident")

  if (length(sample_vars) == 0) {
    stop("group.by must contain at least one sample variable.")
  }

  if (!all(sample_vars %in% colnames(object[[]]))) {
    stop(
      "Missing metadata columns: ",
      paste(
        setdiff(sample_vars, colnames(object[[]])),
        collapse = ", "
      )
    )
  }

  # The identity term is required to estimate the requested contrast.
  if (!"ident" %in% all.vars(design)) {
    stop("The model formula must contain 'ident'.")
  }

  # Every model variable must be available in the pseudobulk metadata.
  if (!all(all.vars(design) %in% c(sample_vars, "ident"))) {
    stop("All model variables must be included in group.by.")
  }

  # Raw single-cell counts are retained for calculating pct.1 and pct.2.
  cell_counts <- SeuratObject::GetAssayData(
    object,
    assay = assay,
    layer = "counts"
  )

  # For two identities, calculate only the first-versus-second contrast.
  # For more than two identities, calculate each identity versus rest.
  targets <- if (length(groups) == 2) groups[1] else groups

  run_comparison <- function(target) {

    reference <- if (length(groups) == 2) groups[2] else "rest"

    if (verbose) {
      message(
        "Calculating pseudobulk differential expression for ",
        target, " vs ", reference, " ..."
      )
    }

    # Obtain metadata variables defining the biological pseudobulk samples.
    meta <- object[[]][, sample_vars, drop = FALSE]

    # Preserve the original identities for a two-group comparison. For
    # one-versus-rest comparisons, collapse all non-target identities into
    # a common reference group.
    meta$ident <- if (length(groups) == 2) {
      as.character(SeuratObject::Idents(object))
    } else {
      ifelse(
        SeuratObject::Idents(object) == target,
        target,
        "rest"
      )
    }

    # Set the reference level explicitly. This also defines the direction of
    # the reported DESeq2 log2 fold change.
    meta$ident <- factor(
      meta$ident,
      levels = c(reference, target)
    )

    # Generate a safe internal identifier for every unique combination of
    # sample variables and comparison group.
    key <- do.call(
      paste,
      c(lapply(meta, as.character), sep = "\r")
    )

    meta$.pb_id <- sprintf(
      "PB%04d",
      match(key, unique(key))
    )

    # Construct one metadata row per pseudobulk sample.
    pb_meta <- meta[
      !duplicated(meta$.pb_id),
      ,
      drop = FALSE
    ]

    rownames(pb_meta) <- pb_meta$.pb_id

    # Record the number of cells contributing to every pseudobulk sample.
    pb_meta$n_cells <- as.integer(
      table(meta$.pb_id)[pb_meta$.pb_id]
    )

    # Add the temporary pseudobulk identifier to the local Seurat object.
    object$.pb_id <- meta$.pb_id

    # Sum raw counts within each pseudobulk sample. Internal progress messages
    # are suppressed so that only the comparison-level message is displayed.
    pb_counts <- suppressMessages(
      Seurat::PseudobulkExpression(
        object = object,
        assays = assay,
        features = features,
        group.by = ".pb_id",
        layer = "counts",
        method = "aggregate",
        return.seurat = FALSE,
        verbose = FALSE
      )
    )[[assay]]

    # Match pseudobulk metadata to the count-matrix column order.
    pb_meta <- pb_meta[
      colnames(pb_counts),
      ,
      drop = FALSE
    ]

    # Exclude pseudobulk samples supported by fewer than min.cells cells.
    keep_samples <- pb_meta$n_cells >= min.cells

    pb_counts <- pb_counts[
      ,
      keep_samples,
      drop = FALSE
    ]

    pb_meta <- pb_meta[
      keep_samples,
      ,
      drop = FALSE
    ]

    pb_meta$ident <- droplevels(pb_meta$ident)

    # Restrict cell-level detection calculations to cells contributing to
    # retained pseudobulk samples.
    used_cells <- meta$.pb_id %in% rownames(pb_meta)

    counts_used <- cell_counts[
      ,
      rownames(meta)[used_cells],
      drop = FALSE
    ]

    groups_used <- meta$ident[used_cells]

    # Calculate the fraction of cells with at least one raw count.
    pct.1 <- Matrix::rowMeans(
      counts_used[
        ,
        groups_used == target,
        drop = FALSE
      ] > 0
    )

    pct.2 <- Matrix::rowMeans(
      counts_used[
        ,
        groups_used == reference,
        drop = FALSE
      ] > 0
    )

    # Ensure that the supplied model is identifiable for the current set of
    # pseudobulk samples.
    model_matrix <- stats::model.matrix(
      design,
      data = pb_meta
    )

    if (qr(model_matrix)$rank < ncol(model_matrix)) {
      stop(
        "Model matrix is not full rank for comparison: ",
        target, " vs ", reference
      )
    }

    # Construct and fit the negative-binomial DESeq2 model.
    dds <- DESeq2::DESeqDataSetFromMatrix(
      countData = round(as.matrix(pb_counts)),
      colData = pb_meta,
      design = design
    )

    dds <- suppressMessages(
      DESeq2::DESeq(
        dds,
        minReplicatesForReplace = Inf,
        quiet = TRUE
      )
    )

    # Extract the target-versus-reference contrast.
    deseq_result <- DESeq2::results(
      dds,
      contrast = c("ident", target, reference)
    )

    # Combine cell-level detection frequencies with the unmodified DESeq2
    # result columns.
    result <- data.frame(
      gene = rownames(deseq_result),
      cluster = target,
      comparison = paste(target, "vs", reference),
      pct.1 = unname(pct.1[rownames(deseq_result)]),
      pct.2 = unname(pct.2[rownames(deseq_result)]),
      as.data.frame(deseq_result),
      row.names = NULL,
      check.names = FALSE
    )

    # Apply FindMarkers-like detection and fold-change filters.
    keep_genes <- pmax(result$pct.1, result$pct.2) >= min.pct

    if (only.pos) {
      keep_genes <- keep_genes &
        result$log2FoldChange >= logfc.threshold
    } else {
      keep_genes <- keep_genes &
        abs(result$log2FoldChange) >= logfc.threshold
    }

    # Genes without an estimable fold change do not pass the filters.
    keep_genes[is.na(keep_genes)] <- FALSE

    # Retain passing genes and rank them by the unadjusted DESeq2 p-value.
    result <- result[keep_genes, , drop = FALSE]
    result <- result[order(result$pvalue), , drop = FALSE]

    result
  }

  # Run all requested identity comparisons and combine their results.
  do.call(
    rbind,
    lapply(targets, run_comparison)
  )
}
