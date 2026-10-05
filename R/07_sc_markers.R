#' Display metadata and marker expression on a Seurat UMAP
#' @param object Seurat object.
#' @param features Character vector of genes.
#' @param group_by Metadata columns for identity plots.
#' @param reduction Seurat reduction.
#' @param label,label_size Cluster labels and their size.
#' @param alpha,feature_point_size,cluster_point_size Point appearance.
#' @param raster Whether to rasterize points.
#' @param feature_colors Feature expression palette; by default RColorBrewer Blues.
#' @param group_columns Optional group palette forwarded to scCustomize.
#' @param raster_dpi Raster resolution for feature points.
#' @param widths Relative widths of cluster and feature panels.
#' @return Arranged ggplot object.
#' @export
plot_marker_umap <- function(object, features, group_by = c("timepoint", "harmony_clusters"),
                             reduction = "umap.harmony", label = TRUE, label_size = 4,
                             alpha = 0.75, feature_point_size = 0.01,
                             cluster_point_size = NULL, raster = TRUE,
                             feature_colors = NULL, group_columns = NULL,
                             raster_dpi = c(3000, 3000), widths = c(0.4, 0.6)) {
  .require_package("scCustomize")
  .require_package("ggpubr")
  .check_columns(object[[]], group_by)
  if (!length(features)) stop("At least one feature is required.")
  if (is.null(feature_colors)) {
    .require_package("RColorBrewer")
    feature_colors <- RColorBrewer::brewer.pal(9, "Blues")
  }
  cluster_args <- list(object, group.by = group_by, reduction = reduction,
                       combine = TRUE, label = label, label.size = label_size,
                       raster = raster, num_columns = 1, pt.size = cluster_point_size)
  if (!is.null(group_columns)) cluster_args$colors_use <- group_columns
  p_clusters <- do.call(scCustomize::DimPlot_scCustom, cluster_args)
  p_features <- scCustomize::FeaturePlot_scCustom(
    object, features = features, colors_use = feature_colors, label = FALSE,
    reduction = reduction, raster = raster, alpha_exp = alpha,
    raster.dpi = raster_dpi, pt.size = feature_point_size)
  ggpubr::ggarrange(p_clusters, p_features, align = "hv", widths = widths)
}

#' Summarize marker expression by cell group
#' @param object Seurat object.
#' @param features Features to summarize.
#' @param group_by Grouping metadata.
#' @param colors Optional group palette.
#' @param downsample Maximum total heatmap cells or NULL.
#' @param seed Seed for deterministic downsampling.
#' @param feature_colors Optional expression gradient.
#' @return ggplot object.
#' @export
plot_marker_summary <- function(object, features, group_by = "timepoint",
                                colors = NULL, downsample = 5000L, seed = 2534L,
                                feature_colors = NULL) {
  .require_package("Seurat")
  .require_package("ggpubr")
  .check_columns(object[[]], group_by)
  if (is.null(feature_colors)) {
    .require_package("RColorBrewer")
    feature_colors <- RColorBrewer::brewer.pal(9, "Blues")
  }
  cells <- NULL
  if (!is.null(downsample)) {
    set.seed(seed)
    cells <- sample(colnames(object), min(downsample, ncol(object)))
  }
  ridge <- Seurat::RidgePlot(object, features = features, group.by = group_by, ncol = 6) &
    ggplot2::ylab("")
  if (!is.null(colors)) ridge <- ridge & ggplot2::scale_fill_manual(values = unname(colors))
  dots <- Seurat::DotPlot(object, features = features, group.by = group_by) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.box.background = ggplot2::element_rect(fill = "white", color = NA),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)) +
    ggplot2::ylab("") + ggplot2::scale_color_distiller(palette = "Blues", direction = 1)
  heatmap <- Seurat::DoHeatmap(object, features = features, group.by = group_by,
                               group.colors = unname(colors), slot = "data", raster = TRUE,
                               cells = cells) +
    ggplot2::scale_fill_gradientn(colours = grDevices::colorRampPalette(feature_colors)(100),
                                   na.value = "grey90") +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10))
  ggpubr::ggarrange(ridge, ggpubr::ggarrange(dots, heatmap, widths = c(0.3, 0.7)),
                    nrow = 2, heights = c(0.3, 0.7))
}

#' Analyze marker ranking without GO or file output
#' @param markers Data frame with `cluster`, `gene` and `avg_log2FC`.
#' @param n Top genes per cluster.
#' @param remove_pseudogenes Remove historical automated gene prefixes.
#' @return Named list containing `top_markers`.
#' @export
analyze_cluster_markers <- function(markers, n = 10L, remove_pseudogenes = TRUE) {
  .check_columns(markers, c("cluster", "gene", "avg_log2FC"))
  if (remove_pseudogenes) markers <- markers[
    !grepl("AC[0-9]{3,}|AL[0-9]{3,}|AP[0-9]{3,}|AS[0-9]{3,}", markers$gene),
    , drop = FALSE]
  if (all(c("pct.1", "pct.2") %in% names(markers))) {
    markers$score <- (markers$avg_log2FC * markers$pct.1) / markers$pct.2
  }
  groups <- split(markers, markers$cluster)
  top <- lapply(groups, utils::head, n = n)
  list(top_markers = do.call(rbind, top))
}


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

