#' Historical observed-design-profile contrast
#'
#' Legacy compatibility interface preserving the historical `contraster()`
#' behavior. The contrast is calculated from observed model-matrix profiles and
#' is retained only for reproducibility of historical analyses.
#'
#' @param dds A DESeqDataSet.
#' @param group1,group2 Lists of factor/value selections defining the two
#'   historical comparison groups.
#' @param weighted Retain sample-frequency weighting. `FALSE` compares unique
#'   observed design rows, matching the historical implementation.
#' @return A numeric contrast vector in design-matrix coefficient order.
#' @export
contraster <- function(dds, group1, group2, weighted = FALSE) {
  .legacy_api_message("contraster", "contraster")
  .legacy_frozen_contraster(dds = dds, group1 = group1, group2 = group2, weighted = weighted)
}

#' Historical DESeq2 contrast workflow
#'
#' Legacy compatibility interface preserving the historical `apply_contrasts()`
#' behavior, including its observed-design-profile contrast construction,
#' optional `ashr` shrinkage, annotation merge, and optional Excel output.
#'
#' For new standard factor comparisons, use `deseq_contrast()`. For interaction
#' effects or other complex scientific contrasts, define the contrast explicitly
#' with `DESeq2::results()`.
#'
#' @param dds Fitted DESeqDataSet.
#' @param trt,ctrl Legacy treatment and control labels.
#' @param lfcThres,pThres Legacy log2 fold-change and adjusted p-value thresholds.
#' @param condition Legacy metadata factor name.
#' @param annObj Legacy annotation data frame.
#' @param shrink Apply historical `ashr` shrinkage.
#' @param path Legacy output path used when writing shrunken annotated results.
#' @return Historical DESeq2 result object or annotated data frame.
#' @export
apply_contrasts <- function(dds, trt, ctrl, lfcThres = 0, pThres = 0.05,
                            condition = "condition", annObj = NULL, shrink = FALSE,
                            path = NULL) {
  .legacy_api_message("apply_contrasts", "deseq_contrast")
  .legacy_frozen_apply_contrasts(dds = dds, trt = trt, ctrl = ctrl, lfcThres = lfcThres,
                                 pThres = pThres, condition = condition, annObj = annObj,
                                 shrink = shrink, path = path)
}

#' @rdname read_edger_counts
#' @param counts.file Legacy CSV file argument.
#' @export
read_edgeR_counts <- function(counts.file) {
  .legacy_api_message("read_edgeR_counts", "read_edger_counts")
  read_edger_counts(file = counts.file)
}

#' @rdname plot_transcript_distribution
#' @param anno.col Legacy annotation-column argument.
#' @export
plot_transcript_dist <- function(data, anno.col) {
  .legacy_api_message("plot_transcript_dist", "plot_transcript_distribution")
  .legacy_frozen_plot_transcript_dist(data, anno.col)
}

#' @rdname calculate_local_mixing
#' @param seurat_obj Legacy Seurat object argument.
#' @export
calculate_mixing_metric <- function(seurat_obj, group_by = "sample_id",
                                    reduction = "umap.unintegrated", k = 50) {
  .legacy_api_message("calculate_mixing_metric", "calculate_local_mixing")
  calculate_local_mixing(object = seurat_obj, group_by = group_by, reduction = reduction, k = k)
}

#' @rdname screen_cluster_resolutions
#' @param seurat_obj Legacy Seurat object argument.
#' @param outdir Legacy figure output directory.
#' @export
cluster_resolution_clustering <- function(seurat_obj, outdir,
    resolutions = c(0.2, 0.4, 0.5, 0.6, 0.8, 1)) {
  .legacy_api_message("cluster_resolution_clustering", "screen_cluster_resolutions")
  .require_package("ggpubr")
  .require_package("clustree")
  .require_package("scCustomize")
  result <- screen_cluster_resolutions(object = seurat_obj, resolutions = resolutions)
  plots <- lapply(result$res_cols, function(column) {
    scCustomize::DimPlot_scCustom(result$seurat_obj_clusters, group.by = column,
                    reduction = "umap.harmony", label = TRUE, label.size = 4,
                    raster = TRUE) + ggplot2::ggtitle(column)
  })
  result$umap_plot <- ggpubr::ggarrange(plotlist = plots)
  result$clustree_plot <- clustree::clustree(result$seurat_obj_clusters)
  colors <- .legacy_object("colors", parent.frame())
  export_plot_dual(paste0(outdir, "UMAP_cluster_resolutions"), result$umap_plot, width = 15, height = 7)
  export_plot_dual(paste0(outdir, "clustree_results"),
    result$clustree_plot + ggplot2::scale_color_manual(values = unname(colors)),
    width = 10, height = 8)
  result
}

#' @rdname find_resolution_markers
#' @param seurat_obj Legacy Seurat object argument.
#' @export
cluster_resolution_markers <- function(seurat_obj, res_cols = NULL,
    remove_lncRNA = TRUE, min_pct = 0.1, logfc_threshold = 0.25) {
  .legacy_api_message("cluster_resolution_markers", "find_resolution_markers")
  find_resolution_markers(object = seurat_obj, res_cols = res_cols,
                          remove_lncRNA = remove_lncRNA,
                          min_pct = min_pct, logfc_threshold = logfc_threshold)
}

#' @rdname plot_sc_qc_ridge
#' @param seurat.obj Legacy Seurat object argument.
#' @param upper.xlim Legacy upper x-axis display limit.
#' @export
custom_RidgePlot <- function(seurat.obj, metric, upper.xlim = NULL) {
  .legacy_api_message("custom_RidgePlot", "plot_sc_qc_ridge")
  colors <- .legacy_object("colors", parent.frame())
  .legacy_frozen_custom_RidgePlot(seurat.obj, metric, upper.xlim, colors)
}

#' @rdname plot_marker_umap
#' @param seurat.obj Legacy Seurat object; when NULL, `integrated_seurat` is read from the calling environment.
#' @param group.by Legacy metadata grouping columns.
#' @param label.size Legacy cluster-label size.
#' @param feature.pt.size,cluster.pt.size Legacy feature and cluster point sizes.
#' @export
plot_markers_UMAP <- function(features, seurat.obj = NULL,
                              group.by = c("timepoint", "harmony_clusters"),
                              reduction = "umap.harmony", label.size = 4,
                              label = TRUE, alpha = 0.75, feature.pt.size = 0.01,
                              cluster.pt.size = NULL) {
  .legacy_api_message("plot_markers_UMAP", "plot_marker_umap")
  if (is.null(seurat.obj)) seurat.obj <- .legacy_object("integrated_seurat", parent.frame())
  .require_package("scCustomize")
  .require_package("ggpubr")
  blues9 <- .legacy_object("blues9", parent.frame())
  .legacy_frozen_plot_markers_UMAP(features, seurat.obj, group.by, reduction,
    label.size, label, alpha, feature.pt.size, cluster.pt.size, blues9)
}

#' @rdname plot_marker_summary
#' @param seurat.obj Legacy Seurat object; when NULL, `integrated_seurat` is read from the calling environment.
#' @export
plot_combined_markers <- function(features, seurat.obj = NULL, downsample = TRUE) {
  .legacy_api_message("plot_combined_markers", "plot_marker_summary")
  if (is.null(seurat.obj)) seurat.obj <- .legacy_object("integrated_seurat", parent.frame())
  .require_package("Seurat")
  .require_package("ggpubr")
  colors <- .legacy_object("colors", parent.frame())
  blues9 <- .legacy_object("blues9", parent.frame())
  .legacy_frozen_plot_combined_markers(features, seurat.obj, downsample, colors, blues9)
}

#' @rdname plot_tradeseq_patterns
#' @param plot_df Legacy tradeSeq plotting data frame.
#' @export
plot_pattern_clusters_tradeseq <- function(plot_df, collapse = FALSE,
                                            alpha = 0.7, colors, nrow = NULL,
                                            ncol = NULL) {
  .legacy_api_message("plot_pattern_clusters_tradeseq", "plot_tradeseq_patterns")
  plot_tradeseq_patterns(data = plot_df, colors = colors, collapse = collapse,
                         alpha = alpha, nrow = nrow, ncol = ncol)
}

#' @rdname enrichment_to_tables
#' @export
prepare_go_df_list <- function(go_results, go_results_simplified = NULL) {
  .legacy_api_message("prepare_go_df_list", "enrichment_to_tables")
  enrichment_to_tables(go_results = go_results,
                        go_results_simplified = go_results_simplified)
}

#' @rdname estimate_pseudotime_threshold
#' @param plot_type Legacy plot type, either `scatter` or `boxplot`.
#' @param timepoint_col Metadata column used for boxplot grouping.
#' @param exclude_timepoints Optional timepoints excluded from boxplots.
#' @param pt_color,fit_color,vline_color Legacy point, GAM-fit, and threshold-line colors.
#' @param pt_alpha,pt_size Legacy point opacity and size.
#' @param xlab,ylab Optional axis labels.
#' @export
plot_pseudotime <- function(data, pseudotime_col, score_col = NULL,
                            plot_type = c("scatter", "boxplot"),
                            lineage_assignment_col = "lineage_assignment",
                            timepoint_col = "timepoint", exclude_timepoints = NULL,
                            method = c("inflection", "drop", "maximum", "minimum"),
                            which_n = 1, pseudotime_range = c(0, 0.1), n_grid = 500,
                            pt_color = "#BDBDBD", fit_color = "#2166AC",
                            vline_color = "#D6604D", pt_alpha = 0.3, pt_size = 0.8,
                            xlab = NULL, ylab = NULL) {
  .legacy_api_message("plot_pseudotime", "estimate_pseudotime_threshold")
  if (is.null(score_col)) stop("score_col is required to compute the threshold.")
  threshold <- estimate_pseudotime_threshold(data = data,
      pseudotime_col = pseudotime_col, score_col = score_col,
      method = method, which_n = which_n, pseudotime_range = pseudotime_range,
      n_grid = n_grid, lineage_assignment_col = lineage_assignment_col)
  message("Threshold from ", score_col, " (", match.arg(method), "): ", round(threshold, 4))
  plot_pseudotime_score(data = data, pseudotime_col = pseudotime_col,
      score_col = score_col, threshold = threshold, plot_type = plot_type,
      lineage_assignment_col = lineage_assignment_col,
      timepoint_col = timepoint_col, exclude_timepoints = exclude_timepoints,
      pseudotime_range = pseudotime_range, pt_color = pt_color,
      fit_color = fit_color, vline_color = vline_color, pt_alpha = pt_alpha,
      pt_size = pt_size, xlab = xlab, ylab = ylab)
}

#' @rdname compare_expression_sets
#' @param dds Legacy DESeqDataSet providing counts and `cell` metadata.
#' @param results_object Legacy differential-expression result table.
#' @param pThres Legacy adjusted p-value threshold.
#' @param biotype_filter Optional legacy biotype filter.
#' @param output_dir Legacy directory for exported gene-set CSV files.
#' @param plot_title Optional legacy Venn-plot title.
#' @export
extract_and_plot_venn <- function(dds, results_object, pThres = 0.05,
                                  expression_threshold = 1, biotype_filter = NULL,
                                  output_dir = "Results/", plot_title = NULL) {
  .legacy_api_message("extract_and_plot_venn", "compare_expression_sets")
  .require_package("SummarizedExperiment")
  metadata <- as.data.frame(SummarizedExperiment::colData(dds))
  .check_columns(metadata, "cell")
  groups <- list(DSCs = rownames(metadata)[metadata$cell == "DSC"],
                 Melanocytes = rownames(metadata)[metadata$cell == "Melanocytes"])
  results <- as.data.frame(results_object)
  if (!"ENSEMBL" %in% names(results)) results$ENSEMBL <- rownames(results)
  biotype_col <- if ("GENETYPE_biomaRt" %in% names(results)) "GENETYPE_biomaRt" else "GENETYPE_AnnoDBI"
  sets <- compare_expression_sets(counts = SummarizedExperiment::assay(dds),
     results = results, groups = groups, p_threshold = pThres,
     expression_threshold = expression_threshold, biotypes = biotype_filter,
     biotype_col = biotype_col)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  suffix <- if (is.null(biotype_filter)) "" else paste0("_", paste(biotype_filter, collapse = "_"))
  names_out <- c("DSC_exclusive_transcripts", "Melanocytes_exclusive_transcripts",
                 "DSC_Melanocytes_overlap_transcripts")
  genes_out <- c(sets$exclusive, list(sets$shared))
  for (i in seq_along(genes_out)) {
    genes <- genes_out[[i]]
    with_comma <- if (length(genes)) paste0(genes, ",") else character()
    utils::write.csv(data.frame(Gene = genes, ENSEMBL_input = with_comma),
      file.path(output_dir, paste0(names_out[i], suffix, ".csv")), row.names = FALSE)
  }
  significant <- !is.na(results$padj) & results$padj < pThres
  if (!is.null(biotype_filter)) {
    significant <- significant & !is.na(results[[biotype_col]]) &
      results[[biotype_col]] %in% biotype_filter
  }
  n_diff <- sum(significant)
  n_venn <- sum(lengths(sets$exclusive)) + length(sets$shared)
  caption <- paste0(n_diff, " differentially expressed genes.\n",
    "Exclusive genes defined as average expression \u2264 ", expression_threshold,
    " counts in one cell type and > ", expression_threshold,
    " in the other.\n", n_diff - n_venn,
    " genes were excluded due to low expression in both.")
  plot_gene_overlap(sets$sets, title = plot_title, caption = caption)
}

#' @rdname plot_bulk_expression
#' @param vsd.obj Legacy variance-stabilized expression object.
#' @param results.object Legacy annotated differential-expression table.
#' @param merge.plots Legacy switch between faceted and merged plotting.
#' @param p.size Legacy significance-label size.
#' @param sig.anno Legacy significance annotation mode (`stars` or `padj`).
#' @export
plot_control_expression_comparison <- function(vsd.obj, results.object,
    genes, nrow = NULL, ncol = NULL, factorize = FALSE,
    merge.plots = FALSE, p.size = 5, sig.anno = c("stars", "padj")) {
  .legacy_api_message("plot_control_expression_comparison", "plot_bulk_expression")
  .legacy_frozen_plot_control_expression_comparison(vsd.obj, results.object,
    genes, nrow, ncol, factorize, merge.plots, p.size, sig.anno)
}

#' @rdname plot_de_heatmap
#' @param dds Legacy DESeqDataSet retained for interface compatibility.
#' @param vsd Legacy variance-stabilized expression object.
#' @param results_object Legacy annotated differential-expression table.
#' @param biotype_filter Optional legacy biotype filter.
#' @export
plot_biotype_heatmap <- function(dds, vsd, results_object,
    biotype_filter = NULL, heatmap_fontsize = 18,
    show_row_dend = FALSE, show_row_names = FALSE,
    plot_title = NULL, ...) {
  .legacy_api_message("plot_biotype_heatmap", "plot_de_heatmap")
  .legacy_frozen_plot_biotype_heatmap(dds, vsd, results_object,
    biotype_filter, heatmap_fontsize, show_row_dend, show_row_names, plot_title, ...)
}

#' @rdname plot_enrichment_bubble
#' @param df Legacy enrichment data frame.
#' @param name_col Legacy term-name column.
#' @param pval_col Legacy p-value/FDR column.
#' @param color_scale Legacy low/high color vector.
#' @export
bubble_plot_clusterprofiler_style <- function(df, name_col,
    score_col = "GeneRatio", pval_col = "p.adjust", genes_col = "geneID",
    top_n = 20, color_scale = c("lightgrey", "#4292C6"),
    size_range = c(3, 10), name_label = "Terms", rotate_x = FALSE) {
  .legacy_api_message("bubble_plot_clusterprofiler_style", "plot_enrichment_bubble")
  .legacy_frozen_bubble_plot_clusterprofiler_style(df, name_col, score_col,
    pval_col, genes_col, top_n, color_scale, size_range, name_label, rotate_x)
}

#' @rdname plot_enrichment_pair
#' @param enrich_res Legacy enrichment-result list.
#' @export
plot_enrich_dotpair <- function(enrich_res, type = c("GO", "KEGG"),
                                show_category = 15, title_prefix = NULL,
                                font_size = 12) {
  .legacy_api_message("plot_enrich_dotpair", "plot_enrichment_pair")
  plot_enrichment_pair(enriched = enrich_res, type = type,
                       show_category = show_category, title_prefix = title_prefix,
                       font_size = font_size)
}

#' Plot a score as violins ordered by group median
#'
#' Orders metadata groups by the median of a selected score and draws the
#' historical scCustomize violin grammar used by UVDHDS analyses.
#' @param seurat_obj Seurat object containing the score and grouping metadata.
#' @param score_name Metadata score or feature to plot.
#' @param group_by Metadata column defining groups.
#' @param pt.size Point size passed to `scCustomize::VlnPlot_scCustom()`.
#' @param alpha Point opacity passed to `scCustomize::VlnPlot_scCustom()`.
#' @param font_size Base font size for the classic theme.
#' @param rotate_x Logical; rotate x-axis labels by 45 degrees.
#' @param cluster_colors Optional named vector of group fill colors.
#' @param ... Additional arguments passed to `scCustomize::VlnPlot_scCustom()`.
#' @return A ggplot object with groups ordered by median score.
#' @export
plot_ordered_violin <- function(seurat_obj, score_name = "DifferentiationScore_UCell",
    group_by = "harmony_clusters", pt.size = 0.1, alpha = 0.05,
    font_size = 14, rotate_x = FALSE, cluster_colors = NULL, ...) {
  .require_package("scCustomize")
  .check_columns(seurat_obj[[]], c(score_name, group_by))
  scores <- seurat_obj[[]]
  medians <- tapply(scores[[score_name]], scores[[group_by]], stats::median, na.rm = TRUE)
  order <- names(sort(medians))
  seurat_obj@meta.data[[group_by]] <- factor(scores[[group_by]], levels = order)
  plot <- scCustomize::VlnPlot_scCustom(seurat_obj, features = score_name,
                          group.by = group_by, pt.size = pt.size, alpha = alpha, ...) +
    ggplot2::theme_classic(base_size = font_size) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = if (rotate_x) 45 else 0,
                                                        hjust = if (rotate_x) 1 else 0.5),
                    legend.position = "none")
  if (!is.null(cluster_colors)) plot <- plot + ggplot2::scale_fill_manual(values = cluster_colors[order])
  plot
}

#' @rdname analyze_cluster_markers
#' @param seurat_obj Legacy Seurat object used for the optional marker heatmap.
#' @param colors Legacy cluster palette.
#' @param plot_heatmap Logical; include the legacy marker heatmap.
#' @param simplify_cutoff Legacy GO simplification cutoff.
#' @param showCategory Number of GO categories displayed by the legacy plot.
#' @export
cluster_marker_go_analysis <- function(seurat_obj, markers,
    colors = scCustomize::DiscretePalette_scCustomize(num_colors = 36, palette = "polychrome"),
    n = 10, remove_pseudogenes = TRUE,
    plot_heatmap = TRUE, simplify_cutoff = 0.5, showCategory = 10) {
  .legacy_api_message("cluster_marker_go_analysis", "analyze_cluster_markers")
  .require_package("Seurat")
  .require_package("clusterProfiler")
  .require_package("org.Hs.eg.db")
  .require_package("enrichplot")
  .check_columns(markers, c("gene", "cluster", "avg_log2FC", "pct.1", "pct.2"))
  if (remove_pseudogenes) markers <- markers[!grepl("AC[0-9]{3,}|AL[0-9]{3,}|AP[0-9]{3,}|AS[0-9]{3,}", markers$gene), , drop = FALSE]
  markers$score <- (markers$avg_log2FC * markers$pct.1) / markers$pct.2
  # Historical function selects the first n entries as supplied, not the top n by score.
  top <- do.call(rbind, lapply(split(markers, markers$cluster), utils::head, n = n))
  heatmap <- if (plot_heatmap) Seurat::DoHeatmap(seurat_obj, features = unique(top$gene),
      group.colors = colors) + Seurat::NoLegend() +
      ggplot2::theme(plot.margin = grid::unit(c(1, 4, 1, 1), "cm")) else NULL
  by_cluster <- split(markers$gene, markers$cluster)
  go <- lapply(by_cluster, function(genes) {
    entrez <- suppressMessages(clusterProfiler::bitr(unique(genes), fromType = "SYMBOL",
      toType = "ENTREZID", OrgDb = org.Hs.eg.db::org.Hs.eg.db))$ENTREZID
    if (!length(entrez)) return(NULL)
    suppressMessages(clusterProfiler::enrichGO(gene = entrez, OrgDb = org.Hs.eg.db::org.Hs.eg.db,
      ont = "BP", keyType = "ENTREZID", pAdjustMethod = "BH", readable = TRUE))
  })
  simplified <- lapply(go, function(x) if (is.null(x)) NULL else
    suppressMessages(clusterProfiler::simplify(x, cutoff = simplify_cutoff,
                                                by = "p.adjust", select_fun = min)))
  plots <- lapply(names(simplified), function(label) {
    x <- simplified[[label]]
    if (is.null(x)) return(NULL)
    enrichplot::dotplot(x, showCategory = showCategory,
                        title = paste("Cluster", label, "GO enrichment")) +
      enrichplot::set_enrichplot_color(type = "fill", colors = c("#4292C6", "lightgrey"))
  })
  names(plots) <- names(simplified)
  list(heatmap = heatmap, top_markers = top, go_results = go,
       go_results_simplified = simplified, go_plotlist = plots)
}
