#' Plot a transcript annotation distribution
#' @param data Data frame containing gene annotations.
#' @param annotation Column holding transcript classes.
#' @param ylab Optional descriptive y-axis label.
#' @param colors Optional palette, recycled over classes.
#' @return ggplot bar chart.
#' @export
plot_transcript_distribution <- function(data, annotation, ylab = "Number of transcripts", colors = NULL) {
  .require_package("ggplot2")
  .check_columns(data, annotation)
  counts <- as.data.frame(table(data[[annotation]]))
  counts$Var1 <- stats::reorder(counts$Var1, counts$Freq, decreasing = TRUE)
  if (is.null(colors)) colors <- rev(grDevices::colorRampPalette(
    c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B"))(
      length(unique(data[[annotation]]))))
  ggplot2::ggplot(counts, ggplot2::aes(x = .data$Var1, y = .data$Freq,
                                      fill = .data$Var1)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(),
                      color = "black", alpha = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = .data$Freq), nudge_y = 200) +
    ggplot2::ylab(ylab) + ggplot2::scale_fill_manual(values = colors) +
    ggplot2::theme_bw(base_size = 13) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
                   axis.title.x = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank()) +
    ggplot2::ggtitle(paste0(annotation, " annotation"))
}

#' Plot normalized bulk expression by a metadata group
#' @param transformed DESeqTransform or normalized expression matrix.
#' @param genes Gene identifiers present in the transformed assay.
#' @param metadata Sample metadata, row names aligned with assay columns.
#' @param group_col Metadata grouping column.
#' @param facet Show genes in separate panels.
#' @param colors Group colours.
#' @param nrow,ncol Optional facet dimensions.
#' @param p_size Size of optional significance labels.
#' @param significance Optional table with `gene` and `label` columns.
#' @param factorize Preserve the supplied gene order.
#' @return ggplot expression plot.
#' @export
plot_bulk_expression <- function(transformed, genes, metadata, group_col,
                                 facet = TRUE, colors = NULL, nrow = NULL,
                                 ncol = NULL, p_size = 5,
                                 significance = NULL, factorize = FALSE) {
  .require_package("ggplot2")
  .check_columns(metadata, group_col)
  values <- if (is.matrix(transformed)) transformed else {
    .require_package("SummarizedExperiment")
    SummarizedExperiment::assay(transformed)
  }
  if (!all(genes %in% rownames(values))) stop("Missing expression genes: ", paste(setdiff(genes, rownames(values)), collapse = ", "))
  if (!all(colnames(values) %in% rownames(metadata))) stop("Metadata row names must contain all samples.")
  frame <- do.call(rbind, lapply(genes, function(gene) data.frame(
    gene = gene, sample = colnames(values), expression = as.numeric(values[gene, ]),
    group = as.character(metadata[colnames(values), group_col]), stringsAsFactors = FALSE)))
  frame$group <- factor(frame$group, levels = unique(frame$group))
  if (factorize) frame$gene <- factor(frame$gene, levels = genes)
  if (is.null(colors)) {
    .require_package("ggsci")
    colors <- ggsci::pal_npg("nrc")(length(levels(frame$group)))
  }
  if (facet) {
    p <- ggplot2::ggplot(frame, ggplot2::aes(x = .data$group,
      y = .data$expression, fill = .data$group)) +
      ggplot2::geom_boxplot(alpha = 0.6, outlier.shape = NA,
                            width = 0.5, color = "black") +
      ggplot2::geom_jitter(width = 0.15, size = 1.5, shape = 21,
                           stroke = 0.3, alpha = 0.8, color = "black") +
      ggplot2::facet_wrap(~gene, scales = "free_y", nrow = nrow, ncol = ncol)
  } else {
    p <- ggplot2::ggplot(frame, ggplot2::aes(x = .data$gene,
      y = .data$expression, fill = .data$group)) +
      ggplot2::geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.5,
        color = "black", position = ggplot2::position_dodge(width = 0.7)) +
      ggplot2::geom_jitter(shape = 21, stroke = 0.3, alpha = 0.8,
        color = "black", position = ggplot2::position_dodge(width = 0.7)) +
      ggplot2::geom_vline(xintercept = seq(1.5, length(unique(frame$gene)) - 0.5,
        by = 1), linetype = "dashed", color = "gray40")
  }
  if (!is.null(significance)) {
    .check_columns(significance, c("gene", "label"))
    p <- p + ggplot2::geom_text(data = significance,
      ggplot2::aes(x = if (facet) length(levels(frame$group)) else .data$gene,
        y = Inf, label = .data$label), inherit.aes = FALSE, vjust = 1.2,
      size = p_size)
  }
  p <- p + ggplot2::scale_fill_manual(values = colors) +
    ggplot2::ylab("Normalized expression (VSD)") +
    ggplot2::theme_classic(base_size = 13) +
    ggplot2::theme(axis.line = ggplot2::element_line(size = 0.6, color = "black"),
      axis.text = ggplot2::element_text(color = "black"),
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom", legend.title = ggplot2::element_blank(),
      legend.key.size = grid::unit(0.6, "cm"),
      strip.text = ggplot2::element_text(face = "bold", size = 10),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.5),
      plot.margin = grid::unit(c(0.4, 0.4, 0.4, 0.4), "cm"))
  p
}

#' Heatmap of differential genes
#' @param expression Numeric gene-by-sample matrix or SummarizedExperiment.
#' @param genes Row names to plot.
#' @param annotation Optional sample metadata with sample names as row names.
#' @param annotation_columns Optional sample metadata column names.
#' @param colors Continuous color palette.
#' @param cluster_rows,cluster_columns Heatmap clustering switches.
#' @param annotation_colors Named list of annotation palettes.
#' @param gene_labels Named vector mapping gene IDs to plot labels.
#' @param plot_title,heatmap_fontsize Title and annotation font size.
#' @param show_row_dend,show_row_names,show_column_names Visibility switches.
#' @param use_raster Rasterize the heatmap body.
#' @param clustering_method_row,clustering_method_columns Clustering linkage methods.
#' @param clustering_distance_row,clustering_distance_column Clustering distances.
#' @param ... Additional ComplexHeatmap arguments.
#' @return ComplexHeatmap object; call `ComplexHeatmap::draw()` to render.
#' @export
plot_de_heatmap <- function(expression, genes, annotation = NULL,
                            annotation_columns = NULL,
                            colors = NULL, cluster_rows = TRUE, cluster_columns = TRUE,
                            annotation_colors = NULL, gene_labels = NULL,
                            plot_title = NULL, heatmap_fontsize = 18,
                            show_row_dend = FALSE, show_row_names = FALSE,
                            show_column_names = FALSE, use_raster = TRUE,
                            clustering_method_row = "average",
                            clustering_method_columns = "average",
                            clustering_distance_row = "pearson",
                            clustering_distance_column = "euclidean", ...) {
  .require_package("ComplexHeatmap")
  values <- if (is.matrix(expression)) expression else {
    .require_package("SummarizedExperiment")
    SummarizedExperiment::assay(expression)
  }
  if (!all(genes %in% rownames(values))) stop("All genes must be present in the expression matrix.")
  values <- values[genes, , drop = FALSE]
  scaled <- t(scale(t(values)))
  scaled <- scaled[stats::complete.cases(scaled) &
                     rowSums(is.infinite(scaled)) == 0, , drop = FALSE]
  if (!nrow(scaled)) stop("No finite scaled expression rows remain.")
  if (!is.null(gene_labels)) {
    if (is.null(names(gene_labels))) stop("gene_labels must be named by gene ID.")
    labels <- gene_labels[rownames(scaled)]
    rownames(scaled)[!is.na(labels) & nzchar(labels)] <- labels[!is.na(labels) & nzchar(labels)]
  }
  top <- NULL
  if (!is.null(annotation_columns)) {
    .check_columns(annotation, annotation_columns)
    if (!all(colnames(values) %in% rownames(annotation))) stop("Sample metadata must match expression columns.")
    top <- ComplexHeatmap::HeatmapAnnotation(
      df = annotation[colnames(values), annotation_columns, drop = FALSE],
      col = annotation_colors,
      annotation_name_gp = grid::gpar(fontsize = heatmap_fontsize, fontface = "bold"))
  }
  .require_package("circlize")
  if (is.null(colors)) {
    .require_package("RColorBrewer")
    palette <- grDevices::colorRampPalette(
      rev(RColorBrewer::brewer.pal(7, "RdYlBu")))(100)
    colors <- palette[c(1, 51, 100)]
  }
  color_function <- circlize::colorRamp2(c(-2, 0, 2), colors)
  if (is.null(plot_title)) plot_title <- paste0("Differentially Expressed Genes (", length(genes), ")")
  ComplexHeatmap::Heatmap(scaled, col = color_function, top_annotation = top,
    cluster_rows = cluster_rows, cluster_columns = cluster_columns,
    clustering_method_row = clustering_method_row,
    clustering_method_columns = clustering_method_columns,
    clustering_distance_row = clustering_distance_row,
    clustering_distance_column = clustering_distance_column,
    show_row_dend = show_row_dend, show_row_names = show_row_names,
    show_column_names = show_column_names, use_raster = use_raster,
    column_names_gp = grid::gpar(fontsize = 10), column_title = plot_title,
    heatmap_legend_param = list(title = "row Z-score", at = seq(-2, 2, by = 1),
      color_bar = "continuous", title_position = "topcenter",
      legend_direction = "horizontal", legend_width = grid::unit(4, "cm")), ...)
}

#' Plot principal components of transformed bulk RNA-seq samples
#' @param transformed DESeqTransform object.
#' @param groups One or two metadata grouping variables.
#' @param components Two principal component indices.
#' @param colors Optional colors for the first grouping variable.
#' @param point_size Point size.
#' @param title,subtitle Plot headings.
#' @param text_size Base theme font size.
#' @param shapes Optional group shape palette.
#' @param labelled Label PCA samples by name.
#' @return ggplot PCA object.
#' @export
plot_bulk_pca <- function(transformed, groups, components = c(1, 2),
                          colors = NULL, point_size = 3, title = "",
                          subtitle = "", text_size = 12, shapes = NULL,
                          labelled = FALSE) {
  .require_package("DESeq2")
  .require_package("ggplot2")
  if (length(components) != 2L || !length(groups) || length(groups) > 2L) {
    stop("Provide one or two groups and exactly two components.")
  }
  frame <- DESeq2::plotPCA(transformed, intgroup = groups,
                           pcsToUse = components, returnData = TRUE)
  variance <- round(100 * attr(frame, "percentVar"))
  x <- paste0("PC", components[1L]); y <- paste0("PC", components[2L])
  mapping <- ggplot2::aes(x = .data[[x]], y = .data[[y]], color = .data[[groups[1L]]])
  if (length(groups) == 2L) {
    mapping <- ggplot2::aes(x = .data[[x]], y = .data[[y]],
                            color = .data[[groups[1L]]], shape = .data[[groups[2L]]])
  }
  if (is.null(colors)) {
    .require_package("ggsci")
    colors <- c(ggsci::pal_npg("nrc")(10), ggsci::pal_jco()(10))
  }
  plot <- ggplot2::ggplot(frame, mapping) + ggplot2::geom_point(size = point_size) +
    ggplot2::labs(x = sprintf("%s: %s%% variance", x, variance[1L]),
                  y = sprintf("%s: %s%% variance", y, variance[2L]),
                  title = title, subtitle = subtitle) + ggplot2::theme_bw(base_size = text_size) +
    ggplot2::scale_color_manual(values = colors)
  if (!is.null(shapes)) plot <- plot + ggplot2::scale_shape_manual(values = shapes)
  if (labelled) plot <- plot + ggplot2::geom_label(ggplot2::aes(label = .data$name))
  plot
}

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

