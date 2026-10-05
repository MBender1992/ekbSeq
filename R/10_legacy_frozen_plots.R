# Historical plotting grammars from the supplied UVDHDS project helpers.
# These functions are internal; the exported compatibility entry points live in
# 11_compatibility_wrappers.R.

.legacy_frozen_plot_markers_UMAP <- function(features, seurat.obj, group.by,
    reduction, label.size, label, alpha, feature.pt.size, cluster.pt.size, blues9) {
  p_clusters <- scCustomize::DimPlot_scCustom(
    seurat.obj, group.by = group.by, reduction = reduction,
    combine = TRUE, label = label, label.size = label.size, raster = TRUE,
    num_columns = 1, pt.size = cluster.pt.size)
  p_features <- scCustomize::FeaturePlot_scCustom(
    seurat.obj, features = features, colors_use = blues9, label = FALSE,
    reduction = reduction, raster = TRUE, alpha_exp = alpha,
    raster.dpi = c(3000, 3000), pt.size = feature.pt.size)
  ggpubr::ggarrange(p_clusters, p_features, align = "hv", widths = c(0.4, 0.6))
}

.legacy_frozen_plot_combined_markers <- function(features, seurat.obj, downsample,
                                                  colors, blues9) {
  names(colors) <- NULL
  blue_gradient <- grDevices::colorRampPalette(blues9)(100)
  if (downsample) {
    set.seed(2534)
    cells.use <- sample(colnames(seurat.obj), 5000)
  } else {
    cells.use <- NULL
  }
  p1 <- Seurat::RidgePlot(seurat.obj, features = features, group.by = "timepoint", ncol = 6) &
    ggplot2::ylab("") & ggplot2::scale_fill_manual(values = colors)
  p2 <- Seurat::DotPlot(seurat.obj, features = features, group.by = "timepoint") +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.box.background = ggplot2::element_rect(fill = "white", color = NA),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)) +
    ggplot2::ylab("") + ggplot2::scale_color_distiller(palette = "Blues", direction = 1)
  p3 <- Seurat::DoHeatmap(seurat.obj, features = features, group.by = "timepoint",
                         group.colors = colors, slot = "data", raster = TRUE,
                         cells = cells.use) +
    ggplot2::scale_fill_gradientn(colors = blue_gradient, na.value = "grey90") +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10))
  ggpubr::ggarrange(p1, ggpubr::ggarrange(p2, p3, widths = c(0.3, 0.7)),
                    nrow = 2, heights = c(0.3, 0.7))
}

.legacy_frozen_plot_biotype_heatmap <- function(dds, vsd, results_object,
    biotype_filter, heatmap_fontsize, show_row_dend, show_row_names, plot_title, ...) {
  .require_package("SummarizedExperiment")
  .require_package("ComplexHeatmap")
  .require_package("RColorBrewer")
  .require_package("circlize")
  .require_package("ggsci")
  if (!is.null(biotype_filter)) {
    if ("GENETYPE_biomaRt" %in% colnames(results_object)) {
      gene_types <- results_object$GENETYPE_biomaRt
    } else if ("GENETYPE_AnnoDBI" %in% colnames(results_object)) {
      gene_types <- results_object$GENETYPE_AnnoDBI
    } else {
      stop("Biotype filter requested, but no GENETYPE_biomaRt or GENETYPE_AnnoDBI column found.")
    }
    valid_genes <- results_object$ENSEMBL[!is.na(gene_types) & gene_types %in% biotype_filter]
    sig_genes <- intersect(results_object$ENSEMBL, valid_genes)
  } else {
    sig_genes <- results_object$ENSEMBL
  }
  if (!length(sig_genes)) stop("No genes found for specified biotype filter.")
  mat <- SummarizedExperiment::assay(vsd)[sig_genes, , drop = FALSE]
  datScaled <- t(scale(t(mat)))
  datScaled <- datScaled[stats::complete.cases(datScaled) &
                           rowSums(is.infinite(datScaled)) == 0, , drop = FALSE]
  anno_df <- as.data.frame(SummarizedExperiment::colData(vsd)[, c("cell", "donor")])
  colnames(anno_df) <- c("Cell_type", "Donor")
  donors <- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(7, "Set1")))(
    length(unique(anno_df$Donor)))
  names(donors) <- unique(anno_df$Donor)
  annColors <- ComplexHeatmap::HeatmapAnnotation(
    df = anno_df,
    col = list(Cell_type = c(DSC = ggsci::pal_npg("nrc")(4)[1],
                             Melanocytes = ggsci::pal_npg("nrc")(4)[2]),
               Donor = donors),
    annotation_legend_param = list(
      Cell_type = list(nrow = 1), Time = list(nrow = 1),
      title_gp = grid::gpar(fontsize = heatmap_fontsize),
      labels_gp = grid::gpar(fontsize = heatmap_fontsize)),
    annotation_name_gp = grid::gpar(fontsize = heatmap_fontsize, fontface = "bold"))
  colors <- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(7, "RdYlBu")))(100)
  col_fun <- circlize::colorRamp2(c(-2, 0, 2), c(colors[1], colors[51], colors[100]))
  gene_count <- length(sig_genes)
  title_text <- if (is.null(plot_title))
    paste0("Differentially Expressed Genes (", gene_count, ")") else plot_title
  if ("SYMBOL" %in% colnames(results_object)) {
    ensembl_to_symbol <- results_object$SYMBOL
    names(ensembl_to_symbol) <- results_object$ENSEMBL
    common_genes <- intersect(rownames(datScaled), names(ensembl_to_symbol))
    rownames(datScaled)[rownames(datScaled) %in% common_genes] <-
      ensembl_to_symbol[common_genes]
  }
  ComplexHeatmap::Heatmap(datScaled, col = col_fun, top_annotation = annColors,
    clustering_method_row = "average", clustering_method_columns = "average",
    clustering_distance_row = "pearson", clustering_distance_column = "euclidean",
    show_row_dend = show_row_dend, show_row_names = show_row_names,
    show_column_names = FALSE, use_raster = TRUE,
    column_names_gp = grid::gpar(fontsize = 10), column_title = title_text,
    ...,
    heatmap_legend_param = list(title = "row Z-score", at = seq(-2, 2, by = 1),
      color_bar = "continuous", title_position = "topcenter",
      legend_direction = "horizontal", legend_width = grid::unit(4, "cm")))
}

.legacy_frozen_plot_transcript_dist <- function(data, anno.col) {
  n_cols <- length(unique(data[[anno.col]]))
  frequency <- as.data.frame(table(data[[anno.col]]))
  frequency$Var1 <- stats::reorder(frequency$Var1, frequency$Freq, decreasing = TRUE)
  ggplot2::ggplot(frequency, ggplot2::aes(x = .data$Var1, y = .data$Freq,
                                         fill = .data$Var1)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(),
                      color = "black", alpha = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = .data$Freq), nudge_y = 200) +
    ggplot2::ylab("Number of significantly altered transcripts \n between Melanocytes and DSCs") +
    ggplot2::scale_fill_manual(values = rev(grDevices::colorRampPalette(
      c("#F7FBFF", "#C6DBEF", "#6BAED6", "#2171B5", "#08306B"))(n_cols))) +
    ggplot2::theme_bw(base_size = 13) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
                   axis.title.x = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank()) +
    ggplot2::ggtitle(paste0(anno.col, " annotation"))
}

.legacy_frozen_custom_RidgePlot <- function(seurat.obj, metric, upper.xlim,
                                             colors) {
  .require_package("ggprism")
  metadata <- seurat.obj@meta.data
  p <- Seurat::RidgePlot(seurat.obj, metric, cols = colors) +
    ggplot2::ggtitle(paste0(metric, " per cell")) +
    list(ggprism::theme_prism(),
         ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                          vjust = 1, hjust = 1),
                        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")),
         ggplot2::guides(
           x = ggprism::guide_prism_offset(),
           y = ggprism::guide_prism_offset()
         )) +
    ggplot2::theme(legend.position = "none", axis.title.y = ggplot2::element_blank()) +
    ggplot2::geom_vline(xintercept = stats::median(metadata[[metric]]),
                        size = 0.9, lty = 1, color = "darkred")
  if (!is.null(upper.xlim)) p + ggplot2::xlim(0, upper.xlim) else p
}


.legacy_frozen_plot_control_expression_comparison <- function(
    vsd.obj,
    results.object,
    genes,
    nrow = NULL,
    ncol = NULL,
    factorize = FALSE,
    merge.plots = FALSE,
    p.size = 5,
    sig.anno = c("stars", "padj")
) {

  sig.anno <- match.arg(sig.anno)

  ## Extract expression matrix
  vsd_counts <- SummarizedExperiment::assay(vsd.obj)
  metadata <- SummarizedExperiment::colData(vsd.obj)

  ## Map ENSEMBL IDs to SYMBOLs
  ensembl_to_symbol <- results.object %>%
    dplyr::filter(SYMBOL %in% genes) %>%
    dplyr::select(ENSEMBL, SYMBOL) %>%
    dplyr::distinct()

  if(any(colnames(vsd_counts) != metadata$sample) & all(toupper(colnames(vsd_counts)) == toupper(metadata$ID))){
    colnames(vsd_counts) <- metadata$sample
  } else if(any(colnames(vsd_counts) != metadata$sample) & any(toupper(colnames(vsd_counts)) != toupper(metadata$ID))){
    stop("The column names of the vsd count matrix are not identical to the metadata IDs.")
  }

  ## Extract relevant VSD rows
  gene_rows <- which(rownames(vsd_counts) %in% ensembl_to_symbol$ENSEMBL)
  df_expr <- t(vsd_counts[gene_rows, ]) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("sample")

  ## Rename columns with SYMBOL
  colnames(df_expr)[-1] <- ensembl_to_symbol$SYMBOL[match(colnames(df_expr)[-1], ensembl_to_symbol$ENSEMBL)]

  ## Long format
  df_long <- df_expr %>%
    tidyr::pivot_longer(cols = -sample, names_to = "gene", values_to = "vsd") %>%
    dplyr::left_join(as.data.frame(metadata), by = "sample")

 sig_annotations <- results.object %>%
    dplyr::filter(SYMBOL %in% genes) %>%
    dplyr::mutate(
      stars = dplyr::case_when(
        is.na(padj)  ~ "",
        padj < 0.001 ~ "***",
        padj < 0.01  ~ "**",
        padj < 0.05  ~ "*",
        TRUE         ~ ""
      ),
      label = dplyr::case_when(
        sig.anno == "stars" ~ stars,
        sig.anno == "padj"  ~ paste0("padj = ", formatC(padj, format = "E", digits = 1))
      )
    ) %>%
    dplyr::select(SYMBOL, label) %>%
    dplyr::distinct()

  df_long <- df_long %>%
    dplyr::left_join(sig_annotations, by = c("gene" = "SYMBOL"))

  if(factorize == TRUE){
    df_long <-  df_long %>%
      dplyr::mutate(gene = factor(gene, levels = genes))
  }

  ## Universal ggplot components
  fill_vals <- c("DSC" = "#CD534CFF", "Melanocytes" = "#0073C2FF")
  text_anno <- geom_text(
    data = dplyr::distinct(df_long, gene, label),
    aes(label = label, x = if(merge.plots) gene else 2, y = Inf),
    vjust = 1.2, inherit.aes = FALSE, size = p.size
  )

  ## Construct plot depending on 'merge'
  if (!merge.plots) {
    p <- ggplot(df_long, aes(x = cell, y = vsd, fill = cell)) +
      geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.5, color = "black") +
      geom_jitter(width = 0.15, size = 1.5, shape = 21, stroke = 0.3, alpha = 0.8, color = "black") +
      facet_wrap(~ gene, scales = "free_y", nrow = nrow, ncol = ncol)
  } else {
    p <- ggplot(df_long, aes(x = gene, y = vsd, fill = cell)) +
      geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.5, color = "black", position = position_dodge(width = 0.7)) +
      geom_jitter(shape = 21, stroke = 0.3, alpha = 0.8, color = "black",
                  position = position_dodge(width = 0.7)) +
      geom_vline(xintercept = seq(1.5, length(unique(df_long$gene)) - 0.5, by = 1), linetype = "dashed", color = "gray40") +
      xlab("Gene")
  }

  ## Add universal ggplot components
  p <- p +
    text_anno +
    scale_fill_manual(values = fill_vals) +
    ylab("Normalized expression (VSD)") +
    theme_classic(base_size = 13) +
    theme(
      axis.line = element_line(size = 0.6, color = "black"),
      axis.text = element_text(color = "black"),
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key.size = grid::unit(0.6, "cm"),
      strip.text = element_text(face = "bold", size = 10),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      plot.margin = grid::unit(c(0.4, 0.4, 0.4, 0.4), "cm")
    )

  return(p)
}


.legacy_frozen_bubble_plot_clusterprofiler_style <- function(
    df,
    name_col,
    score_col = "GeneRatio",
    pval_col = "p.adjust",
    genes_col = "geneID",
    top_n = 20,
    color_scale = c("lightgrey", "#4292C6"),
    size_range = c(3, 10),
    name_label = "Terms",
    rotate_x = FALSE
) {
  # require packages (do not attach; assume user has them)
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required but not installed")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("dplyr required but not installed")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("rlang required but not installed")

  stopifnot(is.data.frame(df))
  stopifnot(all(c(name_col, score_col, pval_col, genes_col) %in% colnames(df)))

  # helper to parse a possible ratio string like "5/200" -> numeric 5/200
  parse_ratio_safe <- function(x) {
    # if already numeric, return as.numeric
    if (is.numeric(x)) return(as.numeric(x))
    x_chr <- as.character(x)
    # if contains "/", compute numerator/denominator
    if (any(grepl("/", x_chr, fixed = TRUE), na.rm = TRUE)) {
      sapply(x_chr, function(xx) {
        if (is.na(xx) || xx == "") return(NA_real_)
        if (!grepl("/", xx, fixed = TRUE)) {
          # fallback to numeric coercion
          out <- suppressWarnings(as.numeric(xx))
          if (is.na(out)) return(NA_real_) else return(out)
        }
        parts <- strsplit(xx, "/", fixed = TRUE)[[1]]
        num <- suppressWarnings(as.numeric(parts[1]))
        den <- suppressWarnings(as.numeric(parts[2]))
        if (is.na(num) || is.na(den) || den == 0) return(NA_real_)
        num / den
      }, USE.NAMES = FALSE)
    } else {
      # try numeric coercion
      out <- suppressWarnings(as.numeric(x_chr))
      out
    }
  }

  plot_df <- df %>%
    dplyr::mutate(
      # compute gene counts from genes_col (split on "/" or ",")
      n_genes = ifelse(
        is.na(.data[[genes_col]]),
        NA_integer_,
        sapply(strsplit(as.character(.data[[genes_col]]), "/|,"), function(v) length(v))
      ),
      # parse score column: accept ratios like "5/200" or numeric values
      score = parse_ratio_safe(.data[[score_col]]),
      pval = as.numeric(.data[[pval_col]]),
      name = as.character(.data[[name_col]])
    ) %>%
    dplyr::filter(!is.na(score), !is.na(pval))

  if (nrow(plot_df) == 0) {
    stop("No rows with non-missing score and p-value after coercion.")
  }

  # Sort and select top_n by p-value ascending
  plot_df <- plot_df %>%
    dplyr::arrange(pval) %>%
    dplyr::slice(seq_len(min(top_n, nrow(.))))

  # If no gene counts computed (all NA), try to compute from gene column again more robustly:
  if (all(is.na(plot_df$n_genes))) {
    plot_df$n_genes <- sapply(strsplit(as.character(plot_df[[genes_col]]), "/|,"), function(v) if (length(v) == 1 && v == "") 0L else length(v))
  }

  # Check if all scores are negative (flip axis if true) - kept for consistency with clusterProfiler-style
  all_negative <- all(plot_df$score < 0, na.rm = TRUE)

  # Reorder y-axis so items are ordered by score (lowest at top). This mirrors clusterProfiler visuals.
  plot_df$name <- factor(plot_df$name, levels = plot_df$name[order(plot_df$score, decreasing = FALSE)])

  if (all_negative) {
    # if all negative, reverse so most negative (smallest) is at top
    plot_df$name <- factor(plot_df$name, levels = plot_df$name[order(plot_df$score, decreasing = TRUE)])
  }

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$score, y = .data$name, size = .data$n_genes, color = .data$pval)) +
    ggplot2::geom_point(alpha = 0.8) +
    ggplot2::scale_size_continuous(range = size_range, name = "Gene count") +
    # reverse transform so that smaller p-values use the "high" colour
    ggplot2::scale_color_gradient(low = color_scale[1], high = color_scale[2], name = pval_col, trans = "reverse") +
    ggplot2::labs(
      x = score_col,
      y = "",
      title = paste("Top", min(top_n, nrow(plot_df)), name_label)
    ) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9))

  if (all_negative) {
    p <- p + ggplot2::scale_x_reverse()
  }
  if (rotate_x) {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1))
  }

  return(p)
}

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
#' @noRd

.legacy_frozen_apply_contrasts <- function(dds, trt, ctrl, lfcThres = 0, pThres = 0.05, condition = "condition", annObj = NULL, shrink = FALSE, path = NULL) {
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

#' Function to define complex contrasts in DESeq results function.
#'
#' Function is taken from https://www.atakanekiz.com/technical/a-guide-to-designs-and-contrasts-in-DESeq2/ to allow complex
#' contrasts including difference of differences and individual comparisons.
#' @param dds DESeq object containing colData and design
#' @param group1 list of character vectors each with 2 or more items
#' @param group2 list of character vectors each with 2 or more items
#' @param weighted logical indicating whether weighted contrasts should be applied. Default is FALSE.
#' @noRd

.legacy_frozen_contraster <- function(dds, group1, group2, weighted = F){

  mod_mat <- stats::model.matrix(DESeq2::design(dds), SummarizedExperiment::colData(dds))

  grp1_rows <- list()
  grp2_rows <- list()

  for(i in 1:length(group1)){

    grp1_rows[[i]] <- colData(dds)[[group1[[i]][1]]] %in% group1[[i]][2:length(group1[[i]])]

  }

  for(i in 1:length(group2)){

    grp2_rows[[i]] <- colData(dds)[[group2[[i]][1]]] %in% group2[[i]][2:length(group2[[i]])]

  }
  grp1_rows <- Reduce(function(x, y) x & y, grp1_rows)
  grp2_rows <- Reduce(function(x, y) x & y, grp2_rows)

  mod_mat1 <- mod_mat[grp1_rows, ,drop=F]
  mod_mat2 <- mod_mat[grp2_rows, ,drop=F]

  if(!weighted){
    mod_mat1 <- mod_mat1[!duplicated(mod_mat1),,drop=F]
    mod_mat2 <- mod_mat2[!duplicated(mod_mat2),,drop=F]
  }
  return(colMeans(mod_mat1)-colMeans(mod_mat2))
}

