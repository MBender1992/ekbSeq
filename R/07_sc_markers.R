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
