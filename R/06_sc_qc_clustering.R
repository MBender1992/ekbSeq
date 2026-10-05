#' Mean local inverse Simpson mixing on two embedding dimensions
#' @param object Seurat object with a reduction and metadata.
#' @param group_by Metadata column defining sample groups.
#' @param reduction Reduction containing at least two dimensions.
#' @param k Neighbors excluding each focal cell.
#' @return One numeric mean inverse Simpson diversity.
#' @export
calculate_local_mixing <- function(object, group_by = "sample_id",
                                   reduction = "umap.unintegrated", k = 50) {
  .require_package("SeuratObject")
  .require_package("FNN")
  .check_columns(object[[]], group_by)
  embedding <- SeuratObject::Embeddings(object, reduction = reduction)
  if (ncol(embedding) < 2L) stop("Reduction must have at least two dimensions.")
  if (length(k) != 1L || k < 1L || k >= nrow(embedding)) stop("k must be between 1 and the number of cells minus one.")
  labels <- object[[]][[group_by]]
  if (anyNA(labels)) stop("Grouping labels must not contain missing values.")
  indices <- FNN::get.knn(embedding[, 1:2, drop = FALSE], k = k)$nn.index
  mean(vapply(seq_len(nrow(indices)), function(i) {
    probabilities <- table(labels[indices[i, ]]) / k
    1 / sum(probabilities^2)
  }, numeric(1)))
}

#' Screen Seurat cluster resolutions without writing files
#' @param object Seurat object with a precomputed neighbor graph.
#' @param resolutions Numeric resolutions to inspect.
#' @param graph_name Graph name; `NULL` retains Seurat's default.
#' @param algorithm Clustering algorithm (historically 4, Leiden).
#' @param prefix Resolution metadata prefix for discovery.
#' @return A list with `seurat_obj_clusters` and `res_cols`.
#' @export
screen_cluster_resolutions <- function(object, resolutions = c(0.2, 0.4, 0.5, 0.6, 0.8, 1),
                                       graph_name = NULL, algorithm = 4,
                                       prefix = "RNA_snn_res") {
  .require_package("Seurat")
  if (!length(resolutions) || anyNA(resolutions) || any(resolutions <= 0)) stop("Resolutions must be positive numbers.")
  for (resolution in resolutions) {
    args <- list(object = object, resolution = resolution, algorithm = algorithm)
    if (!is.null(graph_name)) args$graph.name <- graph_name
    object <- do.call(Seurat::FindClusters, args)
    last <- ncol(object[[]])
    current <- object[[]][[last]]
    object@meta.data[[last]] <- factor(current, levels = as.character(sort(as.numeric(levels(current)))))
  }
  list(seurat_obj_clusters = object,
       res_cols = grep(paste0("^", prefix), colnames(object[[]]), value = TRUE))
}

#' Find positive markers at stored cluster resolutions
#' @param object Seurat object with cluster metadata.
#' @param res_cols Resolution metadata columns or NULL for automatic discovery.
#' @param prefix Prefix used for automatic resolution discovery.
#' @param remove_lncRNA Whether to remove legacy automated gene names.
#' @param min_pct,logfc_threshold Marker selection thresholds.
#' @return Named list of Seurat marker data frames.
#' @export
find_resolution_markers <- function(object, res_cols = NULL, prefix = "RNA_snn_res",
                                    remove_lncRNA = TRUE, min_pct = 0.1,
                                    logfc_threshold = 0.25) {
  .require_package("Seurat")
  .require_package("SeuratObject")
  if (is.null(res_cols)) res_cols <- grep(paste0("^", prefix), names(object[[]]), value = TRUE)
  if (!length(res_cols)) stop("No resolution columns found.")
  .check_columns(object[[]], res_cols)
  setNames(lapply(res_cols, function(column) {
    SeuratObject::Idents(object) <- object[[]][[column]]
    result <- Seurat::FindAllMarkers(object, only.pos = TRUE, min.pct = min_pct,
                                     logfc.threshold = logfc_threshold)
    if (remove_lncRNA && nrow(result)) {
      result <- result[!grepl("LINC|AC[0-9]{3,}|AL[0-9]{3,}|AP[0-9]{3,}", result$gene), , drop = FALSE]
    }
    result
  }), res_cols)
}

#' Plot a single-cell QC metric as a ridge plot
#' @param object Seurat object with a numeric metric and cell identities.
#' @param metric Metadata metric name.
#' @param upper_xlim Optional upper display limit.
#' @param colors Optional named palette for the identities.
#' @return ggplot ridge plot with a median reference line.
#' @export
plot_sc_qc_ridge <- function(object, metric, upper_xlim = NULL, colors = NULL) {
  .require_package("Seurat")
  .require_package("ggprism")
  .check_columns(object[[]], metric)
  p <- Seurat::RidgePlot(object, features = metric, cols = colors) +
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
    ggplot2::geom_vline(xintercept = stats::median(object[[]][[metric]]),
      size = 0.9, lty = 1, color = "darkred")
  if (!is.null(upper_xlim)) p <- p + ggplot2::xlim(0, upper_xlim)
  p
}
