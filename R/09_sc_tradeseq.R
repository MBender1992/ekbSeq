#' Plot precomputed tradeSeq expression patterns
#' @param data Data frame with `cluster`, `gene`, `pseudotime`, `expr`, `lineage`.
#' @param colors Named lineage palette.
#' @param collapse Show one smoothed curve per lineage instead of each gene.
#' @param alpha Point/curve transparency.
#' @param nrow,ncol Facet dimensions.
#' @return ggplot object; the function does not fit a tradeSeq model.
#' @export
plot_tradeseq_patterns <- function(data, colors, collapse = FALSE, alpha = 0.7,
                                   nrow = NULL, ncol = NULL) {
  .require_package("ggplot2")
  .check_columns(data, c("cluster", "gene", "pseudotime", "expr", "lineage"))
  if (missing(colors) || !length(colors)) stop("Provide explicit lineage colors.")
  group <- if (collapse) interaction(data$lineage) else
    interaction(data$gene, data$lineage)
  data$.curve <- group
  .require_package("dplyr")
  .require_package("stringr")
  labels <- dplyr::summarise(dplyr::group_by(data, .data$cluster),
    genes = paste(sort(unique(.data$gene)), collapse = ", "), .groups = "drop")
  labels$genes <- stringr::str_wrap(labels$genes, width = 40)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data$pseudotime,
                                            y = .data$expr, color = .data$lineage,
                                            group = .data$.curve))
  plot <- if (collapse) plot + ggplot2::geom_smooth(size = 1, alpha = 0.8) else
    plot + ggplot2::geom_line(size = 0.7, alpha = alpha)
  plot + ggplot2::facet_wrap(~cluster, scales = "free_y", nrow = nrow, ncol = ncol) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::labs(title = if (collapse) "Clustered Expression Patterns Across Pseudotime" else
      "Gene Expression Trajectories by Cluster and Lineage",
      x = "Scaled Pseudotime", y = "Normalized expression") +
    ggplot2::theme_classic() +
    ggplot2::theme(strip.text = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(hjust = 0.5), legend.position = "top") +
    ggplot2::geom_text(data = labels, ggplot2::aes(label = .data$genes),
      x = Inf, y = Inf, hjust = 1.1, vjust = 1.1, color = "black", size = 3,
      inherit.aes = FALSE, fontface = "italic")
}

#' Combine two enrichment directions in one plot
#' @param enriched Named list with `go_up`/`go_down` or `kegg_up`/`kegg_down`.
#' @param type GO or KEGG.
#' @param show_category Maximum terms per panel.
#' @param title_prefix Optional title prefix.
#' @param font_size Dotplot font size.
#' @return ggarrange plot; empty directions are shown as blank labeled panels.
#' @export
plot_enrichment_pair <- function(enriched, type = c("GO", "KEGG"), show_category = 15,
                                 title_prefix = NULL, font_size = 12) {
  .require_package("ggpubr")
  .require_package("ggplot2")
  type <- match.arg(type)
  stem <- if (type == "GO") "go" else "kegg"
  plots <- lapply(c("up", "down"), function(direction) {
    object <- enriched[[paste0(stem, "_", direction)]]
    if (is.null(title_prefix)) {
      heading <- paste(if (type == "GO") "GO BP" else "KEGG",
                       if (direction == "up") "(Upregulated)" else "(Downregulated)")
    } else {
      heading <- paste(title_prefix, if (type == "GO") "GO BP" else "KEGG",
                       if (direction == "up") "(Up)" else "(Down)")
    }
    if (is.null(object) || !nrow(as.data.frame(object))) {
      return(ggplot2::ggplot() + ggplot2::theme_void() +
               ggplot2::ggtitle(if (direction == "up") "No enrichment (Up)" else
                 "No enrichment (Down)"))
    }
    .require_package("enrichplot")
    enrichplot::dotplot(object, showCategory = show_category, font.size = font_size,
                        title = heading) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        plot.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 40, unit = "pt"))
  })
  ggpubr::ggarrange(plotlist = plots, ncol = 2, nrow = 1,
                    common.legend = TRUE, legend = "right", align = "h")
}

#' Enrich tradeSeq lineage results using the historical tested selection logic
#' @param results tradeSeq table with gene symbols in row names.
#' @param lineage Lineage name in p-value/logFC column suffixes.
#' @param universe Optional background genes.
#' @param species_db OrgDb database.
#' @param ontology GO ontology.
#' @param kegg Run KEGG enrichment.
#' @param simplify Simplify GO terms.
#' @param similarity_cutoff GO similarity cutoff.
#' @return Named list of GO/KEGG objects and up/down gene vectors.
#' @export
enrich_tradeseq <- function(results, lineage, universe = NULL,
                            species_db = org.Hs.eg.db::org.Hs.eg.db,
                            ontology = "BP", kegg = TRUE, simplify = TRUE,
                            similarity_cutoff = 0.7) {
  do_enrichment_tradeSeq(tradeSeqRes = results, lineage_name = lineage,
    universe = universe, species_db = species_db, ont = ontology,
    do_kegg = kegg, go_simplify = simplify,
    simplify_cutoff = similarity_cutoff)
}
