#' tradeSeq_GOheatmap_one_lineage
#'
#' Creates a pseudotime heatmap with gene and GO annotation for a single lineage using tradeSeq results.
#'
#' @param tradeSeqRes Data.frame (rownames = gene symbols) with tradeSeq test results (association, startVsEndTest, etc.)
#' @param sce_fit tradeSeq fit object (from fitGAM)
#' @param counts Count matrix (genes x cells)
#' @param pseudotime Data.frame with pseudotime values (rownames = cell names, columns = Lineage names)
#' @param lineage_name Name of the lineage to plot (e.g. "Lineage1" or "Lineage2")
#' @param nSmoothPoints Number of pseudotime points for smoothing (default: 100)
#' @param topNgenes Number of genes per cluster to highlight with anno_mark (default: 10)
#' @param nGOsimp Number of simplified GO terms per cluster to show in barplot (default: 5)
#' @param go_ont GO ontology to use ("BP", "MF", "CC"), default "BP"
#' @param OrgDb Organism annotation package (e.g. org.Hs.eg.db)
#' @param keyType Key type for GO analysis (default: "SYMBOL")
#' @param pval_col Pattern of p-value column in tradeSeqRes (default: "pvalue_")
#' @param stat_col Pattern of stat column in tradeSeqRes (default: "waldStat_")
#' @param row_split Number of gene clusters for heatmap (default: 4)
#'
#' @return List with heatmap object, ggplot object, and intermediate data for the lineage
#' @export

# tradeSeq_GOheatmap <- function(
#     tradeSeqRes,
#     sce_fit,
#     counts,
#     pseudotime,
#     lineage_name,
#     nSmoothPoints = 100,
#     topNgenes = 10,
#     nGOsimp = 5,
#     go_ont = "BP",
#     OrgDb = org.Hs.eg.db,
#     keyType = "SYMBOL",
#     pval_col = "pvalue_",
#     stat_col = "waldStat_",
#     row_split = 4
# ) {
#
#   # 1. Select significant DE genes for lineage
#   pt <- pseudotime[, lineage_name]
#   cell_order <- order(pt)
#   lin_lower <- tolower(lineage_name)
#   pval_colname <- grep(paste0("^", pval_col, lin_lower), colnames(tradeSeqRes), value = TRUE)
#   stat_colname <- grep(paste0("^", stat_col, lin_lower), colnames(tradeSeqRes), value = TRUE)
#   if (length(pval_colname) == 0) stop(paste0("No p-value column found for ", lineage_name))
#   sig_genes <- rownames(tradeSeqRes)[tradeSeqRes[[pval_colname]] < 0.05]
#   sig_genes <- sig_genes[!is.na(sig_genes) & sig_genes != ""]
#   sig_genes_in_fit <- intersect(sig_genes, rownames(sce_fit))
#   top_table <- tradeSeqRes[sig_genes, , drop = FALSE]
#
#   # 2. Order cells by pseudotime for the lineage
#   ordered_counts <- counts[rownames(counts) %in% sig_genes, cell_order, drop = FALSE]
#   ordered_pseudotime <- pt[cell_order]
#
#   # 3. Get smoothed expression
#   smoothed <- predictSmooth(sce_fit, gene = sig_genes_in_fit, nPoints = nSmoothPoints, tidy = FALSE)
#   smoothed <- smoothed[, grepl(lin_lower, colnames(smoothed)), drop = FALSE]
#   expr_mat_scaled <- t(scale(t(smoothed)))
#   expr_mat_scaled <- expr_mat_scaled[apply(expr_mat_scaled, 1, function(x) !all(is.na(x)) & sd(x, na.rm=TRUE) > 0), , drop = FALSE]
#
#   # Color for pseudotime
#   pseudotime_norm <- seq(0, 1, length.out = nSmoothPoints)
#   top_anno <- HeatmapAnnotation(
#     Pseudotime = anno_simple(
#       pseudotime_norm,
#       col = colorRamp2(seq(0, 1, length.out = nSmoothPoints),
#                        colorRampPalette(brewer.pal(9, "YlGnBu"))(nSmoothPoints))
#     ),
#     show_legend = TRUE
#   )
#
#   # 4. Clustering & annotation
#   ht <- Heatmap(expr_mat_scaled,
#                 name = "Z-score",
#                 cluster_rows = TRUE,
#                 cluster_columns = FALSE,
#                 show_row_names = FALSE,
#                 show_column_names = FALSE,
#                 col = colorRamp2(c(-2, 0, 2), c("#4575B4", "#FFFFFF", "#D73027")),
#                 row_split = row_split,
#                 top_annotation = top_anno)
#   pdf(NULL)
#   ht_drawn <- draw(ht)
#   dev.off()
#   row_clusters <- row_order(ht_drawn)
#
#   # Get topN genes per cluster for annotation
#   top_genes_all_clusters <- c()
#   for (cl in seq_along(row_clusters)) {
#     cluster_idx <- row_clusters[[cl]]
#     cluster_genes <- rownames(expr_mat_scaled)[cluster_idx]
#     stats <- top_table[cluster_genes, , drop = FALSE]
#     # Use stat_col for ordering
#     if (length(stat_colname) == 0) {
#       topN <- head(cluster_genes, topNgenes)
#     } else {
#       stats$stat_val <- stats[[stat_colname]]
#       topN <- stats %>% arrange(desc(stat_val)) %>% head(topNgenes) %>% rownames()
#     }
#     top_genes_all_clusters <- c(top_genes_all_clusters, topN)
#   }
#
#   highlight_genes <- unique(top_genes_all_clusters)
#
#   highlight_at <- which(rownames(expr_mat_scaled) %in% highlight_genes)
#   highlight_labels <- rownames(expr_mat_scaled)[highlight_at]
#
#   row_anno <- rowAnnotation(
#     Gene = anno_mark(
#       at = highlight_at,
#       labels = highlight_labels,
#       labels_gp = gpar(col = "black", fontsize = 8)
#     )
#   )
#
#   ht2 <- Heatmap(expr_mat_scaled,
#                  name = "Z-score",
#                  cluster_rows = TRUE,
#                  cluster_columns = FALSE,
#                  show_row_names = FALSE,
#                  show_column_names = FALSE,
#                  col = colorRamp2(c(-2, 0, 2), c("#4575B4", "#FFFFFF", "#D73027")),
#                  row_split = row_split,
#                  top_annotation = top_anno,
#                  right_annotation = row_anno)
#
#   cluster_genes_list <- lapply(row_clusters, function(idx) rownames(expr_mat_scaled)[idx])
#
#   # 5. GO enrichment & simplification
#   go_results_list <- lapply(cluster_genes_list, function(genes) {
#     enrichGO(
#       gene          = genes,
#       OrgDb         = OrgDb,
#       keyType       = keyType,
#       ont           = go_ont,
#       pAdjustMethod = "BH",
#       pvalueCutoff  = 0.05,
#       readable      = TRUE
#     )
#   })
#   simplified_go_results_list <- lapply(go_results_list, function(ego){
#     simplify(ego, cutoff = 0.7, by = "p.adjust", select_fun = min)
#   })
#
#   # 6. Barplot of top GO terms per cluster (simplified)
#   go_bar_data <- do.call(rbind, lapply(seq_along(simplified_go_results_list), function(clust) {
#     res <- simplified_go_results_list[[clust]]
#     if (is.null(res) || nrow(res) == 0) return(NULL)
#     d <- res@result %>%
#       arrange(p.adjust) %>%
#       head(nGOsimp)
#     d$Cluster <- paste0("Cluster ", clust)
#     d
#   }))
#   if (is.null(go_bar_data) || nrow(go_bar_data) == 0) {
#     warning("No enriched terms found for lineage ", lineage_name)
#     go_barplot <- NULL
#   } else {
#     go_bar_data$score <- -log10(go_bar_data$p.adjust)
#     go_bar_data <- go_bar_data %>%
#       group_by(Cluster) %>%
#       mutate(Description = fct_reorder(Description, score))
#     go_bar_data$neglog10_padj <- -log10(go_bar_data$p.adjust)
#     go_barplot <- ggplot(go_bar_data, aes(x = score, y = Description, fill = neglog10_padj)) +
#       geom_col(width = 0.7) +
#       geom_text(
#         aes(x = 0.05, label = Description),
#         hjust = 0, color = "black", size = 3.5, fontface = "bold",
#         nudge_x = 0, check_overlap = TRUE
#       ) +
#       facet_grid(Cluster ~ ., scales = "free_y", space = "free_y") +
#       scale_fill_gradientn(
#         colors = RColorBrewer::brewer.pal(6, "YlGnBu")[2:5],
#         name = expression('-log'[10]*'(adj. p-value)')
#       ) +
#       labs(
#         x = "Gene enrichment score",
#         y = NULL,
#         title = paste0("Top GO Terms per Cluster (", lineage_name, ")")
#       ) +
#       theme_minimal(base_size = 13) +
#       theme(
#         strip.text.y = element_text(angle = 0, face = "bold"),
#         axis.text.y = element_blank(),
#         panel.grid.major.y = element_blank(),
#         legend.position = "right"
#       )
#   }
#
#   # Return results for this lineage
#   return(list(
#     heatmap = ht2,
#     go_barplot = go_barplot,
#     go_bar_data = go_bar_data,
#     simplified_go_results_list = simplified_go_results_list,
#     cluster_genes_list = cluster_genes_list,
#     expr_mat_scaled = expr_mat_scaled
#   ))
# }


tradeSeq_GOheatmap <- function(
    tradeSeqRes,
    sce_fit,
    counts,
    pseudotime,
    lineage_name,
    nSmoothPoints = 100,
    topNgenes = 10,
    nGOsimp = 5,
    go_ont = "BP",
    OrgDb = org.Hs.eg.db,
    keyType = "SYMBOL",
    pval_col = "pvalue_",
    stat_col = "waldStat_",
    row_split = 4
) {
  # 1. Select significant DE genes for lineage
  pt <- pseudotime[, lineage_name]
  cell_order <- order(pt)
  lin_lower <- tolower(lineage_name)
  pval_colname <- grep(paste0("^", pval_col, lin_lower), colnames(tradeSeqRes), value = TRUE)
  stat_colname <- grep(paste0("^", stat_col, lin_lower), colnames(tradeSeqRes), value = TRUE)
  if (length(pval_colname) == 0) stop(paste0("No p-value column found for ", lineage_name))
  sig_genes <- rownames(tradeSeqRes)[tradeSeqRes[[pval_colname]] < 0.05]
  sig_genes <- sig_genes[!is.na(sig_genes) & sig_genes != ""]
  sig_genes_in_fit <- intersect(sig_genes, rownames(sce_fit))
  top_table <- tradeSeqRes[sig_genes, , drop = FALSE]

  # 2. Order cells by pseudotime for the lineage
  ordered_counts <- counts[rownames(counts) %in% sig_genes, cell_order, drop = FALSE]
  ordered_pseudotime <- pt[cell_order]

  # 3. Get smoothed expression
  smoothed <- predictSmooth(sce_fit, gene = sig_genes_in_fit, nPoints = nSmoothPoints, tidy = FALSE)
  smoothed <- smoothed[, grepl(lin_lower, colnames(smoothed)), drop = FALSE]
  expr_mat_scaled <- t(scale(t(smoothed)))
  expr_mat_scaled <- expr_mat_scaled[apply(expr_mat_scaled, 1, function(x) !all(is.na(x)) & sd(x, na.rm=TRUE) > 0), , drop = FALSE]

  # Color for pseudotime
  pseudotime_norm <- seq(0, 1, length.out = nSmoothPoints)
  top_anno <- HeatmapAnnotation(
    Pseudotime = anno_simple(
      pseudotime_norm,
      col = colorRamp2(seq(0, 1, length.out = nSmoothPoints),
                       colorRampPalette(RColorBrewer::brewer.pal(9, "YlGnBu"))(nSmoothPoints))
    ),
    show_legend = TRUE
  )

  # 4. Clustering & annotation
  ht <- Heatmap(expr_mat_scaled,
                name = "Z-score",
                cluster_rows = TRUE,
                cluster_columns = FALSE,
                show_row_names = FALSE,
                show_column_names = FALSE,
                col = colorRamp2(c(-2, 0, 2), c("#4575B4", "#FFFFFF", "#D73027")),
                row_split = row_split,
                top_annotation = top_anno)
  pdf(NULL)
  ht_drawn <- draw(ht)
  dev.off()
  row_clusters <- row_order(ht_drawn)

  # Get topN genes per cluster for annotation
  top_genes_all_clusters <- c()
  for (cl in seq_along(row_clusters)) {
    cluster_idx <- row_clusters[[cl]]
    cluster_genes <- rownames(expr_mat_scaled)[cluster_idx]
    stats <- top_table[cluster_genes, , drop = FALSE]
    # Use stat_col for ordering
    if (length(stat_colname) == 0) {
      topN <- head(cluster_genes, topNgenes)
    } else {
      stats$stat_val <- stats[[stat_colname]]
      topN <- stats %>% arrange(desc(stat_val)) %>% head(topNgenes) %>% rownames()
    }
    top_genes_all_clusters <- c(top_genes_all_clusters, topN)
  }

  highlight_genes <- unique(top_genes_all_clusters)

  highlight_at <- which(rownames(expr_mat_scaled) %in% highlight_genes)
  highlight_labels <- rownames(expr_mat_scaled)[highlight_at]

  row_anno <- rowAnnotation(
    Gene = anno_mark(
      at = highlight_at,
      labels = highlight_labels,
      labels_gp = gpar(col = "black", fontsize = 8)
    )
  )

  ht2 <- Heatmap(expr_mat_scaled,
                 name = "Z-score",
                 cluster_rows = TRUE,
                 cluster_columns = FALSE,
                 show_row_names = FALSE,
                 show_column_names = FALSE,
                 col = colorRamp2(c(-2, 0, 2), c("#4575B4", "#FFFFFF", "#D73027")),
                 row_split = row_split,
                 top_annotation = top_anno,
                 right_annotation = row_anno)

  cluster_genes_list <- lapply(row_clusters, function(idx) rownames(expr_mat_scaled)[idx])

  # 5. GO enrichment & simplification
  go_results_list <- lapply(cluster_genes_list, function(genes) {
    enrichGO(
      gene          = genes,
      OrgDb         = OrgDb,
      keyType       = keyType,
      ont           = go_ont,
      pAdjustMethod = "BH",
      pvalueCutoff  = 0.05,
      readable      = TRUE
    )
  })
  simplified_go_results_list <- lapply(go_results_list, function(ego){
    simplify(ego, cutoff = 0.7, by = "p.adjust", select_fun = min)
  })

  # 5b. KEGG enrichment
  # Convert gene symbols to ENTREZ IDs for each cluster
  cluster_entrez_list <- lapply(cluster_genes_list, function(genes) {
    ids <- suppressMessages(
      clusterProfiler::bitr(genes, fromType = keyType, toType = "ENTREZID", OrgDb = OrgDb)
    )
    unique(ids$ENTREZID)
  })
  kegg_results_list <- lapply(cluster_entrez_list, function(entrez) {
    if (length(entrez) < 5) return(NULL)
    enrichKEGG(
      gene         = entrez,
      organism     = 'hsa',    # For human
      pAdjustMethod = "BH",
      pvalueCutoff  = 0.05
    )
  })

  # 5c. Top KEGG terms per cluster (top 5)
  kegg_bar_data <- do.call(rbind, lapply(seq_along(kegg_results_list), function(clust) {
    res <- kegg_results_list[[clust]]
    if (is.null(res) || nrow(res) == 0) return(NULL)
    d <- res@result %>%
      arrange(p.adjust) %>%
      head(5)
    d$Cluster <- paste0("Cluster ", clust)
    d
  }))
  if (is.null(kegg_bar_data) || nrow(kegg_bar_data) == 0) {
    warning("No enriched KEGG pathways found for lineage ", lineage_name)
    kegg_barplot <- NULL
  } else {
    kegg_bar_data$score <- -log10(kegg_bar_data$p.adjust)
    kegg_bar_data <- kegg_bar_data %>%
      group_by(Cluster) %>%
      mutate(Description = fct_reorder(Description, score))
    kegg_bar_data$neglog10_padj <- -log10(kegg_bar_data$p.adjust)
    kegg_barplot <- ggplot(kegg_bar_data, aes(x = score, y = Description, fill = neglog10_padj)) +
      geom_col(width = 0.7) +
      geom_text(
        aes(x = 0.05, label = Description),
        hjust = 0, color = "black", size = 3.5, fontface = "bold",
        nudge_x = 0, check_overlap = TRUE
      ) +
      facet_grid(Cluster ~ ., scales = "free_y", space = "free_y") +
      scale_fill_gradientn(
        colors = RColorBrewer::brewer.pal(6, "YlGnBu")[2:5],
        name = expression('-log'[10]*'(adj. p-value)')
      ) +
      labs(
        x = "Gene enrichment score",
        y = NULL,
        title = paste0("Top KEGG Pathways per Cluster (", lineage_name, ")")
      ) +
      theme_minimal(base_size = 13) +
      theme(
        strip.text.y = element_text(angle = 0, face = "bold"),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "right"
      )
  }

  # 6. Barplot of top GO terms per cluster (simplified)
  go_bar_data <- do.call(rbind, lapply(seq_along(simplified_go_results_list), function(clust) {
    res <- simplified_go_results_list[[clust]]
    if (is.null(res) || nrow(res) == 0) return(NULL)
    d <- res@result %>%
      arrange(p.adjust) %>%
      head(nGOsimp)
    d$Cluster <- paste0("Cluster ", clust)
    d
  }))
  if (is.null(go_bar_data) || nrow(go_bar_data) == 0) {
    warning("No enriched terms found for lineage ", lineage_name)
    go_barplot <- NULL
  } else {
    go_bar_data$score <- -log10(go_bar_data$p.adjust)
    go_bar_data <- go_bar_data %>%
      group_by(Cluster) %>%
      mutate(Description = fct_reorder(Description, score))
    go_bar_data$neglog10_padj <- -log10(go_bar_data$p.adjust)
    go_barplot <- ggplot(go_bar_data, aes(x = score, y = Description, fill = neglog10_padj)) +
      geom_col(width = 0.7) +
      geom_text(
        aes(x = 0.05, label = Description),
        hjust = 0, color = "black", size = 3.5, fontface = "bold",
        nudge_x = 0, check_overlap = TRUE
      ) +
      facet_grid(Cluster ~ ., scales = "free_y", space = "free_y") +
      scale_fill_gradientn(
        colors = RColorBrewer::brewer.pal(6, "YlGnBu")[2:5],
        name = expression('-log'[10]*'(adj. p-value)')
      ) +
      labs(
        x = "Gene enrichment score",
        y = NULL,
        title = paste0("Top GO Terms per Cluster (", lineage_name, ")")
      ) +
      theme_minimal(base_size = 13) +
      theme(
        strip.text.y = element_text(angle = 0, face = "bold"),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "right"
      )
  }

  # Return results for this lineage
  return(list(
    heatmap = ht2,
    go_barplot = go_barplot,
    go_bar_data = go_bar_data,
    simplified_go_results_list = simplified_go_results_list,
    kegg_barplot = kegg_barplot,
    kegg_bar_data = kegg_bar_data,
    kegg_results_list = kegg_results_list,
    cluster_genes_list = cluster_genes_list,
    expr_mat_scaled = expr_mat_scaled
  ))
}
