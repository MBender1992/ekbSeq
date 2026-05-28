#' Perform GO enrichment analysis and bubble plot visualization for tradeSeq differential expression results
#'
#' This function extracts significantly up- and downregulated genes for a given lineage comparison from tradeSeq results,
#' performs Gene Ontology (GO) enrichment analysis using clusterProfiler, simplifies redundant GO terms, and creates
#' side-by-side bubble plots of the top enriched biological processes for up- and downregulated genes.
#'
#' @param res Data frame. TradeSeq results with gene rownames and columns for p-values, wald statistics, and log fold change.
#' @param comparison_name Character. Name of the comparison, used for plot titles (e.g., "lin1 vs lin2") and to extract comparisons. Need to include the number of the assigned lineages in the correct order. 
#' @param top_n Integer. Number of top GO terms to display in bubble plots (default: 20).
#' @param pval_thresh Numeric. P-value threshold for significance (default: 0.05).
#' @param ont Character. Ontology for GO enrichment ("BP", "MF", "CC"; default: "BP").
#' @param simplify_cutoff Numeric. Cutoff for the \code{simplify} function to reduce redundancy (default: 0.7).
#'
#' @return A list with three elements:
#' \describe{
#'   \item{up_GO}{clusterProfiler enrichResult for upregulated genes (simplified)}
#'   \item{down_GO}{clusterProfiler enrichResult for downregulated genes (simplified)}
#'   \item{plot}{A ggarrange object showing side-by-side bubble plots for up- and downregulated GO terms}
#' }
#'
#' @details
#' The function expects gene symbols as rownames of \code{res}. These are mapped to ENTREZ IDs for GO enrichment. 
#' Upregulated genes are those with significant p-value and positive log fold change; downregulated are significant and negative.
#' The function uses clusterProfiler for enrichment, enrichplot for bubble plots, and ggpubr::ggarrange for layout.
#'
#' @export
#' 
plot_tradeSeq_GO <- function(res, 
                             comparison_name = "lin1 vs. lin2", 
                             top_n = 20, 
                             pval_thresh = 0.05, 
                             ont = "BP", 
                             simplify_cutoff = 0.7) {
  # Extract correct columns based on comparison name
  name_split <- unlist(strsplit(comparison_name, " "))
  str1 <- str_extract(name_split[1], "\\d+")
  str2 <- str_extract(name_split[length(name_split)], "\\d+")
  pval_col <- paste0("pvalue_", str1, "vs", str2)
  wald_col <- paste0("waldStat", str1, "vs", str2)
  logFC_col <- paste0("logFC", str1, "_", str2)
  
  # Extract significant genes
  sig_genes <- res[res[[pval_col]] < pval_thresh, ]
  up_genes <- rownames(sig_genes[sig_genes[[logFC_col]] > 0, ])
  down_genes <- rownames(sig_genes[sig_genes[[logFC_col]] < 0, ])
  
  # Convert gene symbols to ENTREZID
  up_eg <- bitr(up_genes, fromType="SYMBOL", toType="ENTREZID", OrgDb=org.Hs.eg.db)$ENTREZID
  down_eg <- bitr(down_genes, fromType="SYMBOL", toType="ENTREZID", OrgDb=org.Hs.eg.db)$ENTREZID
  
  # GO enrichment
  ego_up <- enrichGO(gene = up_eg, OrgDb = org.Hs.eg.db, keyType = "ENTREZID",
                     ont = ont, pAdjustMethod = "BH", pvalueCutoff = pval_thresh, readable = TRUE)
  ego_down <- enrichGO(gene = down_eg, OrgDb = org.Hs.eg.db, keyType = "ENTREZID",
                       ont = ont, pAdjustMethod = "BH", pvalueCutoff = pval_thresh, readable = TRUE)
  
  # Simplify GO terms
  ego_up_s <- simplify(ego_up, cutoff = simplify_cutoff, by = "p.adjust", select_fun = min)
  ego_down_s <- simplify(ego_down, cutoff = simplify_cutoff, by = "p.adjust", select_fun = min)
  
  # Bubble plots
  p_up <- dotplot(ego_up_s, showCategory=top_n, title=paste("Upregulated in", comparison_name)) +
    theme(axis.text.y = element_text(size=9))
  p_down <- dotplot(ego_down_s, showCategory=top_n, title=paste("Downregulated in", comparison_name)) +
    theme(axis.text.y = element_text(size=9))
  
  # Combine plots
  combined_plot <- ggarrange(p_up, p_down, nrow = 1)
  
  return(list(
    up_GO = ego_up_s,
    down_GO = ego_down_s,
    plot = combined_plot
  ))
}
