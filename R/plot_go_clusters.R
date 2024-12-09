#' Function to plot and save
#'
#' This function uses a gene list of ENTREZ IDs as input for clusterProfiler's function \link[clusterProfiler]{compareCluster} with the parameters \emph{fun = enrichGO},
#' \emph{ont = "BP} and \emph{OrgDb = org.Hs.eg.db}. The package \emph{org.Hs.eg.db} is required for this function to work properly.
#'
#' @param gene.list List containing ENTREZ IDs of enriched genes. List should be generated with the list_signif_genes() function from the ekbSeq package.
#' @param pvalueCutoff adjusted pvalue cutoff on enrichment tests to report
#' @param path Path were results should be stored. Can be an absolute path or relative path based on the working directory.
#' @export

## function to do GO enrichment analysis on multiple lists of DE genes and plot results as dotplot
plot_go_clusters <- function(gene.list, pvalueCutoff = 0.05, path){
  names <- str_remove(deparse(substitute(gene.list)), "ls_")
  ## calculate clustered pathways
  ck <- compareCluster(geneCluster = gene.list, fun = enrichGO, ont = "BP", keyType = "ENTREZID", OrgDb = org.Hs.eg.db, pvalueCutoff = pvalueCutoff)
  ck <- enrichplot::pairwise_termsim(ck)
  ck <- setReadable(ck, OrgDb = org.Hs.eg.db, keyType="ENTREZID")

  p <- dotplot(ck, font.size = 11) + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  svg(paste0(path, names, "_GOBP.svg"), width=6, height=14)
  print(p)
  dev.off()
}
