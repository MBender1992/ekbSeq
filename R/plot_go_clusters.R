#' Function to plot and save GO clusters
#'
#' This function uses a gene list of ENTREZ IDs as input for clusterProfiler's function \link[clusterProfiler]{compareCluster} with the parameters \emph{fun = enrichGO},
#' \emph{ont = "BP"} and \emph{OrgDb = org.Hs.eg.db}. The package \emph{org.Hs.eg.db} is required for this function to work properly.
#'
#' @param gene.list List containing ENTREZ IDs of enriched genes. List should be generated with the list_signif_genes() function from the ekbSeq package.
#' @param pvalueCutoff adjusted pvalue cutoff on enrichment tests to report
#' @param sim.thres similarity threshold (0-1). Some guidance: Large (allowed similarity=0.9), Medium (0.7), Small (0.5), Tiny (0.4) Defaults to Medium (0.7)
#' @param sym.colors Logical indicating whether colors distribution should be symmetrical. Default is FALSE.
#' @param font.size Font size.
#' @param return.res Logical indicating whether results should be returned. If TRUE original enrichGO results and reduced terms will be stored as list object.
#' @param showCategory A number or a list of terms. If it is a number, the first n terms will be displayed. If it is a list of terms, the selected terms will be displayed.
#' @export

plot_go_clusters <- function(gene.list, showCategory = 5, sim.thres = 0.7, sym.colors = FALSE, return.res = FALSE,  font.size = 12, pvalueCutoff = 0.05){
  term <- NULL

  names <- str_remove(deparse(substitute(gene.list)), "ls_")
  ## calculate clustered pathways
  ck <- compareCluster(geneCluster = gene.list, fun = enrichGO, ont = "BP", keyType = "ENTREZID", OrgDb = org.Hs.eg.db, pvalueCutoff = pvalueCutoff)
  ck <- enrichplot::pairwise_termsim(ck)
  ck <- setReadable(ck, OrgDb = org.Hs.eg.db, keyType="ENTREZID")
  simMatrix <- calculateSimMatrix(ck@compareClusterResult$ID, orgdb = "org.Hs.eg.db", ont = "BP", method = "Rel")
  scores <- stats::setNames(-log10(ck@compareClusterResult$p.adjust), ck@compareClusterResult$ID)
  res <- reduceSimMatrix(simMatrix, scores, threshold = sim.thres, orgdb = "org.Hs.eg.db")
  reducedTerms <- res[res$go == res$parent, ]
  ## define data frame for barplot
  df_bp <- head(reducedTerms, 20)
  df_bp$term <- factor(df_bp$term)
  df_bp$term <- fct_reorder(df_bp$term, df_bp$score)

  ## define viridis colors
  rt_min <- sqrt(min(reducedTerms$score))
  rt_max <- max(reducedTerms$score)
  if(sym.colors == FALSE){
    colour_breaks <- c(rt_min, rt_min*2, rt_min*3, rt_min*5, rt_min*6, rt_max*0.8, rt_max)
  } else {
    colour_breaks <- seq(rt_min, rt_max, by = (rt_max-rt_min)/6)
  }
  colours <- c("#440154FF", "#470D60FF", "#39558CFF", "#26818EFF",  "#1F998AFF", "#C9E020FF", "#FDE725FF")

  p_dp <- dotplot(ck, showCategory = showCategory,  font.size = font.size) + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) + scale_fill_viridis_c()
  p_wc <- plot_wordcloud(ck@compareClusterResult$Description)
  p_bp <- ggbarplot(df_bp, x = "term", y = "score", fill = "score") +
    scale_y_continuous(expand = c(0,0)) + xlab("") + ylab("-log10 adjusted pvalue") +
    scale_fill_gradientn(name = "-log10 adjusted pvalue", limits  = range(reducedTerms$score), colours = colours[c(1, seq_along(colours), length(colours))],  values  = c(0, scales::rescale(colour_breaks, from = range(reducedTerms$score)), 1)) +
    coord_flip()

  ## arrange plots
  p_aux <- ggarrange(p_wc, p_bp, labels = c("A", "B"), ncol = 1)
  p <- ggarrange(p_aux, p_dp, labels = c("", "C"), ncol = 2, widths = c(1, 0.5))
  p <- annotate_figure(p, top = text_grob(paste0("Biological processes (n=", dim(unique(ck@compareClusterResult$ID))[1],") enriched in ", names, " genes"), face = "bold", size = 14))
  # svg(paste0(path, names, "_GOBP_wc.svg"), width=18, height=14)
  # print(p)
  # dev.off()

  if(return.res == TRUE){
    list(enrich_go = ck, reduced_go = reducedTerms, plot = p)
  } else {
    return(p)
  }
}

