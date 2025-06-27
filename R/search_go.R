#' Function to plot subset of go terms based on search string
#'
#' This function uses results of \link[clusterProfiler]{compareCluster} or \link[clusterProfiler]{enrichGO} as input to search for go terms of interest
#'  based on provided string. Output are a .csv containing a list of matching go terms ranked by their occurence in the list of all enrich go terms and a figure
#'  containing genes involved in those go terms (ranked by their respective logFoldChange).
#'
#' @param enrich.res Enrichment results generated with \link[clusterProfiler]{compareCluster} or \link[clusterProfiler]{enrichGO}
#' @param all.res Annotated results of DESeq analysis containing (at least) p.value, gene ID, gene name and logFoldChange
#' @param path Path were results should be stored. Can be an absolute path or relative path based on the working directory.
#' @param search.string Logical indicating whether colors distribution should be symmetrical. Default is FALSE.
#' @param fig.height Figure height (in inches).
#' @param fig.width Figure width (in inches).
#' @param flip.coords Logical indicating whether x-y coordinates should be flipped. Default is TRUE.
#' @export

search_go <- function(enrich.res, all.res, path = getwd(), search.string = " ", fig.width = 7, fig.height = 16, flip.coords = TRUE){
  ind <- str_detect(enrich.res$Description, search.string)
  search.string <- str_replace_all(search.string, "\\|", "_")
  tbl <- enrich.res[ind,]
  tbl$Rank <- which(ind)
  genes <- unique(unlist(stringr::str_split(tbl$geneID, "/")))

  ## write results into table
  tbl %>%
    dplyr::select(c("Rank", "ID", "Description", "zScore", "p.adjust", "geneID", "Count")) %>%
    stats::setNames(c("Rank", "ID", "Description", "Z-score", "Adjusted P-value", "Genes", "Count")) %>%
    write.csv(paste0(path, "/selected_go_terms_", search.string, ".csv"))

  ## plot genes which play a role within the selected pathways
  all.res <- all.res[all.res$SYMBOL %in% genes,]
  all.res$SYMBOL <- factor(all.res$SYMBOL)
  all.res$SYMBOL <- fct_reorder(all.res$SYMBOL, all.res$log2FoldChange)
  p <- ggbarplot(all.res, x= "SYMBOL", y = "log2FoldChange", fill = "log2FoldChange") +
    scale_fill_viridis_c() +
    scale_y_continuous(expand = c(0,0)) +
    ylab("")

  if(flip.coords == TRUE){
    p <- p + coord_flip()
  }

  svg(paste0(path, "/selected_go_terms_", search.string, ".svg"), width=fig.width, height=fig.height)
  print(p)
  dev.off()
}

