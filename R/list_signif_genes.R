#' Function to list ENTREZ IDs of significantly changed genes
#'
#' This function lists significantly up or downregulated genes as ENTREZ IDs for later use in clusterProfiler's function \link[clusterProfiler]{compareCluster}. 
#' 
#' @param list List containing results of DE analysis generated with \link[DESeq2]{results} function. 
#' @param p.threshold P-value threshold used to define differentially expressed genes
#' @param lfc.threshold Log-fold change threshold used to define differentially expressed genes.
#' @param direction Character specifying whether ENTREZ IDs of upregulated ("greater") or downregulated ("lesser") genes should be extracted
#' @export

list_signif_genes <- function(list, p.threshold = 0.05, lfc.threshold = 0, direction = c("greater", "lesser")){
  lapply(1:length(list), function(x){
    tmp <- list[[x]]
    if(direction == "greater"){
      res <- tmp[!is.na(tmp$padj) & tmp$padj < p.threshold & !is.na(tmp$ENTREZID) & tmp$log2FoldChange > lfc.threshold, ]$ENTREZID
    }   else if(direction == "lesser"){
      res <- tmp[!is.na(tmp$padj) & tmp$padj < p.threshold & !is.na(tmp$ENTREZID) & tmp$log2FoldChange < lfc.threshold, ]$ENTREZID
    } else {
      stop("Please specify a direction.")
    }
    res
  })
}
