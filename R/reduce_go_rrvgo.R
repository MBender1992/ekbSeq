#' Reduce and Annotate GO Terms Using RRvgo
#'
#' This function reduces redundant GO terms from a clusterProfiler enrichment object using semantic similarity via the RRvgo package,
#' and annotates the representative terms with clusterProfiler results.
#'
#' @param ego_obj A clusterProfiler enrichment result object (e.g. from `enrichGO`).
#' @param ont Character. The ontology to use for similarity calculation. Default is `"BP"` (biological process).
#' @param orgdb Character or AnnotationDbi object. The organism database, e.g. `"org.Hs.eg.db"`.
#' @param sim.thres Numeric. Similarity threshold for RRvgo reduction (between 0 and 1). Default is `0.7`.
#'
#' @return A data frame containing the reduced representative GO terms and their annotation from the clusterProfiler enrichment results.
#'
#' @details
#' This function performs the following steps:
#' \enumerate{
#'   \item Calculates semantic similarity between enriched GO terms.
#'   \item Reduces redundant terms using RRvgo.
#'   \item Annotates representative (parent) terms with clusterProfiler results.
#' }
#'
#' @seealso \code{\link[rrvgo]{calculateSimMatrix}}, \code{\link[rrvgo]{reduceSimMatrix}}, \code{\link[clusterProfiler]{enrichGO}}
#' @examples
#' \dontrun{
#' ego <- enrichGO(gene = geneList, OrgDb = org.Hs.eg.db, ont = "BP", keyType = "ENTREZID")
#' reduced <- reduce_go_rrvgo(ego)
#' head(reduced)
#' }
#' @export

reduce_go_rrvgo <- function(ego_obj, ont = "BP", orgdb = "org.Hs.eg.db", sim.thres = 0.7){
  enriched_genes <- NULL
  all_genes <- NULL
  GeneRatio <- NULL
  ck <- ego_obj
  ck <- enrichplot::pairwise_termsim(ck)
  ck <- clusterProfiler::setReadable(ck, OrgDb = org.Hs.eg.db, keyType="ENTREZID")
  simMatrix <- rrvgo::calculateSimMatrix(ck$ID, orgdb = orgdb, ont = ont, method = "Rel")
  scores <- stats::setNames(-log10(ck$p.adjust), ck$ID)
  res <- rrvgo::reduceSimMatrix(simMatrix, scores, threshold = sim.thres, orgdb = orgdb)
  reducedTerms <- res[res$go == res$parent, ] %>%
    left_join(data.frame(ck), by = c("go" = "ID")) %>%
    tidyr::separate(GeneRatio, c("enriched_genes", "all_genes")) %>%
    mutate(GeneRatio = as.numeric(enriched_genes)/as.numeric(all_genes))
  return(reducedTerms)
}




