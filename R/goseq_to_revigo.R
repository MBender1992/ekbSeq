#'  Perform goseq analysis and reduce terms based on similarity threshold
#'
#' This function takes a named gene list with Gene IDs as names and 1 or 0 for significantly or unchanged genes, respectively, as input.
#' It is possible to analyze any gene set by assigning 1 to the genes of interest and 0 for other genes.
#' @param gene.list named gene list containing 1 (gene of interest) or 0 (other gene). Names can be either Ensemble IDs
#' or ENTREZ IDs. Based on the name the identifier has to be set to either "ensGene" or "knownGene" for Ensemble and Entrez IDs, respectively.
#' @param genome reference genome. For further details see \code{goseq} function [nullp()].
#' @param identifier gene identifier as specified in gene.list.
#' @param ont GO ontology. Either "BP" for "biological process", "MF" for "molecular function" or "CC" for cellular compartment. Default is "BP".
#' @param sim.thres similarity threshold passed on to the \code{rrvgo} function [reduceSimMatrix()].
#' @param pval.thres threshold to filter enriched pathways.
#' @export

goseq_to_revigo <- function(gene.list, genome, identifier = "ensGene", ont = "BP", sim.thres = 0.7, pval.thres = 0.05) {
  goseqOnt <- paste("GO:", ont, sep = "")
  pwf <- nullp(gene.list, genome, identifier)
  goResults <- goseq(pwf, genome, identifier, test.cats = c(goseqOnt))
  goResults <- goResults[goResults$over_represented_pvalue < pval.thres, ]

  simMatrix <- calculateSimMatrix(goResults$category, orgdb = "org.Hs.eg.db", ont = ont, method = "Rel")
  scores <- stats::setNames(-log10(goResults$over_represented_pvalue), goResults$category)
  res <- reduceSimMatrix(simMatrix, scores, threshold = sim.thres, orgdb = "org.Hs.eg.db")
  res[res$go == res$parent, ]
}



