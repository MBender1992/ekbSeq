#' GO Enrichment and Redundancy Reduction Using goseq and rrvgo
#'
#' This function performs Gene Ontology (GO) enrichment analysis using the \code{goseq} package and reduces the resulting terms
#' based on semantic similarity using \code{rrvgo}. It is useful for highlighting the most representative GO terms among significantly enriched results.
#'
#' @param gene.list Named numeric vector of 0s and 1s, indicating genes of interest (1) and background genes (0).
#' Names should correspond to gene IDs, either Ensembl or Entrez, depending on the \code{identifier} used.
#' @param genome Character string specifying the reference genome (e.g., \code{"hg38"}). Passed to \code{nullp} and \code{goseq}.
#' @param identifier Character string. Gene ID format: \code{"ensGene"} for Ensembl or \code{"knownGene"} for Entrez IDs.
#' @param ont Character string specifying the GO ontology. One of \code{"BP"} (Biological Process), \code{"MF"} (Molecular Function), or \code{"CC"} (Cellular Component). Default is \code{"BP"}.
#' @param sim.thres Numeric. Similarity threshold used by \code{rrvgo::reduceSimMatrix()}. Default is \code{0.7}.
#' @param pval.thres Numeric. Adjusted p-value threshold for filtering enriched GO terms. Default is \code{0.05}.
#'
#' @return A data frame containing non-redundant representative GO terms after enrichment and similarity reduction.
#'
#' @details This function:
#' \enumerate{
#'   \item Performs GO enrichment using \code{goseq()}.
#'   \item Adjusts p-values using FDR.
#'   \item Filters results below a p-value threshold.
#'   \item Computes a semantic similarity matrix via \code{rrvgo::calculateSimMatrix()}.
#'   \item Reduces the matrix using \code{rrvgo::reduceSimMatrix()} to retain only representative GO terms.
#' }
#' @examples
#' \dontrun{
#' gene_list <- c(GeneA = 1, GeneB = 0, GeneC = 1, GeneD = 0)
#' goseq_to_revigo(gene.list = gene_list, genome = "hg38", identifier = "ensGene")
#' }
#'
#' @export

goseq_to_revigo <- function(gene.list, genome, identifier = "ensGene", ont = "BP", sim.thres = 0.7, pval.thres = 0.05) {
  goseqOnt <- paste("GO:", ont, sep = "")
  pwf <- nullp(gene.list, genome, identifier)
  goResults <- goseq(pwf, genome, identifier, test.cats = c(goseqOnt))
  goResults$padj <- rstatix::adjust_pvalue(goResults$over_represented_pvalue, method = "fdr")
  goResults <- goResults[goResults$padj < pval.thres, ]
  simMatrix <- calculateSimMatrix(goResults$category, orgdb = "org.Hs.eg.db", ont = ont, method = "Rel")
  scores <- stats::setNames(-log10(goResults$padj), goResults$category)
  res <- reduceSimMatrix(simMatrix, scores, threshold = sim.thres, orgdb = "org.Hs.eg.db")
  res[res$go == res$parent, ]
}

