#' Extract high dispersion genes
#'
#' @param dds.obj DESeq2 object containg differential expression data
#' @param norm.counts normalized count data (e.g. rlog or vst transformed count data).
#' @param res results table containing gene IDs as ENSEMBL ID as well as gene names stored as "SYMBOL".
#' @param disp.thres dispersion threshold. All genes higher than the specified number will be extracted.
#' @param min.count remove low expressed genes which would interfere with the calculation of dispersion estimates.
#' @param n.min the smallest sample size of the experiment which refers to the number of samples which need a count above the threshold set in
#' min.count to be considered.
#' @export

high_dispersion_genes <- function(dds.obj, norm.counts, res, min.count = 10, n.min = dim(dds.obj)[2]/2, disp.thres = 1){
  arg1 <- S4Vectors::mcols(dds.obj, use.names = TRUE)$dispersion > disp.thres
  arg2 <- rowSums(SummarizedExperiment::assay(dds.obj) >= min.count) > n.min

  indDisp <- which(arg1 & arg2)
  rldHighDisp <- SummarizedExperiment::assay(norm.counts)[indDisp,]

  highDispGenes <- res[res$ENSEMBL %in% rownames(rldHighDisp),]$SYMBOL
  highDispGenes[!is.na(highDispGenes)]
}
