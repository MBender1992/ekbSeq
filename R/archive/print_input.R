#' Function to print input thresholds for the DESeq pipeline.
#'
#' Output is a flextable object with prespecified styling. Needs a SummarizedExperiment object and a DESeq Object as well
#' as significant and log fold-change thresholds and the minimum read count size as well as the minimum group Size. Additionally
#' the contrast formula needs to be specified.
#' @param SummarizedExp SummarizedExperiment object.
#' @param DESeqObj DESeq object.
#' @param pThres p-Value threshold.
#' @param lfcThres log fold-change threshold
#' @param minimumCount minimum read count for a gene to be included. The threshold works together with the smallestGroupSize
#' argument. For example a minimumCount = 10 and smallestGroupSize = 3 would eliminate all genes who do not reach a count of 10
#' in at least 3 samples.
#' @param smallestGroupSize sample size of the smallest group. For more details see argument minimumCount.
#' @param contrast design formula as specified to obtain the DESeq object.
#' @param fontSize size of table font.
#' @export

print_input <- function(SummarizedExp, DESeqObj, pThres = 0.05, lfcThres = 1, minimumCount = 10,
                        smallestGroupSize, contrast, fontSize = 9){

    ft <- data.frame(Thresholds = NA, pThres, lfcThres, minimumCount, smallestGroupSize, formula = paste(as.character(contrast), collapse = ""),
                   Transcripts = NA, n_total = nrow(SummarizedExperiment::assay(SummarizedExp)), n_expr = nrow(SummarizedExperiment::assay(DESeqObj))) %>%
    tidyr::gather("rownames", "value") %>%
    dplyr::mutate(rownames = factor(rownames, levels = c("Thresholds", "pThres", "lfcThres", "minimumCount",
                                                  "smallestGroupSize",  "formula", "Transcripts", "n_total", "n_expr"), labels = c("Thresholds", "P-value", "Log-fold change", "Minimum read count", "Smallest group size",                             "Design Formula", "Transcript number",  "Total number of transcripts",
                                                                                                                                   "Filtered number of transcripts"))) %>%
    flextable() %>%
    set_header_labels(rownames = "", value = "Value") %>%
    bold(part = "header") %>%
    bold(i = c(1,6,7), j = 1) %>%
    padding(i = c(2,3,4,5,8,9), j = 1, padding.left = 10)%>%
    line_spacing(space = 0.4, part = "body") %>%
    flextable::footnote(i = 9, j = 1,
                        value = as_paragraph(
                          c(paste("All transcripts which are above the minimum read count threshold (", minimumCount,") in at
                      least ", smallestGroupSize, " samples.", sep = ""))
                        ),
                        ref_symbols = c("\u2020"),
                        part = "body") %>%
    fontsize(size = fontSize, part = "all") %>%
    theme_zebra()

  emR::FitFlextableToPage(ft, pgwidth = 7)
}
