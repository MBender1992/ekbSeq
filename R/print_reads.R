#' Function to print reads per sample
#'
#' Output is a flextable object with prespecified styling. 
#' @param readsRaw named vector including the total number of reads per sample. 
#' @param readsFiltered named vector including the total number of reads per sample after applying filtering (e.g. removing lowly expressed transcripts). 
#' @param minimumCount minimum read count for a gene to be included. The threshold works together with the smallestGroupSize
#' argument. For example a minimumCount = 10 and smallestGroupSize = 3 would eliminate all genes who do not reach a count of 10 
#' in at least 3 samples.
#' @param smallestGroupSize sample size of the smallest group. For more details see argument minimumCount.
#' @export

print_reads <- function(readsRaw, readsFiltered, minimumCount = 10, smallestGroupSize){
  nreads <- as.data.frame(rbind(formatC(readsRaw, format = "e", digits = 2), 
                                formatC(readsFiltered, format = "e", digits = 2)))
  rownames <- c("Number of reads (total)", "Number of reads (filtered)")
  
  # transform into ngsekb function
  ft <- cbind(rownames, nreads) %>% flextable() %>%
    set_header_labels(rownames = "") %>%
    bold(part = "header") %>%
    flextable::footnote(i = 2, j = 1,
                        value = as_paragraph(
                          c(paste("All transcripts which are above the minimum read count threshold (", minimumCount,") in at
                      least", smallestGroupSize, " samples.", sep = ""))
                        ),
                        ref_symbols = c("\u2020"),
                        part = "body") %>%
    fontsize(size = 8, part = "all") %>%
    theme_zebra()
  
  emR::FitFlextableToPage(ft)
}