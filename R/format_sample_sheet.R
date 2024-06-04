#' Save workspace and significant sig.res table from NGS analysis
#'
#' @param sample.sheet csv sample sheet from the Illumina NextSeq 1000 sequencing platform.
#' @export

## define as ekbSeq function
format_sample_sheet <- function(sample.sheet){
  ## replace some values with \n for newlines
  sample.sheet$Value2 <- str_replace_all(sample.sheet$Value2, "\\_", "\\_\n")
  sample.sheet$Value2 <- str_replace(sample.sheet$Value2, "\\_\n", "\\_")
  sample.sheet$Value3 <- str_replace(sample.sheet$Value3, "Stran", "Stran-\n")
  sample.sheet$Value3 <- str_replace(sample.sheet$Value3, "Prep", "Prep-\n")
  sample.sheet$Value4 <- str_replace(sample.sheet$Value4, "AUDI", "AUDI\n")
  sample.sheet$Value4 <- str_replace(sample.sheet$Value4, "Adapter", "Adapter-\n")
  sample.sheet
}


