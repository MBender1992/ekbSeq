#' Sanity check for NGS data prepared with DESeq
#'
#' This function checks whether the dds, rld, res and allRes object all have the same lengths or if discrepancies are
#' present.
#' @param rdata rdata object containing DESeq results which will be used for the downstream analysis
#' @param deseq.obj DESeq2 object
#' @param rlog.cm rlog transformed count matrix
#' @param all.res annotated results data.frame
#' @param res.df results data.frame
#' @export

sanity_check <- function(rdata, deseq.obj = "dds", rlog.cm = "rld", all.res = "allRes", res.df = "res"){
  if(nrow(assay(eval(parse(text = deseq.obj)))) == nrow(assay(eval(parse(text = rlog.cm)))) &
     nrow(assay(eval(parse(text = deseq.obj)))) == nrow(eval(parse(text = all.res))) &
     nrow(assay(eval(parse(text = deseq.obj)))) == nrow(eval(parse(text = res.df)))){
    print("DESeq object, transformed count matrix and results data.frame all have the same length. Sanity check passed.")
  } else {
    print("One of the objects DEseq, transformed count matrix or results data.frame is different from the others. Sanity check failed.")
  }

  print(rdata)
}

## change function to accept strings and paste strings as standard arguments

