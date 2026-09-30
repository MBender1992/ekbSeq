# Template for a separate, opt-in comparison on user-supplied checkpoints.
# Run in a private project environment after package installation.
# The package test suite never runs this file.

compare_pseudotime_thresholds <- function(metadata, pseudotime_col, score_col,
                                           legacy_plot_function, method) {
  old_plot <- legacy_plot_function(metadata, pseudotime_col,
                                   score_col = score_col, method = method)
  old <- attr(old_plot, "threshold")
  current <- ekbSeq::estimate_pseudotime_threshold(
    metadata, pseudotime_col, score_col, method = method)
  data.frame(method = method, old_threshold = old, new_threshold = current,
             difference = current - old)
}

# For pseudobulk DE, compare the old/current results by gene and identity:
#   gene, comparison, log2FoldChange, pvalue, padj, pct.1 and pct.2.
# For cluster screening, compare the resolution column names, factors and
# `seurat_obj_clusters` identities on the same reduced object.
# Never check these results against a reconstructed analysis of raw reads.
