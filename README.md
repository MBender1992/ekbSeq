# ekbSeq

`ekbSeq` is an R package of reusable methods for bulk and single-cell RNA-seq
analysis. It supports DESeq2 contrasts, pseudobulk DE, enrichment results,
single-cell diagnostics, marker displays, pseudotime plots and publication
figure export. Biological definitions stay with the projects that use them.

## Quick examples

```r
library(ekbSeq)

# A vector of DESeq2 model coefficients: first group minus second group.
contrast <- make_deseq_contrast(dds,
  group1 = list(c("condition", "treated")),
  group2 = list(c("condition", "control")))

# Separate numeric analysis from visualization and file output.
overlap <- compare_expression_sets(counts, result_table,
  groups = list(control = control_samples, treated = treated_samples))
plot_gene_overlap(overlap$sets)

# Inspect local group mixing in a two-dimensional embedding.
mixing <- calculate_local_mixing(seurat_object,
  group_by = "sample_id", reduction = "umap")

# Compute a threshold independently of its plot.
point <- estimate_pseudotime_threshold(cell_metadata, "Lineage1",
  "MelanocyteScore_UCell", method = "inflection")
plot_pseudotime_score(cell_metadata, "Lineage1", "MelanocyteScore_UCell",
  threshold = point)
```

Functions from older analyses remain available. For example,
`apply_contrasts()` maps to `deseq_contrast()` and `plot_pseudotime()` continues
to attach a `threshold` attribute to the returned plot. Legacy interfaces are
supported for reproducibility and print an informational message only once
per R session. Their older assumptions about specific projects are documented
in the source and are not defaults for the new API.

The source tree contains `SPECIFICATIONS.md` for contracts and scientific
conventions, `NEWS.md` for migration mappings, and `HANDOFF.md` for validation
status. Specialized packages are needed only for functions that use them.
The R package sources include `tests/testthat` with small synthetic fixtures;
the published sequencing data are not needed to run the package tests.

## Source package

After extracting the ZIP, inspect `DESCRIPTION`, install the required R/Bioconductor
dependencies in your own environment, and use a standard R package installation
method of your choice. Integration into a repository and release decisions are
left to the package maintainer.
