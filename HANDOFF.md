# Handoff: ekbSeq source candidate

The packaged version is **0.99.0**, a candidate for 1.0.0 after the complete
test suite, `R CMD check`, roxygen generation and separate real-data parity
review. R 4.3.3 was installed in the temporary execution environment after
the first source archive was made. The package itself was not installed.

## Changes in this source tree

- Added focused source modules for input, Excel export, DESeq2 contrasts,
  expression overlap, bulk plots, enrichment, single-cell diagnostics,
  markers, pseudotime and tradeSeq plots.
- Added `11_compatibility_wrappers.R` for historical project call names;
  existing published functions remain in their original source files.
- Kept `find_markers_pseudobulk()` and `export_plot_dual()` in their baseline
  source files, without rewriting their publication-critical calculation or
  file-output behavior.
- Moved Seurat, SeuratObject, scCustomize and plotly from required Imports to
  Suggests. Core tools continue to use DESeq2, ggplot2 and other historical
  dependencies listed in DESCRIPTION.
- Added eight groups of synthetic tests, this handoff, API documentation, a
  migration summary and a validation template under `inst/validation/`.
- `man/ekbSeq-v1.Rd` provides aliases for new public names; full roxygen
  manual pages still need generation and review with all imports installed.

## Main API mappings

| Legacy | Canonical |
| --- | --- |
| `contraster` | `make_deseq_contrast` |
| `apply_contrasts` | `deseq_contrast` |
| `read_edgeR_counts` | `read_edger_counts` |
| `plot_transcript_dist` | `plot_transcript_distribution` |
| `extract_and_plot_venn` | `compare_expression_sets` + `plot_gene_overlap` |
| `calculate_mixing_metric` | `calculate_local_mixing` |
| `cluster_resolution_clustering` | `screen_cluster_resolutions` |
| `cluster_resolution_markers` | `find_resolution_markers` |
| `plot_markers_UMAP` | `plot_marker_umap` |
| `plot_combined_markers` | `plot_marker_summary` |
| `plot_pseudotime` | `estimate_pseudotime_threshold` + `plot_pseudotime_score` |
| `plot_pattern_clusters_tradeseq` | `plot_tradeseq_patterns` |
| `prepare_go_df_list` | `enrichment_to_tables` |

The existing `plot_volcano`, `plot_3d_diffusion`, `normalize_svg_text`,
`export_plot_dual` and `find_markers_pseudobulk` remain public. The older GO,
tradeSeq and word-cloud functions remain available as historical interfaces.
CellChat temporal comparison helpers, publication-specific marker panels and
`run_tradeSeq_pairwise_plots()` remain project-local.

## Validation performed

- Checked all R source and test files for balanced braces, brackets, strings
  and parentheses with a static scanner (this is **not** an R parse check).
- Verified that every NAMESPACE export has a corresponding function definition
  and that no public function name is defined twice.
- Searched canonical modules for project-specific biological labels and local
  absolute paths; none were found.
- Confirmed no active `library()` or `require()` calls in package R sources.
- Parsed all 40 R source and test files with R; parsed all 20 Rd files with
  `tools::parse_Rd()`; both checks passed.
- `R CMD build ekbSeq` succeeded and created `ekbSeq_0.99.0.tar.gz` locally.
- Sourced the relevant modules and ran individual `testthat::test_file()` tests
  for synthetic DESeq2 contrast orientation, overlap, export (Excel and SVG/PNG),
  enrichment, marker ranking, tradeSeq pattern plotting and pseudotime.
  These tests passed in source mode with the dependencies available here.

**Blocked:** `testthat::test_local()` could not load the package because 24
required imports were unavailable. `R CMD check --no-manual` reached dependency
validation and reported the same missing imports, plus unavailable optional
packages; it did not execute the package checks. The compatibility test that
inspects `asNamespace("ekbSeq")` could not run in source mode. Roxygen2 was
installed but `roxygenise()` was not run because loading the package requires
the missing imports. Package installation, optional dependency integration,
the full test suite and real-data comparisons remain unverified.

## Known compatibility limitations to resolve before 1.0.0

- The older project plot wrappers preserve their call signatures and key
  return classes/list names but do not reproduce every original color, label,
  panel layout or significance annotation. In particular,
  `plot_control_expression_comparison()` and `plot_biotype_heatmap()` require
  closer review of their complete historical visual contracts.
- The Excel and image exporters passed small local PNG/SVG/XLSX tests, but
  target graphics devices and project plots still need inspection.
- Pseudotime numerical logic was transcribed from the supplied helper and
  requires comparison to the original function on synthetic and saved data.
- The published baseline `find_markers_pseudobulk()` was retained, but its
  proposed synthetic parity test still needs to be added and executed.
- More synthetic checks are needed for DESeq2 shrinkage, interaction design,
  cluster marker schemas and both CellChat-free visualization code paths.
- Optional dependency metadata and generated documentation require `R CMD
  check` and roxygen2 review. No green status should be inferred.

## Suggested local commands when R is available

Run these on the extracted source in an environment with the listed
dependencies, before installing it for project work:

```r
roxygen2::roxygenise("ekbSeq")
testthat::test_local("ekbSeq")
```

At a shell with R installed: `R CMD build ekbSeq` followed by `R CMD check`
on the built tarball. Compare the original and new functions against stored
project checkpoints: donor-level DLR pseudobulk genes and signed log2FC,
pseudotime thresholds, resolution metadata/marker tables, and old/new figure
objects. The template in `inst/validation/validation_real_data.R` is opt-in and
requires objects supplied by the project; no private paths or data are bundled.
