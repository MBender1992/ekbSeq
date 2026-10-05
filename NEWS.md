# ekbSeq 1.1.0

## Package structure

- Consolidated the package source into domain-specific modules for I/O, export,
  bulk RNA-seq, enrichment, single-cell QC and clustering, marker analysis,
  trajectory analysis and tradeSeq workflows.
- Added `00_imports.R` as the central roxygen2 namespace declaration file.
- Consolidated historical API implementations into `12_legacy_api.R`.
- Retained frozen historical implementations separately from public
  compatibility wrappers.
- Integrated functions that remain part of the current API into their
  corresponding domain modules instead of maintaining separate one-function
  source files.
- The local `R/archive/` directory is no longer part of the version-controlled
  package source.

## Namespace and package maintenance

- Replaced broad package imports where appropriate with explicit imports to
  reduce namespace conflicts during package loading.
- Resolved namespace conflicts involving functions imported from
  `SummarizedExperiment`, `flextable`, `enrichplot` and `ggpubr`.
- Updated line-width arguments for current ggplot2 versions by replacing the
  deprecated `size` aesthetic for lines with `linewidth`.
- Cleaned package metadata and source organization without intentionally
  changing validated analytical behavior.

## API compatibility

The canonical API remains the recommended interface for new analyses.
Historical function names remain supported for reproducibility and are not
deprecated.

Legacy compatibility interfaces retain their historical argument conventions,
return structures and, where relevant, plotting behavior. Informational legacy
messages are emitted once per function per R session.

No deliberate breaking changes to the public API were introduced in 1.1.0.

# ekbSeq 1.0.0

First stable release following the major package refactor.

The 1.0 series introduced a modular package architecture, explicit canonical
APIs and a backwards-compatibility layer for historical ekbSeq and project
workflows.

Core goals of the refactor were:

- explicit function inputs and predictable return values;
- separation of computation, visualization and export where appropriate;
- preservation of scientifically relevant historical behavior;
- removal of project-specific biological assumptions from canonical APIs;
- compatibility with established UVDHDS and bulk RNA-seq workflows;
- synthetic automated tests complemented by real-data compatibility
  validation.

Major API mappings introduced with the v1 refactor include:

| Historical call | Preferred API |
| --- | --- |
| `contraster()` | `make_deseq_contrast()` |
| `apply_contrasts()` | `deseq_contrast()` |
| `plot_transcript_dist()` | `plot_transcript_distribution()` |
| `read_edgeR_counts()` | `read_edger_counts()` |
| `extract_and_plot_venn()` | `compare_expression_sets()` and `plot_gene_overlap()` |
| `calculate_mixing_metric()` | `calculate_local_mixing()` |
| `cluster_resolution_clustering()` | `screen_cluster_resolutions()` |
| `cluster_resolution_markers()` | `find_resolution_markers()` |
| `plot_markers_UMAP()` | `plot_marker_umap()` |
| `plot_pseudotime()` | `estimate_pseudotime_threshold()` and `plot_pseudotime_score()` |
| `prepare_go_df_list()` | `enrichment_to_tables()` |
| `plot_pattern_clusters_tradeseq()` | `plot_tradeseq_patterns()` |

Legacy names remain available where required for historical reproducibility.