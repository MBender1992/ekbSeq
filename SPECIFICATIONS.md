# ekbSeq package specifications

## Scope

`ekbSeq` provides reusable functions for bulk and single-cell transcriptomics
analysis in R.

The package is intended to centralize analysis logic that is reused across
sequencing projects while keeping project-specific biological assumptions,
sample definitions, marker panels, paths and manuscript workflows inside the
corresponding project repositories.

The package contains two public API layers:

1. the canonical API for new analyses;
2. the legacy compatibility API for reproducibility of historical workflows.

The canonical API is the recommended interface for new projects.

Legacy interfaces remain supported where they are required to reproduce
existing analyses. They are compatibility interfaces and are not considered
deprecated solely because a canonical replacement exists.


## Source architecture

The package source is organized by analytical domain:

```text
R/
+-- 00_imports.R
+-- 00_utils.R
+-- 01_io.R
+-- 02_export.R
+-- 03_bulk_deseq2.R
+-- 04_bulk_visualization.R
+-- 05_enrichment.R
+-- 06_sc_qc_clustering.R
+-- 07_sc_markers.R
+-- 08_sc_trajectory.R
+-- 09_sc_tradeseq.R
+-- 10_legacy_frozen_plots.R
+-- 11_compatibility_wrappers.R
+-- 12_legacy_api.R