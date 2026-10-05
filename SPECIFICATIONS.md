# ekbSeq 1.0 architecture

ekbSeq provides reusable operations for bulk and single-cell transcriptomics.
Canonical functions separate computation, plotting and writing. Existing public
functions remain supported for reproducibility; new functions take explicit
objects and parameters. A legacy message is informational and appears once per
function per R session.

## Source layout

`00_utils.R` contains validation and compatibility state; `01_io.R` and
`02_export.R` handle input and output; `03_bulk_deseq2.R` and
`04_bulk_visualization.R` contain bulk tools; `05_enrichment.R` contains shared
enrichment code; `06_sc_qc_clustering.R`, `07_sc_markers.R`,
`08_sc_trajectory.R` and `09_sc_tradeseq.R` contain single-cell tools;
`11_compatibility_wrappers.R` retains historical project call styles.
Preexisting source files remain available, including publication-critical
`find_markers_pseudobulk.R`. CellChat project helpers are intentionally absent
from the public API.

## Scientific conventions

- `make_deseq_contrast()` uses group 1 minus group 2 model-matrix columns;
  `deseq_contrast()` uses treatment relative to control. Unweighted designs
  deduplicate rows before taking group means, following `contraster()`.
- `find_markers_pseudobulk()` retains the original sample aggregation, design,
  sample filtering and fold-change direction. Inspect its function documentation
  before choosing sample and identity groups.
- `calculate_local_mixing()` uses k nearest neighbors in the first **two**
  coordinates of the selected embedding and averages inverse Simpson diversity.
- `estimate_pseudotime_threshold()` matches the original GAM prediction grid:
  the largest positive derivative for `inflection`, most negative derivative
  for `drop`, and nth sign-change extremum for `maximum`/`minimum`, falling back
  to the global extremum. `plot_pseudotime()` attaches the threshold to the plot.
- `analyze_cluster_markers()` ranks on log2FC; the legacy combined
  `cluster_marker_go_analysis()` keeps its historical first-n input ordering.
- New canonical functions do not encode UVDHDS donors, cell states, marker
  panels, palettes, lineage definitions, time points or paths.

## Dependencies and returns

Small utilities work without Seurat and CellChat. Seurat, scCustomize, plotly,
ComplexHeatmap and other specialized plotting libraries are suggested rather
than attached by default, with checks at call sites. Canonical computations
return standard numeric vectors, lists or data frames; plot functions return
ggplot/ComplexHeatmap objects. `export_excel()` returns the path invisibly.
Legacy cluster screening returns `seurat_obj_clusters`, `res_cols`,
`umap_plot`, `clustree_plot`; combined marker plus GO returns `heatmap`,
`top_markers`, `go_results`, `go_results_simplified`, `go_plotlist`.

## Test strategy

Tests use synthetic count matrices, small data frames, optional tiny Seurat
objects and temporary output directories. No private FASTQ or UVDHDS saved
objects are loaded. Numerical parity with the real publication checkpoints is
intentionally left for the project validation stage. Changes to high-risk
statistics require a separate baseline comparison before release.

## Project scope

The published UVDHDS scripts continue to define biology, CellChat comparisons,
CellRank and fate labels. `run_tradeSeq_pairwise_plots()` and the local CellChat
functions remain in the project; ekbSeq does not wrap ordinary Seurat,
DESeq2 or tradeSeq preprocessing calls merely for branding.
