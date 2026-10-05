# ekbSeq

`ekbSeq` is an R package providing reusable analysis and visualization tools for
bulk RNA-sequencing, single-cell RNA-sequencing and related transcriptomic
workflows.

The package was developed to centralize analysis functionality that is repeatedly
used across sequencing projects while keeping project-specific biological logic
inside the corresponding analysis repositories.

`ekbSeq` complements established bioinformatics packages such as DESeq2, Seurat,
clusterProfiler and tradeSeq. It does not attempt to replace their core analysis
frameworks. Instead, it provides reusable higher-level functionality for common
tasks such as contrast construction, visualization, enrichment analysis,
pseudobulk differential expression, clustering diagnostics and trajectory
analysis.

The package is designed around four main principles:

- reusable and project-independent analysis functions;
- explicit and predictable interfaces for new analyses;
- preservation of scientifically relevant behavior;
- backwards compatibility with historical ekbSeq workflows.

---

## Scope

`ekbSeq` currently provides functionality for:

- bulk RNA-seq differential expression workflows;
- DESeq2 contrast construction and result handling;
- transcript-level and gene-level visualization;
- PCA and expression visualization;
- volcano plots and differential-expression heatmaps;
- gene-set overlap analysis;
- Gene Ontology and related enrichment analysis;
- enrichment visualization;
- single-cell quality-control visualization;
- clustering-resolution assessment;
- neighborhood mixing metrics;
- marker visualization and marker analysis;
- pseudobulk differential expression from single-cell data;
- pseudotime threshold estimation and visualization;
- tradeSeq result processing and visualization;
- reusable plot and table export.

The package deliberately does not encode project-specific biological assumptions
such as fixed marker panels, cell-state definitions, donor identifiers,
publication-specific palettes or experiment-specific output paths.

---

## Installation

The development and release versions are hosted on GitHub.

Install the current version using `remotes`:

```r
install.packages("remotes")
remotes::install_github("MBender1992/ekbSeq")
```

or using `devtools`:

```r
install.packages("devtools")
devtools::install_github("MBender1992/ekbSeq")
```

Load the package with:

```r
library(ekbSeq)
```

A specific release can be installed by supplying its Git tag:

```r
remotes::install_github("MBender1992/ekbSeq@v1.1.0")
```

This is recommended when a sequencing analysis needs to remain reproducible
against a defined package version.

---

## Package design

The public API is divided conceptually into two layers.

### Canonical API

The canonical API is the recommended interface for new analyses.

Canonical functions are designed to:

- receive important analysis objects explicitly;
- avoid hidden project-level state;
- expose relevant parameters;
- use predictable return values;
- separate computation, visualization and export where appropriate;
- avoid hard-coded project-specific biology;
- preserve established scientific conventions where possible.

### Legacy compatibility API

Historical ekbSeq functions remain available for reproducibility of existing
analysis scripts.

Where a historical function has been replaced by a cleaner canonical interface,
the historical name is retained as a compatibility function.

Legacy functions are supported interfaces and are not automatically considered
deprecated.

When appropriate, a legacy function emits an informational message indicating
the preferred canonical function for new analyses.

This makes it possible to maintain older analysis projects while using a cleaner
API for new work.

---

## Source organization

The package source is organized by analytical domain:

```text
R/
├── 00_imports.R
├── 00_utils.R
├── 01_io.R
├── 02_export.R
├── 03_bulk_deseq2.R
├── 04_bulk_visualization.R
├── 05_enrichment.R
├── 06_sc_qc_clustering.R
├── 07_sc_markers.R
├── 08_sc_trajectory.R
├── 09_sc_tradeseq.R
├── 10_legacy_frozen.R
├── 11_compatibility_wrappers.R
└── 12_legacy_api.R
```

This organization is intended to make future development straightforward:
new functionality should normally be added to the module corresponding to its
analytical domain rather than creating additional single-function source files.

---

# Bulk RNA-seq

## DESeq2 contrasts

`ekbSeq` provides tools for constructing and evaluating DESeq2 contrasts while
making comparison direction explicit.

The main canonical functions are:

```r
make_deseq_contrast()
deseq_contrast()
```

`make_deseq_contrast()` constructs contrast vectors from the fitted model
matrix.

`deseq_contrast()` provides higher-level extraction of differential-expression
results using an explicitly defined treatment-versus-control comparison.

Historical interfaces remain available:

```r
contraster()
apply_contrasts()
```

For new analyses, the canonical functions are recommended.

### Contrast direction

The orientation of a contrast is scientifically important because it determines
the sign of the reported log2 fold change.

The canonical API therefore treats comparison direction as an explicit part of
the analysis contract.

Analyses should always document which condition represents the numerator and
which represents the reference condition.

---

## Reading count data

Count tables generated by edgeR-compatible workflows can be imported using:

```r
read_edger_counts()
```

The historical interface

```r
read_edgeR_counts()
```

remains available for existing scripts.

---

## PCA

Bulk RNA-seq PCA visualization is provided through:

```r
plot_bulk_pca()
```

The historical interface

```r
pca_plot()
```

is retained for backwards compatibility.

The canonical function is intended for reusable PCA visualization from
transformed expression data while allowing grouping and visualization
parameters to be defined explicitly.

---

## Expression visualization

Several functions support visualization of differential-expression results and
normalized expression data:

```r
plot_bulk_expression()
plot_transcript_distribution()
plot_de_heatmap()
plot_volcano()
```

These cover complementary use cases:

- `plot_bulk_expression()` visualizes expression values for selected genes;
- `plot_transcript_distribution()` summarizes transcript or biotype
  distributions;
- `plot_de_heatmap()` generates differential-expression heatmaps;
- `plot_volcano()` visualizes statistical significance and effect size.

`plot_volcano()` remains part of the stable public API and can be used for both
exploratory analyses and publication-oriented figures.

---

## Comparing differential-expression signatures

Gene sets from multiple comparisons can be evaluated using:

```r
compare_expression_sets()
plot_gene_overlap()
```

These functions separate the computational comparison of expression signatures
from their visualization.

This makes it possible to reuse the resulting overlap tables independently of
the plotting method.

---

# Functional enrichment

`ekbSeq` contains reusable tools for Gene Ontology and related enrichment
workflows.

The main canonical functions are:

```r
extract_significant_genes()
enrich_go_terms()
enrich_go_clusters()
enrich_goseq()
reduce_go_terms()
filter_enrichment_terms()
enrichment_to_tables()
plot_enrichment_bubble()
plot_enrichment_pair()
```

---

## Selecting significant genes

```r
extract_significant_genes()
```

extracts gene sets from differential-expression result tables for downstream
functional analysis.

The function is intended to keep gene selection separate from enrichment and
visualization.

---

## Gene Ontology enrichment

For standard GO enrichment:

```r
enrich_go_terms()
```

For enrichment across several gene clusters or gene sets:

```r
enrich_go_clusters()
```

For RNA-seq enrichment workflows that account for gene-length bias:

```r
enrich_goseq()
```

---

## Reducing redundant GO terms

Highly overlapping GO results can be summarized using:

```r
reduce_go_terms()
```

This provides a reusable redundancy-reduction step that can be applied
independently of downstream visualization.

---

## Enrichment visualization

Generic enrichment result tables can be visualized using:

```r
plot_enrichment_bubble()
plot_enrichment_pair()
```

`plot_enrichment_bubble()` is designed as a reusable visualization layer for
different enrichment sources rather than being tied to a single upstream
package.

`plot_enrichment_pair()` supports paired or directional enrichment
visualizations, for example when comparing upregulated and downregulated gene
sets.

---

# Single-cell RNA-seq

## Quality-control visualization

Single-cell QC distributions can be visualized using:

```r
plot_sc_qc_ridge()
```

The function provides ridge-based visualization of QC metrics across sample or
cell groups.

The historical interface:

```r
custom_RidgePlot()
```

remains supported for older workflows.

---

## Neighborhood mixing

Local mixing of cells from different samples or groups can be quantified using:

```r
calculate_local_mixing()
```

The metric evaluates the composition of nearest-neighbor environments in a
selected dimensionality-reduction space and summarizes local diversity using
inverse Simpson diversity.

The historical interface:

```r
calculate_mixing_metric()
```

remains available for reproducibility.

---

## Clustering-resolution assessment

The effect of different clustering resolutions can be investigated using:

```r
screen_cluster_resolutions()
find_resolution_markers()
```

These functions separate two common steps:

1. generate clustering solutions across several resolutions;
2. evaluate marker structure associated with those solutions.

Historical interfaces are retained:

```r
cluster_resolution_clustering()
cluster_resolution_markers()
```

---

# Marker analysis

## Marker UMAPs

Marker expression on low-dimensional embeddings can be visualized using:

```r
plot_marker_umap()
```

The function supports reusable marker visualization while allowing the object,
features and grouping variables to be supplied explicitly.

The historical interface:

```r
plot_markers_UMAP()
```

remains available.

---

## Marker summaries

Multiple marker signals can be summarized using:

```r
plot_marker_summary()
```

and ordered expression distributions can be generated using:

```r
plot_ordered_violin()
```

These functions are intended for reusable marker characterization without
embedding project-specific marker definitions in the package.

---

## Cluster marker analysis

Cluster-level marker analysis can be performed with:

```r
analyze_cluster_markers()
```

The function provides reusable analysis logic while allowing biological
interpretation and project-specific marker selection to remain in the analysis
repository.

---

## Pseudobulk differential expression

One of the central single-cell analysis functions in `ekbSeq` is:

```r
find_markers_pseudobulk()
```

The function performs marker analysis after aggregation of single-cell counts
into pseudobulk samples.

Pseudobulk analysis is particularly useful when cells originate from multiple
biological replicates and statistical inference should operate at the replicate
rather than individual-cell level.

The exact aggregation variables and model should always reflect the experimental
design.

Because pseudobulk differential expression can directly affect scientific
conclusions, changes to grouping, comparison direction or statistical design
are treated as high-risk changes within the package.

---

# Trajectory and pseudotime analysis

## Pseudotime thresholds

Pseudotime-associated score changes can be evaluated using:

```r
estimate_pseudotime_threshold()
```

The function fits a generalized additive model and derives thresholds from the
shape of the fitted trajectory.

Supported threshold concepts include:

- increasing inflection;
- decreasing or drop point;
- local maximum;
- local minimum.

Visualization is separated into:

```r
plot_pseudotime_score()
```

This allows threshold estimation to be reused independently of plotting.

The historical combined interface:

```r
plot_pseudotime()
```

remains available for reproducibility.

---

## Diffusion-space visualization

Three-dimensional diffusion embeddings can be visualized using:

```r
plot_3d_diffusion()
```

This function remains part of the stable public API.

---

# tradeSeq workflows

`ekbSeq` includes helper functions for working with trajectory-associated
tradeSeq results.

The primary canonical functions are:

```r
enrich_tradeseq()
plot_tradeseq_patterns()
```

`enrich_tradeseq()` connects tradeSeq-derived gene sets with functional
enrichment analysis.

`plot_tradeseq_patterns()` visualizes expression patterns across pseudotime or
lineages.

Historical functions remain available where required by older analysis scripts.

---

# Export utilities

Plots and tables can be exported using reusable package utilities.

## Plot export

```r
export_plot_dual()
```

supports export of plotting objects to both raster and vector formats and is
used across bulk and single-cell workflows.

## Excel export

```r
export_excel()
```

writes named result tables to Excel workbooks.

These utilities are intended to keep file-export logic outside individual
analysis functions wherever possible.

---

# Canonical and historical function names

Some historical functions have clearer canonical replacements.

| Historical interface | Recommended interface |
| --- | --- |
| `contraster()` | `make_deseq_contrast()` |
| `apply_contrasts()` | `deseq_contrast()` |
| `read_edgeR_counts()` | `read_edger_counts()` |
| `plot_transcript_dist()` | `plot_transcript_distribution()` |
| `pca_plot()` | `plot_bulk_pca()` |
| `list_signif_genes()` | `extract_significant_genes()` |
| `reduce_go_rrvgo()` | `reduce_go_terms()` |
| `calculate_mixing_metric()` | `calculate_local_mixing()` |
| `cluster_resolution_clustering()` | `screen_cluster_resolutions()` |
| `cluster_resolution_markers()` | `find_resolution_markers()` |
| `plot_markers_UMAP()` | `plot_marker_umap()` |
| `plot_combined_markers()` | `plot_marker_summary()` |
| `plot_pseudotime()` | `estimate_pseudotime_threshold()` + `plot_pseudotime_score()` |
| `prepare_go_df_list()` | `enrichment_to_tables()` |
| `plot_pattern_clusters_tradeseq()` | `plot_tradeseq_patterns()` |

Historical interfaces remain available where required for reproducibility.

New projects should preferentially use the canonical interface.

---

# What intentionally remains outside ekbSeq

`ekbSeq` is intended to provide reusable analytical infrastructure rather than
complete project-specific pipelines.

The following should normally remain in individual project repositories:

- sample-specific metadata preparation;
- experimental condition definitions;
- fixed donor lists;
- project-specific marker panels;
- manuscript-specific cell-state nomenclature;
- publication-specific figure assembly;
- hard-coded cell-type orders;
- project-specific color schemes;
- absolute input or output paths;
- experiment-specific CellChat assumptions;
- manuscript-specific workflow orchestration.

For example, a project may use `plot_marker_umap()` from `ekbSeq`, while the
choice of markers, cell populations, colors and panel arrangement remains in the
project analysis script.

This separation makes the package reusable without embedding the biology of one
specific study into general-purpose functions.

---

# Reproducibility

For reproducible sequencing analyses, the package version should be recorded
together with the analysis.

For example:

```r
packageVersion("ekbSeq")
sessionInfo()
```

For publication analyses, it is recommended to record:

- the `ekbSeq` release or Git commit;
- the R version;
- relevant upstream package versions;
- random seeds where applicable;
- analysis parameters;
- contrast definitions;
- filtering criteria;
- sample exclusions;
- relevant project-specific metadata decisions.

Git tags are retained so that analyses can be reconstructed using earlier
package versions when necessary.

---

# Testing and validation

`ekbSeq` uses `testthat` for automated package testing.

The package test suite is designed around small synthetic fixtures rather than
private sequencing datasets.

Tests cover areas including:

- DESeq2 contrast construction;
- expression-set comparison;
- export utilities;
- enrichment visualization;
- single-cell QC and clustering;
- marker analysis;
- trajectory analysis;
- tradeSeq visualization;
- legacy compatibility behavior.

Historical compatibility is treated separately from canonical API quality.

For scientifically sensitive functions, synthetic package tests may be
supplemented by targeted validation against established real-data results,
previous package releases or frozen historical implementations.

This is particularly relevant for functions where small changes could alter:

- contrast direction;
- log2 fold-change signs;
- clustering behavior;
- pseudobulk aggregation;
- pseudotime thresholds;
- enrichment direction;
- plot interpretation.

---

# Development

For package development, clone the repository and install the required
development tools.

```r
install.packages(c("devtools", "roxygen2"))
```

From the package root:

```r
devtools::document()
devtools::test()
devtools::check()
```

The package should also be tested after installation:

```r
devtools::install()
library(ekbSeq)
```

`NAMESPACE` and the documentation under `man/` are generated through roxygen2
and should not normally be edited manually.

New functions should be added to the appropriate analytical module.

For example:

```text
bulk DESeq2 logic          -> 03_bulk_deseq2.R
bulk plots                 -> 04_bulk_visualization.R
enrichment                 -> 05_enrichment.R
single-cell QC/clustering  -> 06_sc_qc_clustering.R
single-cell markers        -> 07_sc_markers.R
trajectory analysis        -> 08_sc_trajectory.R
tradeSeq                   -> 09_sc_tradeseq.R
```

Legacy compatibility code should remain separated from the canonical
implementation.

---

# Coding principles

Contributions should aim for:

- small, focused functions;
- explicit inputs;
- predictable outputs;
- namespace-qualified dependency calls;
- informative validation errors;
- limited hidden state;
- reproducible statistical conventions;
- minimal project-specific assumptions;
- compatibility with established analysis workflows.

Avoid introducing new abstraction layers unless they remove genuine duplication
or improve the scientific interface.

Standard R objects such as vectors, data frames, lists and ggplot objects are
preferred where suitable.

---

# Documentation

Individual functions are documented through standard R help pages.

For example:

```r
?deseq_contrast
?plot_volcano
?find_markers_pseudobulk
?estimate_pseudotime_threshold
```

Additional package-level information is available in:

- `README.md` — package overview and entry point;
- `SPECIFICATIONS.md` — architecture, API and reproducibility principles;
- `NEWS.md` — release history and API changes.

---

# Reporting issues

Bug reports and feature requests can be submitted through GitHub Issues:

https://github.com/MBender1992/ekbSeq/issues

A useful bug report should preferably contain:

- a minimal reproducible example;
- synthetic or publicly shareable data;
- the complete error or warning message;
- `sessionInfo()`;
- the installed `ekbSeq` version.

Do not upload confidential or identifiable research or patient data to public
issues.

---

# License

`ekbSeq` is distributed under the license specified in the package repository.

See the `LICENSE` file for details.
