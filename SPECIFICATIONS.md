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
├── 10_legacy_frozen_plots.R
├── 11_compatibility_wrappers.R
└── 12_legacy_api.R
```

`00_imports.R` contains package-level roxygen2 import declarations.

`00_utils.R` contains internal validation, shared utility functions and
compatibility state.

Modules `01` through `09` contain the canonical implementation grouped by
analysis domain.

`10_legacy_frozen_plots.R` contains frozen historical implementations that are
required when delegation to a canonical implementation would not reproduce the
historical behavior closely enough. Functions in this layer are internal and
must not be exported.

`11_compatibility_wrappers.R` contains public historical function names that
delegate to frozen or canonical implementations while preserving historical
call conventions.

`12_legacy_api.R` contains retained historical public functions for which the
original implementation remains the appropriate compatibility interface.

The local `R/archive/` directory may be used during development to retain
historical source material, but it is not part of the version-controlled or
distributed package source.

## API philosophy

Canonical functions should:

- accept important objects explicitly;
- avoid dependence on hidden variables in the calling environment;
- expose scientifically relevant parameters;
- use predictable argument names and return values;
- avoid project-specific paths, donor identifiers, marker panels or cell-state definitions;
- separate computation, visualization and file export where this improves reuse;
- retain established computational behavior as the default when generalization does not require a scientific change.

For direct DESeq2 factor comparisons, the statistical contrast must be defined by
DESeq2 itself. `ekbSeq` must not infer or reconstruct a biological contrast from
the observed covariate composition of treatment and control groups.

The package should not create wrappers around standard upstream functions merely
for naming consistency. An ekbSeq function should provide reusable analysis
logic, harmonization, validation, visualization or workflow behavior beyond a
direct call to the upstream package.

## Legacy compatibility

Historical ekbSeq functions and selected historical project helper functions are
retained when required for reproducibility.

Compatibility functions may preserve behavior that would not be selected for a
new canonical API, including historical argument names, implicit-object
resolution, return-list names, plotting defaults and file-writing behavior.

`contraster()` and `apply_contrasts()` specifically retain the historical
observed-design-profile contrast behavior. This behavior is preserved for
historical reproducibility and is not used by the canonical `deseq_contrast()`
interface.

Legacy interfaces may emit an informational message identifying the preferred
canonical replacement where one exists. Such messages are informational rather
than deprecation warnings and should normally appear only once per function per
R session.

Frozen internal helpers use names such as `.legacy_frozen_*`. They must remain
internal and must not appear in the exported namespace.

Changes to legacy implementations should be conservative. Scientific or visual
behavior should not be altered merely for stylistic consistency with the
canonical API.

## Scientific invariants

Scientific reproducibility takes precedence over internal code aesthetics.

### Differential expression

For a standard factor comparison, `deseq_contrast()` delegates the statistical
contrast definition directly to DESeq2 using:

```r
DESeq2::results(
  dds,
  contrast = c(condition, treatment, control)
)
```

Positive log2 fold changes therefore represent treatment relative to control.

In additive designs such as:

```r
~ batch + condition
```

or:

```r
~ processing + condition
```

additional model terms remain adjustment variables. They are not added to the
biological contrast because treatment and control happen to contain different
observed proportions of batch, processing or other covariate levels.

`deseq_contrast()` is intended for direct factor-level comparisons. Interaction
effects, difference-in-differences and other complex scientific contrasts should
be defined explicitly with `DESeq2::results()` using the coefficient or contrast
appropriate to the experimental question. `ekbSeq` does not attempt to infer the
meaning of complex experimental designs automatically.

When `shrink = TRUE`, `deseq_contrast()` first generates the direct DESeq2 factor
contrast and then applies `ashr` shrinkage to that exact `DESeqResults` object.
The shrunken effect therefore represents the same biological comparison as the
unshrunken result.

The historical `contraster()` and `apply_contrasts()` interfaces deliberately
retain their established observed-design-profile behavior for reproducibility.
They must not be used internally to define canonical factor comparisons.

Changes affecting contrast orientation, coefficient construction, log2 fold
change direction, statistical tests or multiple-testing results require explicit
validation.

### Pseudobulk analysis

`find_markers_pseudobulk()` retains the established pseudobulk aggregation,
sample filtering, design construction and fold-change orientation.

Changes to aggregation units, donor handling, comparison direction or
statistical design are considered scientifically relevant changes and require
dedicated validation.

### Single-cell neighborhood mixing

`calculate_local_mixing()` computes local mixing from nearest neighbors in the
selected embedding and summarizes local diversity using inverse Simpson
diversity.

Changes to the embedding dimensions, neighbor definition or diversity metric
are considered behavioral changes.

### Pseudotime thresholds

`estimate_pseudotime_threshold()` retains the established GAM-based threshold
logic.

The supported threshold definitions include the largest positive derivative for
`inflection`, the most negative derivative for `drop`, and extrema based on
derivative sign changes for `maximum` and `minimum`, with the established
fallback behavior.

The legacy `plot_pseudotime()` interface preserves the corresponding historical
plot behavior and threshold contract.

### Marker analysis

Canonical marker-analysis functions may provide explicit ranking and selection
rules.

Historical marker functions retain their historical ordering and selection
behavior where existing analyses depend on it.

## Return contracts

Canonical computation functions should return standard R objects whenever
possible, primarily data frames, vectors and named lists.

Visualization functions should return plot objects rather than write files
implicitly unless file output is an explicit part of the function contract.

Export functions may write files and should return the generated path or output
object invisibly where appropriate.

Public return structures that are used by existing analysis scripts should not
be changed without a deliberate API change.

Examples of historical contracts that must remain stable include named list
elements used by clustering, marker and enrichment workflows.

## Dependencies

Dependencies should be declared explicitly.

Package code should not call `library()` or `require()` inside exported
functions.

External functions should either be namespace-qualified or imported explicitly
through the package namespace.

Broad imports should be avoided when narrower imports are practical,
particularly when packages export common function names that can create
namespace conflicts.

Dependencies required only by specialized functionality may be handled
conditionally where doing so does not make core package behavior unreliable or
unnecessarily complex.

Dependency cleanup must not change the scientific implementation of established
functions merely to reduce the number of package dependencies.

## Project-specific functionality

Project-specific biology remains outside the canonical ekbSeq API.

This includes, for example:

- fixed donor or sample identifiers;
- UVDHDS-specific cell states;
- publication-specific marker panels;
- fixed lineage definitions;
- manuscript-specific palettes;
- project-specific output paths and filenames;
- CellChat receiver or sender definitions tied to a specific experiment;
- project orchestration workflows that combine multiple analysis stages.

Such logic belongs in the corresponding project repository.

General functionality should only be moved into ekbSeq when it is reusable
across projects and can be expressed without embedding project-specific
biological assumptions.

## Documentation

Exported functions should use roxygen2 documentation and document:

- purpose;
- arguments;
- return value;
- relevant statistical behavior;
- important assumptions;
- scientifically meaningful direction or threshold conventions where applicable.

The canonical and legacy interfaces should be distinguishable in the
documentation.

Documentation for a legacy interface should identify the preferred canonical
replacement where one exists, while making clear that the legacy interface
remains supported for reproducibility.

## Testing strategy

Automated package tests should use small synthetic fixtures and must not depend
on private sequencing datasets.

Suitable fixtures include:

- small count matrices;
- small DESeq2 datasets;
- small data frames;
- small Seurat objects;
- deterministic random seeds;
- temporary files and directories.

Tests should verify numerical behavior, object contracts, important plot
structure, file side effects and historical call patterns where relevant.

For canonical DESeq2 factor comparisons, tests must compare `deseq_contrast()`
directly against `DESeq2::results(..., contrast = c(...))` in both balanced and
covariate-imbalanced additive designs. Shrinkage tests must confirm that `ashr`
operates on the same direct factor contrast. Separate compatibility tests must
confirm that `contraster()` and `apply_contrasts()` retain their frozen
historical behavior.

Compatibility tests should include real historical calling conventions rather
than testing only idealized canonical calls.

Large private datasets and publication analysis objects are not part of the
package test suite.

## Real-data compatibility validation

Automated synthetic tests complement, but do not replace, targeted validation
against established project results when scientifically sensitive functions
are changed.

For high-risk functions, validation may include comparison against:

- previous released ekbSeq versions;
- frozen historical implementations;
- established project checkpoints;
- known numerical output tables;
- representative plots generated from real project data.

Changes to high-risk scientific behavior require explicit review before release.

## Release discipline

Patch releases should primarily contain backwards-compatible bug fixes and
maintenance changes.

Minor releases may add new canonical functionality or reorganize package
internals while preserving established analysis contracts.

Before release, the package should pass:

```r
devtools::document()
devtools::test()
devtools::check()
```

The installed package should also load cleanly without avoidable namespace
warnings.

Version tags should be retained to support reproducibility of analyses that
depend on earlier package states.
