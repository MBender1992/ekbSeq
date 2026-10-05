# ekbSeq 0.99.0: backward compatibility audit

## Scope and evidence

This correction starts from the **later 0.99.0 source candidate**, retaining its
previous small pseudotime dependency and test-font corrections. The user's
comparison was made with the **earlier** ZIP; none of its findings are treated
as evidence about the later ZIP without checking the current source. Sources
compared: `ekbSeq_baseline/R` (pre-refactor 0.5.2 source), the supplied
`UV-DHDS_functions_scRNAseq.R`, `UV-DHDS_functions.R`, `functions.R`, the
UVDHDS analysis scripts, `ekbSeq_v1_migration_map.md`, and this package's
0.99.0 source. The two bulk helper files differ in the default bubble accent:
the UVDHDS version uses `#4292C6`, while `functions.R` uses `#CD534CFF`.
The wrapper retains the UVDHDS version's default. Callers of the other project
helper should explicitly pass their original `color_scale`.

Statuses describe **the initial 0.99.0 candidate**: **1** = historical API
source-compatible; **2** = canonical generalization appropriate; **3** = legacy
contract violation identified (targeted correction applied); **4** = canonical
drift identified (targeted correction applied); **5** = input/test mismatch;
**6** = project-specific and intentionally outside the package. A corrected
source implementation is **not** a claim of image identity: no saved UVDHDS
plotting objects or a working R environment were available for runtime visual
comparison in this session. “Visual changed” below means the 0.99.0 source
was changed to follow the historical plot, not that a pixel comparison passed.

## Public baseline 0.5.2 functions

The “historical source” in this table is the corresponding
`ekbSeq_baseline/R/<name>.R`; exports, formals and body changes were compared
against the current same-named source file and any wrappers in
`R/11_compatibility_wrappers.R`. “Real” means project checkpoint parity.

| Historical function | Canonical function | Historical source | Status | Mismatch found | Change made in this audit | Scientific/numerical changed? | Visual changed? | Remaining validation |
|---|---|---|---|---|---|---|---|---|
| `apply_contrasts` | `deseq_contrast` | baseline | 1 | Previously wrapped; contrast sign already tested | None | No | No | Real DE and shrinkage checkpoint |
| `contraster` | `make_deseq_contrast` | baseline | 1 | Previously wrapped; tested contrast direction | None | No | No | Real interaction design |
| `do_enrichment_tradeSeq` | `enrich_tradeseq` | baseline | 1 | Baseline body differs only by informational message | None | No | No | Enrichment database run |
| `export_plot_dual` | same | baseline | 1 | Byte-identical implementation | None | No | No | Real graphics devices |
| `find_markers_pseudobulk` | same | baseline | 1 | Byte-identical publication-critical implementation | None | No | No | Real donor-level checkpoint |
| `goseq_to_revigo` | `enrich_goseq` | baseline | 1 | Baseline body differs only by informational message | None | No | No | Real GO database run |
| `high_dispersion_genes` | legacy only | baseline | 1 | Byte-identical implementation | None | No | No | Old DESeq2 fixture |
| `ipa_bubble_plot` | `plot_enrichment_bubble` | baseline | 1, 4 | Legacy body differs only by message; canonical lacked reversed scale and negative-score ordering | Corrected canonical scale, order, labels and theme | No | Yes | Real IPA figures |
| `list_signif_genes` | `extract_significant_genes` | baseline | 1 | Baseline body differs only by message | None | No | No | Multiple-table fixture |
| `normalize_svg_text` | same | baseline | 1 | Byte-identical implementation | None | No | No | SVG export inspection |
| `pca_plot` | `plot_bulk_pca` | baseline | 1, 4 | Legacy body differs only by message; canonical palette/theme differed | Restored historical grammar in canonical; optional aesthetics remain explicit | No | Yes | PCA figure comparison |
| `plot_3d_diffusion` | same | baseline | 1 | Byte-identical implementation | None | No | No | Interactive object with saved data |
| `plot_go` | `enrich_go_terms` | baseline | 1 | Baseline body differs only by message; monolithic legacy workflow kept | None | No | No | Real GO visualization |
| `plot_go_clusters` | `enrich_go_clusters` | baseline | 1 | Baseline body differs only by message | None | No | No | Real multi-cluster GO |
| `plot_tradeSeq_GO` | `enrich_tradeseq` / `plot_enrichment_pair` | baseline | 1 | Baseline body differs only by message | None | No | No | Real tradeSeq GO figures |
| `plot_volcano` | same | baseline | 1 | Pre-existing finite-value and symmetric-axis bug fixes; permitted in migration map | None | No | No | Old/new plot after axis adjustment |
| `plot_wordcloud` | legacy only | baseline | 1 | Baseline body differs only by message | None | No | No | Wordcloud device availability |
| `reduce_go_rrvgo` | `reduce_go_terms` | baseline | 1 | Baseline body differs only by message | None | No | No | Real rrvgo checkpoint |
| `search_go` | `filter_enrichment_terms` | baseline | 1 | Baseline body differs only by message | None | No | No | Original CSV and plot output |

## Project helper functions preserved as legacy API

Source abbreviations: **sc** = supplied `UV-DHDS_functions_scRNAseq.R`;
**bulk** = supplied `UV-DHDS_functions.R` (also compared to `functions.R`).
The code of project-only CellChat helpers and unused marker-density helpers
was checked against the migration map and remains in the project (status 6).

| Historical function | Canonical function | Historical source | Status | Mismatch found in initial 0.99.0 | Change made in this audit | Scientific/numerical changed? | Visual changed? | Remaining validation |
|---|---|---|---|---|---|---|---|---|
| `read_edgeR_counts` | `read_edger_counts` | bulk | 1, 2 | Prior synthetic parity; no new mismatch | None | No | No | Real edgeR CSV |
| `plot_transcript_dist` | `plot_transcript_distribution` | bulk | 3, 4 | Bar order, palette, base font, label nudge and legend differed | Frozen historical wrapper and historical canonical grammar | No | Yes | Device comparison |
| `extract_and_plot_venn` | `compare_expression_sets` / `plot_gene_overlap` | bulk | 3, 4 | Historic fill/layout and caption lost; empty exclusive set CSV failed | Restored diagram parameters/caption; zero-row CSV fix | No | Yes | Real Venn + CSV comparison |
| `plot_biotype_heatmap` | `plot_de_heatmap` | bulk | 3, 4 | Cell/donor annotation, Set1/NPG/RdYlBu colours, row scaling, labels, clustering and legend lost | Frozen ComplexHeatmap implementation; configurable canonical historical grammar | No | Yes | Same genes/settings; compare Heatmap slots and rendered files |
| `plot_control_expression_comparison` | `plot_bulk_expression` | bulk | 3, 4 | Symbol mapping, sample handling, stars/padj, per-gene/merged layout, jitter and theme lost | Frozen original function with namespace-qualified calls; generalized canonical matching grammar | No | Yes | Real VST metadata and plot comparison |
| `bubble_plot_clusterprofiler_style` | `plot_enrichment_bubble` | bulk | 3, 4 | Colour transform, score reversal and title/layout differed | Frozen original UVDHDS bubble; canonical axis/scale/theme updated | No | Yes | IPA and GO figures, both source palettes |
| `calculate_mixing_metric` | `calculate_local_mixing` | sc | 1, 2 | Exact numerical parity already observed on three real reductions | None | No | No | Preserve real-data checkpoint |
| `cluster_resolution_clustering` | `screen_cluster_resolutions` | sc | 3 | Used Seurat DimPlot, omitted palette on exported clustree | Restored scCustomize backend and historical clustree export/return distinction | No | Yes | Reduced Seurat checkpoint and two output files |
| `cluster_resolution_markers` | `find_resolution_markers` | sc | 1, 2 | Same thresholds, positive selection, filtering; inline comment only in old formal | None | No | No | Real marker tables |
| `custom_RidgePlot` | `plot_sc_qc_ridge` | sc | 3, 4 | Historical prism theme, guide, x limit and median geometry lost | Frozen ridge wrapper and canonical historical grammar | No | Yes | QC plot with original `colors` |
| `plot_markers_UMAP` | `plot_marker_umap` | sc | 3, 4 | Seurat plotting backend changed panel layout/palette/raster/alpha | Frozen scCustomize calls, explicit old `blues9` resolution; canonical takes explicit palette and settings | No | Yes | Original scCustomize version and real marker plot |
| `plot_combined_markers` | `plot_marker_summary` | sc | 3, 4 | Canonical DotPlot-only and legacy colours/themes/downsampling differed | Frozen historical ridge/dot/heatmap combination and 2534 seed; canonical restored same composition | No | Yes | At least 5,000 real cells and original palettes |
| `plot_ordered_violin` | same | sc | 3 | Seurat violin replaced historical scCustomize backend; alpha omitted | Restored `VlnPlot_scCustom` and alpha | No | Yes | Score median order and UVDHDS plot |
| `cluster_marker_go_analysis` | `analyze_cluster_markers` | sc | 3, 4 | Heatmap margin and GO colour missed; canonical resorted top markers | Restored original visual settings and first-n input ordering in canonical | **Yes: corrected canonical ranking** | Yes | Real marker and GO tables, especially input order |
| `plot_enrich_dotpair` | `plot_enrichment_pair` | sc | 3, 4 | Historical titles, font, shared legend and pair layout lost | Reinstated title/font/legend/layout options | No | Yes | Real enrichResult pair |
| `prepare_go_df_list` | `enrichment_to_tables` | sc | 1, 2 | Same names (`_GO`/`_simplified`) and ID logic | None | No | No | Original Excel workbook |
| `plot_pattern_clusters_tradeseq` | `plot_tradeseq_patterns` | sc | 3, 4 | Missing gene labels, title/theme, smoother/line settings | Restored historical grouping, label annotations and plot grammar | No | Yes | Saved tradeSeq pattern plot |
| `plot_pseudotime` | `estimate_pseudotime_threshold` / `plot_pseudotime_score` | sc | 3, 4 | Threshold computational method aligned; scatter raster/annotation/theme and box whisker scaling lost | Restored historical plotting layers in canonical; kept threshold core | No | Yes | Exact stored threshold plus both plot types |

### Project-local decisions

`plot_markers_density`, `plot_gene_pseudotime`, `plot_pseudotime_density`,
`run_tradeSeq_pairwise_plots`, `load_cellChat_temporal`, `get_signaling_mat`,
`plot_cellchat_pathway`, `plotLRContributionCustom`, and
`plot_differential_signaling_heatmap` stay outside the package as specified in
the migration map. No UVDHDS or OTC project script was modified.

### Constraints and remaining checks

- The `blues9` definition does not appear in the supplied helpers/scripts.
  Legacy calls resolve the historical caller object and never silently replace
  it. Canonical calls accept `feature_colors`; its default assumes the common
  nine-colour RColorBrewer Blues palette, **not proven to equal every caller's
  original `blues9`**. Pass the recorded original vector for strict parity.
- The `colors` palette in the sc helper is the ordered 36-colour
  `scCustomize::DiscretePalette_scCustomize(..., palette = "polychrome")`.
  Legacy calls resolve the existing palette in the caller, including project
  assignments and subsetting. Canonical functions accept palette arguments.
- The historical combined marker plot samples exactly 5,000 cells when
  `downsample = TRUE` and fails if fewer exist. The frozen legacy helper
  preserves that behaviour; canonical sampling safely caps at object size.
- No project checkpoint, image comparison, or complete package execution was
  possible here. R was available in the preceding workspace but disappeared
  when this execution environment was reset. Attempts to reinstall R through
  both configured and Ubuntu archive APT sources failed at the network layer.
  Earlier successful R checks apply to the **pre-audit source**, not this ZIP.
  Braces/quotes, export names, matching historical formals, frozen helper
  visibility, base-file diffs and the archive were checked statically here.
- The original sources differ on the bulk bubble accent. Keep the historical
  project-specific `color_scale` explicit when reproducing a given plot.
- The older `apply_contrasts`/`contraster`/mixing numerical implementations
  and byte-identical `find_markers_pseudobulk` remain untouched.

### Files modified in this audit

`DESCRIPTION`; `R/03_bulk_deseq2.R`; `R/04_bulk_visualization.R`;
`R/05_enrichment.R`; `R/06_sc_qc_clustering.R`; `R/07_sc_markers.R`;
`R/08_sc_trajectory.R`; `R/09_sc_tradeseq.R`;
`R/10_legacy_frozen_plots.R` (new); `R/11_compatibility_wrappers.R`;
`tests/testthat/test-compatibility.R`; `tests/testthat/test-sc-markers.R`;
`tests/testthat/test-sc-trajectory.R`;
`tests/testthat/test-visual-compatibility.R` (new); and this report.

No package version, public export or external project script was changed.
