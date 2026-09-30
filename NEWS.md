# ekbSeq 1.0.0 candidate

- Split reusable sequencing helpers into bulk, enrichment, single-cell and
  export source modules.
- Kept the established pseudobulk DE and dual PNG/SVG export interfaces.
- Added canonical explicit APIs and informational legacy messages.
- Added synthetic test fixtures and a project checkpoint validation template.
- Kept publication-specific biology and CellChat comparison code outside the
  public package API.

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
