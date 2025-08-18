#' do_enrichment_tradeSeq
#'
#' Wrapper to perform GO and KEGG enrichment for up- and down-regulated genes from tradeSeq results for a specified lineage.
#'
#' @param tradeSeqRes Data.frame with tradeSeq results (gene rownames).
#' @param lineage_name Character, e.g. "lineage1" (case-insensitive, will match column patterns).
#' @param universe Optional gene universe (SYMBOLs).
#' @param species_db OrgDb object (default org.Hs.eg.db).
#' @param ont GO ontology ("BP", "MF", "CC"), default "BP".
#' @param do_kegg Logical, whether to perform KEGG enrichment.
#'
#' @return List with enrichment results for up/down genes (GO/KEGG).
#' @export

do_enrichment_tradeSeq <- function(
    tradeSeqRes,
    lineage_name,
    universe = NULL,
    species_db = org.Hs.eg.db,
    ont = "BP",
    do_kegg = TRUE,
    go_simplify = TRUE,          # NEW: add option to turn simplify on/off
    simplify_cutoff = 0.7        # How similar before merging (default 0.7)
) {

  # Helper: Convert SYMBOL to ENTREZID
  symbol2entrez <- function(symbols, db) {
    suppressMessages(bitr(symbols, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = db)[["ENTREZID"]])
  }

  # Find correct columns
  lineage_name <- tolower(lineage_name)
  pval_col <- grep(paste0("^pvalue_", lineage_name), colnames(tradeSeqRes), value = TRUE)
  logfc_col <- grep(paste0("^logFC", lineage_name), colnames(tradeSeqRes), value = TRUE)
  if (length(pval_col) == 0 || length(logfc_col) == 0)
    stop("Couldn't find pvalue or logFC column for requested lineage.")

  # Significant genes
  sig <- tradeSeqRes %>% filter(.data[[pval_col]] < 0.05)
  up <- sig %>% filter(.data[[logfc_col]] > 0)
  down <- sig %>% filter(.data[[logfc_col]] < 0)

  up_genes <- rownames(up)
  down_genes <- rownames(down)
  univ_entrez <- if (!is.null(universe)) symbol2entrez(universe, species_db) else NULL

  # Convert to ENTREZ
  up_entrez <- symbol2entrez(up_genes, species_db)
  down_entrez <- symbol2entrez(down_genes, species_db)

  # GO enrichment
  go_up <- enrichGO(gene = up_entrez, OrgDb = species_db, ont = ont, universe = univ_entrez,
                    pAdjustMethod = "BH", pvalueCutoff = 0.05, readable = TRUE)
  go_down <- enrichGO(gene = down_entrez, OrgDb = species_db, ont = ont, universe = univ_entrez,
                      pAdjustMethod = "BH", pvalueCutoff = 0.05, readable = TRUE)

  # Simplify GO results to reduce redundancy
  if (go_simplify) {
    if (!is.null(go_up) && nrow(as.data.frame(go_up)) > 0) {
      go_up <- simplify(go_up, cutoff = simplify_cutoff, by = "p.adjust", select_fun = min)
    }
    if (!is.null(go_down) && nrow(as.data.frame(go_down)) > 0) {
      go_down <- simplify(go_down, cutoff = simplify_cutoff, by = "p.adjust", select_fun = min)
    }
  }

  # Optionally: KEGG enrichment
  kegg_up <- NULL; kegg_down <- NULL
  if (do_kegg) {
    kegg_up <- enrichKEGG(gene = up_entrez, organism = "hsa", universe = univ_entrez,
                          pAdjustMethod = "BH", pvalueCutoff = 0.05)
    kegg_down <- enrichKEGG(gene = down_entrez, organism = "hsa", universe = univ_entrez,
                            pAdjustMethod = "BH", pvalueCutoff = 0.05)
  }
  return(list(
    go_up = go_up,
    go_down = go_down,
    kegg_up = kegg_up,
    kegg_down = kegg_down,
    up_genes = up_genes,
    down_genes = down_genes
  ))
}
