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
#' @param go_simplify Logical, whether GO terms should be simplified to reduce redundancy using clusterProfiler::simplify. Default is TRUE
#' @param simplify_cutoff similarity threshold
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
  .legacy_api_message("do_enrichment_tradeSeq", "enrich_tradeseq")

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
      go_up <- clusterProfiler::simplify(go_up, cutoff = simplify_cutoff, by = "p.adjust", select_fun = min)
    }
    if (!is.null(go_down) && nrow(as.data.frame(go_down)) > 0) {
      go_down <- clusterProfiler::simplify(go_down, cutoff = simplify_cutoff, by = "p.adjust", select_fun = min)
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

#' GO Enrichment and Redundancy Reduction Using goseq and rrvgo
#'
#' This function performs Gene Ontology (GO) enrichment analysis using the \code{goseq} package and reduces the resulting terms
#' based on semantic similarity using \code{rrvgo}. It is useful for highlighting the most representative GO terms among significantly enriched results.
#'
#' @param gene.list Named numeric vector of 0s and 1s, indicating genes of interest (1) and background genes (0).
#' Names should correspond to gene IDs, either Ensembl or Entrez, depending on the \code{identifier} used.
#' @param genome Character string specifying the reference genome (e.g., \code{"hg38"}). Passed to \code{nullp} and \code{goseq}.
#' @param identifier Character string. Gene ID format: \code{"ensGene"} for Ensembl or \code{"knownGene"} for Entrez IDs.
#' @param ont Character string specifying the GO ontology. One of \code{"BP"} (Biological Process), \code{"MF"} (Molecular Function), or \code{"CC"} (Cellular Component). Default is \code{"BP"}.
#' @param sim.thres Numeric. Similarity threshold used by \code{rrvgo::reduceSimMatrix()}. Default is \code{0.7}.
#' @param pval.thres Numeric. Adjusted p-value threshold for filtering enriched GO terms. Default is \code{0.05}.
#'
#' @return A data frame containing non-redundant representative GO terms after enrichment and similarity reduction.
#'
#' @details This function:
#' \enumerate{
#'   \item Performs GO enrichment using \code{goseq()}.
#'   \item Adjusts p-values using FDR.
#'   \item Filters results below a p-value threshold.
#'   \item Computes a semantic similarity matrix via \code{rrvgo::calculateSimMatrix()}.
#'   \item Reduces the matrix using \code{rrvgo::reduceSimMatrix()} to retain only representative GO terms.
#' }
#' @examples
#' \dontrun{
#' gene_list <- c(GeneA = 1, GeneB = 0, GeneC = 1, GeneD = 0)
#' goseq_to_revigo(gene.list = gene_list, genome = "hg38", identifier = "ensGene")
#' }
#'
#' @export

goseq_to_revigo <- function(gene.list, genome, identifier = "ensGene", ont = "BP", sim.thres = 0.7, pval.thres = 0.05) {
  .legacy_api_message("goseq_to_revigo", "enrich_goseq")
  goseqOnt <- paste("GO:", ont, sep = "")
  pwf <- nullp(gene.list, genome, identifier)
  goResults <- goseq(pwf, genome, identifier, test.cats = c(goseqOnt))
  goResults$padj <- rstatix::adjust_pvalue(goResults$over_represented_pvalue, method = "fdr")
  goResults <- goResults[goResults$padj < pval.thres, ]
  simMatrix <- calculateSimMatrix(goResults$category, orgdb = "org.Hs.eg.db", ont = ont, method = "Rel")
  scores <- stats::setNames(-log10(goResults$padj), goResults$category)
  res <- reduceSimMatrix(simMatrix, scores, threshold = sim.thres, orgdb = "org.Hs.eg.db")
  res[res$go == res$parent, ]
}

#' Extract high dispersion genes
#'
#' @param dds.obj DESeq2 object containg differential expression data
#' @param norm.counts normalized count data (e.g. rlog or vst transformed count data).
#' @param res results table containing gene IDs as ENSEMBL ID as well as gene names stored as "SYMBOL".
#' @param disp.thres dispersion threshold. All genes higher than the specified number will be extracted.
#' @param min.count remove low expressed genes which would interfere with the calculation of dispersion estimates.
#' @param n.min the smallest sample size of the experiment which refers to the number of samples which need a count above the threshold set in
#' min.count to be considered.
#' @export

high_dispersion_genes <- function(dds.obj, norm.counts, res, min.count = 10, n.min = dim(dds.obj)[2]/2, disp.thres = 1){
  arg1 <- S4Vectors::mcols(dds.obj, use.names = TRUE)$dispersion > disp.thres
  arg2 <- rowSums(SummarizedExperiment::assay(dds.obj) >= min.count) > n.min

  indDisp <- which(arg1 & arg2)
  rldHighDisp <- SummarizedExperiment::assay(norm.counts)[indDisp,]

  highDispGenes <- res[res$ENSEMBL %in% rownames(rldHighDisp),]$SYMBOL
  highDispGenes[!is.na(highDispGenes)]
}

#' Create a clusterProfiler-style bubble plot for Ingenuity Pathway Analysis (IPA) results (or similar)
#'
#' Build a bubble plot showing terms (pathways, upstream regulators, ...) on the y-axis,
#' a scoring metric on the x-axis (e.g. z-score), bubble size proportional to the number
#' of member genes, and bubble color mapped to a p-value. The function sorts results
#' by p-value (keeps the top N) and reorders the y-axis by score for clear visual representation.
#'
#' This is intended for IPA output (e.g. canonical pathways or upstream regulators) but
#' will work with any data.frame that contains:
#' - a column with term names,
#' - a numeric scoring metric,
#' - a p-value column,
#' - and a column listing member genes (comma- or slash-separated).
#'
#' @param df data.frame Input results table.
#' @param name_col string Name of the column in df that contains the term (pathway/regulator) names.
#' @param score_col string Column name for the scoring metric to plot on the x-axis. Default "z_score".
#' @param pval_col string Column name containing p-values to map to color. Default "padj".
#' @param genes_col string Column name containing group member identifiers (used to compute bubble size).
#'        Members may be comma- or slash-separated strings. Default "Molecules".
#' @param top_n integer Number of top terms to keep after sorting by p-value. Default 20.
#' @param color_scale character(2) Two colours giving the low and high ends of the colour scale.
#'        Default c("lightgrey", "#CD534CFF").
#' @param size_range numeric(2) Range of point sizes for ggplot2::scale_size_continuous. Default c(3, 10).
#' @param name_label string Text used in the plot title along with top_n (e.g. "upregulated IPA pathways").
#' @param rotate_x logical If TRUE rotate x-axis labels 45 degrees (useful when axis labels are long). Default FALSE.
#'
#' @return A ggplot2 object (scatter/bubble plot). You can print it or save it with ggsave.
#'
#' @details
#' The function:
#' - checks that the requested columns exist,
#' - computes the number of genes per term by splitting the genes_col on commas or slashes,
#' - coerces the score and p-value to numeric and filters rows with missing values,
#' - selects the top_n rows by ascending p-value,
#' - reorders the y-axis factor by the score so that high/low scores are visually ordered,
#' - if all scores are negative, flips the x-axis so the most negative values appear at the top
#'   (consistent with some clusterProfiler visual conventions),
#' - maps bubble size to gene count and colour to p-value (colour scale uses reverse transformation
#'   so lower p-values appear with the high colour).
#'
#' @examples
#' \dontrun{
#' # IPA canonical pathways (up/down)
#' p1 <- ipa_bubble_plot(dat_pw_up,
#'                       name_col = "Ingenuity_Canonical_Pathways",
#'                       name_label = "upregulated IPA pathways")
#' p2 <- ipa_bubble_plot(dat_pw_down,
#'                       name_col = "Ingenuity_Canonical_Pathways",
#'                       name_label = "downregulated IPA pathways")
#'
#' # IPA upstream regulators (specifying different column names)
#' p1 <- ipa_bubble_plot(dat_upstream_up,
#'                       name_col = "Upstream_Regulator",
#'                       score_col = "Activation_z_score",
#'                       pval_col = "p_value_of_overlap",
#'                       genes_col = "Target_Molecules_in_Dataset",
#'                       name_label = "upregulated upstream regulators")
#'
#' p2 <- ipa_bubble_plot(dat_upstream_down,
#'                       name_col = "Upstream_Regulator",
#'                       score_col = "Activation_z_score",
#'                       pval_col = "p_value_of_overlap",
#'                       genes_col = "Target_Molecules_in_Dataset",
#'                       name_label = "downregulated upstream regulators")
#' }
#'
#' @export
ipa_bubble_plot <- function(
    df,
    name_col,
    score_col = "z_score",
    pval_col = "padj",
    genes_col = "Molecules",
    top_n = 20,
    color_scale = c("lightgrey", "#4292C6"), #"#CD534CFF"
    size_range = c(3, 10),
    name_label = "Term",
    rotate_x = FALSE
) {
  .legacy_api_message("ipa_bubble_plot", "plot_enrichment_bubble")
  # Defensive: check columns exist
  stopifnot(all(c(name_col, score_col, pval_col, genes_col) %in% colnames(df)))

  # Prepare the data
    plot_df <- df %>%
    dplyr::mutate(
      n_genes = ifelse(
        is.na(.data[[genes_col]]),
        NA,
        sapply(strsplit(as.character(.data[[genes_col]]), ",|/"), length)
      ),
      score = as.numeric(.data[[score_col]]),
      pval  = as.numeric(.data[[pval_col]]),
      name  = .data[[name_col]]
    ) %>%
    dplyr::filter(!is.na(.data$score), !is.na(.data$pval))

  # Sort and select top_n by pvalue
  plot_df <- plot_df %>%
    dplyr::arrange(.data$pval) %>%
    dplyr::slice(seq_len(min(top_n, nrow(plot_df))))

  # Check if all scores are negative (flip axis if true)
  all_negative <- all(plot_df$score < 0, na.rm = TRUE)

  # Reorder y-axis so lowest scores (most negative) are at the top, highest at bottom
  # This works for both negative and positive scores, and is consistent with clusterProfiler
  plot_df$name <- factor(plot_df$name, levels = plot_df$name[order(plot_df$score, decreasing = FALSE)])

  if(all_negative) plot_df$name <- factor(plot_df$name, levels = plot_df$name[order(plot_df$score, decreasing = TRUE)])

  p <- ggplot(plot_df, aes(x = .data$score, y = .data$name, size = .data$n_genes, color = .data$pval)) +
    geom_point(alpha = 0.8) +
    scale_size_continuous(range = size_range, name = "Gene count") +
    scale_color_gradient(low = color_scale[1], high = color_scale[2], name = pval_col, trans = "reverse") +
    labs(
      x = score_col,
      y = "",
      title = paste("Top", top_n, name_label)
    ) +
    theme_bw(base_size = 14) +
    theme(axis.text.y = element_text(size = 9))

  if (all_negative) {
    p <- p + scale_x_reverse()
  }
  if (rotate_x) {
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  }
  return(p)
}

#' Function to list ENTREZ IDs of significantly changed genes
#'
#' This function lists significantly up or downregulated genes as ENTREZ IDs for later use in clusterProfiler's function \link[clusterProfiler]{compareCluster}. 
#' 
#' @param list List containing results of DE analysis generated with \link[DESeq2]{results} function. 
#' @param p.threshold P-value threshold used to define differentially expressed genes
#' @param lfc.threshold Log-fold change threshold used to define differentially expressed genes.
#' @param direction Character specifying whether ENTREZ IDs of upregulated ("greater") or downregulated ("lesser") genes should be extracted
#' @export

list_signif_genes <- function(list, p.threshold = 0.05, lfc.threshold = 0, direction = c("greater", "lesser")){
  .legacy_api_message("list_signif_genes", "extract_significant_genes")
  lapply(1:length(list), function(x){
    tmp <- list[[x]]
    if(direction == "greater"){
      res <- tmp[!is.na(tmp$padj) & tmp$padj < p.threshold & !is.na(tmp$ENTREZID) & tmp$log2FoldChange > lfc.threshold, ]$ENTREZID
    }   else if(direction == "lesser"){
      res <- tmp[!is.na(tmp$padj) & tmp$padj < p.threshold & !is.na(tmp$ENTREZID) & tmp$log2FoldChange < lfc.threshold, ]$ENTREZID
    } else {
      stop("Please specify a direction.")
    }
    res
  })
}

#' Function to plot the first 2 principal components of a rlog or vst transformed count matrix.
#'
#' Output is a ggplot object showing PC1 and PC2 for a RNASeq experiment. For a 1 factor design, points are colored by this group,
#' for a 2 factor design the first factor is represented by colors the second factor by shape.
#' @param data Transformed count matrix.
#' @param type Specify type of transformation which was used for generation of count matrix.
#' @param pointSize Size of points.
#' @param textSize Size of plot text.
#' @param title plot title.
#' @param colors character vector with colors used in scale_color_manual. Default is npg colors from ggsci package.
#' @param shapes character vector with shapes. Default are ggplot shapes if no shapes are supplied.
#' @param subtitle plot subtitle.
#' @param labelled logical indicating whether points or text labels should be plotted. Default is FALSE to plot points.
#' @inheritParams DESeq2::plotPCA
#' @export

pca_plot <- function(data, pcsToUse, title = "", subtitle = "",  type = "VST", colors = NULL, shapes = NULL, pointSize = 3, textSize= 12, labelled = FALSE, intgroup){
  .legacy_api_message("pca_plot", "plot_bulk_pca")

  if(is.null(colors)){
    # Get original NPG palette (10 colors)
    npg_colors <- pal_npg("nrc")(10)
    jco_colors <- pal_jco()(10)
    colors <- c(npg_colors, jco_colors)
  }
  if(length(intgroup) > 1 && is.factor(data[[intgroup[2]]])) {
    shape_aes <- intgroup[2]
  } else {
    shape_aes <- NULL # Don't map shape
  }
  pca <- DESeq2::plotPCA(data, intgroup = intgroup, returnData = TRUE, pcsToUse = pcsToUse)
  percentVar <- round(100 * attr(pca, "percentVar"))
  name <- NULL
  p <- ggplot(pca, aes_string(x = paste0("PC", pcsToUse[1]), y = paste0("PC", pcsToUse[2]), color = intgroup[1], shape = shape_aes)) +
    geom_point(size =pointSize) +
    labs(title = title, subtitle = subtitle) +
    xlab(paste0("PC", pcsToUse[1], ": ", percentVar[1], "% variance")) +
    ylab(paste0("PC", pcsToUse[2],": ", percentVar[2], "% variance")) +
    ggtitle(title) + # remove if function throws error
    theme_bw(base_size = textSize) +
    scale_color_manual(values = colors)
  if(!is.null(shapes)){
    p <- p + scale_shape_manual(values = shapes)
  }
  if(labelled == TRUE) p + geom_label(aes(label = name)) else p
}

#' Function to plot GO clusters
#'
#' This function uses a gene list of ENTREZ IDs as input for clusterProfiler's function \link[clusterProfiler]{compareCluster} with the parameters \emph{fun = enrichGO},
#' \emph{ont = "BP"} and \emph{OrgDb = org.Hs.eg.db}. The package \emph{org.Hs.eg.db} is required for this function to work properly.
#'
#' @param genes Vector containing ENTREZ IDs of differentially expressed genes.
#' @param pvalueCutoff adjusted pvalue cutoff on enrichment tests to report
#' @param sim.thres similarity threshold (0-1). Some guidance: Large (allowed similarity=0.9), Medium (0.7), Small (0.5), Tiny (0.4) Defaults to Medium (0.7)
#' @param path Path were results should be stored. Can be an absolute path or relative path based on the working directory.
#' @param sym.colors Logical indicating whether colors distribution should be symmetrical. Default is FALSE.
#' @param return.res Logical indicating whether results should be returned. If TRUE original enrichGO results and reduced terms will be stored as list object.
#' @param font.size Font size.
#' @param showCategory A number or a list of terms. If it is a number, the first n terms will be displayed. If it is a list of terms, the selected terms will be displayed.
#' @export

plot_go <- function(genes, showCategory = 20, sim.thres = 0.7, sym.colors = FALSE, return.res = FALSE, font.size = 12, pvalueCutoff = 0.05, path){
  .legacy_api_message("plot_go", "enrich_go_terms")
  term <- NULL

  names <- str_remove(deparse(substitute(genes)), "_genes")
  ## calculate clustered pathways
  ego <- enrichGO(genes, ont = "BP", keyType = "ENTREZID", OrgDb = org.Hs.eg.db, pvalueCutoff = 0.05)
  reducedTerms <- reduce_go_rrvgo(ego)

  ## define data frame for barplot
  df_bp <- head(reducedTerms, 20)
  df_bp$term <- factor(df_bp$term)
  df_bp$term <- fct_reorder(df_bp$term, df_bp$score)

  ## define viridis colors
  rt_min <- sqrt(min(reducedTerms$score))
  rt_max <- max(reducedTerms$score)
  if(sym.colors == FALSE){
    colour_breaks <- c(rt_min, rt_min*2, rt_min*3, rt_min*5, rt_min*6, rt_max*0.8, rt_max)
  } else {
    colour_breaks <- seq(rt_min, rt_max, by = (rt_max-rt_min)/6)
  }
  colours <- c("#440154FF", "#470D60FF", "#39558CFF", "#26818EFF",  "#1F998AFF", "#C9E020FF", "#FDE725FF")

  p_dp <- dotplot(ego, showCategory = showCategory,  font.size = font.size) + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) + scale_fill_viridis_c()
  p_wc <- plot_wordcloud(ego$Description)
  p_bp <- ggbarplot(df_bp, x = "term", y = "score", fill = "score") +
    scale_y_continuous(expand = c(0,0)) + xlab("") + ylab("-log10 adjusted pvalue") +
    scale_fill_gradientn(name = "-log10 adjusted pvalue", limits  = range(reducedTerms$score), colours = colours[c(1, seq_along(colours), length(colours))],  values  = c(0, scales::rescale(colour_breaks, from = range(reducedTerms$score)), 1)) +
    coord_flip()

  ## arrange plots
  p_aux <- ggarrange(p_wc, p_bp, labels = c("A", "B"), ncol = 1)
  p <- ggarrange(p_aux, p_dp, labels = c("", "C"), ncol = 2, widths = c(1, 0.5))
  p <- annotate_figure(p, top = text_grob(paste0("Biological processes (n=", dim(ego)[1],") enriched in ", names, " genes"), face = "bold", size = 14))

  if(return.res == TRUE){
    list(enrich_go = ego, reduced_go = reducedTerms, plot = p)
  } else {
    return(p)
  }
}

#' Function to plot and save GO clusters
#'
#' This function uses a gene list of ENTREZ IDs as input for clusterProfiler's function \link[clusterProfiler]{compareCluster} with the parameters \emph{fun = enrichGO},
#' \emph{ont = "BP"} and \emph{OrgDb = org.Hs.eg.db}. The package \emph{org.Hs.eg.db} is required for this function to work properly.
#'
#' @param gene.list List containing ENTREZ IDs of enriched genes. List should be generated with the list_signif_genes() function from the ekbSeq package.
#' @param pvalueCutoff adjusted pvalue cutoff on enrichment tests to report
#' @param sim.thres similarity threshold (0-1). Some guidance: Large (allowed similarity=0.9), Medium (0.7), Small (0.5), Tiny (0.4) Defaults to Medium (0.7)
#' @param sym.colors Logical indicating whether colors distribution should be symmetrical. Default is FALSE.
#' @param font.size Font size.
#' @param return.res Logical indicating whether results should be returned. If TRUE original enrichGO results and reduced terms will be stored as list object.
#' @param showCategory A number or a list of terms. If it is a number, the first n terms will be displayed. If it is a list of terms, the selected terms will be displayed.
#' @export

plot_go_clusters <- function(gene.list, showCategory = 5, sim.thres = 0.7, sym.colors = FALSE, return.res = FALSE,  font.size = 12, pvalueCutoff = 0.05){
  .legacy_api_message("plot_go_clusters", "enrich_go_clusters")
  term <- NULL

  names <- str_remove(deparse(substitute(gene.list)), "ls_")
  ## calculate clustered pathways
  ck <- compareCluster(geneClusters = gene.list, fun = enrichGO, ont = "BP", keyType = "ENTREZID", OrgDb = org.Hs.eg.db, pvalueCutoff = pvalueCutoff)
  ck <- enrichplot::pairwise_termsim(ck)
  ck <- setReadable(ck, OrgDb = org.Hs.eg.db, keyType="ENTREZID")
  simMatrix <- calculateSimMatrix(ck@compareClusterResult$ID, orgdb = "org.Hs.eg.db", ont = "BP", method = "Rel")
  scores <- stats::setNames(-log10(ck@compareClusterResult$p.adjust), ck@compareClusterResult$ID)
  res <- reduceSimMatrix(simMatrix, scores, threshold = sim.thres, orgdb = "org.Hs.eg.db")
  reducedTerms <- res[res$go == res$parent, ]
  ## define data frame for barplot
  df_bp <- head(reducedTerms, 20)
  df_bp$term <- factor(df_bp$term)
  df_bp$term <- fct_reorder(df_bp$term, df_bp$score)

  ## define viridis colors
  rt_min <- sqrt(min(reducedTerms$score))
  rt_max <- max(reducedTerms$score)
  if(sym.colors == FALSE){
    colour_breaks <- c(rt_min, rt_min*2, rt_min*3, rt_min*5, rt_min*6, rt_max*0.8, rt_max)
  } else {
    colour_breaks <- seq(rt_min, rt_max, by = (rt_max-rt_min)/6)
  }
  colours <- c("#440154FF", "#470D60FF", "#39558CFF", "#26818EFF",  "#1F998AFF", "#C9E020FF", "#FDE725FF")

  p_dp <- dotplot(ck, showCategory = showCategory,  font.size = font.size) + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) + scale_fill_viridis_c()
  p_wc <- plot_wordcloud(ck@compareClusterResult$Description)
  p_bp <- ggbarplot(df_bp, x = "term", y = "score", fill = "score") +
    scale_y_continuous(expand = c(0,0)) + xlab("") + ylab("-log10 adjusted pvalue") +
    scale_fill_gradientn(name = "-log10 adjusted pvalue", limits  = range(reducedTerms$score), colours = colours[c(1, seq_along(colours), length(colours))],  values  = c(0, scales::rescale(colour_breaks, from = range(reducedTerms$score)), 1)) +
    coord_flip()

  ## arrange plots
  p_aux <- ggarrange(p_wc, p_bp, labels = c("A", "B"), ncol = 1)
  p <- ggarrange(p_aux, p_dp, labels = c("", "C"), ncol = 2, widths = c(1, 0.5))
  p <- annotate_figure(p, top = text_grob(paste0("Biological processes (n=", dim(unique(ck@compareClusterResult$ID))[1],") enriched in ", names, " genes"), face = "bold", size = 14))

  if(return.res == TRUE){
    list(enrich_go = ck, reduced_go = reducedTerms, plot = p)
  } else {
    return(p)
  }
}

#' Perform GO enrichment analysis and bubble plot visualization for tradeSeq differential expression results
#'
#' This function extracts significantly up- and downregulated genes for a given lineage comparison from tradeSeq results,
#' performs Gene Ontology (GO) enrichment analysis using clusterProfiler, simplifies redundant GO terms, and creates
#' side-by-side bubble plots of the top enriched biological processes for up- and downregulated genes.
#'
#' @param res Data frame. TradeSeq results with gene rownames and columns for p-values, wald statistics, and log fold change.
#' @param comparison_name Character. Name of the comparison, used for plot titles (e.g., "lin1 vs lin2") and to extract comparisons. Need to include the number of the assigned lineages in the correct order. 
#' @param top_n Integer. Number of top GO terms to display in bubble plots (default: 20).
#' @param pval_thresh Numeric. P-value threshold for significance (default: 0.05).
#' @param ont Character. Ontology for GO enrichment ("BP", "MF", "CC"; default: "BP").
#' @param simplify_cutoff Numeric. Cutoff for the \code{simplify} function to reduce redundancy (default: 0.7).
#'
#' @return A list with three elements:
#' \describe{
#'   \item{up_GO}{clusterProfiler enrichResult for upregulated genes (simplified)}
#'   \item{down_GO}{clusterProfiler enrichResult for downregulated genes (simplified)}
#'   \item{plot}{A ggarrange object showing side-by-side bubble plots for up- and downregulated GO terms}
#' }
#'
#' @details
#' The function expects gene symbols as rownames of \code{res}. These are mapped to ENTREZ IDs for GO enrichment. 
#' Upregulated genes are those with significant p-value and positive log fold change; downregulated are significant and negative.
#' The function uses clusterProfiler for enrichment, enrichplot for bubble plots, and ggpubr::ggarrange for layout.
#'
#' @export
#' 
plot_tradeSeq_GO <- function(res, 
                             comparison_name = "lin1 vs. lin2", 
                             top_n = 20, 
                             pval_thresh = 0.05, 
                             ont = "BP", 
                             simplify_cutoff = 0.7) {
  .legacy_api_message("plot_tradeSeq_GO", "plot_enrichment_pair")
  # Extract correct columns based on comparison name
  name_split <- unlist(strsplit(comparison_name, " "))
  str1 <- str_extract(name_split[1], "\\d+")
  str2 <- str_extract(name_split[length(name_split)], "\\d+")
  pval_col <- paste0("pvalue_", str1, "vs", str2)
  wald_col <- paste0("waldStat", str1, "vs", str2)
  logFC_col <- paste0("logFC", str1, "_", str2)
  
  # Extract significant genes
  sig_genes <- res[res[[pval_col]] < pval_thresh, ]
  up_genes <- rownames(sig_genes[sig_genes[[logFC_col]] > 0, ])
  down_genes <- rownames(sig_genes[sig_genes[[logFC_col]] < 0, ])
  
  # Convert gene symbols to ENTREZID
  up_eg <- bitr(up_genes, fromType="SYMBOL", toType="ENTREZID", OrgDb=org.Hs.eg.db)$ENTREZID
  down_eg <- bitr(down_genes, fromType="SYMBOL", toType="ENTREZID", OrgDb=org.Hs.eg.db)$ENTREZID
  
  # GO enrichment
  ego_up <- enrichGO(gene = up_eg, OrgDb = org.Hs.eg.db, keyType = "ENTREZID",
                     ont = ont, pAdjustMethod = "BH", pvalueCutoff = pval_thresh, readable = TRUE)
  ego_down <- enrichGO(gene = down_eg, OrgDb = org.Hs.eg.db, keyType = "ENTREZID",
                       ont = ont, pAdjustMethod = "BH", pvalueCutoff = pval_thresh, readable = TRUE)
  
  # Simplify GO terms
  ego_up_s <- simplify(ego_up, cutoff = simplify_cutoff, by = "p.adjust", select_fun = min)
  ego_down_s <- simplify(ego_down, cutoff = simplify_cutoff, by = "p.adjust", select_fun = min)
  
  # Bubble plots
  p_up <- dotplot(ego_up_s, showCategory=top_n, title=paste("Upregulated in", comparison_name)) +
    theme(axis.text.y = element_text(size=9))
  p_down <- dotplot(ego_down_s, showCategory=top_n, title=paste("Downregulated in", comparison_name)) +
    theme(axis.text.y = element_text(size=9))
  
  # Combine plots
  combined_plot <- ggarrange(p_up, p_down, nrow = 1)
  
  return(list(
    up_GO = ego_up_s,
    down_GO = ego_down_s,
    plot = combined_plot
  ))
}

#' Function to plot and save
#'
#' Text is searched for common terms and a wordcloud is generated based on frequency.
#'
#' @param text Character vector containing a list of pathway strings.
#' @export

# library(tm)
# library(SnowballC)

plot_wordcloud <- function(text){
  .legacy_api_message("plot_wordcloud", "plot_wordcloud")
  freq <- NULL

  docs <- Corpus(VectorSource(text))
  docs <- docs %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("english"))
  dtm <- TermDocumentMatrix(docs)
  matrix <- as.matrix(dtm)
  words <- sort(rowSums(matrix),decreasing=TRUE)
  x <- names(words)
  Encoding(x) <- 'latin1'
  names(words) <- x
  df <- data.frame(word = names(words), freq=words)
  ggplot(df, aes(label = word, size = freq, color = word)) +
    geom_text_wordcloud(area_corr = TRUE) +
    scale_size_area(max_size = 50, trans = power_trans(1/.7)) +
    theme_minimal() +
    scale_color_viridis_d()
}

#' Reduce and Annotate GO Terms Using RRvgo
#'
#' This function reduces redundant GO terms from a clusterProfiler enrichment object using semantic similarity via the RRvgo package,
#' and annotates the representative terms with clusterProfiler results.
#'
#' @param ego_obj A clusterProfiler enrichment result object (e.g. from `enrichGO`).
#' @param ont Character. The ontology to use for similarity calculation. Default is `"BP"` (biological process).
#' @param orgdb Character or AnnotationDbi object. The organism database, e.g. `"org.Hs.eg.db"`.
#' @param sim.thres Numeric. Similarity threshold for RRvgo reduction (between 0 and 1). Default is `0.7`.
#'
#' @return A data frame containing the reduced representative GO terms and their annotation from the clusterProfiler enrichment results.
#'
#' @details
#' This function performs the following steps:
#' \enumerate{
#'   \item Calculates semantic similarity between enriched GO terms.
#'   \item Reduces redundant terms using RRvgo.
#'   \item Annotates representative (parent) terms with clusterProfiler results.
#' }
#'
#' @seealso \code{\link[rrvgo]{calculateSimMatrix}}, \code{\link[rrvgo]{reduceSimMatrix}}, \code{\link[clusterProfiler]{enrichGO}}
#' @examples
#' \dontrun{
#' ego <- enrichGO(gene = geneList, OrgDb = org.Hs.eg.db, ont = "BP", keyType = "ENTREZID")
#' reduced <- reduce_go_rrvgo(ego)
#' head(reduced)
#' }
#' @export

reduce_go_rrvgo <- function(ego_obj, ont = "BP", orgdb = "org.Hs.eg.db", sim.thres = 0.7){
  .legacy_api_message("reduce_go_rrvgo", "reduce_go_terms")
  enriched_genes <- NULL
  all_genes <- NULL
  GeneRatio <- NULL
  ck <- ego_obj
  ck <- enrichplot::pairwise_termsim(ck)
  ck <- clusterProfiler::setReadable(ck, OrgDb = org.Hs.eg.db, keyType="ENTREZID")
  simMatrix <- rrvgo::calculateSimMatrix(ck$ID, orgdb = orgdb, ont = ont, method = "Rel")
  scores <- stats::setNames(-log10(ck$p.adjust), ck$ID)
  res <- rrvgo::reduceSimMatrix(simMatrix, scores, threshold = sim.thres, orgdb = orgdb)
  reducedTerms <- res[res$go == res$parent, ] %>%
    left_join(data.frame(ck), by = c("go" = "ID")) %>%
    tidyr::separate(GeneRatio, c("enriched_genes", "all_genes")) %>%
    mutate(GeneRatio = as.numeric(enriched_genes)/as.numeric(all_genes))
  return(reducedTerms)
}

#' Function to plot subset of go terms based on search string
#'
#' This function uses results of \link[clusterProfiler]{compareCluster} or \link[clusterProfiler]{enrichGO} as input to search for go terms of interest
#'  based on provided string. Output are a .csv containing a list of matching go terms ranked by their occurence in the list of all enrich go terms and a figure
#'  containing genes involved in those go terms (ranked by their respective logFoldChange).
#'
#' @param enrich.res Enrichment results generated with \link[clusterProfiler]{compareCluster} or \link[clusterProfiler]{enrichGO}
#' @param all.res Annotated results of DESeq analysis containing (at least) p.value, gene ID, gene name and logFoldChange
#' @param path Path were results should be stored. Can be an absolute path or relative path based on the working directory.
#' @param search.string Logical indicating whether colors distribution should be symmetrical. Default is FALSE.
#' @param fig.height Figure height (in inches).
#' @param fig.width Figure width (in inches).
#' @param flip.coords Logical indicating whether x-y coordinates should be flipped. Default is TRUE.
#' @export

search_go <- function(enrich.res, all.res, path = getwd(), search.string = " ", fig.width = 7, fig.height = 16, flip.coords = TRUE){
  .legacy_api_message("search_go", "filter_enrichment_terms")
  ind <- str_detect(enrich.res$Description, search.string)
  search.string <- str_replace_all(search.string, "\\|", "_")
  tbl <- enrich.res[ind,]
  tbl$Rank <- which(ind)
  genes <- unique(unlist(stringr::str_split(tbl$geneID, "/")))

  ## write results into table
  tbl %>%
    dplyr::select(c("Rank", "ID", "Description", "zScore", "p.adjust", "geneID", "Count")) %>%
    stats::setNames(c("Rank", "ID", "Description", "Z-score", "Adjusted P-value", "Genes", "Count")) %>%
    write.csv(paste0(path, "/selected_go_terms_", search.string, ".csv"))

  ## plot genes which play a role within the selected pathways
  all.res <- all.res[all.res$SYMBOL %in% genes,]
  all.res$SYMBOL <- factor(all.res$SYMBOL)
  all.res$SYMBOL <- fct_reorder(all.res$SYMBOL, all.res$log2FoldChange)
  p <- ggbarplot(all.res, x= "SYMBOL", y = "log2FoldChange", fill = "log2FoldChange") +
    scale_fill_viridis_c() +
    scale_y_continuous(expand = c(0,0)) +
    ylab("")

  if(flip.coords == TRUE){
    p <- p + coord_flip()
  }

  svg(paste0(path, "/selected_go_terms_", search.string, ".svg"), width=fig.width, height=fig.height)
  print(p)
  dev.off()
}
