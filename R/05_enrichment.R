#' Generic bubble plot for enrichment results
#'
#' @param data Enrichment table.
#' @param term_col,score_col,p_col Column names containing the enrichment term,
#'   x-axis score and p-value/FDR used for colouring.
#' @param genes_col Optional column containing member identifiers. Members separated
#'   by "/", "," or ";" are counted and used for bubble size when `size_col` is NULL.
#' @param top_n Maximum number of displayed terms.
#' @param colors Low and high colors for p-values.
#' @param size_range Numeric vector defining the displayed bubble-size range.
#' @param name_label Label used in the plot title.
#' @param rotate_x Logical; rotate x-axis labels by 45 degrees.
#' @param size_col Optional numeric column used directly for bubble size. Takes
#'   precedence over `genes_col`.
#' @param size_label Legend title for bubble size.
#'
#' @return A ggplot object.
#' @export
plot_enrichment_bubble <- function(data, term_col, score_col, p_col,
                                   genes_col = NULL, top_n = 20L,
                                   colors = c("lightgrey", "#4292C6"),
                                   size_range = c(3, 10), name_label = "Terms",
                                   rotate_x = FALSE, size_col = NULL,
                                   size_label = "Gene count") {

  .require_package("ggplot2")
  .check_columns(data, c(term_col, score_col, p_col, genes_col, size_col))

  if (!nrow(data)) stop("No enrichment rows to plot.")

  parse_score <- function(x) {
    vapply(as.character(x), function(value) {
      if (is.na(value)) return(NA_real_)

      pieces <- strsplit(value, "/", fixed = TRUE)[[1L]]

      if (length(pieces) == 2L) {
        denominator <- suppressWarnings(as.numeric(pieces[2L]))
        if (is.na(denominator) || denominator == 0) return(NA_real_)

        return(suppressWarnings(as.numeric(pieces[1L])) / denominator)
      }

      suppressWarnings(as.numeric(value))
    }, numeric(1L))
  }

  data$.score <- parse_score(data[[score_col]])
  data$.p <- suppressWarnings(as.numeric(data[[p_col]]))
  data$.term <- as.character(data[[term_col]])

  ## bubble size: explicit numeric column > member count > constant size
  if (!is.null(size_col)) {

    data$.members <- suppressWarnings(as.numeric(data[[size_col]]))

  } else if (!is.null(genes_col)) {

    data$.members <- vapply(as.character(data[[genes_col]]), function(s) {
      if (is.na(s) || !nzchar(s)) return(NA_integer_)

      members <- unlist(strsplit(s, "[/,;]"))
      members <- trimws(members)
      members <- members[nzchar(members)]

      length(members)
    }, integer(1L))

  } else {

    data$.members <- rep(1L, nrow(data))
  }

  data <- data[is.finite(data$.score) & is.finite(data$.p) & is.finite(data$.members), , drop = FALSE]

  if (!nrow(data)) stop("No valid score/p-value pairs remain.")

  data <- utils::head(data[order(data$.p), , drop = FALSE], top_n)

  all_negative <- all(data$.score < 0, na.rm = TRUE)
  ordering <- order(data$.score, decreasing = all_negative)
  data$.term <- factor(data$.term, levels = unique(data$.term[ordering]))

  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .data$.score,
      y = .data$.term,
      color = .data$.p,
      size = .data$.members
    )
  ) +
    ggplot2::geom_point(alpha = 0.8) +
    ggplot2::scale_size_continuous(range = size_range, name = size_label) +
    ggplot2::scale_color_gradient(
      low = colors[1L], high = colors[2L],
      name = p_col, trans = "reverse"
    ) +
    ggplot2::labs(
      x = score_col,
      y = "",
      title = paste("Top", min(top_n, nrow(data)), name_label)
    ) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 9))

  if (all_negative) {
    plot <- plot + ggplot2::scale_x_reverse()
  }

  if (rotate_x) {
    plot <- plot +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
      )
  }

  plot
}

#' Extract significant genes across result tables
#' @param results Named list or one data frame.
#' @param gene_col,p_col,lfc_col Column names.
#' @param p_threshold Adjusted p-value cutoff.
#' @return List of upregulated and downregulated gene vectors per result.
#' @export
extract_significant_genes <- function(results, gene_col = "ENTREZID", p_col = "padj",
                                      lfc_col = "log2FoldChange", p_threshold = 0.05) {
  if (is.data.frame(results)) results <- list(result = results)
  lapply(results, function(table) {
    .check_columns(table, c(gene_col, p_col, lfc_col))
    significant <- !is.na(table[[p_col]]) & table[[p_col]] < p_threshold
    list(up = unique(table[[gene_col]][significant & !is.na(table[[lfc_col]]) & table[[lfc_col]] > 0]),
         down = unique(table[[gene_col]][significant & !is.na(table[[lfc_col]]) & table[[lfc_col]] < 0]))
  })
}

#' Remove redundant GO terms via rrvgo
#' @param go_ids GO IDs and corresponding scores.
#' @param scores Named scores or NULL for equal scores.
#' @param organism_db Organism database used by rrvgo.
#' @param threshold Semantic similarity cutoff.
#' @return rrvgo reduction data frame.
#' @export
reduce_go_terms <- function(go_ids, scores = NULL, organism_db = "org.Hs.eg.db",
                            threshold = 0.7) {
  .require_package("rrvgo")
  ids <- as.character(go_ids)
  if (is.null(scores)) scores <- stats::setNames(rep(1, length(ids)), ids)
  matrix <- rrvgo::calculateSimMatrix(ids, orgdb = organism_db, ont = "BP")
  rrvgo::reduceSimMatrix(matrix, scores = scores, threshold = threshold, orgdb = organism_db)
}

#' Convert enrichment result lists to named data frames
#' @param go_results Named list of enrichment objects or data frames.
#' @param go_results_simplified Optional parallel simplified list.
#' @return Named list of data frames suitable for Excel export.
#' @export
enrichment_to_tables <- function(go_results, go_results_simplified = NULL) {
  convert <- function(x) {
    frame <- as.data.frame(x)
    if (!"ID" %in% names(frame) && !is.null(rownames(frame))) {
      frame <- cbind(ID = rownames(frame), frame)
    }
    rownames(frame) <- NULL
    frame
  }
  output <- list()
  for (name in names(go_results)) {
    if (is.null(go_results_simplified)) {
      output[[name]] <- convert(go_results[[name]])
    } else {
      output[[paste0(name, "_GO")]] <- convert(go_results[[name]])
      output[[paste0(name, "_simplified")]] <- convert(go_results_simplified[[name]])
    }
  }
  output
}

#' Calculate GO enrichment without plotting or writing files
#' @param genes Entrez IDs.
#' @param organism_db OrgDb object.
#' @param ontology GO ontology.
#' @param p_cutoff Adjusted significance cutoff.
#' @return `clusterProfiler` enrichment object.
#' @export
enrich_go_terms <- function(genes, organism_db, ontology = "BP", p_cutoff = 0.05) {
  .require_package("clusterProfiler")
  clusterProfiler::enrichGO(gene = genes, OrgDb = organism_db,
                            ont = ontology, keyType = "ENTREZID",
                            pvalueCutoff = p_cutoff)
}

#' Calculate GO terms separately for each gene set
#' @param gene_sets Named list of Entrez ID vectors.
#' @param organism_db OrgDb object.
#' @param ontology GO ontology.
#' @return Named list of enrichment objects.
#' @export
enrich_go_clusters <- function(gene_sets, organism_db, ontology = "BP") {
  if (is.null(names(gene_sets))) stop("gene_sets must be named.")
  lapply(gene_sets, enrich_go_terms, organism_db = organism_db, ontology = ontology)
}

#' Filter enriched terms using literal text or a regular expression
#' @param results Enrichment data frame.
#' @param pattern Search pattern.
#' @param term_col Description column.
#' @param fixed Whether pattern is literal.
#' @return Filtered data frame without file side effects.
#' @export
filter_enrichment_terms <- function(results, pattern, term_col = "Description", fixed = TRUE) {
  .check_columns(results, term_col)
  results[grepl(pattern, results[[term_col]], fixed = fixed, ignore.case = TRUE), , drop = FALSE]
}

#' Calculate length-bias-corrected GO enrichment with explicit organism database
#' @param genes Named 0/1 selection vector in the requested identifier space.
#' @param genome Reference genome understood by goseq.
#' @param identifier Gene identifier format understood by goseq.
#' @param ontology GO ontology.
#' @param organism_db Organism annotation database for rrvgo.
#' @param p_cutoff FDR cutoff; strict less-than comparison.
#' @param similarity_cutoff GO semantic similarity threshold.
#' @return Data frame of representative GO terms.
#' @export
enrich_goseq <- function(genes, genome, identifier = "ensGene", ontology = "BP",
                         organism_db = "org.Hs.eg.db", p_cutoff = 0.05,
                         similarity_cutoff = 0.7) {
  .require_package("goseq")
  .require_package("rrvgo")
  pwf <- goseq::nullp(genes, genome, identifier)
  results <- goseq::goseq(pwf, genome, identifier,
                           test.cats = paste0("GO:", ontology))
  results$padj <- stats::p.adjust(results$over_represented_pvalue, method = "fdr")
  results <- results[!is.na(results$padj) & results$padj < p_cutoff, , drop = FALSE]
  if (!nrow(results)) return(data.frame())
  scores <- stats::setNames(-log10(results$padj), results$category)
  similarities <- rrvgo::calculateSimMatrix(results$category, orgdb = organism_db,
                                             ont = ontology, method = "Rel")
  reduced <- rrvgo::reduceSimMatrix(similarities, scores,
                                    threshold = similarity_cutoff, orgdb = organism_db)
  reduced[reduced$go == reduced$parent, , drop = FALSE]
}
