test_that("transcript wrapper retains historical axis and palette", {
  sample <- data.frame(biotype = c("mRNA", "mRNA", "lncRNA"))
  plot <- suppressMessages(plot_transcript_dist(sample, "biotype"))
  expect_identical(plot$labels$y,
    "Number of significantly altered transcripts \n between Melanocytes and DSCs")
  expect_identical(plot$labels$title, "biotype annotation")
  expect_equal(length(plot$layers), 2L)
  expect_s3_class(plot$layers[[1]]$geom, "GeomBar")
  expect_s3_class(plot$layers[[2]]$geom, "GeomText")
  expect_true(!is.null(plot$scales$get_scales("fill")))
})

test_that("historical enrichment bubble reverses negative score axis", {
  table <- data.frame(term = c("A", "B"), ratio = c(-2, -1),
    q = c(0.02, 0.01), genes = c("G1/G2", "G3/G4/G5"))
  plot <- suppressMessages(bubble_plot_clusterprofiler_style(table,
    name_col = "term", score_col = "ratio", pval_col = "q",
    genes_col = "genes"))
  expect_identical(plot$labels$title, "Top 2 Terms")
  expect_true(inherits(plot$scales$get_scales("x"), "ScaleContinuousPosition"))
  expect_equal(plot$data$score, c(-1, -2))
  expect_equal(plot$data$n_genes, c(3L, 2L))
})

test_that("empty exclusive sets still write zero-row historical CSV files", {
  skip_if_not_installed("SummarizedExperiment")
  skip_if_not_installed("ggvenn")
  counts <- matrix(c(5, 5, 5, 5), nrow = 1,
    dimnames = list("ENSG001", c("DSC1", "MEL1", "DSC2", "MEL2")))
  metadata <- S4Vectors::DataFrame(cell = c("DSC", "Melanocytes", "DSC", "Melanocytes"),
    row.names = colnames(counts))
  object <- SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = counts), colData = metadata)
  result <- data.frame(ENSEMBL = "ENSG001", padj = 0.01, row.names = "ENSG001")
  folder <- tempfile("ekbseq-venn-")
  plot <- suppressMessages(extract_and_plot_venn(object, result, output_dir = folder))
  expect_s3_class(plot, "ggplot")
  for (stem in c("DSC_exclusive_transcripts", "Melanocytes_exclusive_transcripts")) {
    file <- file.path(folder, paste0(stem, ".csv"))
    expect_true(file.exists(file))
    expect_equal(nrow(utils::read.csv(file)), 0L)
    expect_named(utils::read.csv(file), c("Gene", "ENSEMBL_input"))
  }
})
