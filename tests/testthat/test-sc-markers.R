test_that("canonical marker summary retains historical first-n input order", {
  genes <- data.frame(cluster = c(0, 0, 1, 1), gene = c("B", "A", "D", "C"),
                      avg_log2FC = c(1, 2, 1, 2))
  selected <- analyze_cluster_markers(genes, n = 1)$top_markers
  expect_setequal(selected$gene, c("B", "D"))
})
