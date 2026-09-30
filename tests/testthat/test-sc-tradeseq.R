test_that("pattern plot uses the given lineage colors without fitting a GAM", {
  data <- expand.grid(cluster = c("a", "b"), gene = c("G1", "G2"),
                      lineage = c("L1", "L2"), pseudotime = seq(0, 1, length.out = 5))
  data$expr <- data$pseudotime
  colors <- c(L1 = "red", L2 = "blue")
  expect_s3_class(plot_tradeseq_patterns(data, colors), "ggplot")
  expect_s3_class(plot_pattern_clusters_tradeseq(data, colors = colors,
                                                   collapse = TRUE), "ggplot")
})
