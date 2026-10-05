test_that("enrichment bubble accepts ratios and numeric scores", {
  table <- data.frame(term = c("alpha", "beta"), score = c("5/200", "3/100"),
                      q = c(0.02, 0.01), genes = c("A/B", "C/D/E"))
  plot <- plot_enrichment_bubble(table, "term", "score", "q", "genes")
  expect_s3_class(plot, "ggplot")
  expect_equal(sort(plot$data$.score), sort(c(0.025, 0.03)))
  expect_equal(sort(plot$data$.members), c(2L, 3L))
})

test_that("enrichment table conversion retains historical sheet names", {
  results <- list(C1 = data.frame(term = "GO:0001", row.names = "GO:0001"))
  tables <- enrichment_to_tables(results, results)
  expect_named(tables, c("C1_GO", "C1_simplified"))
  expect_true("ID" %in% names(tables$C1_GO))
})
