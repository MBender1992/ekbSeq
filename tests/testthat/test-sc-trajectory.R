test_that("threshold follows largest positive and negative GAM derivatives", {
  skip_if_not_installed("mgcv")
  x <- seq(0, 0.1, length.out = 100)
  df <- data.frame(Lineage1 = x,
                   score_up = plogis((x - 0.04) * 150),
                   score_down = 1 - plogis((x - 0.06) * 150),
                   lineage_assignment = 1L, timepoint = "day")
  inflection <- estimate_pseudotime_threshold(df, "Lineage1", "score_up", "inflection")
  drop <- estimate_pseudotime_threshold(df, "Lineage1", "score_down", "drop")
  expect_lt(abs(inflection - 0.04), 0.015)
  expect_lt(abs(drop - 0.06), 0.015)
  if (requireNamespace("ggrastr", quietly = TRUE) &&
      requireNamespace("ggprism", quietly = TRUE)) {
    plot <- plot_pseudotime(df, "Lineage1", score_col = "score_up")
    expect_equal(attr(plot, "threshold"), inflection)
  }
})
