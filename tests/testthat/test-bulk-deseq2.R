test_that("contrast direction and interaction design follow historical model matrix", {
  skip_if_not_installed("DESeq2")
  count <- matrix(rep(c(20L, 30L, 45L, 55L), each = 20), nrow = 20,
                  dimnames = list(paste0("g", 1:20), paste0("s", 1:4)))
  samples <- data.frame(condition = factor(c("ctrl", "ctrl", "trt", "trt")),
                        batch = factor(c("a", "b", "a", "b")), row.names = colnames(count))
  dds <- DESeq2::DESeqDataSetFromMatrix(count, samples, design = ~ batch + condition)
  contrast <- make_deseq_contrast(dds, list(c("condition", "trt")),
                                  list(c("condition", "ctrl")))
  expected <- colMeans(stats::model.matrix(DESeq2::design(dds), samples)[3:4, , drop = FALSE]) -
    colMeans(stats::model.matrix(DESeq2::design(dds), samples)[1:2, , drop = FALSE])
  expect_equal(contrast, expected)
  expect_equal(contraster(dds, list(c("condition", "trt")),
                          list(c("condition", "ctrl"))), contrast)
})

test_that("expression overlap retains exclusive and shared genes", {
  counts <- matrix(c(9, 0, 8, 0, 0, 7, 8, 0), nrow = 2,
                   dimnames = list(c("g1", "g2"), paste0("s", 1:4)))
  results <- data.frame(padj = c(0.01, 0.02), row.names = rownames(counts))
  overlap <- compare_expression_sets(counts, results,
                    list(A = c("s1", "s2"), B = c("s3", "s4")), expression_threshold = 1)
  expect_setequal(overlap$sets$A, "g1")
  expect_setequal(overlap$sets$B, c("g1", "g2"))
  expect_setequal(overlap$shared, "g1")
})
