make_bulk_test_dds <- function(batch, condition, seed = 1200L) {
  set.seed(seed)
  n_genes <- 120L
  n_samples <- length(condition)
  batch <- factor(batch, levels = c("A", "B"))
  condition <- factor(condition, levels = c("control", "treated"))
  base_mu <- seq(80, 160, length.out = n_genes)
  mu <- matrix(rep(base_mu, n_samples), nrow = n_genes)
  mu[1:30, condition == "treated"] <- mu[1:30, condition == "treated"] * 2
  mu[31:60, batch == "B"] <- mu[31:60, batch == "B"] * 4
  counts <- matrix(stats::rnbinom(n_genes * n_samples, mu = as.vector(mu), size = 30), nrow = n_genes,
                   dimnames = list(paste0("g", seq_len(n_genes)), paste0("s", seq_len(n_samples))))
  samples <- data.frame(batch = batch, condition = condition, row.names = colnames(counts))
  dds <- DESeq2::DESeqDataSetFromMatrix(counts, samples, design = ~ batch + condition)
  DESeq2::DESeq(dds, quiet = TRUE, fitType = "mean")
}

expect_same_deseq_results <- function(observed, expected, tolerance = 0) {
  columns <- intersect(c("log2FoldChange", "lfcSE", "stat", "pvalue", "padj"),
                       intersect(colnames(observed), colnames(expected)))
  testthat::expect_equal(as.data.frame(observed)[, columns, drop = FALSE],
                         as.data.frame(expected)[, columns, drop = FALSE], tolerance = tolerance)
}

test_that("canonical factor contrast matches DESeq2 in a balanced additive design", {
  skip_if_not_installed("DESeq2")
  dds <- make_bulk_test_dds(batch = c("A", "B", "A", "B", "A", "B", "A", "B"),
                            condition = c(rep("control", 4), rep("treated", 4)), seed = 1201L)

  expected <- DESeq2::results(dds, contrast = c("condition", "treated", "control"), alpha = 0.05, lfcThreshold = 0)
  observed <- deseq_contrast(dds, treatment = "treated", control = "control", condition = "condition")
  expect_same_deseq_results(observed, expected)

  legacy_contrast <- suppressMessages(contraster(dds, group1 = list(c("condition", "treated")),
                                                  group2 = list(c("condition", "control"))))
  batch_coef <- grep("^batch", names(legacy_contrast), value = TRUE)
  condition_coef <- grep("^condition", names(legacy_contrast), value = TRUE)
  expect_true(length(batch_coef) == 1L)
  expect_true(length(condition_coef) == 1L)
  expect_equal(unname(legacy_contrast[batch_coef]), 0, tolerance = 0)
  expect_equal(unname(legacy_contrast[condition_coef]), 1, tolerance = 0)
})

test_that("canonical factor contrast ignores observed batch composition", {
  skip_if_not_installed("DESeq2")
  dds <- make_bulk_test_dds(batch = c("A", "A", "B", "B", "B", "B", "B", "B"),
                            condition = c(rep("control", 4), rep("treated", 4)), seed = 1202L)

  expected <- DESeq2::results(dds, contrast = c("condition", "treated", "control"), alpha = 0.05, lfcThreshold = 0)
  observed <- deseq_contrast(dds, treatment = "treated", control = "control", condition = "condition")
  expect_same_deseq_results(observed, expected)

  legacy_contrast <- suppressMessages(contraster(dds, group1 = list(c("condition", "treated")),
                                                  group2 = list(c("condition", "control"))))
  batch_coef <- grep("^batch", names(legacy_contrast), value = TRUE)
  expect_true(length(batch_coef) == 1L)
  expect_gt(abs(unname(legacy_contrast[batch_coef])), 0)

  legacy <- DESeq2::results(dds, contrast = legacy_contrast, alpha = 0.05, lfcThreshold = 0)
  delta <- abs(as.data.frame(legacy)$log2FoldChange - as.data.frame(expected)$log2FoldChange)
  expect_gt(max(delta, na.rm = TRUE), 1e-6)
})

test_that("ashr shrinkage uses the same direct DESeq2 factor contrast", {
  skip_if_not_installed("DESeq2")
  skip_if_not_installed("ashr")
  dds <- make_bulk_test_dds(batch = c("A", "A", "B", "B", "B", "B", "B", "B"),
                            condition = c(rep("control", 4), rep("treated", 4)), seed = 1203L)

  raw <- DESeq2::results(dds, contrast = c("condition", "treated", "control"), alpha = 0.05, lfcThreshold = 0)
  expected <- DESeq2::lfcShrink(dds, res = raw, type = "ashr", quiet = TRUE)
  observed <- deseq_contrast(dds, treatment = "treated", control = "control",
                             condition = "condition", shrink = TRUE)
  expect_same_deseq_results(observed, expected, tolerance = 1e-12)
})

test_that("legacy contrast wrappers retain frozen historical behavior", {
  skip_if_not_installed("DESeq2")
  dds <- make_bulk_test_dds(batch = c("A", "A", "B", "B", "B", "B", "B", "B"),
                            condition = c(rep("control", 4), rep("treated", 4)), seed = 1204L)
  frozen_contraster <- getFromNamespace(".legacy_frozen_contraster", "ekbSeq")
  frozen_apply <- getFromNamespace(".legacy_frozen_apply_contrasts", "ekbSeq")

  group1 <- list(c("condition", "treated"))
  group2 <- list(c("condition", "control"))
  expected_contrast <- frozen_contraster(dds, group1 = group1, group2 = group2, weighted = FALSE)
  observed_contrast <- suppressMessages(contraster(dds, group1 = group1, group2 = group2, weighted = FALSE))
  expect_equal(observed_contrast, expected_contrast, tolerance = 0)

  expected <- suppressMessages(frozen_apply(dds, trt = "treated", ctrl = "control", condition = "condition"))
  observed <- suppressMessages(apply_contrasts(dds, trt = "treated", ctrl = "control", condition = "condition"))
  expect_same_deseq_results(observed, expected, tolerance = 0)
  expect_false("make_deseq_contrast" %in% getNamespaceExports("ekbSeq"))
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
