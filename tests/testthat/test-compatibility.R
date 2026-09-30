test_that("legacy information is emitted once per function", {
  pkg <- asNamespace("ekbSeq")
  seen <- get(".ekbseq_legacy_seen", envir = pkg)
  rm(list = ls(seen), envir = seen)
  info <- get(".legacy_api_message", envir = pkg)
  expect_message(info("test_name", "replacement"), "legacy ekbSeq API")
  expect_silent(info("test_name", "replacement"))
})

test_that("historical omitted object is resolved from caller", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("ggpubr")
  skip_if_not_installed("scCustomize")
  skip_if_not_installed("RColorBrewer")
  blues9 <- RColorBrewer::brewer.pal(9, "Blues")
  counts <- matrix(rpois(400, 4), nrow = 20,
                   dimnames = list(paste0("g", 1:20), paste0("cell", 1:20)))
  integrated_seurat <- Seurat::CreateSeuratObject(counts)
  integrated_seurat$timepoint <- "day0"
  integrated_seurat$harmony_clusters <- "0"
  integrated_seurat <- Seurat::NormalizeData(integrated_seurat)
  integrated_seurat <- Seurat::FindVariableFeatures(integrated_seurat)
  integrated_seurat <- Seurat::ScaleData(integrated_seurat)
  integrated_seurat <- Seurat::RunPCA(integrated_seurat, npcs = 5)
  integrated_seurat <- Seurat::RunUMAP(integrated_seurat, dims = 1:5,
    reduction.name = "umap.harmony", n.neighbors = 5)
  expect_s3_class(plot_markers_UMAP("g1", group.by = "timepoint", label = FALSE), "ggplot")
})
