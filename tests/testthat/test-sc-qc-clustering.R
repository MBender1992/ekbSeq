test_that("local mixing is one for a single sample group", {
  skip_if_not_installed("Seurat")
  skip_if_not_installed("FNN")
  counts <- matrix(rpois(200, 5), nrow = 10,
                   dimnames = list(paste0("g", 1:10), paste0("c", 1:20)))
  object <- Seurat::CreateSeuratObject(counts)
  object$sample_id <- "same"
  embedding <- cbind(UMAP_1 = seq_len(20), UMAP_2 = seq_len(20)^2)
  rownames(embedding) <- colnames(object)
  object[["umap.unintegrated"]] <- SeuratObject::CreateDimReducObject(
    embeddings = embedding,
    key = "UMAP_", assay = "RNA")
  expect_equal(calculate_local_mixing(object, k = 3), 1)
  expect_equal(calculate_mixing_metric(object, k = 3), 1)
})
