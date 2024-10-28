#' Function to define complex contrasts in DESeq results function.
#'
#' Function is taken from https://www.atakanekiz.com/technical/a-guide-to-designs-and-contrasts-in-DESeq2/ to allow complex
#' contrasts including difference of differences and individual comparisons
#' @param dds DESeq object containing colData and design
#' @param group1 list of character vectors each with 2 or more items
#' @param group2 list of character vectors each with 2 or more items
#' @param weighted logical indicating whether weighted contrasts should be applied. Default is FALSE.
#' @export

contraster <- function(dds, group1, group2, weighted = F){

  mod_mat <- stats::model.matrix(DESeq2::design(dds), SummarizedExperiment::colData(dds))

  grp1_rows <- list()
  grp2_rows <- list()

  for(i in 1:length(group1)){

    grp1_rows[[i]] <- colData(dds)[[group1[[i]][1]]] %in% group1[[i]][2:length(group1[[i]])]

  }

  for(i in 1:length(group2)){

    grp2_rows[[i]] <- colData(dds)[[group2[[i]][1]]] %in% group2[[i]][2:length(group2[[i]])]

  }
  grp1_rows <- Reduce(function(x, y) x & y, grp1_rows)
  grp2_rows <- Reduce(function(x, y) x & y, grp2_rows)

  mod_mat1 <- mod_mat[grp1_rows, ,drop=F]
  mod_mat2 <- mod_mat[grp2_rows, ,drop=F]

  if(!weighted){
    mod_mat1 <- mod_mat1[!duplicated(mod_mat1),,drop=F]
    mod_mat2 <- mod_mat2[!duplicated(mod_mat2),,drop=F]
  }
  return(colMeans(mod_mat1)-colMeans(mod_mat2))
}
