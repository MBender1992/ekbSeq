#' Function to plot the first 2 principal components of a rlog or vst transformed count matrix.
#'
#' Output is a ggplot object showing PC1 and PC2 for a RNASeq experiment. For a 1 factor design, points are colored by this group,
#' for a 2 factor design the first factor is represented by colors the second factor by shape.
#' @param data Transformed count matrix.
#' @param percentVar Explained variance by PC1 and PC2
#' @param type Specify type of transformation which was used for generation of count matrix.
#' @param pointSize Size of points.
#' @param textSize Size of plot text.
#' @param labelled logical indicating whether points or text labels should be plotted. Default is FALSE to plot points.
#' @param fct1 main factor of interest in the design formula
#' @param fct2 covariate which is adjusted for if included in the design formula
#' @export

pca_plot <- function(data, percentVar, type, pointSize = 3, textSize= 12, labelled = FALSE, fct1, fct2 = NULL){
  name <- NULL
  p <- ggplot(data, aes_string(x = "PC1", y = "PC2", color = fct1, shape = fct2)) +
    geom_point(size =pointSize) +
    xlab(paste0("PC1: ", percentVar[1], "% variance")) +
    ylab(paste0("PC2: ", percentVar[2], "% variance")) +
    ggtitle(paste("PCA with", type,"data")) +
    theme_bw(base_size = textSize) +
    ggsci::scale_color_npg()
  if(labelled == TRUE) p + geom_label(aes(label = name)) else p
}
