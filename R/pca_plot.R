#' Function to plot the first 2 principal components of a rlog or vst transformed count matrix.
#'
#' Output is a ggplot object showing PC1 and PC2 for a RNASeq experiment. For a 1 factor design, points are colored by this group,
#' for a 2 factor design the first factor is represented by colors the second factor by shape.
#' @param data Transformed count matrix.
#' @param type Specify type of transformation which was used for generation of count matrix.
#' @param pointSize Size of points.
#' @param textSize Size of plot text.
#' @param title plot title.
#' @param subtitle plot subtitle.
#' @param labelled logical indicating whether points or text labels should be plotted. Default is FALSE to plot points.
#' @inheritParams DESeq2::plotPCA
#' @export

pca_plot <- function(data, pcsToUse, title = "", subtitle = "",  type = "VST", pointSize = 3, textSize= 12, labelled = FALSE, intgroup){
  pca <- DESeq2::plotPCA(data, intgroup = intgroup, returnData = TRUE, pcsToUse = pcsToUse)
  percentVar <- round(100 * attr(pca, "percentVar"))
  name <- NULL
  p <- ggplot(pca, aes_string(x = paste0("PC", pcsToUse[1]), y = paste0("PC", pcsToUse[2]), color = intgroup[1], shape = intgroup[2])) +
    geom_point(size =pointSize) +
    labs(title = title, subtitle = subtitle) +
    xlab(paste0("PC", pcsToUse[1], ": ", percentVar[1], "% variance")) +
    ylab(paste0("PC", pcsToUse[2],": ", percentVar[2], "% variance")) +
    ggtitle(title) + # remove if function throws error
    theme_bw(base_size = textSize) +
    ggsci::scale_color_npg()
  if(labelled == TRUE) p + geom_label(aes(label = name)) else p
}

