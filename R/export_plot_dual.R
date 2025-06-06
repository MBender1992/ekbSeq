#' Export a Plot to Both PNG and SVG Formats
#'
#' Saves a given plot expression as both a PNG and an SVG file using base R graphics devices.
#' This is useful for workflows where vector (SVG) and raster (PNG) outputs are both needed
#' for presentations, publications, or web use.
#'
#' @param filename_base Character. Base filename (without extension) for the output files.
#' @param plot_expr Expression. A quoted plotting expression, passed using \code{quote({ ... })}.
#' @param width Integer. Width of the images in inches Default is 8
#' @param height Integer. Height of the images in inches Default is 6
#' @param dpi Integer. Resolution (dots per inch) for the PNG. Default is 96.
#'
#' @return Saves two files: \code{name.png} and \code{name.svg} in the working directory. Invisibly returns \code{NULL}.
#' @export

export_plot_dual <- function(filename_base, plot_expr, width = 8, height = 6, dpi = 600) {
  # Save as PNG
  png(paste0(filename_base, ".png"), width = width, height = height, unit = "in", res = dpi)
  eval(plot_expr)
  dev.off()

  # Save as SVG
  svg(paste0(filename_base, ".svg"), width = width, height = height)
  eval(plot_expr)
  dev.off()
}
