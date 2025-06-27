#' Save a Plot as PNG and SVG Files Simultaneously
#'
#' This function saves a plot in both PNG and SVG formats using either `ggsave()` for **ggplot2** objects or base R graphics devices for base plotting expressions or functions.
#'
#' If a **ggplot** object or compatible plot object (e.g., `ggarrange`) is provided, it uses `ggsave()` to save the plot.
#' For base R plots, you can pass either a plotting expression (quoted) or a plotting function which will be evaluated inside the device context.
#'
#' @param filename_base Character. Base file path (without extension) where the plots will be saved.
#' @param plot_expr A plot object, function, or expression to be saved.
#'   - If a ggplot object (class `"gg"`, `"ggplot"`, or `"ggarrange"`), it is saved via `ggsave()`.
#'   - If a function or quoted expression, it is evaluated inside `png()` and `svg()` devices for base R plots.
#' @param width Numeric. Width of the plot in inches. Default is 8.
#' @param height Numeric. Height of the plot in inches. Default is 6.
#' @param dpi Numeric. Resolution (dots per inch) for PNG output. Default is 600.
#'
#' @details
#' The function automatically adds `.png` and `.svg` extensions to `filename_base` and saves the plot in both formats.
#' For **ggplot2** objects, `ggsave()` is used for both formats with appropriate arguments.
#' For base plotting, the plot is drawn inside `png()` and `svg()` graphic devices using the provided expression or function.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' export_plot_dual2("my_ggplot", p)
#'
#' # Using a base plot as a function
#' export_plot_dual2("my_baseplot", function() plot(mtcars$mpg, mtcars$wt))
#'
#' # Using a base plot as an expression
#' export_plot_dual2("my_baseplot_expr", quote(plot(mtcars$mpg, mtcars$wt)))
#' }
#'
#' @export

export_plot_dual <- function(filename_base, plot_expr = NULL, width = 8, height = 6, dpi = 600) {
  if (inherits(plot_expr, c("gg", "ggplot", "ggarrange", "grob"))) {
    ggsave(paste0(filename_base, ".png"), plot_expr,  width = width, height = height, dpi = dpi)
    ggsave(paste0(filename_base, ".svg"), plot_expr,  width = width, height = height, device = "svg")
  } else {
    # Save as PNG
    png(paste0(filename_base, ".png"), width = width, height = height, unit = "in", res = dpi)
    eval(plot_expr)
    dev.off()

    # Save as SVG
    svg(paste0(filename_base, ".svg"), width = width, height = height)
    eval(plot_expr)
    dev.off()
  }
}
