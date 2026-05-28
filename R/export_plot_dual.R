#' Save a Plot as PNG and SVG Files Simultaneously
#'
#' This function saves a plot in both PNG and SVG formats using either `ggsave()` for **ggplot2** objects or base R graphics devices for base plotting expressions or functions.
#'
#' PNG export uses `ggsave()` for ggplot objects. SVG export uses `grDevices::svg()` with
#' Cairo backend, which ensures text elements are stored without scale() transforms,
#' making them fully and correctly editable in Inkscape (font size changes scale uniformly,
#' no squishing). For point-heavy plots (e.g. UMAPs), `ggrastr` is used automatically to
#' rasterize geometry layers inside the SVG, keeping Inkscape responsive.
#'
#' @param filename_base Character. Base file path (without extension) where the plots will be saved.
#' @param plot_expr A plot object, function, or expression to be saved.
#'   - If a ggplot object (class `"gg"`, `"ggplot"`, or `"ggarrange"`), it is saved via `ggsave()` (PNG) and `grDevices::svg()` (SVG).
#'   - If a function or quoted expression, it is evaluated inside `png()` and `grDevices::svg()` devices for base R plots.
#' @param width Numeric. Width of the plot in inches. Default is 8.
#' @param height Numeric. Height of the plot in inches. Default is 6.
#' @param dpi Numeric. Resolution (dots per inch) for PNG output and rasterized layers. Default is 600.
#' @param font Character. Font family for SVG output. Default is `"Liberation Sans"`.
#' @param rasterize Logical. If TRUE and `ggrastr` is installed, point/raster layers are
#'   rasterized inside the SVG for faster Inkscape performance. Default is TRUE.
#'
#' @return Invisibly returns `NULL`.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' export_plot_dual("my_ggplot", p)
#'
#' # Disable rasterization
#' export_plot_dual("my_ggplot", p, rasterize = FALSE)
#'
#' # Base R plot as function
#' export_plot_dual("my_baseplot", function() plot(mtcars$mpg, mtcars$wt))
#'
#' # Base R plot as expression
#' export_plot_dual("my_baseplot_expr", quote(plot(mtcars$mpg, mtcars$wt)))
#' }
#'
#' @export
export_plot_dual <- function(filename_base, plot_expr = NULL, width = 8, height = 6,
                             dpi = 600, font = "Liberation Sans", rasterize = TRUE) {

  .svg_device <- function(filepath, p) {
    # fix_text_size = FALSE removes textLength from the SVG
    svglite::svglite(filepath,
                     width = width,
                     height = height,
                     system_fonts = list(sans = font),
                     fix_text_size = FALSE)
    print(p)
    dev.off()
  }

  .eval_plot <- function() {
    if (is.function(plot_expr)) {
      res <- plot_expr()
    } else {
      res <- eval(plot_expr)
    }
    if (inherits(res, "grob")) grid::grid.draw(res)
  }

  if (inherits(plot_expr, c("gg", "ggplot", "ggarrange", "grob"))) {

    # PNG
    ggsave(paste0(filename_base, ".png"), plot_expr,
           width = width, height = height, dpi = dpi, bg = "white")

    # SVG: optional ggrastr rasterizing
    if (rasterize && requireNamespace("ggrastr", quietly = TRUE)) {
      plot_svg <- ggrastr::rasterise(plot_expr, dpi = dpi, dev = "ragg")
    } else {
      if (rasterize && !requireNamespace("ggrastr", quietly = TRUE)) {
        message("ggrastr nicht gefunden - SVG wird ohne Rasterisierung gespeichert.\n",
                "Installieren mit: install.packages('ggrastr')")
      }
      plot_svg <- plot_expr
    }

    .svg_device(paste0(filename_base, ".svg"), plot_svg)

  } else {

    # PNG
    png(paste0(filename_base, ".png"), width = width, height = height, units = "in", res = dpi)
    .eval_plot()
    dev.off()

    # SVG
    svglite::svglite(paste0(filename_base, ".svg"),
                     width = width,
                     height = height,
                     system_fonts = list(sans = font),
                     fix_text_size = FALSE)
    grid::grid.newpage()
    .eval_plot()
    dev.off()

  }

  invisible(NULL)
}
