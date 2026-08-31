#' Save a Plot as PNG and SVG Files Simultaneously
#'
#' This function saves a plot in both PNG and SVG formats. It supports ggplot2
#' objects, patchwork/ggarrange-like objects, base R plotting functions or
#' expressions, and grid-based plots such as ComplexHeatmap.
#'
#' PNG export uses either `ggsave()` for ggplot-like objects or a Cairo-based
#' PNG device for base/grid plotting expressions. SVG export uses either
#' `svglite::svglite()`, `Cairo::CairoSVG()`, or the base R `svg()` device.
#'
#' @param filename_base Character. Base file path without file extension.
#' @param plot_expr A plot object, function, or quoted expression to be saved.
#' @param width Numeric. Width of the plot in inches. Default is 8.
#' @param height Numeric. Height of the plot in inches. Default is 6.
#' @param dpi Numeric. Resolution in dots per inch for PNG output and rasterized layers. Default is 600.
#' @param font Character. Font family used for SVG output. Default is `"Liberation Sans"`.
#' @param rasterize Logical. If TRUE and `ggrastr` is installed, ggplot geometry layers are rasterized inside the SVG. Default is TRUE.
#' @param grid_capture Logical. If TRUE, captures grid-based plots with `grid::grid.grabExpr()` before SVG export, which can prevent empty SVG output for ComplexHeatmap-style plots. Default is FALSE.
#' @param svg_backend Character. SVG backend to use. `"svglite"` keeps text highly editable in Inkscape, `"cairo"` can be useful for some non-ggplot outputs, and `"base"` uses `grDevices::svg()` as a fallback for base graphics. Default is `"svglite"`.
#' @param base_graphics Logical. If TRUE, the plot is exported using direct base graphics devices without `grid::grid.newpage()` or grid capture. This is useful for base R or igraph-style plots such as CellChat network plots. Default is FALSE.
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
export_plot_dual <- function(filename_base,
                             plot_expr = NULL,
                             width = 8,
                             height = 6,
                             dpi = 600,
                             font = "Liberation Sans",
                             rasterize = TRUE,
                             grid_capture = FALSE,
                             svg_backend = c("svglite", "cairo", "base"),
                             base_graphics = FALSE) {

  svg_backend <- match.arg(svg_backend)
  caller_env <- parent.frame()

  output_dir <- dirname(filename_base)
  if (!identical(output_dir, ".") && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  .eval_plot <- function() {
    if (is.function(plot_expr)) {
      res <- plot_expr()
    } else {
      res <- eval(plot_expr, envir = caller_env)
    }

    if (inherits(res, "grob")) {
      grid::grid.draw(res)
    }

    invisible(res)
  }

  .eval_base_plot <- function() {
    if (is.function(plot_expr)) {
      plot_expr()
    } else {
      eval(plot_expr, envir = caller_env)
    }

    invisible(NULL)
  }

  .open_svg_device <- function(filepath) {
    if (identical(svg_backend, "svglite")) {

      svglite::svglite(
        file = filepath,
        width = width,
        height = height,
        system_fonts = list(sans = font),
        fix_text_size = FALSE,
        bg = "white"
      )

    } else if (identical(svg_backend, "cairo")) {

      if (!requireNamespace("Cairo", quietly = TRUE)) {
        stop(
          "The Cairo package is required when svg_backend = 'cairo'. ",
          "Install it with: install.packages('Cairo')"
        )
      }

      Cairo::CairoSVG(
        filename = filepath,
        width = width,
        height = height,
        bg = "white"
      )

    } else if (identical(svg_backend, "base")) {

      grDevices::svg(
        filename = filepath,
        width = width,
        height = height,
        bg = "white",
        onefile = FALSE
      )
    }
  }

  .save_svg_plot_object <- function(filepath, p) {
    .open_svg_device(filepath)
    print(p)
    dev.off()
  }

  # Direct base graphics export path.
  # This must not use grid.newpage(), because base graphics and grid graphics
  # use different drawing systems.
  if (base_graphics) {

    png(
      filename = paste0(filename_base, ".png"),
      width = width,
      height = height,
      units = "in",
      res = dpi,
      type = "cairo",
      bg = "white"
    )
    .eval_base_plot()
    dev.off()

    .open_svg_device(paste0(filename_base, ".svg"))
    .eval_base_plot()
    dev.off()

    return(invisible(NULL))
  }

  is_gg_like <- inherits(plot_expr, c("gg", "ggplot", "ggarrange", "grob", "patchwork"))

  if (is_gg_like && !grid_capture) {

    # PNG export for ggplot-like objects
    ggplot2::ggsave(
      filename = paste0(filename_base, ".png"),
      plot = plot_expr,
      width = width,
      height = height,
      dpi = dpi,
      bg = "white"
    )

    # SVG export with optional rasterization of heavy ggplot geometry layers
    if (rasterize && requireNamespace("ggrastr", quietly = TRUE)) {
      plot_svg <- ggrastr::rasterise(plot_expr, dpi = dpi, dev = "ragg")
    } else {
      if (rasterize && !requireNamespace("ggrastr", quietly = TRUE)) {
        message(
          "ggrastr was not found - SVG will be saved without rasterization.\n",
          "Install it with: install.packages('ggrastr')"
        )
      }
      plot_svg <- plot_expr
    }

    .save_svg_plot_object(paste0(filename_base, ".svg"), plot_svg)

  } else {

    # PNG export for grid or ComplexHeatmap-style plots
    png(
      filename = paste0(filename_base, ".png"),
      width = width,
      height = height,
      units = "in",
      res = dpi,
      type = "cairo",
      bg = "white"
    )
    grid::grid.newpage()
    .eval_plot()
    dev.off()

    # SVG export
    if (grid_capture) {

      # Capture grid-based plots first, then redraw the captured grob into the SVG device.
      # This is useful for ComplexHeatmap and similar grid-based plots.
      plot_grob <- grid::grid.grabExpr(
        {
          grid::grid.newpage()
          .eval_plot()
        },
        width = width,
        height = height
      )

      .open_svg_device(paste0(filename_base, ".svg"))
      grid::grid.newpage()
      grid::grid.draw(plot_grob)
      dev.off()

    } else {

      # Direct SVG export for grid-like plotting functions or expressions.
      .open_svg_device(paste0(filename_base, ".svg"))
      grid::grid.newpage()
      .eval_plot()
      dev.off()
    }
  }

  invisible(NULL)
}
